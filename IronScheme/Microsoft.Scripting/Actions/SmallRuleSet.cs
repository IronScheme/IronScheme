/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation. 
 *
 * This source code is subject to terms and conditions of the Microsoft Permissive License. A 
 * copy of the license can be found in the License.html file at the root of this distribution. If 
 * you cannot locate the  Microsoft Permissive License, please send an email to 
 * dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Microsoft Permissive License.
 *
 * You must not remove this notice, or any other, from this software.
 *
 *
 * ***************************************************************************/

using System;
using System.Reflection;
using System.Reflection.Emit;
using System.Diagnostics;
using System.Collections.Generic;

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Actions {

    /// <summary>
    /// This holds a set of rules for a particular DynamicSite.  Any given
    /// SmallRuleSet instance is immutable and therefore they may be cached
    /// and shared.  At the moment, the only ones that are shared are
    /// SmallRuleSets with a single rule.
    /// 
    /// When a new rule is added, then a new SmallRuleSet will be created
    /// that contains all existing rules that are still considered valid with
    /// the new rule added to the front of the list.  The target generated for
    /// this type will simply try each of the rules in order and emit the
    /// standard DynamicSite.UpdateBindingAndInvoke fallback call at the end.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    internal class SmallRuleSet<T> : RuleSet<T> {
        private const int MaxRules = 10;
        private IList<StandardRule<T>> _rules;
        private DynamicMethod _monomorphicTemplate;

        internal SmallRuleSet(IList<StandardRule<T>> rules) {
            this._rules = rules;
        }

        public override RuleSet<T> AddRule(StandardRule<T> newRule) {
            // Can the rule become invalidated between its creation and
            // its insertion into the set?
            Debug.Assert(newRule.IsValid, "Adding an invalid rule");

            IList<StandardRule<T>> newRules = new List<StandardRule<T>>();
            newRules.Add(newRule);
            foreach (StandardRule<T> rule in _rules) {
                if (rule.IsValid) {
                    newRules.Add(rule);
                }
            }
            
            if (newRules.Count > MaxRules) {
                return EmptyRuleSet<T>.FixedInstance;
            } else {
                return new SmallRuleSet<T>(newRules);
            }
        }

        public override StandardRule<T> GetRule(CodeContext context, params object[] args) {
            context = DynamicSiteHelpers.GetEvaluationContext<T>(context.LanguageContext, ref args);

            for(int i = 0; i<_rules.Count; i++) {
                StandardRule<T> rule = _rules[i];

                using (context.Scope.TemporaryVariableContext(rule.TemporaryVariables, rule.ParamVariables, args)) {
                    if (!rule.IsValid) {
                        continue;
                    }

                    if (rule.Test == null || (bool)rule.Test.Evaluate(context)) {
                        return rule;
                    }
                }
            }
            return null;
        }

        public override bool HasMonomorphicTarget(T target) {
            Debug.Assert(target != null);

            foreach (StandardRule<T> rule in _rules) {
                if (target.Equals(rule.MonomorphicRuleSet.RawTarget)) {
                    return true;
                }
            }
            return false;
        }

        protected override T MakeTarget(CodeContext context) {
            if (_rules.Count == 1 && this != _rules[0].MonomorphicRuleSet) {
                // use the monomorphic rule if we only have 1 rule.
                return _rules[0].MonomorphicRuleSet.GetOrMakeTarget(context);
            }

            PerfTrack.NoteEvent(PerfTrack.Categories.Rules, "GenerateRule");

            MethodInfo mi = typeof(T).GetMethod("Invoke");
            CodeGen cg = ScriptDomainManager.CurrentManager.Snippets.Assembly.DefineMethod(
                StubName,
                mi.ReturnType,
                ReflectionUtils.GetParameterTypes(mi.GetParameters()),
                new ConstantPool()
            );

            cg.EmitLineInfo = false;
            cg.Binder = context.LanguageContext.Binder;

            if (DynamicSiteHelpers.IsFastTarget(typeof(T))) {
                cg.ContextSlot = new PropertySlot(cg.ArgumentSlots[0], typeof(FastDynamicSite).GetProperty("Context"));
            } else {
                cg.ContextSlot = cg.ArgumentSlots[1];
            }

            foreach (StandardRule<T> rule in _rules) {
                Label nextTest = cg.DefineLabel();
                rule.Emit(cg, nextTest);
                cg.MarkLabel(nextTest);
            }
            EmitNoMatch(cg);

            if (_rules.Count == 1 &&
                this == _rules[0].MonomorphicRuleSet &&
                _rules[0].TemplateParameterCount > 0 &&
                cg.IsDynamicMethod) {
                _monomorphicTemplate = (DynamicMethod)cg.MethodInfo;
            }

            return (T)(object)cg.CreateDelegate(typeof(T));
        }


        private void EmitNoMatch(CodeGen cg) {
            for (int i = 0; i < cg.ArgumentSlots.Count; i++) {
                cg.ArgumentSlots[i].EmitGet(cg);
            }
            cg.EmitCall(cg.ArgumentSlots[0].Type, "UpdateBindingAndInvoke");
            cg.EmitReturn();
            cg.Finish();
        }

        internal DynamicMethod MonomorphicTemplate {
            get {
                return _monomorphicTemplate;
            }
        }

#if DEBUG
        private string StubName {
            get {
                Walker w = new Walker();
                _rules[0].Target.Walk(w);
                if (w.Name.Length > 1000) {
                    return w.Name.Substring(0, 1000);
                }
                return w.Name;
            }

        }

        class Walker : Ast.Walker {
            public string Name = "_stub_";

            public override bool Walk(MethodCallExpression node) {
                if (Name == "_stub_") {
                    Name = node.Method.ReflectedType + "." + node.Method.Name;
                }
                return false;
            }

            public override bool Walk(NewExpression node) {
                if (Name == "_stub_") {
                    Name = node.Constructor.DeclaringType + "..ctor";
                }
                return false;
            }

            public override bool Walk(ThrowExpression node) {
                if (Name == "_stub_") {
                    NewExpression ne = node.Exception as NewExpression;
                    MethodCallExpression callExpr;

                    if (ne != null) {
                        Name = "Error##" + ne.Constructor.DeclaringType.Name;
                        foreach (Expression arg in ne.Arguments) {
                            AddArgument(arg);
                        }
                    } else if ((callExpr = (node.Exception as MethodCallExpression)) != null) {
                        Name = "Error##" + callExpr.Method.ReflectedType + "." + callExpr.Method.Name;
                        foreach (Expression arg in callExpr.Arguments) {
                            AddArgument(arg);
                        }
                    }
                }
                return base.Walk(node);
            }

            private void AddArgument(Expression arg) {
                ConstantExpression ce = arg as ConstantExpression;
                if (ce != null) {
                    Name += ce.Value == null ? "(null)" : ce.Value.ToString().Replace('.', '_').Replace('+', '_');
                }
            }
        }
#else
        private const string StubName = "_stub_";
#endif
    }
}
