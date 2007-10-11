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
using System.Collections.Generic;
using System.Reflection.Emit;
using System.Reflection;
using System.Diagnostics;

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Actions {
    using Ast = Microsoft.Scripting.Ast.Ast;

    /// <summary>
    /// A dynamic test that can invalidate a rule at runtime.  The definition of an
    /// invalid rule is one whose Test will always return false.  In theory a set of
    /// validators is not needed as this could be encoded in the test itself; however,
    /// in practice it is much simpler to include these helpers.
    /// </summary>
    /// <returns>Whether or not the rule should still be considered valid.</returns>
    public delegate bool Validator();

    public abstract class StandardRule {
        internal List<Validator> _validators;
        internal Expression _test;
        internal Statement _target;

        // TODO revisit these fields and their uses when CodeBlock moves down
        internal Expression[] _parameters;
        internal Variable[] _paramVariables;
        internal List<Variable> _temps;
        internal VariableReference[] _references;
        internal List<object> _templateData;

        private bool _error;

        internal StandardRule() { }

        /// <summary>
        /// An expression that should return true iff Target should be executed
        /// </summary>
        public Expression Test {
            get { return _test; }
        }

        /// <summary>
        /// Gets the logical parameters to the dynamic site in the form of Expressions.
        /// </summary>
        public Expression[] Parameters {
            get {
                return _parameters;
            }
        }

        /// <summary>
        /// Allocates a temporary variable for use during the rule.
        /// </summary>
        public Variable GetTemporary(Type type, string name) {
            if (_temps == null) {
                _temps = new List<Variable>();
            }
            Variable ret = Variable.Temporary(SymbolTable.StringToId(name), null, type);
            _temps.Add(ret);
            return ret;
        }

        public void SetTest(Expression test) {
            Contract.RequiresNotNull(test, "test");
            if (_test != null) throw new InvalidOperationException();
            _test = test;
        }

        public Statement MakeReturn(ActionBinder binder, Expression expr) {
            // we create a temporary here so that ConvertExpression doesn't need to (because it has no way to declare locals).
            if (expr.Type != typeof(void)) {
                Variable variable = GetTemporary(expr.Type, "$retVal");
                Expression read = Ast.ReadDefined(variable);
                Expression conv = binder.ConvertExpression(read, ReturnType);
                if (conv == read) return Ast.Return(expr);

                return Ast.Return(Ast.Comma(Ast.Assign(variable, expr), conv));
            }
            return Ast.Return(binder.ConvertExpression(expr, ReturnType));
        }

        public Statement MakeError(ActionBinder binder, Expression expr) {
            _error = true;
            return Ast.Statement(Ast.Throw(expr));
        }

        public bool IsError {
            get {
                return _error;
            }
        }


        public void AddTest(Expression expression) {
            Assert.NotNull(expression);
            if (_test == null) {
                _test = expression;
            } else {
                _test = Ast.AndAlso(_test, expression);
            }
        }

        public void SetTarget(Statement target) {
            Contract.RequiresNotNull(target, "target");
            _target = target;
        }

        public void AddValidator(Validator validator) {
            if (_validators == null) _validators = new List<Validator>();
            _validators.Add(validator);
        }

        /// <summary>
        /// The code to execute if the Test is true.
        /// </summary>
        public Statement Target {
            get { return _target; }
        }

        /// <summary>
        ///  Gets the temporary variables allocated by this rule.
        /// </summary>
        internal Variable[] TemporaryVariables {
            get {
                return _temps == null ? new Variable[] { } : _temps.ToArray();
            }
        }

        /// <summary>
        /// If not valid, this indicates that the given Test can never return true and therefore
        /// this rule should be removed from any RuleSets when convenient in order to 
        /// reduce memory usage and the number of active rules.
        /// </summary>
        public bool IsValid {
            get {
                if (_validators == null) return true;

                foreach (Validator v in _validators) {
                    if (!v()) return false;
                }
                return true;
            }
        }

        public abstract Type ReturnType {
            get;
        }

        public static Expression MakeTypeTestExpression(Type t, Expression expr) {
            // we must always check for non-sealed types explicitly - otherwise we end up
            // doing fast-path behavior on a subtype which overrides behavior that wasn't
            // present for the base type.
            //TODO there's a question about nulls here
            if (CompilerHelpers.IsSealed(t) && t == expr.Type) {
                if (t.IsValueType) {
                    return Ast.True();
                }
                return Ast.NotEqual(expr, Ast.Null());
            }

            return Ast.AndAlso(
                Ast.NotEqual(
                    expr,
                    Ast.Null()),
                Ast.Equal(
                    Ast.Call(
                        expr, typeof(object).GetMethod("GetType")),
                        Ast.Constant(t)));
        }

        /// <summary>
        /// Adds a templated constant that can enable code sharing across rules.
        /// </summary>
        public Expression AddTemplatedConstant(Type type, object value) {
            Contract.RequiresNotNull(type, "type");
            if (value != null) {
                if (!type.IsAssignableFrom(value.GetType())) {
                    throw new ArgumentException("type must be assignable from value");
                }
            } else {
                if (!type.IsValueType) {
                    throw new ArgumentException("value must not be null for value types");
                }
            }

            if (_templateData == null) _templateData = new List<object>(1);
            Type genType = typeof(TemplatedValue<>).MakeGenericType(type);
            object template = Activator.CreateInstance(genType, value, _templateData.Count);

            _templateData.Add(value);
            return Ast.ReadProperty(Ast.RuntimeConstant(template), genType.GetProperty("Value"));
        }

        public Expression AddTemplatedWeakConstant(Type type, object value) {
            if (value != null) {
                if (!type.IsAssignableFrom(value.GetType())) {
                    throw new ArgumentException("type must be assignable from value");
                }
            } else {
                if (!type.IsValueType) {
                    throw new ArgumentException("value must not be null for value types");
                }
            }

            Expression expr = AddTemplatedConstant(typeof(WeakReference), new WeakReference(value));

            return Ast.ReadProperty(expr, typeof(WeakReference).GetProperty("Target"));
        }

    }

    /// <summary>
    /// A rule is the mechanism that LanguageBinders use to specify both what code to execute (the Target)
    /// for a particular action on a particular set of objects, but also a Test that guards the Target.
    /// Whenver the Test returns true, it is assumed that the Target will be the correct action to
    /// take on the arguments.
        /// 
    /// In the current design, a StandardRule is also used to provide a mini binding scope for the
    /// parameters and temporary variables that might be needed by the Test and Target.  This will
    /// probably change in the future as we unify around the notion of CodeBlocks.
    /// </summary>
    /// <typeparam name="T">The type of delegate for the DynamicSites this rule may apply to.</typeparam>
    public class StandardRule<T> : StandardRule {
        private SmallRuleSet<T> _ruleSet;

        public StandardRule() {
            int firstParameter = DynamicSiteHelpers.IsFastTarget(typeof(T)) ? 1 : 2;
            
            ParameterInfo[] pis = typeof(T).GetMethod("Invoke").GetParameters();
            if (!DynamicSiteHelpers.IsBigTarget(typeof(T))) {
                _parameters = new Expression[pis.Length - firstParameter];
                List<Variable> paramVars = new List<Variable>();
                for (int i = firstParameter; i < pis.Length; i++) {
                    Variable p = MakeParameter(i, "$arg" + (i - firstParameter), pis[i].ParameterType);
                    paramVars.Add(p);
                    _parameters[i - firstParameter] = Ast.ReadDefined(p);
                }
                _paramVariables = paramVars.ToArray();
            } else {
                MakeTupleParameters(firstParameter, typeof(T).GetGenericArguments()[0]);
            }
        }

        private void MakeTupleParameters(int firstParameter, Type tupleType) {
            int count = Tuple.GetSize(tupleType);

            Variable tupleVar = MakeParameter(firstParameter, "$arg0", tupleType);
            _paramVariables = new Variable[] { tupleVar };
            Expression tuple = Ast.ReadDefined(tupleVar);

            _parameters = new Expression[count];
            for (int i = 0; i < _parameters.Length; i++) {
                Expression tupleAccess = tuple;
                foreach (PropertyInfo pi in Tuple.GetAccessPath(tupleType, i)) {
                    tupleAccess = Ast.ReadProperty(tupleAccess, pi);
                }
                _parameters[i] = tupleAccess;
            }
        }

        /// <summary>
        /// Gets the logical parameters to the dynamic site in the form of Variables.
        /// </summary>
        internal Variable[] ParamVariables {
            get {
                return _paramVariables;
            }
        }      

        /// <summary>
        /// Gets the number of logical parameters the dynamic site is provided with.
        /// </summary>
        public int ParameterCount {
            get {
                return _parameters.Length;
            }
        }

        private Variable MakeParameter(int index, string name, Type type) {
            Variable ret = Variable.Parameter(null, SymbolTable.StringToId(name), type, null);

            ret.ParameterIndex = index;
            return ret;
        }

        /// <summary>
        /// Each rule holds onto an immutable RuleSet that contains this rule only.
        /// This should heavily optimize monomorphic call sites.
        /// </summary>
        internal SmallRuleSet<T> MonomorphicRuleSet {
            get {
                if (_ruleSet == null) {
                    _ruleSet = new SmallRuleSet<T>(new StandardRule<T>[] { this });
                }
                return _ruleSet;
            }
        }
        
        public void Emit(CodeGen cg, Label ifFalse) {
            Assert.NotNull(_test, _target);

            // Need to make sure we aren't generating into two different CodeGens at the same time
            lock (this) {
                // First, finish binding my variable references
                if (_references == null) {
                    _references = RuleBinder.Bind(_test, _target);
                }

                foreach (VariableReference vr in _references) {
                    vr.CreateSlot(cg);
                }

                if (_test != null) {
                    _test.EmitBranchFalse(cg, ifFalse);
                }

                // Now do the generation
                _target.Emit(cg);

                // free any temps now that we're done generating
                // TODO: Keep temp slots aside sot that they can be freed
                //if (_temps != null) {
                //    foreach (Variable vr in _temps) {
                //        cg.FreeLocalTmp(vr.Slot);
                //    }
                //}
            }
        }


        public override string ToString() {
            return string.Format("StandardRule({0})", _target);
        }

        public override Type ReturnType {
            get {
                return typeof(T).GetMethod("Invoke").ReturnType;
            }
        }        

        public void MakeTest(params Type[] types) {
            _test = MakeTestForTypes(types, 0);
        }

        public Expression MakeTestForTypes(Type[] types, int index) {
            Expression test = MakeTypeTest(types[index], index);
            if (index+1 < types.Length) {
                Expression nextTests = MakeTestForTypes(types, index + 1);
                if (test.IsConstant(true)) {
                    return nextTests;
                } else if (nextTests.IsConstant(true)) {
                    return test;
                } else {
                    return Ast.AndAlso(test, nextTests);
                }
            } else {
                return test;
            }
        }

        public Expression MakeTypeTest(Type type, int index) {
            return MakeTypeTest(type, Parameters[index]);
        }

        public Expression MakeTypeTest(Type type, Expression tested) {
            if (type == null || type == typeof(None)) {
                return Ast.Equal(tested, Ast.Null());
            }

            return MakeTypeTestExpression(type, tested);
        }
        
        public Expression MakeTypeTestExpression(Type t, int param) {
            return MakeTypeTestExpression(t, Parameters[param]);
        }        

        #region Factory Methods
        public static StandardRule<T> Simple(ActionBinder binder, MethodBase target, params Type[] types) {
            StandardRule<T> ret = new StandardRule<T>();
            MethodCandidate mc = MethodBinder.MakeBinder(binder, target.Name, new MethodBase[] { target }, BinderType.Normal).MakeBindingTarget(CallType.None, types);

            ret.MakeTest(types);
            ret.SetTarget(ret.MakeReturn(binder, mc.Target.MakeExpression(binder, ret, ret.Parameters, types)));
            return ret;
        }        
        
        #endregion


        /// <summary>
        /// Returns a TemplatedRuleBuilder which can be used to replace data.  See TemplatedRuleBuilder
        /// for more information.
        /// </summary>
        /// <returns></returns>
        public TemplatedRuleBuilder<T> GetTemplateBuilder() {
            if (_test == null || _target == null) throw new InvalidOperationException();
            if (_templateData == null) throw new InvalidOperationException("no template arguments created");

            return new TemplatedRuleBuilder<T>(this);
        }

        internal int TemplateParameterCount {
            get {
                if (_templateData == null) return 0;
                return _templateData.Count;
            }
        }

        internal object[] TemplateData {
            get {
                return _templateData.ToArray();
            }
        }

#if DEBUG
        public string Dump {
            get {
                using (System.IO.StringWriter writer = new System.IO.StringWriter()) {
                    AstWriter.ForceDump(Test, "Test", writer);
                    writer.WriteLine();
                    AstWriter.ForceDump(Target, "Target", writer);
                    return writer.ToString();
                }
            }
        }
#endif
    }
}
 
