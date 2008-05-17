
#if FULL
/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. A 
 * copy of the license can be found in the License.html file at the root of this distribution. If 
 * you cannot locate the  Microsoft Public License, please send an email to 
 * dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 *
 *
 * ***************************************************************************/

using System;
using System.Collections.Generic;
using System.Text;

using Microsoft.Scripting.Ast;
using System.Diagnostics;
using System.Reflection.Emit;

namespace Microsoft.Scripting.Actions {
    /// <summary>
    /// Provides support for creating rules which can have runtime constants 
    /// replaced without recompiling the rule.
    /// 
    /// Template parameters can be added to a rule by calling 
    /// StandardRule.AddTemplatedConstant with a type and the value for the 
    /// current rule.  When the first templated rule is finished being constructed 
    /// calling StandardRule.GetTemplateBuilder returns a template builder which 
    /// can be used on future rules.
    /// 
    /// For future template requests the rule stil needs to be generated 
    /// (this is a current limitation due to needing to have a version
    /// of the AST w/ the correct constants at evaluation time).  Call 
    /// TempalatedRuleBuilder.MakeRuleFromTemplate with the new template parameters 
    /// (in the same order as AddTemplatedConstant was called) and the rule will be updated to 
    /// to enable code sharing.
    /// </summary>
    public class TemplatedRuleBuilder<T> {
        private StandardRule<T> _rule;

        internal TemplatedRuleBuilder(StandardRule<T> template) {
            _rule = template;
        }

        /// <summary>
        /// Updates the provided rule to use the previously created templated rule with the newly provided parameters.
        /// </summary>        
        public void CopyTemplateToRule(CodeContext context, StandardRule<T> rule) {
            if (_rule.TemplateParameterCount != rule.TemplateParameterCount) {
                throw new ArgumentException(String.Format("Incompatible rules.  Expected {0} template parameters, got {1}", _rule.TemplateParameterCount, rule.TemplateParameterCount));
            }

            Delegate existingDelegate = (Delegate)(object)_rule.MonomorphicRuleSet.GetOrMakeTarget(context);

            rule.MonomorphicRuleSet.RawTarget = CloneDelegate(rule.TemplateData, existingDelegate);
        }

        private T CloneDelegate(object[] newData, Delegate existingDelegate) {
            T dlg;
            DynamicMethod templateMethod = _rule.MonomorphicRuleSet.MonomorphicTemplate;
            if (templateMethod != null) {
                dlg = (T)(object)templateMethod.CreateDelegate(typeof(T), CloneData(existingDelegate.Target, newData));
            } else {
                dlg = (T)(object)Delegate.CreateDelegate(typeof(T), CloneData(existingDelegate.Target, newData), existingDelegate.Method);
            }
            return dlg;
        }

        private object CloneData(object data, params object[] newData) {
            Debug.Assert(data != null);

            object[] dataArr = data as object[];
            if (dataArr != null) return CopyArray(newData, dataArr);

            Tuple nt = data as Tuple;
            if (nt != null) return CopyTuple(newData, nt);

            throw new InvalidOperationException("bad data bound to delegate");
        }

        private static Tuple CopyTuple(object[] newData, Tuple oldTuple) {
            Tuple res = (Tuple)Activator.CreateInstance(oldTuple.GetType());
            for (int i = 0; i < oldTuple.Capacity; i++) {
                ITemplatedValue itv = oldTuple.GetValue(i) as ITemplatedValue;
                if (itv == null) {
                    res.SetValue(i, res.GetValue(i));
                }

                res.SetValue(i, itv.CopyWithNewValue(newData[i]));
            }
            return res;
        }

        private static object[] CopyArray(object[] newData, object[] oldData) {
            object[] res = new object[oldData.Length];
            for (int i = 0; i < oldData.Length; i++) {
                ITemplatedValue itv = oldData[i] as ITemplatedValue;
                if (itv == null) {
                    res[i] = oldData[i];
                    continue;
                }

                res[i] = itv.CopyWithNewValue(newData[itv.Index]);
            }
            return res;
        }
    }

    interface ITemplatedValue {
        int Index { get; }
        object CopyWithNewValue(object value);
        object ObjectValue { get; }
    }

    public class TemplatedValue<T> : ITemplatedValue {
        private T _value;
        private int _index;

        public TemplatedValue(T value, int index) {
            _index = index;
            _value = value;
        }

        public T Value {
            get {
                return _value;
            }
        }

        #region ITemplatedValue Members

        int ITemplatedValue.Index {
            get {
                return _index;
            }
        }

        object ITemplatedValue.CopyWithNewValue(object value) {
            return new TemplatedValue<T>((T)value, _index);
        }

        object ITemplatedValue.ObjectValue {
            get {
                return _value;
            }
        }

        #endregion
    }
}

#endif	
