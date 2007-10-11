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
using System.Diagnostics;
using System.Collections.Generic;

namespace Microsoft.Scripting.Actions {

    /// <summary>
    /// A RuleSet is a collection of rules to apply to the objects at a DynamicSite.  Each Rule also
    /// includes a target that is to be called if the rules' conditions are met.
    /// RuleSets are all immutable.
    /// </summary>
    internal abstract class RuleSet<T> {
        private T _target;

        public static RuleSet<T> EmptyRules {
            get {
                return EmptyRuleSet<T>.Instance;
            }
        }

        public abstract RuleSet<T> AddRule(StandardRule<T> newRule);
        public abstract StandardRule<T> GetRule(CodeContext context, params object [] args);
        public abstract bool HasMonomorphicTarget(T target);       

        protected abstract T MakeTarget(CodeContext context);

        public T GetOrMakeTarget(CodeContext context) {
            if (_target == null) {
                _target = MakeTarget(context);
            }
            return _target;
        }

        public T RawTarget {
            get {
                return _target;
            }
            set {
                _target = value;
            }
        }
    }
}
