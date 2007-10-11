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

namespace Microsoft.Scripting.Actions {
    internal class EmptyRuleSet<T> : RuleSet<T> {
        public static RuleSet<T> Instance = new EmptyRuleSet<T>(true);
        public static RuleSet<T> FixedInstance = new EmptyRuleSet<T>(false);

        private bool _supportAdding;

        private EmptyRuleSet(bool supportAdding) {
            this._supportAdding = supportAdding;
        }

        public override RuleSet<T> AddRule(StandardRule<T> newRule) {
            if (_supportAdding) return newRule.MonomorphicRuleSet;
            else return this;
        }

        public override StandardRule<T> GetRule(CodeContext context, params object[] args) {
            return null;
        }

        public override bool HasMonomorphicTarget(T target) {
            return false;
        }

        protected override T MakeTarget(CodeContext context) {
            if (DynamicSiteHelpers.IsBigTarget(typeof(T))) {
                if (DynamicSiteHelpers.IsFastTarget(typeof(T))) {
                    return (T)(object)DynamicSiteHelpers.MakeUninitializedBigFastTarget(typeof(T));
                } else {
                    return (T)(object)DynamicSiteHelpers.MakeUninitializedBigTarget(typeof(T));
                }
            } else if (DynamicSiteHelpers.IsFastTarget(typeof(T))) {
                return (T)(object)DynamicSiteHelpers.MakeUninitializedFastTarget(typeof(T));
            } else {
                return (T)(object)DynamicSiteHelpers.MakeUninitializedTarget(typeof(T));
            }
        }
    }
}
