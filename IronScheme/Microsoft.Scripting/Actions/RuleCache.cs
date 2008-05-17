
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
using System.Diagnostics;

namespace Microsoft.Scripting.Actions {
    /// <summary>
    /// This is a cache of all generated rules (per ActionBinder)
    /// </summary>
    internal class RuleCache {
        private readonly Dictionary<DynamicAction, ActionRuleCache> _rules = new Dictionary<DynamicAction,ActionRuleCache>();

        internal void Clear() {
            _rules.Clear();
        }

        private ActionRuleCache FindActionRuleCache(DynamicAction action) {
            ActionRuleCache actionRuleCache;
            lock (this) {
                if (!_rules.TryGetValue(action, out actionRuleCache)) {
                    actionRuleCache = new ActionRuleCache();
                    _rules[action] = actionRuleCache;
                }
            }

            return actionRuleCache;
        }

        internal StandardRule<T> FindRule<T>(CodeContext callerContext, DynamicAction action, object[] args) {
            ActionRuleCache actionRuleCache = FindActionRuleCache(action);

            return actionRuleCache.FindRule<T>(callerContext, args);
        }

        internal StandardRule<T> ExecuteRuleAndUpdateSite<T>(CodeContext callerContext, DynamicAction action, object[] args, object site, ref T target, ref RuleSet<T> rules, out object result) {
            ActionRuleCache actionRuleCache = FindActionRuleCache(action);

            return actionRuleCache.ExecuteRuleAndUpdateSite<T>(callerContext, args, site, ref target, ref rules, out result);
        }

        internal void AddRule<T>(DynamicAction action, object[] args, StandardRule<T> rule) {
            ActionRuleCache actionRuleCache = FindActionRuleCache(action);
            actionRuleCache.AddRule<T>(args, rule);
        }

        /// <summary>
        /// All the cached rules for a given Action (per LanguageBinder)
        /// </summary>
        internal class ActionRuleCache {
            private Dictionary<Type, object> _trees = new Dictionary<Type,object>();

            public ActionRuleCache() {
            }

            private RuleTree<T> GetOrMakeTree<T>() {
                lock (_trees) {
                    if (!_trees.ContainsKey(typeof(T))) {
                        _trees[typeof(T)] = RuleTree<T>.MakeRuleTree();
                    }
                    return (RuleTree<T>)_trees[typeof(T)];
                }
            }

            public void AddRule<T>(object[] args, StandardRule<T> newRule) {
                GetOrMakeTree<T>().AddRule(args, newRule);
            }

            public StandardRule<T> FindRule<T>(CodeContext context, object[] args) {
                return GetOrMakeTree<T>().GetRule(context, args);
            }

            public StandardRule<T> ExecuteRuleAndUpdateSite<T>(CodeContext context, object[] args, object site, ref T target, ref RuleSet<T> rules, out object result) {
               return GetOrMakeTree<T>().ExecuteRuleAndUpdateSite(context, args, site, ref target, ref rules, out result);
            }
        }
    }
}

#endif	
