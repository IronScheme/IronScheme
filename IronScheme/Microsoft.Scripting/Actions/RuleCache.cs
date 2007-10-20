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

        private ActionRuleCache FindActionRuleCache(CodeContext callerContext, DynamicAction action) {
            ActionRuleCache actionRuleCache;
            lock (this) {
                if (!_rules.TryGetValue(action, out actionRuleCache)) {
                    actionRuleCache = new ActionRuleCache(callerContext);
                    _rules[action] = actionRuleCache;
                }
            }

            return actionRuleCache;
        }

        internal StandardRule<T> FindRule<T>(CodeContext callerContext, DynamicAction action, object[] args) {
            ActionRuleCache actionRuleCache = FindActionRuleCache(callerContext, action);
            StandardRule<T> rule = actionRuleCache.FindRule<T>(args);
            if (rule == null || !rule.IsValid) {
                return null;
            }

            return rule;
        }

        internal void AddRule<T>(DynamicAction action, object[] args, StandardRule<T> rule) {
            ActionRuleCache actionRuleCache = FindActionRuleCache(null, action);
            actionRuleCache.AddRule<T>(args, rule);
        }

        /// <summary>
        /// All the cached rules for a given Action (per LanguageBinder)
        /// </summary>
        internal class ActionRuleCache {
            private LanguageContext _context;
            private Dictionary<Type, object> _trees = new Dictionary<Type,object>();

            public ActionRuleCache(CodeContext context) {
                Debug.Assert(context != null);
                _context = context.LanguageContext;
            }

            private RuleTree<T> GetOrMakeTree<T>() {
                lock (_trees) {
                    if (!_trees.ContainsKey(typeof(T))) {
                        _trees[typeof(T)] = RuleTree<T>.MakeRuleTree(_context);
                    }
                    return (RuleTree<T>)_trees[typeof(T)];
                }
            }

            public void AddRule<T>(object[] args, StandardRule<T> newRule) {
                RuleTree<T> rules = GetOrMakeTree<T>();

                // These locks are used to protect the internal dictionaries in the RuleTreeNode types
                // It should be investigated if there is a lock-free read design that could work here
                lock (rules) {
                    rules.AddRule(args, newRule);
                }
            }

            public StandardRule<T> FindRule<T>(object[] args) {
                RuleTree<T> rules = GetOrMakeTree<T>();

                lock (rules) {
                    return rules.GetRule(args);
                }
            }
        }
    }
}
