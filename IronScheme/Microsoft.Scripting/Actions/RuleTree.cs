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
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Actions {
    /// <summary>
    /// This uses linear search to find a rule.  Clearly that doesn't scale super well.
    /// We will address this in the future.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class RuleTree<T> {
        private LanguageContext _context;
        private RuleTable _ruleTable = new RuleTable();        

        public static RuleTree<T> MakeRuleTree(LanguageContext context) {
            Contract.RequiresNotNull(context, "context");

            return new RuleTree<T>(context);
        }

        private RuleTree(LanguageContext context) {
            Debug.Assert(context != null);
            _context = context;
        }

        public StandardRule<T> GetRule(object[] args) {
            // TODO: We can do better granularity than just types.              
            LinkedList<StandardRule<T>> ruleList = GetRuleList(args);

            // Create fake context for evaluating the rule tests
            CodeContext context = DynamicSiteHelpers.GetEvaluationContext<T>(_context, ref args);

            LinkedListNode<StandardRule<T>> node = ruleList.First;
            int index = 0;
            while (node != null) {
                StandardRule<T> rule = node.Value;
                if (!rule.IsValid) {
                    LinkedListNode<StandardRule<T>> nodeToRemove = node;
                    node = node.Next;
                    ruleList.Remove(nodeToRemove);
                    continue;
                }

                PerfTrack.NoteEvent(PerfTrack.Categories.RuleEvaluation, "Evaluating " + index++ + " rule in tree");
                using (context.Scope.TemporaryVariableContext(rule.TemporaryVariables, rule.ParamVariables, args)) {
                    if (rule.Test == null || (bool)rule.Test.Evaluate(context)) {
                        // Tentative optimization of moving rule to front of list when found
                        ruleList.Remove(node);
                        ruleList.AddFirst(node);
                        return rule;
                    }
                }
                node = node.Next;
            }

            PerfTrack.NoteEvent(PerfTrack.Categories.Rules, "NoMatch" + index);

            return null;
        }

        private LinkedList<StandardRule<T>> GetRuleList(object[] args) {
            LinkedList<StandardRule<T>> ruleList;
            lock (_ruleTable) {
                RuleTable curTable = _ruleTable;
                foreach (object o in args) {
                    Type objType = CompilerHelpers.GetType(o);

                    if (curTable.NextTable == null) {
                        curTable.NextTable = new Dictionary<Type, RuleTable>(1);
                    }

                    RuleTable nextTable;
                    if (!curTable.NextTable.TryGetValue(objType, out nextTable)) {
                        curTable.NextTable[objType] = nextTable = new RuleTable();
                    }

                    curTable = nextTable;
                }

                if (curTable.Rules == null) {
                    curTable.Rules = new LinkedList<StandardRule<T>>();
                }

                ruleList = curTable.Rules;
            }
            return ruleList;
        }

        public void AddRule(object[] args, StandardRule<T> rule) {
            GetRuleList(args).AddLast(rule);
        }

        private class RuleTable {
            public Dictionary<Type, RuleTable> NextTable;
            public LinkedList<StandardRule<T>> Rules;
        }
    }    
}
