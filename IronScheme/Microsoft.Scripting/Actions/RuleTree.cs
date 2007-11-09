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
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;
using System.Threading;

namespace Microsoft.Scripting.Actions {
    /// <summary>
    /// This uses linear search to find a rule.  Clearly that doesn't scale super well.
    /// We will address this in the future.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    internal class RuleTree<T> {
        private RuleTable _ruleTable = new RuleTable();        

        public static RuleTree<T> MakeRuleTree() {
            return new RuleTree<T>();
        }

        private RuleTree() {
        }

        public StandardRule<T> GetRule(CodeContext context, object[] args) {
            object dummy;
            T target = default(T);
            RuleSet<T> rules = null;
            return GetRuleMaybeExecute(context, args, false, null, ref target, ref rules, out dummy);
        }

        public StandardRule<T> ExecuteRuleAndUpdateSite(CodeContext callerContext, object[] args, object site, ref T target, ref RuleSet<T> rules, out object result) {
            return GetRuleMaybeExecute(callerContext, args, true, site, ref target, ref rules, out result);
        }

        /// <summary>
        /// This function is a little complex:
        /// 
        /// 1.	It needs to be thread-safe and atomic.  This means the iteration over the list 
        ///     needs to happen w/o releasing a lock, and the tests also get run under lock 
        ///     (they’re simple operations and therefore are safe to do this).  But the target 
        ///     is not safe to run under a lock because it may block and cause a deadlock.  Therefore 
        ///     we need to release the lock before running the target.  
        /// 2.	The code for doing the evaluation is a little complex, and the threading issues makes 
        ///     it more complex.  Therefore factoring it into a version which just looks for a rule, 
        ///     and a version which looks and executes a rule is either going to obscure the locking issues 
        ///     or result in a reasonable duplication of code which may be error prone to maintain.
        /// 3.	The updates of the sites needs to happen before the execution of the rule.  This 
        ///     is due to both stack overflow and performance issues.  If a function goes recursive and we 
        ///     haven’t updated the recursive-call site before then we’ll repeatedly generate and evaluate 
        ///     rules until we stack overflow, and if we don’t stack overflow our perf will 
        ///     suck until the method unwinds once.
        /// 
        /// For those reasons we get the one big method  which takes 7 parameters to handle both updating 
        /// and executing.  One of those parameters is the bool flag to indicate that we should execute 
        /// the rule (that’s how that decision is made).  3 more are the result of the execution and the 
        /// target/rule list to update for the caller.  One more is the site object which we need to lock 
        /// on to make the update to the site atomic.  And finally we get the CodeContext which now flows 
        /// in instead of having RuleTree hold onto a LanguageContext.  This is because we need
        /// the full CodeContext to execute the test/target and it needs to be the real CodeContext so 
        /// we get the proper set of locals/globals flowing through.  
        /// </summary>
        private StandardRule<T> GetRuleMaybeExecute(CodeContext callerContext, object[] args, bool execute, object site, ref T target, ref RuleSet<T> rules, out object result) {
            // TODO: We can do better granularity than just types.              
            LinkedList<StandardRule<T>> ruleList = GetRuleList(args);

            if (DynamicSiteHelpers.IsBigTarget(typeof(T))) {
                args = new object[] { Tuple.MakeTuple(typeof(T).GetGenericArguments()[0], args) };
            }

            bool lockReleased = false;
            int index = 0;
            Monitor.Enter(ruleList);
            try {
                LinkedListNode<StandardRule<T>> node = ruleList.First;                
                while (node != null) {
                    StandardRule<T> rule = node.Value;
                    if (!rule.IsValid) {
                        LinkedListNode<StandardRule<T>> nodeToRemove = node;
                        node = node.Next;
                        ruleList.Remove(nodeToRemove);
                        continue;
                    }
                    PerfTrack.NoteEvent(PerfTrack.Categories.RuleEvaluation, "Evaluating " + index++ + " rule in tree");
                        
                    CodeContext tmpCtx = callerContext.Scope.GetTemporaryVariableContext(callerContext, rule.ParamVariables, args);
                    try {
                        if ((bool)rule.Test.Evaluate(tmpCtx)) {
                            // Tentative optimization of moving rule to front of list when found
                            ruleList.Remove(node);
                            ruleList.AddFirst(node);

                            if (site != null) {
                                DynamicSiteHelpers.UpdateSite<T>(callerContext, site, ref target, ref rules, rule);
                            }

                            // release the lock for calling the target which may block, we assume
                            // the test performs no synchronization
                            Monitor.Exit(ruleList);
                            lockReleased = true;

                            if (execute) {
                                result = rule.Target.Execute(tmpCtx);
                            } else {
                                result = null;
                            }

                            return rule;
                        }
                    } finally {
                        tmpCtx.Scope.TemporaryStorage.Clear();
                    }
                    
                    node = node.Next;
                }
            } finally {
                if (!lockReleased) {
                    Monitor.Exit(ruleList);
                }
            }


            PerfTrack.NoteEvent(PerfTrack.Categories.Rules, "NoMatch" + index);

            result = null;
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
