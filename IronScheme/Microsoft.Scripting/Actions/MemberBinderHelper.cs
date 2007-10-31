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
using System.Text;
using System.Reflection;

using Microsoft.Scripting.Ast;

namespace Microsoft.Scripting.Actions {
    using Ast = Microsoft.Scripting.Ast.Ast;
    using Microsoft.Scripting.Utils;
    using Microsoft.Scripting.Generation;

    public class MemberBinderHelper<T, TActionKind> 
        : BinderHelper<T, TActionKind> where TActionKind : MemberAction {
        private StandardRule<T> _rule;              // the rule being produced
        private Type _strongBoxType;                // null or the specific instantiated type of StrongBox
        private object[] _args;                     // the arguments we're creating a rule for 
        private Statement _body = Ast.Empty();      // the body of the rule as it's built up
        private object _target;

        public MemberBinderHelper(CodeContext context, TActionKind action, object []args)
            : base(context, action) {
            Contract.RequiresNotNull(args, "args");
            if (args.Length == 0) throw new ArgumentException("args must have at least one member");

            _args = args;

            _target = args[0];
            if (CompilerHelpers.IsStrongBox(_target)) {
                _strongBoxType = _target.GetType();
                _target = ((IStrongBox)_target).Value;
            }

            _rule = new StandardRule<T>();
        }

        protected object Target {
            get {
                return _target;
            }
        }

        /// <summary> Gets the Expression that represents the instance we're looking up </summary>
        protected Expression Instance {
            get {
                if (_strongBoxType == null) return _rule.Parameters[0];

                return Ast.Call(
                    typeof(BinderOps).GetMethod("GetBox").MakeGenericMethod(_strongBoxType.GetGenericArguments()),
                    Ast.ConvertHelper(_rule.Parameters[0], _strongBoxType)
                );
            }
        }

        protected Type StrongBoxType {
            get {
                return _strongBoxType;
            }
            set {
                _strongBoxType = value;
            }
        }

        protected StandardRule<T> Rule {
            get {
                return _rule;
            }
            set {
                _rule = value;
            }
        }

        /// <summary> helper to grab the name of the member we're looking up as a string </summary>
        protected string StringName {
            get { return SymbolTable.IdToString(Action.Name); }
        }

        protected TrackerTypes GetMemberType(MemberGroup members, out Expression error) {
            error = null;
            TrackerTypes memberType = TrackerTypes.All;
            for (int i = 0; i < members.Count; i++) {
                MemberTracker mi = members[i];
                if (mi.MemberType != memberType) {
                    if (memberType != TrackerTypes.All) {
                        error = MakeAmbigiousMatchError(members);
                        return TrackerTypes.All;
                    }
                    memberType = mi.MemberType;
                }
            }
            return memberType;
        }

        protected Expression MakeGenericPropertyExpression() {
            return Ast.New(
                typeof(MemberAccessException).GetConstructor(new Type[] { typeof(string) }),
                Ast.Constant(StringName)
            );
        }

        protected Expression MakeIncorrectArgumentExpression(int provided, int expected) {
            return Ast.Call(
                typeof(RuntimeHelpers).GetMethod("TypeErrorForIncorrectArgumentCount", new Type[] { typeof(string), typeof(int), typeof(int) }),
                Ast.Constant(StringName),
                Ast.Constant(provided),
                Ast.Constant(expected)
            );
        }

        private static Expression MakeAmbigiousMatchError(MemberGroup members) {
            StringBuilder sb = new StringBuilder();
            foreach (MethodTracker mi in members) {
                if (sb.Length != 0) sb.Append(", ");
                sb.Append(mi.MemberType);
                sb.Append(" : ");
                sb.Append(mi.ToString());
            }

            return Ast.New(typeof(AmbiguousMatchException).GetConstructor(new Type[] { typeof(string) }),
                        Ast.Constant(sb.ToString()));
        }

        protected void MakeMissingMemberError(Type type) {
            AddToBody(Binder.MakeMissingMemberError(Rule, type, StringName));
        }

        protected void MakeReadOnlyMemberError(Type type) {
            AddToBody(Binder.MakeReadOnlyMemberError(Rule, type, StringName));
        }

        protected void MakeUndeletableMemberError(Type type) {
            AddToBody(Binder.MakeUndeletableMemberError(Rule, type, StringName));
        }

        /// <summary>
        /// There is no setter on Body.  Use AddToBody to extend it instead.
        /// </summary>
        protected Statement Body {
            get {
                return _body;
            }
        }

        /// <summary>
        /// Use this method to extend the Body.  It will create BlockStatements as needed.
        /// </summary>
        /// <param name="statement"></param>
        protected void AddToBody(Statement statement) {
            if (_body is EmptyStatement) {
                _body = statement;
            } else {
                _body = Ast.Block(_body, statement);
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1819:PropertiesShouldNotReturnArrays")] // TODO: fix
        protected object[] Arguments {
            get {
                return _args;
            }
        }
    }
}
