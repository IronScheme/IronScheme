
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
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Actions {
    using Ast = Microsoft.Scripting.Ast.Ast;
    using System.Diagnostics;

    class DoOperationBinderHelper<T> : BinderHelper<T, DoOperationAction> {
        private object[] _args;                                     // arguments we were created with
        private Type[] _types;                                      // types of our arguments
        private StandardRule<T> _rule = new StandardRule<T>();      // the rule we're building and returning

        private static OperatorInfo[] _infos = MakeOperatorTable(); // table of Operators, names, and alt names for looking up methods.
        
        public DoOperationBinderHelper(CodeContext context, DoOperationAction action, object[] args)
            : base(context, action) {
            _args = args;
            _types = CompilerHelpers.GetTypes(args);
            _rule.MakeTest(_types);
        }

        public StandardRule<T> MakeRule() {
            if (Action.Operation == Operators.GetItem || 
                Action.Operation == Operators.SetItem) {
                // try default member first, then look for special name methods.
                if (MakeDefaultMemberRule(Action.Operation)) {
                    return _rule;
                }
            }

            OperatorInfo info = GetOperatorInfo(Action.Operation);
            if (Action.IsComparision) {
                MakeComparisonRule(info);
            } else {
                MakeOperatorRule(info);
            }

            return _rule;
        }

        #region Comparison Operator

        private StandardRule<T> MakeComparisonRule(DoOperationBinderHelper<T>.OperatorInfo info) {
            // check the first type if it has an applicable method
            MethodInfo[] targets = GetApplicableMembers(info);            
            if (targets.Length > 0 && TryMakeBindingTarget(targets)) {
                return _rule;
            }

            // then check the 2nd type.
            targets = GetApplicableMembers(_types[1], info);
            if (targets.Length > 0 && TryMakeBindingTarget(targets)) {
                return _rule;
            }

            // try Compare: cmp(x,y) (>, <, >=, <=, ==, !=) 0
            if (TryNumericComparison(info)) {
                return _rule;
            }

            // try inverting the operator & result (e.g. if looking for Equals try NotEquals, LessThan for GreaterThan)...
            Operators revOp = GetInvertedOperator(info.Operator);
            OperatorInfo revInfo = GetOperatorInfo(revOp);
            Debug.Assert(revInfo != null);

            // try the 1st type's opposite function result negated 
            targets = GetApplicableMembers(revInfo);
            if (targets.Length > 0 && TryMakeInvertedBindingTarget(targets)) {
                return _rule;
            }

            // then check the 2nd type.
            targets = GetApplicableMembers(_types[1], revInfo);
            if (targets.Length > 0 && TryMakeInvertedBindingTarget(targets)) {
                return _rule;
            }

            // see if we're comparing to null w/ an object ref or a Nullable<T>
            if (TryMakeNullComparisonRule()) {
                return _rule;
            }

            // see if this is a primitive type where we're comparing the two values.
            if (TryPrimitiveCompare()) {
                return _rule;
            }

            SetErrorTarget(info);
            return _rule;
        }

        private bool TryNumericComparison(OperatorInfo info) {
            MethodInfo[] targets = FilterNonMethods(_types[0], Binder.GetMember(Action, _types[0], "Compare"));
            if (targets.Length > 0) {
                MethodBinder mb = MethodBinder.MakeBinder(Binder, targets[0].Name, targets, BinderType.Normal);
                MethodCandidate mc = mb.MakeBindingTarget(CallType.None, _types);
                if (mc != null) {
                    Expression call = Ast.Convert(mc.Target.MakeExpression(Binder, _rule, _rule.Parameters, _types), typeof(int));
                    switch (info.Operator) {
                        case Operators.GreaterThan: call = Ast.GreaterThan(call, Ast.Constant(0)); break;
                        case Operators.LessThan: call = Ast.LessThan(call, Ast.Constant(0)); break;
                        case Operators.GreaterThanOrEqual: call = Ast.GreaterThanEquals(call, Ast.Constant(0)); break;
                        case Operators.LessThanOrEqual: call = Ast.LessThanEquals(call, Ast.Constant(0)); break;
                        case Operators.Equals: call = Ast.Equal(call, Ast.Constant(0)); break;
                        case Operators.NotEquals: call = Ast.NotEqual(call, Ast.Constant(0)); break;
                        case Operators.Compare:
                            break;
                    }
                    _rule.SetTarget(_rule.MakeReturn(Binder, call));
                    return true;
                }
            }
            return false;
        }

        private bool TryPrimitiveCompare() {
            if (TypeUtils.GetNonNullableType(_types[0]) == TypeUtils.GetNonNullableType(_types[1]) &&
                TypeUtils.IsNumeric(_types[0])) {
                // TODO: Nullable<PrimitveType> Support
                Expression expr;
                switch (Action.Operation) {
                    case Operators.Equals:             expr = Ast.Equal(Param0, Param1); break;
                    case Operators.NotEquals:          expr = Ast.NotEqual(Param0, Param1); break;
                    case Operators.GreaterThan:        expr = Ast.GreaterThan(Param0, Param1); break;
                    case Operators.LessThan:           expr = Ast.LessThan(Param0, Param1); break;
                    case Operators.GreaterThanOrEqual: expr = Ast.GreaterThanEquals(Param0, Param1); break;
                    case Operators.LessThanOrEqual:    expr = Ast.LessThanEquals(Param0, Param1); break;
                    default: throw new InvalidOperationException();
                }
                _rule.SetTarget(_rule.MakeReturn(Binder, expr)); 
                return true;                
            }
            return false;
        }

        /// <summary>
        /// Produces a rule for comparing a value to null - supports comparing object references and nullable types.
        /// </summary>
        private bool TryMakeNullComparisonRule() {
            if (_types[0] == typeof(None)) {
                if (!_types[1].IsValueType) {
                    _rule.SetTarget(_rule.MakeReturn(Binder, Ast.Equal(_rule.Parameters[1], Ast.Constant(null))));
                } else if (_types[1].GetGenericTypeDefinition() == typeof(Nullable<>)) {
                    _rule.SetTarget(_rule.MakeReturn(Binder, Ast.ReadProperty(Param1, _types[1].GetProperty("HasValue"))));
                } else {
                    return false;
                }
                return true;
            } else if (_types[1] == typeof(None)) {
                if (!_types[0].IsValueType) {
                    _rule.SetTarget(_rule.MakeReturn(Binder, Ast.Equal(_rule.Parameters[0], Ast.Constant(null))));
                } else if (_types[0].GetGenericTypeDefinition() == typeof(Nullable<>)) {
                    _rule.SetTarget(_rule.MakeReturn(Binder, Ast.ReadProperty(Param0, _types[1].GetProperty("HasValue"))));
                } else {
                    return false;
                }
                return true;
            }
            return false;
        }

        #endregion

        #region Operator Rule

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Maintainability", "CA1502:AvoidExcessiveComplexity")] // TODO: fix
        private StandardRule<T> MakeOperatorRule(OperatorInfo info) {
            MethodInfo[] targets = GetApplicableMembers(info);
            if (targets.Length == 0) {
                targets = GetFallbackMembers(_types[0], info);
            }

            if (targets.Length > 0 && TryMakeBindingTarget(targets)) {
                return _rule;
            }

            if (_types.Length > 1) {
                targets = GetApplicableMembers(_types[1], info);
                if (targets.Length > 0 && TryMakeBindingTarget(targets)) {
                    return _rule;
                }
            }

            Operators op = CompilerHelpers.InPlaceOperatorToOperator(info.Operator) ;
            if (op != Operators.None) {
                // recurse to try and get the non-inplace action...
                return MakeOperatorRule(GetOperatorInfo(op));
            }

            if (_types.Length == 2 &&
                TypeUtils.GetNonNullableType(_types[0]) == TypeUtils.GetNonNullableType(_types[1]) &&
                TypeUtils.IsArithmetic(_types[0])) {
                // TODO: Nullable<PrimitveType> Support
                Expression expr;
                switch (info.Operator) {
                    case Operators.Add: expr = Ast.Add(Param0, Param1); break;
                    case Operators.Subtract: expr = Ast.Subtract(Param0, Param1); break;
                    case Operators.Divide: expr = Ast.Divide(Param0, Param1); break;
                    case Operators.Mod: expr = Ast.Modulo(Param0, Param1); break;
                    case Operators.Multiply:expr = Ast.Multiply(Param0, Param1); break;
                    case Operators.LeftShift: expr = Ast.LeftShift(Param0, Param1); break;
                    case Operators.RightShift: expr = Ast.RightShift(Param0, Param1); break;
                    case Operators.BitwiseAnd: expr = Ast.And(Param0, Param1); break;
                    case Operators.BitwiseOr: expr = Ast.Or(Param0, Param1); break;
                    case Operators.Xor: expr = Ast.ExclusiveOr(Param0, Param1); break;
                    default: throw new InvalidOperationException();
                }
                _rule.SetTarget(_rule.MakeReturn(Binder, expr));
                return _rule;
            } else if(_types.Length == 1) {
                if (info.Operator == Operators.Negate && TypeUtils.IsArithmetic(_types[0])) {
                    _rule.SetTarget(_rule.MakeReturn(Binder, Ast.Negate(Param0)));
                    return _rule;
                } else if (info.Operator == Operators.Not && TypeUtils.IsIntegerOrBool(_types[0])) {
                    _rule.SetTarget(_rule.MakeReturn(Binder, Ast.Not(Param0)));
                    return _rule;
                }
            }
            
            if (info.Operator == Operators.IsTrue) {
                if (_types[0] == typeof(bool)) {
                    _rule.SetTarget(_rule.MakeReturn(Binder, Param0));
                    return _rule;
                }
            }

            SetErrorTarget(info);
            return _rule;
        }

        #endregion

        #region Indexer Rule

        private bool MakeDefaultMemberRule(Operators oper) {
            if (_types[0].IsArray) {
                if (Binder.CanConvertFrom(_types[1], typeof(int), NarrowingLevel.All)) {
                    if(oper == Operators.GetItem) {
                        _rule.SetTarget(_rule.MakeReturn(Binder,
                            Ast.ArrayIndex(
                                Param0,
                                ConvertIfNeeded(Param1, typeof(int))
                            )
                        ));
                    } else {
                        _rule.SetTarget(_rule.MakeReturn(Binder,
                            Ast.AssignArrayIndex(
                                Param0,
                                ConvertIfNeeded(Param1, typeof(int)),
                                ConvertIfNeeded(Param2, _types[0].GetElementType())
                            )
                        ));
                    }
                    return true;
                }
            }
            
            MethodInfo[] defaults = GetMethodsFromDefaults(_types[0].GetDefaultMembers(), oper);
            if (defaults.Length != 0) {
                MethodBinder binder = MethodBinder.MakeBinder(Binder,
                    oper == Operators.GetItem ? "get_Item" : "set_Item",
                    defaults,
                    BinderType.Normal);

                MethodCandidate cand = binder.MakeBindingTarget(CallType.ImplicitInstance, _types);

                if (cand != null) {
                    if (oper == Operators.GetItem) {
                        _rule.SetTarget(_rule.MakeReturn(Binder, cand.Target.MakeExpression(Binder, _rule, _rule.Parameters, _types)));
                    } else {

                        _rule.SetTarget(_rule.MakeReturn(Binder,
                            Ast.Comma(0,
                                _rule.Parameters[2],
                                cand.Target.MakeExpression(Binder, _rule, _rule.Parameters, _types)
                            )
                        ));
                    }
                } else {
                    _rule.SetTarget(Binder.MakeInvalidParametersError(binder, Action, CallType.None, defaults, _rule, _args));
                }
                return true;
            }
            
            return false;
        }

        private MethodInfo[] GetMethodsFromDefaults(MemberInfo[] defaults, Operators op) {
            List<MethodInfo> methods = new List<MethodInfo>();
            foreach (MemberInfo mi in defaults) {
                PropertyInfo pi = mi as PropertyInfo;

                if (pi != null) {
                    if (op == Operators.GetItem) {
                        MethodInfo method = pi.GetGetMethod(ScriptDomainManager.Options.PrivateBinding);
                        if (method != null) methods.Add(method);
                    } else if (op == Operators.SetItem) {
                        MethodInfo method = pi.GetSetMethod(ScriptDomainManager.Options.PrivateBinding);
                        if (method != null) methods.Add(method);
                    }
                }
            }

            // if we received methods from both declaring type & base types we need to filter them
            Dictionary<MethodSignatureInfo, MethodInfo> dict = new Dictionary<MethodSignatureInfo, MethodInfo>();
            foreach (MethodInfo mb in methods) {
                MethodSignatureInfo args = new MethodSignatureInfo(mb.IsStatic, mb.GetParameters());
                MethodInfo other;

                if (dict.TryGetValue(args, out other)) {
                    if (other.DeclaringType.IsAssignableFrom(mb.DeclaringType)) {
                        // derived type replaces...
                        dict[args] = mb;
                    } 
                } else {
                    dict[args] = mb;
                }
            }

            return new List<MethodInfo>(dict.Values).ToArray();
        }        

        #endregion

        #region Common helpers

        public Expression Param0 {
            get { return GetParamater(0); }
        }

        public Expression Param1 {
            get { return GetParamater(1); }
        }
        
        public Expression Param2 {
            get { return GetParamater(2); }
        }

        private Expression GetParamater(int index) {
            Expression expr = _rule.Parameters[index];
            if (_types[index].IsAssignableFrom(expr.Type)) return expr;
            return Ast.Convert(expr, _types[index]);
        }

        private bool TryMakeBindingTarget(MethodInfo[] targets) {
            MethodBinder mb = MethodBinder.MakeBinder(Binder, targets[0].Name, targets, BinderType.Normal);
            MethodCandidate mc = mb.MakeBindingTarget(CallType.None, _types);
            if (mc != null) {
                Expression call = mc.Target.MakeExpression(Binder, _rule, _rule.Parameters, _types);
                _rule.SetTarget(_rule.MakeReturn(Binder, call));
                return true;
            }
            return false;
        }

        private bool TryMakeInvertedBindingTarget(MethodInfo[] targets) {
            MethodBinder mb = MethodBinder.MakeBinder(Binder, targets[0].Name, targets, BinderType.Normal);
            MethodCandidate mc = mb.MakeBindingTarget(CallType.None, _types);
            if (mc != null) {
                Expression call = mc.Target.MakeExpression(Binder, _rule, _rule.Parameters, _types);
                _rule.SetTarget(_rule.MakeReturn(Binder, Ast.Not(call)));
                return true;
            }
            return false;
        }

        private static Operators GetInvertedOperator(Operators op) {
            switch (op) {
                case Operators.LessThan: return Operators.GreaterThanOrEqual;
                case Operators.LessThanOrEqual: return Operators.GreaterThan;
                case Operators.GreaterThan: return Operators.LessThanOrEqual;
                case Operators.GreaterThanOrEqual: return Operators.LessThan;
                case Operators.Equals: return Operators.NotEquals;
                case Operators.NotEquals: return Operators.Equals;
                default: throw new InvalidOperationException();
            }
        }

        private static Expression ConvertIfNeeded(Expression expression, Type type) {
            if (expression.Type != type) {
                return Ast.DynamicConvert(expression, type);
            }
            return expression;
        }

        private void SetErrorTarget(OperatorInfo info) {
            _rule.SetTarget(
                _rule.MakeError(
                    Ast.ComplexCallHelper(
                        typeof(RuntimeHelpers).GetMethod("BadArgumentsForOperation"),
                        ArrayUtils.Insert((Expression)Ast.Constant(info.Operator), _rule.Parameters)
                    )
                )
            );
        }

        private MethodInfo[] GetApplicableMembers(OperatorInfo info) {
            return GetApplicableMembers(CompilerHelpers.GetType(_args[0]), info);
        }

        private MethodInfo[] GetApplicableMembers(Type t, OperatorInfo info) {
            MemberGroup members = Binder.GetMember(Action, t, info.Name);
            if (members.Count == 0 && info.AlternateName != null) {
                members = Binder.GetMember(Action, t, info.AlternateName);
            }

            // filter down to just methods
            return FilterNonMethods(t, members);
        }

        /// <summary>
        /// Gets alternate members which are specially recognized by the DLR for specific types when
        /// all other member lookup fails.
        /// </summary>
        private MethodInfo[] GetFallbackMembers(Type t, OperatorInfo info) {
            // if we have an event we need to make a strongly-typed event handler

            if (t == typeof(EventTracker)) {
                EventTracker et = ((EventTracker)_args[0]);
                if (info.Operator == Operators.InPlaceAdd) {
                    return new MethodInfo[] { typeof(BinderOps).GetMethod("EventTrackerInPlaceAdd").MakeGenericMethod(et.Event.EventHandlerType) };
                } else if (info.Operator == Operators.InPlaceSubtract) {
                    return new MethodInfo[] { typeof(BinderOps).GetMethod("EventTrackerInPlaceRemove").MakeGenericMethod(et.Event.EventHandlerType) };
                }
            } else if (t == typeof(BoundMemberTracker)) {
                BoundMemberTracker bmt = ((BoundMemberTracker)_args[0]);
                if (bmt.BoundTo.MemberType == TrackerTypes.Event) {
                    EventTracker et = ((EventTracker)bmt.BoundTo);

                    if (info.Operator == Operators.InPlaceAdd) {
                        return new MethodInfo[] { typeof(BinderOps).GetMethod("BoundEventTrackerInPlaceAdd").MakeGenericMethod(et.Event.EventHandlerType) };
                    } else if (info.Operator == Operators.InPlaceSubtract) {
                        return new MethodInfo[] { typeof(BinderOps).GetMethod("BoundEventTrackerInPlaceRemove").MakeGenericMethod(et.Event.EventHandlerType) };
                    }
                }
            }

            return new MethodInfo[0];
        }

        private static MethodInfo[] FilterNonMethods(Type t, MemberGroup members) {
            List<MethodInfo> methods = new List<MethodInfo>(members.Count);
            foreach (MemberTracker mi in members) {
                if (mi.MemberType == TrackerTypes.Method) {
                    MethodInfo method = ((MethodTracker)mi).Method ;

                    // don't call object methods for None type, but if someone added
                    // methods to null we'd call those.
                    if (method.DeclaringType != typeof(object) || t != typeof(None)) {
                        methods.Add(method);
                    }
                }
            }

            return methods.ToArray();
        }

        private OperatorInfo GetOperatorInfo(Operators op) {
            foreach (OperatorInfo info in _infos) {
                if (info.Operator == op) return info;
            }
            return null;
        }

        #endregion

        #region Operator Info

        private static OperatorInfo[] MakeOperatorTable() {
            List<OperatorInfo> res = new List<OperatorInfo>();

            // alternate names from: http://msdn2.microsoft.com/en-us/library/2sk3x8a7(vs.71).aspx
            //   different in:
            //    comparisons all support alternative names, Xor is "ExclusiveOr" not "Xor"

            // unary operators as defined in Partition I Architecture 9.3.1:
            res.Add(new OperatorInfo(Operators.Decrement,           "op_Decrement",                 "Decrement"));      // --
            res.Add(new OperatorInfo(Operators.Increment,           "op_Increment",                 "Increment"));      // ++
            res.Add(new OperatorInfo(Operators.Negate,              "op_UnaryNegation",             "Negate"));         // - (unary)
            res.Add(new OperatorInfo(Operators.Positive,            "op_UnaryPlus",                 "Plus"));           // + (unary)
            res.Add(new OperatorInfo(Operators.Not,                 "op_LogicalNot",                null));             // !
            res.Add(new OperatorInfo(Operators.IsTrue,              "op_True",                      null));             // not defined
            res.Add(new OperatorInfo(Operators.IsFalse,             "op_False",                     null));             // not defined
            //res.Add(new OperatorInfo(Operators.AddressOf,           "op_AddressOf",                 null));             // & (unary)
            res.Add(new OperatorInfo(Operators.OnesComplement,      "op_OnesComplement",            "OnesComplement")); // ~
            //res.Add(new OperatorInfo(Operators.PointerDereference,  "op_PointerDereference",        null));             // * (unary)

            // binary operators as defined in Partition I Architecture 9.3.2:
            res.Add(new OperatorInfo(Operators.Add,                 "op_Addition",                  "Add"));            // +
            res.Add(new OperatorInfo(Operators.Subtract,            "op_Subtraction",               "Subtract"));       // -
            res.Add(new OperatorInfo(Operators.Multiply,            "op_Multiply",                  "Multiply"));       // *
            res.Add(new OperatorInfo(Operators.Divide,              "op_Division",                  "Divide"));         // /
            res.Add(new OperatorInfo(Operators.Mod,                 "op_Modulus",                   "Mod"));            // %
            res.Add(new OperatorInfo(Operators.Xor,                 "op_ExclusiveOr",               "ExclusiveOr"));    // ^
            res.Add(new OperatorInfo(Operators.BitwiseAnd,          "op_BitwiseAnd",                "BitwiseAnd"));     // &
            res.Add(new OperatorInfo(Operators.BitwiseOr,           "op_BitwiseOr",                 "BitwiseOr"));      // |
            res.Add(new OperatorInfo(Operators.And,                 "op_LogicalAnd",                "And"));            // &&
            res.Add(new OperatorInfo(Operators.Or,                  "op_LogicalOr",                 "Or"));             // ||
            res.Add(new OperatorInfo(Operators.Assign,              "op_Assign",                    "Assign"));         // =
            res.Add(new OperatorInfo(Operators.LeftShift,           "op_LeftShift",                 "LeftShift"));      // <<
            res.Add(new OperatorInfo(Operators.RightShift,          "op_RightShift",                "RightShift"));     // >>
            res.Add(new OperatorInfo(Operators.RightShiftSigned,    "op_SignedRightShift",          "RightShift"));     // not defined
            res.Add(new OperatorInfo(Operators.RightShiftUnsigned,  "op_UnsignedRightShift",        "RightShift"));     // not defined
            res.Add(new OperatorInfo(Operators.Equals,              "op_Equality",                  "Equals"));         // ==   
            res.Add(new OperatorInfo(Operators.GreaterThan,         "op_GreaterThan",               "GreaterThan"));    // >
            res.Add(new OperatorInfo(Operators.LessThan,            "op_LessThan",                  "LessThan"));       // <
            res.Add(new OperatorInfo(Operators.NotEquals,           "op_Inequality",                "NotEquals"));      // != 
            res.Add(new OperatorInfo(Operators.GreaterThanOrEqual,  "op_GreaterThanOrEqual",        "GreaterThanOrEqual"));        // >=
            res.Add(new OperatorInfo(Operators.LessThanOrEqual,     "op_LessThanOrEqual",           "LessThanOrEqual"));        // <=
            res.Add(new OperatorInfo(Operators.InPlaceMultiply,     "op_MultiplicationAssignment",  "Multiply"));       // *=
            res.Add(new OperatorInfo(Operators.InPlaceSubtract,     "op_SubtractionAssignment",     "Subtract"));       // -=
            res.Add(new OperatorInfo(Operators.InPlaceXor,          "op_ExclusiveOrAssignment",     "Xor"));            // ^=
            res.Add(new OperatorInfo(Operators.InPlaceLeftShift,    "op_LeftShiftAssignment",       "LeftShift"));      // <<=
            res.Add(new OperatorInfo(Operators.InPlaceRightShift,   "op_RightShiftAssignment",      "RightShift"));     // >>=
            res.Add(new OperatorInfo(Operators.InPlaceRightShiftUnsigned, "op_UnsignedRightShiftAssignment", "UnsignedRightShift"));     // >>=
            res.Add(new OperatorInfo(Operators.InPlaceMod,          "op_ModulusAssignment",         "Mod"));            // %=
            res.Add(new OperatorInfo(Operators.InPlaceAdd,          "op_AdditionAssignment",        "Add"));            // += 
            res.Add(new OperatorInfo(Operators.InPlaceBitwiseAnd,   "op_BitwiseAndAssignment",      "BitwiseAnd"));     // &=
            res.Add(new OperatorInfo(Operators.InPlaceBitwiseOr,    "op_BitwiseOrAssignment",       "BitwiseOr"));      // |=
            res.Add(new OperatorInfo(Operators.InPlaceDivide,       "op_DivisionAssignment",        "Divide"));         // /=
            res.Add(new OperatorInfo(Operators.Comma,               "op_Comma",                     null));             // ,

            // DLR Extended operators:
            res.Add(new OperatorInfo(Operators.Compare,             "op_Compare",                   "Compare"));        // not defined
            res.Add(new OperatorInfo(Operators.GetItem,             "get_Item",                     "GetItem"));        // x[y]
            res.Add(new OperatorInfo(Operators.SetItem,             "set_Item",                     "SetItem"));        // x[y] = z
            res.Add(new OperatorInfo(Operators.DeleteItem,          "del_Item",                     "DeleteItem"));     // not defined

            res.Add(new OperatorInfo(Operators.GetEnumerator,       "GetEnumerator",                null));
            res.Add(new OperatorInfo(Operators.Dispose,             "Dispose",                      null));


            return res.ToArray();
        }
        
        class OperatorInfo {
            public Operators Operator;
            public string Name;
            public string AlternateName;

            public OperatorInfo(Operators op, string name, string altName) {
                Operator = op;
                Name = name;
                AlternateName = altName;
            }
        }

        #endregion
    }
}

#endif	
