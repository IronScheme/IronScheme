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
using System.Reflection;
using System.Diagnostics;

using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Actions {
    using Ast = Microsoft.Scripting.Ast.Ast;
    using Microsoft.Scripting.Utils;

    public class SetMemberBinderHelper<T> :
        MemberBinderHelper<T, SetMemberAction> {
        private bool _isStatic;

        public SetMemberBinderHelper(CodeContext context, SetMemberAction action, object[] args)
            : base(context, action, args) {
        }

        public StandardRule<T> MakeNewRule() {
            Type targetType = CompilerHelpers.GetType(Target);

            Rule.MakeTest(StrongBoxType ?? targetType);

            if (typeof(TypeTracker).IsAssignableFrom(targetType)) {
                targetType = ((TypeTracker)Target).Type;
                _isStatic = true;
                Rule.AddTest(Ast.Equal(Rule.Parameters[0], Ast.RuntimeConstant(Arguments[0])));
            } 

            if (Target is ICustomMembers) {
                MakeSetCustomMemberRule();
            } else {
                MakeSetMemberRule(targetType);
            }

            Rule.SetTarget(Body);

            return Rule;
        }

        private void MakeSetMemberRule(Type type) {
            if (MakeOperatorSetMemberBody(type, "SetMember")) {
                return;
            }

            MemberGroup members = Binder.GetMember(Action, type, StringName);

            // if lookup failed try the strong-box type if available.
            if (members.Count == 0 && StrongBoxType != null) {
                type = StrongBoxType;
                StrongBoxType = null;

                members = Binder.GetMember(Action, type, StringName);
            }

            Expression error;
            TrackerTypes memberTypes = GetMemberType(members, out error);
            if (error == null) {
                switch (memberTypes) {
                    case TrackerTypes.Method:
                    case TrackerTypes.TypeGroup:
                    case TrackerTypes.Type:
                    case TrackerTypes.Constructor: MakeReadOnlyMemberError(type); break;
                    case TrackerTypes.Event: MakeEventValidation(members); break;
                    case TrackerTypes.Field: MakeFieldRule(type, members); break;
                    case TrackerTypes.Property: MakePropertyRule(type, members); break;
                    case TrackerTypes.All:
                        // no match
                        if (MakeOperatorSetMemberBody(type, "SetMemberAfter")) {
                            return;
                        }
                        MakeMissingMemberError(type);
                        break;
                    default:
                        throw new InvalidOperationException();
                }
            } else {
                AddToBody(Rule.MakeError(error));
            }
        }

        protected virtual StandardRule<T> MakeEventValidation(MemberGroup members) {
            EventTracker ev = (EventTracker)members[0];

            // handles in place addition of events - this validates the user did the right thing, probably too Python specific.
            AddToBody(
                Rule.MakeReturn(
                    Binder,
                    Ast.Call(
                        typeof(BinderOps).GetMethod("SetEvent"),
                        Ast.RuntimeConstant(ev),
                        Rule.Parameters[1]
                    )
                )
            );

            return Rule;
        }

        private void MakePropertyRule(Type targetType, MemberGroup properties) {
            PropertyTracker info = (PropertyTracker)properties[0];

            MethodInfo setter = info.GetSetMethod(true);

            // Allow access to protected getters TODO: this should go, it supports IronPython semantics.
            if (setter != null && !setter.IsPublic && !(setter.IsFamily || setter.IsFamilyOrAssembly)) {
                if (!ScriptDomainManager.Options.PrivateBinding) {
                    setter = null;
                }
            }

            if (setter != null) {
                setter = CompilerHelpers.GetCallableMethod(setter);

                if (info.IsStatic != _isStatic) {
                    // TODO: Too python specific
                    AddToBody(Binder.MakeReadOnlyMemberError(Rule, targetType, StringName));
                } else if (setter.ContainsGenericParameters) {
                    AddToBody(Rule.MakeError(MakeGenericPropertyExpression()));
                } else if (setter.IsPublic && !setter.DeclaringType.IsValueType) {
                    if (_isStatic) {
                        AddToBody(
                            Rule.MakeReturn(
                                Binder, 
                                Ast.SimpleCallHelper(
                                    setter, 
                                    Binder.ConvertExpression(
                                        Rule.Parameters[1],
                                        setter.GetParameters()[0].ParameterType
                                    )
                                )
                            )
                        );
                    } else {
                        AddToBody(Rule.MakeReturn(Binder, MakeReturnValue(Binder.MakeCallExpression(setter, Rule.Parameters))));
                    }
                } else {
                    // TODO: Should be able to do better w/ value types.
                    AddToBody(
                        Rule.MakeReturn(
                            Binder,
                            MakeReturnValue(
                                Ast.Call(
                                    Ast.RuntimeConstant(((ReflectedPropertyTracker)info).Property), // TODO: Private binding on extension properties
                                    typeof(PropertyInfo).GetMethod("SetValue", new Type[] { typeof(object), typeof(object), typeof(object[]) }),
                                    Ast.ConvertHelper(Instance, typeof(object)),
                                    Ast.ConvertHelper(Rule.Parameters[1], typeof(object)),
                                    Ast.NewArray(typeof(object[]))
                                )
                            )
                        )
                    );
                }
            } else {
                AddToBody(Binder.MakeMissingMemberError(Rule, targetType, StringName));
            }
        }

        private void MakeFieldRule(Type targetType, MemberGroup fields) {
            FieldTracker field = (FieldTracker)fields[0];

            if (field.DeclaringType.IsGenericType && field.DeclaringType.GetGenericTypeDefinition() == typeof(StrongBox<>)) {
                // work around a CLR bug where we can't access generic fields from dynamic methods.
                Type[] generic = field.DeclaringType.GetGenericArguments();
                AddToBody(
                    Rule.MakeReturn(Binder,
                        MakeReturnValue(
                            Ast.Call(
                                typeof(BinderOps).GetMethod("UpdateBox").MakeGenericMethod(generic),
                                Ast.ConvertHelper(Instance, field.DeclaringType),
                                Ast.ConvertHelper(Rule.Parameters[1], generic[0])
                            )
                        )
                    )
                );
            } else if (field.IsInitOnly || field.IsLiteral || (field.IsStatic && targetType != field.DeclaringType)) {     // TODO: Field static check too python specific
                AddToBody(Binder.MakeReadOnlyMemberError(Rule, targetType, StringName));
            } else if (field.DeclaringType.IsValueType) {
                AddToBody(Rule.MakeError(Ast.New(typeof(ArgumentException).GetConstructor(new Type[] { typeof(string) }), Ast.Constant("cannot assign to value types"))));
            } else if (field.IsPublic && field.DeclaringType.IsVisible) {
                AddToBody(
                    Rule.MakeReturn(
                        Binder,
                        MakeReturnValue(
                            Ast.AssignField(
                                field.IsStatic ?
                                    null :
                                    Ast.Convert(Rule.Parameters[0], field.DeclaringType),
                                field.Field,
                                Binder.ConvertExpression(Rule.Parameters[1], field.FieldType)
                            )
                        )
                    )
                );
            } else {
                AddToBody(
                    Rule.MakeReturn(
                        Binder,
                        MakeReturnValue(
                            Ast.Call(
                                Ast.ConvertHelper(Ast.RuntimeConstant(field.Field), typeof(FieldInfo)),
                                typeof(FieldInfo).GetMethod("SetValue", new Type[] { typeof(object), typeof(object) }),
                                field.IsStatic ?
                                    Ast.Null() :
                                    (Expression)Ast.ConvertHelper(Instance, typeof(object)),
                                Ast.ConvertHelper(Rule.Parameters[1], typeof(object))
                            )
                        )
                    )
                );
            }
        }

        private void MakeSetCustomMemberRule() {
            AddToBody(
                Rule.MakeReturn(Binder,
                    MakeReturnValue(
                        Ast.Call(
                            Ast.Convert(Rule.Parameters[0], typeof(ICustomMembers)),
                            typeof(ICustomMembers).GetMethod("SetCustomMember"),
                            Ast.CodeContext(),
                            Ast.Constant(Action.Name),
                            Ast.ConvertHelper(Rule.Parameters[1], typeof(object))
                        )
                    )
                )
            );
        }

        private Expression MakeReturnValue(Expression expression) {
            return Ast.Comma(0, Rule.Parameters[1], expression);
        }

        /// <summary> if a member-injector is defined-on or registered-for this type call it </summary>
        private bool MakeOperatorSetMemberBody(Type type, string name) {
            MethodInfo setMem = GetMethod(type, name);
            if (setMem != null && setMem.IsSpecialName) {
                Expression call = Binder.MakeCallExpression(setMem, Rule.Parameters[0], Ast.Constant(StringName), Rule.Parameters[1]);
                Statement ret;

                if (setMem.ReturnType == typeof(bool)) {
                    ret = Ast.If(call, Rule.MakeReturn(Binder, Rule.Parameters[1]));
                } else {
                    ret = Rule.MakeReturn(Binder, Ast.Comma(1, call, Rule.Parameters[1]));
                }
                AddToBody(ret);
                return setMem.ReturnType != typeof(bool);
            }

            return false;
        }
    }
}
