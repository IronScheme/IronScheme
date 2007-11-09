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
using System.Reflection;

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Actions {
    using Ast = Microsoft.Scripting.Ast.Ast;
    using Microsoft.Scripting.Math;

    /// <summary>
    /// BinderHelper for producing rules related to performing conversions.
    /// </summary>
    public class ConvertToBinderHelper<T> : BinderHelper<T, ConvertToAction> {
        private object _arg;
        private StandardRule<T> _rule = new StandardRule<T>();

        public ConvertToBinderHelper(CodeContext context, ConvertToAction action, params object[] args)
            : base(context, action) {
            Contract.Requires(args.Length == 1, "can only convert single argument");

            _arg = args[0];
        }

        public StandardRule<T> MakeRule() {
            Type toType = Action.ToType;
            Type knownType = CompilerHelpers.GetVisibleType(_rule.Parameters[0].Type);

            // check for conversion to object first...
            if (TryConvertToObject(toType, knownType)) {
                _rule.AddTest(Ast.Constant(true));
                return _rule;
            }

            // do checks that aren't based upon strongly knowing the object's type (and don't require tests)
            if (TryAllConversions(toType, knownType)) {
                _rule.AddTest(Ast.Constant(true));
                return _rule;
            }

            // try again w/ a test for the known-type
            Type type = CompilerHelpers.GetType(_arg);
            _rule.AddTest(_rule.MakeTypeTest(type, 0));

            if (TryAllConversions(toType, type)) {
                return _rule;
            }

            // no conversion is available, make an error rule.
            MakeErrorTarget();
            return _rule;
        }

        #region Conversion attempt helpers

        /// <summary>
        /// Checks if the conversion is to object and produces a target if it is.
        /// </summary>
        private bool TryConvertToObject(Type toType, Type knownType) {
            if (toType == typeof(object)) {
                if (knownType.IsValueType) {
                    MakeBoxingTarget();
                } else {
                    MakePerfectMatchTarget();
                }
                return true;
            }
            return false;
        }

        /// <summary>
        /// Checks if any conversions are available and if so builds the target for that conversion.
        /// </summary>
        private bool TryAllConversions(Type toType, Type knownType) {
            return TryAssignableConversion(toType, knownType) ||       // known type -> known type
                TryExtensibleConversion(toType, knownType) ||       // Extensible<T> -> Extensible<T>.Value
                TryUserDefinedConversion(toType, knownType) ||      // op_Implicit
                TryImplicitNumericConversion(toType, knownType) ||  // op_Implicit
                TryNullableConversion(toType, knownType);// null -> Nullable<T> or T -> Nullable<T>                 
        }

        /// <summary>
        /// Checks if the conversion can be handled by a simple cast.
        /// </summary>
        private bool TryAssignableConversion(Type toType, Type type) {
            if (toType.IsAssignableFrom(type) ||
                (type == typeof(None) && (toType.IsClass || toType.IsInterface))) {
                // MakeSimpleConversionTarget handles the ConversionResultKind check
                MakeSimpleConversionTarget(toType);
                return true;
            }

            return false;
        }

        /// <summary>
        /// Checks if the conversion can be handled by calling a user-defined conversion method.
        /// </summary>
        private bool TryUserDefinedConversion(Type toType, Type type) {
            Type fromType = GetUnderlyingType(type);

            if (TryOneConversion(toType, type, fromType, "op_Implicit", true)) {
                return true;
            }

            if (TryOneConversion(toType, type, fromType, "ConvertTo" + toType.Name, true)) {
                return true;
            }

            if (Action.ResultKind == ConversionResultKind.ExplicitCast || Action.ResultKind == ConversionResultKind.ExplicitTry) {
                // finally try explicit conversions
                if (TryOneConversion(toType, type, fromType, "op_Explicit", false)) {
                    return true;
                }

                if (TryOneConversion(toType, type, fromType, "ConvertTo" + toType.Name, false)) {
                    return true;
                }
            }
            
            return false;
        }

        /// <summary>
        /// Helper that checkes both types to see if either one defines the specified conversion
        /// method.
        /// </summary>
        private bool TryOneConversion(Type toType, Type type, Type fromType, string methodName, bool isImplicit) {
            MemberGroup conversions = Binder.GetMember(Action, fromType, methodName);
            if (TryUserDefinedConversion(toType, type, conversions, isImplicit)) {
                return true;
            }

            // then on the type we're trying to convert to
            conversions = Binder.GetMember(Action, toType, methodName);
            if (TryUserDefinedConversion(toType, type, conversions, isImplicit)) {
                return true;
            }
            return false;
        }

        /// <summary>
        /// Checks if any of the members of the MemberGroup provide the applicable conversion and 
        /// if so uses it to build a conversion rule.
        /// </summary>
        private bool TryUserDefinedConversion(Type toType, Type type, MemberGroup conversions, bool isImplicit) {
            Type checkType = GetUnderlyingType(type);

            foreach (MemberTracker mt in conversions) {
                if (mt.MemberType != TrackerTypes.Method) continue;                

                MethodTracker method = (MethodTracker)mt;

                if (isImplicit && method.Method.IsDefined(typeof(ExplicitConversionMethodAttribute), true)) {
                    continue;
                }

                if (method.Method.ReturnType == toType) {   // TODO: IsAssignableFrom?  IsSubclass?
                    ParameterInfo[] pis = method.Method.GetParameters();

                    if (pis.Length == 1 && pis[0].ParameterType.IsAssignableFrom(checkType)) {
                        // we can use this method
                        if (type == checkType) {
                            MakeConversionTarget(method, type, isImplicit);
                        } else {
                            MakeExtensibleConversionTarget(method, type, isImplicit);
                        }
                        return true;
                    }
                }
            }
            return false;
        }

        /// <summary>
        /// Checks if the conversion is to applicable by extracting the value from Extensible of T.
        /// </summary>
        private bool TryExtensibleConversion(Type toType, Type type) {
            Type extensibleType = typeof(Extensible<>).MakeGenericType(toType);
            if (extensibleType.IsAssignableFrom(type)) {
                MakeExtensibleTarget(extensibleType);
                return true;
            }
            return false;
        }

        /// <summary>
        /// Checks if there's an implicit numeric conversion for primitive data types.
        /// </summary>
        private bool TryImplicitNumericConversion(Type toType, Type type) {
            Type checkType = type;
            if (type.IsGenericType && type.GetGenericTypeDefinition() == typeof(Extensible<T>)) {
                checkType = type.GetGenericArguments()[0];
            }

            if (TypeUtils.IsNumeric(toType) && TypeUtils.IsNumeric(checkType)) {
                // check for an explicit conversion
                int tx, ty, fx, fy;
                if (TypeUtils.GetNumericConversionOrder(Type.GetTypeCode(toType), out tx, out ty) &&
                    TypeUtils.GetNumericConversionOrder(Type.GetTypeCode(checkType), out fx, out fy)) {
                    if (TypeUtils.IsImplicitlyConvertible(fx, fy, tx, ty)) {
                        // MakeSimpleConversionTarget handles the ConversionResultKind check
                        if (type == checkType) {
                            MakeSimpleConversionTarget(toType);
                        } else {
                            MakeSimpleExtensibleConversionTarget(toType);
                        }
                        return true;
                    }
                }
            }
            return false;
        }

        /// <summary>
        /// Checks if there's a conversion to/from Nullable of T.
        /// </summary>
        private bool TryNullableConversion(Type toType, Type knownType) {
            if (toType.IsGenericType && toType.GetGenericTypeDefinition() == typeof(Nullable<>)) {
                if (knownType == typeof(None)) {
                    // null -> Nullable<T>
                    MakeNullToNullableOfTTarget(toType);
                    return true;
                } else if (knownType == toType.GetGenericArguments()[0]) {
                    MakeTToNullableOfTTarget(toType, knownType);
                    return true;
                }
            }

            return false;
        }

        #endregion

        #region Rule production helpers

        /// <summary>
        /// Helper to produce an error when a conversion cannot occur
        /// </summary>
        private void MakeErrorTarget() {
            Statement target;

            switch (Action.ResultKind) {
                case ConversionResultKind.ImplicitCast:
                case ConversionResultKind.ExplicitCast:
                    target = Binder.MakeConversionError(Action.ToType, _rule.Parameters[0]).MakeErrorForRule(_rule, Binder);
                    break;
                case ConversionResultKind.ImplicitTry:
                case ConversionResultKind.ExplicitTry:
                    target = CompilerHelpers.GetTryConvertReturnValue(Context, _rule);
                    break;
                default:
                    throw new InvalidOperationException(Action.ResultKind.ToString());
            }
            _rule.SetTarget(target);
        }

        /// <summary>
        /// Helper to produce a rule when no conversion is required (the strong type of the expression
        /// input matches the type we're converting to)
        /// </summary>
        private void MakePerfectMatchTarget() {
            _rule.SetTarget(
                _rule.MakeReturn(
                    Binder,
                    _rule.Parameters[0]
                )
            );
        }

        /// <summary>
        /// Helper to produce a rule which just boxes a value type
        /// </summary>
        private void MakeBoxingTarget() {
            // MakeSimpleConversionTarget handles the ConversionResultKind check
            MakeSimpleConversionTarget(typeof(object));
        }

        /// <summary>
        /// Helper to produce a conversion rule by calling the helper method to do the convert
        /// </summary>
        private void MakeConversionTarget(MethodTracker method, Type fromType, bool isImplicit) {
            Statement ret = _rule.MakeReturn(
                Binder,
                Binder.MakeCallExpression(method.Method, Ast.Convert(_rule.Parameters[0], fromType))
            );

            ret = WrapForThrowingTry(isImplicit, ret);

            _rule.SetTarget(ret);
        }
        
        /// <summary>
        /// Helper to produce a conversion rule by calling the helper method to do the convert
        /// </summary>
        private void MakeExtensibleConversionTarget(MethodTracker method, Type fromType, bool isImplicit) {
            Statement ret = _rule.MakeReturn(
                Binder,
                Binder.MakeCallExpression(method.Method, GetExtensibleValue(fromType))
            );

            ret = WrapForThrowingTry(isImplicit, ret);
            
            _rule.SetTarget(ret);
        }

        /// <summary>
        /// Helper to wrap explicit conversion call into try/catch incase it throws an exception.  If
        /// it throws the default value is returned.
        /// </summary>
        private Statement WrapForThrowingTry(bool isImplicit, Statement ret) {
            if (!isImplicit && Action.ResultKind == ConversionResultKind.ExplicitTry) {
                ret = Ast.Try(ret).Catch(typeof(Exception), CompilerHelpers.GetTryConvertReturnValue(Context, _rule));
            }
            return ret;
        }

        /// <summary>
        /// Helper to produce a rule when no conversion is required (the strong type of the expression
        /// input matches the type we're converting to or has an implicit conversion at the IL level)
        /// </summary>
        private void MakeSimpleConversionTarget(Type toType) {
            _rule.SetTarget(
                _rule.MakeReturn(
                    Binder,
                    Ast.ConvertHelper(_rule.Parameters[0], toType)
                )
            );
        }

        /// <summary>
        /// Helper to produce a rule when no conversion is required from an extensible type's
        /// underlying storage to the type we're converting to.  The type of extensible type
        /// matches the type we're converting to or has an implicit conversion at the IL level.
        /// </summary>
        /// <param name="toType"></param>
        private void MakeSimpleExtensibleConversionTarget(Type toType) {
            Type extType = typeof(Extensible<>).MakeGenericType(toType);
            _rule.SetTarget(
                _rule.MakeReturn(
                    Binder,
                    Ast.ConvertHelper(
                        GetExtensibleValue(extType),
                        toType
                    )
                )
            );            
        }       

        /// <summary>
        /// Helper to extract the value from an Extensible of T
        /// </summary>
        private void MakeExtensibleTarget(Type extensibleType) {
            _rule.SetTarget(
                _rule.MakeReturn(
                    Binder,
                    Ast.ReadProperty(Ast.Convert(_rule.Parameters[0], extensibleType), extensibleType.GetProperty("Value"))
                )
            );
        }

        /// <summary>
        /// Helper to convert a null value to nullable of T
        /// </summary>
        private void MakeNullToNullableOfTTarget(Type toType) {
            _rule.SetTarget(
                _rule.MakeReturn(
                    Binder,
                    Ast.Call(typeof(BinderOps).GetMethod("CreateInstance").MakeGenericMethod(toType))
                )
            );
        }

        /// <summary>
        /// Helper to produce the rule for converting T to Nullable of T
        /// </summary>
        private void MakeTToNullableOfTTarget(Type toType, Type knownType) {
            // T -> Nullable<T>
            _rule.SetTarget(
                _rule.MakeReturn(
                    Binder,
                    Ast.New(
                        toType.GetConstructor(new Type[] { knownType }),
                        Ast.ConvertHelper(_rule.Parameters[0], knownType)
                    )
                )
            );            
        }

        /// <summary>
        /// Helper to extract the Value of an Extensible of T from the
        /// expression being converted.
        /// </summary>
        private Expression GetExtensibleValue(Type extType) {
            return Ast.ReadProperty(
                Ast.ConvertHelper(
                    _rule.Parameters[0],
                    extType
                ),
                extType.GetProperty("Value")
            );
        }

        /// <summary>
        /// Helper that checks if fromType is an Extensible of T or a subtype of 
        /// Extensible of T and if so returns the T.  Otherwise it returns fromType.
        /// 
        /// This is used to treat extensible types the same as their underlying types.
        /// </summary>
        private static Type GetUnderlyingType(Type fromType) {
            Type curType = fromType;
            do {
                if (curType.IsGenericType && curType.GetGenericTypeDefinition() == typeof(Extensible<>)) {
                    fromType = curType.GetGenericArguments()[0];
                }
                curType = curType.BaseType;
            } while (curType != null);
            return fromType;
        }
        
        #endregion
    }
}
