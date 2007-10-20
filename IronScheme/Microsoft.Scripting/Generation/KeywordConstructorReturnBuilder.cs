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
using Microsoft.Scripting.Ast;
using System.Reflection;

namespace Microsoft.Scripting.Generation {
    using Ast = Microsoft.Scripting.Ast.Ast;
    using Microsoft.Scripting.Utils;

    /// <summary>
    /// Updates fields/properties of the returned value with unused keyword parameters.
    /// </summary>
    class KeywordConstructorReturnBuilder : ReturnBuilder {
        private ReturnBuilder _builder;
        private int _kwArgCount;
        private int[] _indexesUsed;
        private MemberInfo[] _membersSet;

        public KeywordConstructorReturnBuilder(ReturnBuilder builder, int kwArgCount, int[]indexesUsed, MemberInfo[] membersSet)
            : base(builder.ReturnType) {
            _builder = builder;
            _kwArgCount = kwArgCount;
            _indexesUsed = indexesUsed;
            _membersSet = membersSet;
        }

        public override object Build(CodeContext context, object[] args, object[]parameters, object ret) {
            for (int i = 0; i < _indexesUsed.Length; i++) {
                object value = parameters[parameters.Length - _kwArgCount + _indexesUsed[i]];
                switch(_membersSet[i].MemberType) {
                    case MemberTypes.Field:
                        ((FieldInfo)_membersSet[i]).SetValue(ret, value);
                        break;
                    case MemberTypes.Property:
                        ((PropertyInfo)_membersSet[i]).SetValue(ret, value, ArrayUtils.EmptyObjects);
                        break;
                }
            }

            return _builder.Build(context, args, parameters, ret);
        }

        internal override Expression ToExpression(MethodBinderContext context, IList<ArgBuilder> args, IList<Expression> parameters, Expression ret) {
            List<Expression> sets = new List<Expression>();

            Variable tmp = context.GetTemporary(ret.Type, "val");

            for (int i = 0; i < _indexesUsed.Length; i++) {
                Expression value = parameters[parameters.Count - _kwArgCount + _indexesUsed[i]];
                switch(_membersSet[i].MemberType) {
                    case MemberTypes.Field:
                        FieldInfo fi = (FieldInfo)_membersSet[i];
                        if (!fi.IsLiteral && !fi.IsInitOnly) {
                            sets.Add(Ast.AssignField(Ast.Read(tmp), fi, Ast.DynamicConvert(value, fi.FieldType)));
                        } else {
                            // call a helper which throws the error but "returns object"
                            sets.Add(
                                Ast.Convert(
                                    Ast.Call(
                                        typeof(RuntimeHelpers).GetMethod("ReadOnlyAssignError"),
                                        Ast.Constant(true),
                                        Ast.Constant(fi.Name)
                                    ),
                                    fi.FieldType
                                )
                            );
                        }                        
                        break;
                    case MemberTypes.Property:
                        PropertyInfo pi = (PropertyInfo)_membersSet[i];
                        if (pi.GetSetMethod(ScriptDomainManager.Options.PrivateBinding) != null) {
                            sets.Add(Ast.AssignProperty(Ast.Read(tmp), pi, Ast.DynamicConvert(value, pi.PropertyType)));
                        } else {
                            // call a helper which throws the error but "returns object"
                            sets.Add(
                                Ast.Convert(
                                    Ast.Call(
                                        typeof(RuntimeHelpers).GetMethod("ReadOnlyAssignError"),
                                        Ast.Constant(false),
                                        Ast.Constant(pi.Name)
                                    ),
                                    pi.PropertyType
                                )
                            );
                        }
                        break;
                }
            }

            Expression newCall = Ast.Comma(
                0,
                ArrayUtils.Insert<Expression>(
                    Ast.Assign(tmp, ret),
                    sets.ToArray()
                )
            );

            return _builder.ToExpression(context, args, parameters, newCall);
        }
    }
}
