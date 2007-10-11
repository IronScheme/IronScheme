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
using System.Reflection.Emit;
using System.Diagnostics;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    /// <summary>
    /// Member expression (statically typed) which represents 
    /// property or field access, both static and instance.
    /// For instance property/field, Expression must be != null.
    /// </summary>
    public class MemberExpression : Expression {
        private readonly MemberInfo _member;
        private readonly Expression _expression;

        public MemberInfo Member {
            get { return _member; }
        }

        public Expression Expression {
            get { return _expression; }
        }

        public override Type Type {
            get {
                switch (_member.MemberType) {
                    case MemberTypes.Field:
                        return ((FieldInfo)_member).FieldType;
                    case MemberTypes.Property:
                        return ((PropertyInfo)_member).PropertyType;
                    default:
                        Debug.Assert(false, "Invalid member type");
                        return null;
                }
            }
        }

        internal MemberExpression(MemberInfo member, Expression expression) {
            _member = member;
            _expression = expression;
        }

        internal override void EmitAddress(CodeGen cg, Type asType) {
           //EmitInstance(cg); // the instance is already emitted!

            if (asType != Type || _member.MemberType != MemberTypes.Field) {
                base.EmitAddress(cg, asType);
            } else {
                EmitInstance(cg); // have to emit here though
                cg.EmitFieldAddress((FieldInfo)_member);
            }
        }

        public override void Emit(CodeGen cg) {
            // emit "this", if any
            EmitInstance(cg);

            switch (_member.MemberType) {
                case MemberTypes.Field:
                    FieldInfo field = (FieldInfo)_member;
                    cg.EmitFieldGet(field);
                    break;                    
                case MemberTypes.Property:
                    PropertyInfo property = (PropertyInfo)_member;
                    cg.EmitPropertyGet(property);
                    break;
                default:
                    Debug.Assert(false, "Invalid member type");
                    break;
            }
        }

        private void EmitInstance(CodeGen cg) {
            if (_expression != null) {
                if (_member.DeclaringType.IsValueType) {
                    _expression.EmitAddress(cg, _member.DeclaringType);
                } else {
                    _expression.Emit(cg);
                }
            }
        }

        protected override object DoEvaluate(CodeContext context) {
            object self = _expression != null ? _expression.Evaluate(context) : null;
            switch (_member.MemberType) {
                case MemberTypes.Field:
                    FieldInfo field = (FieldInfo)_member;
                    return field.GetValue(self);
                case MemberTypes.Property:                    
                    PropertyInfo property = (PropertyInfo)_member;
                    return property.GetValue(self, Utils.ArrayUtils.EmptyObjects);
                default:
                    Debug.Assert(false, "Invalid member type");
                    break;
            }

            throw new InvalidOperationException();
        }

        internal override object EvaluateAssign(CodeContext context, object value) {
            object self = _expression != null ? _expression.Evaluate(context) : null;
            switch (_member.MemberType) {
                case MemberTypes.Field:
                    FieldInfo field = (FieldInfo)_member;
                    field.SetValue(self, value);
                    return value;
                case MemberTypes.Property:
                    PropertyInfo property = (PropertyInfo)_member;
                    property.SetValue(self, value, ArrayUtils.EmptyObjects);
                    return value;
                default:
                    Debug.Assert(false, "Invalid member type");
                    break;
            }

            throw new InvalidOperationException();
        }

        public override void Walk(Walker walker) {
            if (walker.Walk(this)) {
                if (_expression != null) {
                    _expression.Walk(walker);
                }
            }
            walker.PostWalk(this);
        }
    }

    /// <summary>
    /// Factory methods.
    /// </summary>
    public static partial class Ast {
        public static MemberExpression ReadField(Expression expression, Type type, string field) {
            Contract.RequiresNotNull(type, "type");
            Contract.RequiresNotNull(field, "field");
            FieldInfo fi = type.GetField(field);
            if (fi == null) {
                throw new ArgumentException(String.Format("Type {0} doesn't have field {1}", type, field));
            }
            return ReadField(expression, fi);
        }

        /// <summary>
        /// Creates MemberExpression representing field access, instance or static.
        /// For static field, expression must be null and FieldInfo.IsStatic == true
        /// For instance field, expression must be non-null and FieldInfo.IsStatic == false.
        /// </summary>
        /// <param name="expression">Expression that evaluates to the instance for the field access.</param>
        /// <param name="field">Field represented by this Member expression.</param>
        /// <returns>New instance of Member expression</returns>
        public static MemberExpression ReadField(Expression expression, FieldInfo field) {
            Contract.RequiresNotNull(field, "field");
            if (field.IsStatic) {
                Contract.Requires(expression == null, "expression", "Expression must be null for static field");
            } else {
                Contract.RequiresNotNull(expression, "expression");
                Contract.Requires(field.DeclaringType.IsAssignableFrom(expression.Type));
            }

            return new MemberExpression(field, expression);
        }

        public static MemberExpression ReadProperty(Expression expression, Type type, string property) {
            Contract.RequiresNotNull(type, "type");
            Contract.RequiresNotNull(property, "property");
            PropertyInfo pi = type.GetProperty(property);
            if (pi == null) {
                throw new ArgumentException(String.Format("Type {0} doesn't have property {1}", type, property));
            }
            if (!pi.CanRead) {
                throw new ArgumentException(String.Format("Cannot read property {0}.{1}", pi.DeclaringType, pi.Name));
            }

            return ReadProperty(expression, pi);
        }

        /// <summary>
        /// Creates MemberExpression representing property access, instance or static.
        /// For static properties, expression must be null and property.IsStatic == true.
        /// For instance properties, expression must be non-null and property.IsStatic == false.
        /// </summary>
        /// <param name="expression">Expression that evaluates to the instance for instance property access.</param>
        /// <param name="property">PropertyInfo of the property to access</param>
        /// <returns>New instance of the MemberExpression.</returns>
        public static MemberExpression ReadProperty(Expression expression, PropertyInfo property) {
            Contract.RequiresNotNull(property, "property");
            MethodInfo getter = property.GetGetMethod();
            Contract.RequiresNotNull(getter, "property");
            if (getter.IsStatic) {
                Contract.Requires(expression == null, "expression", "Instance must be null for static properties");
            } else {
                Contract.RequiresNotNull(expression, "expression");
                Contract.Requires(property.DeclaringType.IsAssignableFrom(expression.Type), "expression", "Incorrect instance type for the property.");
            }

            return new MemberExpression(property, expression);
        }
    }
}
