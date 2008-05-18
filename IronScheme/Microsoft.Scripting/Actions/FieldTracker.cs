
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
using System.Text;
using System.Reflection;

using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Actions {
    using Ast = Microsoft.Scripting.Ast.Ast;

    public class FieldTracker : MemberTracker {
        private readonly FieldInfo _field;

        public FieldTracker(FieldInfo field) {
            Contract.RequiresNotNull(field, "field");
            _field = field;
        }

        public override Type DeclaringType {
            get { return _field.DeclaringType; }
        }

        public override TrackerTypes MemberType {
            get { return TrackerTypes.Field; }
        }

        public override string Name {
            get { return _field.Name; }
        }

        public bool IsPublic {
            get {
                return _field.IsPublic;
            }
        }

        public bool IsInitOnly {
            get {
                return _field.IsInitOnly;
            }
        }

        public bool IsLiteral {
            get {
                return _field.IsLiteral;
            }
        }

        public Type FieldType {
            get {
                return _field.FieldType;
            }
        }

        public bool IsStatic {
            get {
                return _field.IsStatic;
            }
        }

        public FieldInfo Field {
            get {
                return _field;
            }
        }

        public override string ToString() {
            return _field.ToString();
        }

        #region Public expression builders

        public override Expression GetValue(ActionBinder binder, Type type) {
            if (Field.IsLiteral) {
                return Ast.Constant(Field.GetValue(null));
            }

            if (!IsStatic) {
                // return the field tracker...
                return binder.ReturnMemberTracker(type, this);
            }

            if (IsPublic && DeclaringType.IsPublic) {
                return Ast.ReadField(null, Field);
            }

            return Ast.Call(
                Ast.ConvertHelper(Ast.RuntimeConstant(Field), typeof(FieldInfo)),
                typeof(FieldInfo).GetMethod("GetValue"),
                Ast.Null()
            );
        }

        #endregion

        #region Internal expression builders

        internal override Expression GetBoundValue(ActionBinder binder, Type type, Expression instance) {
            if (DeclaringType.IsGenericType && DeclaringType.GetGenericTypeDefinition() == typeof(StrongBox<>)) {
                // work around a CLR bug where we can't access generic fields from dynamic methods.
                return Ast.Call(
                    typeof(BinderOps).GetMethod("GetBox").MakeGenericMethod(DeclaringType.GetGenericArguments()),
                    Ast.ConvertHelper(instance, DeclaringType)
                );
            }

            if (IsPublic && DeclaringType.IsPublic) {
                return Ast.ReadField(
                    Ast.Convert(instance, Field.DeclaringType),
                    Field
                );
            }

            return Ast.Call(
                Ast.ConvertHelper(Ast.RuntimeConstant(Field), typeof(FieldInfo)),
                typeof(FieldInfo).GetMethod("GetValue"),
                Ast.ConvertHelper(instance, typeof(object))
            );
        }

        internal override MemberTracker BindToInstance(Expression instance) {
            if (IsStatic) return this;

            return new BoundMemberTracker(this, instance);
        }

        #endregion
    }
}

#endif	
