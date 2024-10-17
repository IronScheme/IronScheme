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
using System.Reflection;
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast
{
    public class NewExpression : Expression {
        private readonly ConstructorInfo /*!*/ _constructor;
        private readonly ReadOnlyCollection<Expression> /*!*/ _arguments;
        private readonly ParameterInfo[] /*!*/ _parameterInfos;
        private readonly Type _valueTypeType;

        internal NewExpression(ConstructorInfo /*!*/ constructor, ReadOnlyCollection<Expression> /*!*/ arguments, ParameterInfo[] /*!*/ parameters)
            : base(AstNodeType.New) {
            _constructor = constructor;
            _arguments = arguments;
            _parameterInfos = parameters;
        }

        internal NewExpression(Type valueTypeType)
          : base(AstNodeType.New) {
          _valueTypeType = valueTypeType;
          _arguments = new ReadOnlyCollection<Expression>(new Expression[0]);
          _parameterInfos = new ParameterInfo [0];
        }

        public ConstructorInfo Constructor {
            get { return _constructor; }
        }

        public ReadOnlyCollection<Expression> Arguments {
            get { return _arguments; }
        }

        public override Type Type {
            get {
              if (_valueTypeType != null)
              {
                return _valueTypeType;
              }
                return _constructor.DeclaringType;
            }
        }

        public override void Emit(CodeGen cg) {
          if (_valueTypeType != null)
          {
            EmitLocation(cg);
            cg.EmitMissingValue(_valueTypeType); // seems ok?
          }
          else
          {
            for (int i = 0; i < _parameterInfos.Length; i++)
            {
              _arguments[i].Emit(cg);
              if (_arguments[i].Type != _parameterInfos[i].ParameterType && _arguments[i].Type.IsValueType && typeof(SymbolId) != _arguments[i].Type)
              {
                cg.EmitBoxing(_arguments[i].Type);
              }

            }
            EmitLocation(cg);
            cg.EmitNew(_constructor);
          }
          if (ScriptDomainManager.Options.LightweightDebugging && Span.IsValid)
          {
            cg.EmitConstant(SpanToLong(Span));
            cg.EmitCall(Debugging.DebugMethods.ExpressionOut);
          }
        }

        protected override void EmitLocation(CodeGen cg)
        {
          if (ScriptDomainManager.Options.LightweightDebugging)
          {
            if (!cg.IsDynamicMethod)
            {

              if (Span.IsValid)
              {
                var s = SpanToLong(Span);
                cg.EmitConstant(s);
                cg.EmitCall(Debugging.DebugMethods.ExpressionIn);
              }
            }
          }
          else
          {
            base.EmitLocation(cg);
          }
        }
    }

    /// <summary>
    /// Factory methods.
    /// </summary>
    public static partial class Ast {
      public static NewExpression DefaultValueType(Type t)
      {
        Contract.Requires(t.IsValueType, "t", "Must be a value type");
        return new NewExpression(t);
      }

        public static NewExpression New(ConstructorInfo constructor, params Expression[] arguments) {
            return New(constructor, (IList<Expression>)arguments);
        }

        public static NewExpression New(ConstructorInfo constructor, IList<Expression> arguments) {
            Contract.RequiresNotNull(constructor, "constructor");
            Contract.RequiresNotNullItems(arguments, "arguments");

            ParameterInfo[] parameters = constructor.GetParameters();
            ValidateCallArguments(parameters, arguments);

            return new NewExpression(constructor, CollectionUtils.ToReadOnlyCollection(arguments), parameters);
        }

        public static NewExpression SimpleNewHelper(ConstructorInfo constructor, params Expression[] arguments) {
            Contract.RequiresNotNull(constructor, "constructor");
            Contract.RequiresNotNullItems(arguments, "arguments");

            ParameterInfo [] parameters = constructor.GetParameters();
            Contract.Requires(arguments.Length == parameters.Length, "arguments", "Incorrect number of arguments");

            return New(constructor, ArgumentConvertHelper(arguments, parameters));
        }
    }
}
