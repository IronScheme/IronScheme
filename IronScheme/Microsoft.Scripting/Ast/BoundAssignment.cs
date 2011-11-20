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
using System.Diagnostics;
using System.Reflection.Emit;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Actions;
using System.Reflection;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
  
  public class WriteStatement : Statement
  {
    private readonly Variable /*!*/ _variable;

    public Variable Variable
    {
      get { return _variable; }
    } 

    private Expression /*!*/ _value;

    public Expression Value
    {
      get { return _value; }
      set { _value = value; }
    }

    Expression GetReference(Expression expr)
    {
      if (expr is BoundExpression)
      {
        var be = expr as BoundExpression;
        if (be.Variable.Block == _variable.Block)
        {
          return be.Variable.AssumedValue ?? expr;
        }
      }
      return expr;
    }


    // implementation detail.
    private VariableReference _vr;

    internal WriteStatement(Variable var, Expression value) : base(AstNodeType.WriteStatement, SourceSpan.None)
    {
      _value = value;
      _variable = var;
      if (!value.IsConstant(null))
      {
        _variable.AssumedValue = _variable.AssumedValue == null ? GetReference(value) : null;
      }
    }


    public bool HasNoRef
    {
      get { return _vr == null; }
    }

    internal VariableReference Ref
    {
      get { return _vr; }
      set
      {
        if (value != null)
        {
          Debug.Assert(value != null);
          Debug.Assert(value.Variable == _variable);
          Debug.Assert(_vr == null || _vr.Equals(value));
          _vr = value;
        }
      }
    }

    public override void Emit(CodeGen cg)
    {
      _value.EmitAs(cg, _vr.Slot.Type);
      //if (_vr.Variable.Type == typeof(object) && _value.Type.IsValueType && _value.Type != typeof(SymbolId))
      //{
      //  cg.EmitBoxing(_value.Type);
      //}

      if (ScriptDomainManager.Options.LightweightDebugging && Span.IsValid)
      {
        cg.EmitConstant(SpanToLong(Span));
        cg.EmitCall(Debugging.DebugMethods.ExpressionIn);
      }
      
      _vr.Slot.EmitSet(cg);

      if (ScriptDomainManager.Options.LightweightDebugging && Span.IsValid)
      {
        cg.EmitConstant(SpanToLong(Span));
        cg.EmitCall(Debugging.DebugMethods.ExpressionOut);
      }

      _vr.Variable.SetInitialized();
    }
  }

    public class BoundAssignment : Expression {
        private readonly Variable /*!*/ _variable;
        private Expression /*!*/ _value;
        private bool _defined;

        // implementation detail.
        private VariableReference _vr;

        Expression GetReference(Expression expr)
        {
          if (expr is BoundExpression)
          {
            var be = expr as BoundExpression;
            if (be.Variable.Block == _variable.Block)
            {
              return be.Variable.AssumedValue ?? expr;
            }
          }
          return expr;
        }

        internal BoundAssignment(Variable /*!*/ variable, Expression /*!*/ value)
            : base(AstNodeType.BoundAssignment) {
            _variable = variable;
            _value = value;
            if (value.IsConstant(null))
            {
              Debugger.Break();
            }
            _variable.AssumedValue = _variable.AssumedValue == null ? GetReference(value) : null;
        }

        public Variable Variable {
            get { return _variable; }
        }

        internal VariableReference Ref {
            get { return _vr; }
            set {
                Debug.Assert(value != null);
                Debug.Assert(value.Variable == _variable);
                Debug.Assert(_vr == null || _vr.Equals(value));
                _vr = value;
            }
        }

        public Expression Value {
            get { return _value; }
          set { _value = value; }
        }

        internal bool IsDefined {
            get { return _defined; }
            set { _defined = value; }
        }

        public override Type Type {
            get {
                return typeof(void);
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
                cg.EmitConstant(SpanToLong(Span));
                cg.EmitCall(Debugging.DebugMethods.ExpressionIn);
              }
            }
          }
          else
          {
            base.EmitLocation(cg);
          }
        }

        internal override void EmitAddress(CodeGen cg, Type asType) {
          EmitLocation(cg);
            _value.Emit(cg);
            _vr.Slot.EmitSet(cg);
            //_vr.Slot.EmitGetAddr(cg);
            if (ScriptDomainManager.Options.LightweightDebugging && Span.IsValid)
            {
              cg.EmitConstant(SpanToLong(Span));
              cg.EmitCall(Debugging.DebugMethods.ExpressionOut);
            }
        }

        public override void Emit(CodeGen cg) {
          EmitLocation(cg);
            _value.EmitAs(cg, _vr.Slot.Type);
            //cg.Emit(OpCodes.Dup);
            _vr.Slot.EmitSet(cg);
            if (ScriptDomainManager.Options.LightweightDebugging && Span.IsValid)
            {
              cg.EmitConstant(SpanToLong(Span));
              cg.EmitCall(Debugging.DebugMethods.ExpressionOut);
            }
        }


#if FULL
        protected override object DoEvaluate(CodeContext context) {
            object value = _value.Evaluate(context);
            EvaluateAssign(context, _variable, value);
            return value;
        } 
#endif



#if FULL
        internal override object EvaluateAssign(CodeContext context, object value) {
            return EvaluateAssign(context, Variable, value);
        } 
#endif



#if FULL
        internal static object EvaluateAssign(CodeContext context, Variable var, object value) {
            switch (var.Kind) {
                case Variable.VariableKind.Temporary:
                case Variable.VariableKind.GeneratorTemporary:
                    context.Scope.TemporaryStorage[var] = value;
                    break;
                case Variable.VariableKind.Global:
                    RuntimeHelpers.SetGlobalName(context, var.Name, value);
                    break;
                default:
                    RuntimeHelpers.SetName(context, var.Name, value);
                    break;
            }
            return value;
        } 
#endif

    }

    public static partial class Ast {
        /// <summary>
        /// Performs an assignment variable = value
        /// </summary>
        public static Statement Write(Variable variable, Variable value) {
          //return Statement(Assign(variable, Ast.Read(value)));  
          return new WriteStatement(variable, Ast.Read(value));
        }

        /// <summary>
        /// Performs an assignment variable = value
        /// </summary>
        public static Statement Write(Variable variable, Expression value) {
          //return Statement(Assign(variable, value));  
            return new WriteStatement(variable, value);
        }

        /// <summary>
        /// Performs an assignment variable.field = value
        /// </summary>
        public static Statement Write(Variable variable, FieldInfo field, Expression value) {
            return Statement(AssignField(Read(variable), field, value));
        }

        /// <summary>
        /// Performs an assignment variable.field = value
        /// </summary>
        public static Statement Write(Variable variable, FieldInfo field, Variable value) {
            return Statement(AssignField(Read(variable), field, Read(value)));
        }

        /// <summary>
        /// Performs an assignment variable = right.field
        /// </summary>
        public static Statement Write(Variable variable, Variable right, FieldInfo field) {
            return Statement(Assign(variable, ReadField(Read(right), field)));
        }

        /// <summary>
        /// Performs an assignment variable.leftField = right.rightField
        /// </summary>
        public static Statement Write(Variable variable, FieldInfo leftField, Variable right, FieldInfo rightField) {
            return Statement(AssignField(Read(variable), leftField, ReadField(Read(right), rightField)));
        }

        /// <summary>
        /// Performs an assignment variable = value
        /// </summary>
        public static BoundAssignment Assign(Variable variable, Expression value) {
            Contract.RequiresNotNull(variable, "variable");
            Contract.RequiresNotNull(value, "value");
            Contract.Requires(TypeUtils.CanAssign(variable.Type, value.Type));
            return new BoundAssignment(variable, value);
        }
    }
}
