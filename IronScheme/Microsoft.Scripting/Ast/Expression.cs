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
using System.Reflection.Emit;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    /// <summary>
    /// Summary description for Expr.
    /// </summary>
    public abstract class Expression : Node {
        protected Expression(AstNodeType nodeType)
            : base(nodeType) {
        }

        protected internal static Expression Unwrap(Expression ii)
        {
          while (ii is UnaryExpression && ii.NodeType == AstNodeType.Convert)
          {
            ii = ((UnaryExpression)ii).Operand;
          }
          return ii;
        }


        private SourceLocation _start;
        private SourceLocation _end;


        public SourceLocation Start
        {
          get { return _start; }
        }

        public SourceLocation End
        {
          get { return _end; }
        }

        public SourceSpan Span
        {
          get
          {
            return new SourceSpan(_start, _end);
          }
        }

        public virtual void SetLoc(SourceSpan span)
        {
          _start = span.Start;
          _end = span.End;
        }

      bool IsValidLocation
      {
        get {return _start.IsValid && _end.IsValid;}
      }

      protected virtual void EmitLocation(CodeGen cg)
      {
        if (IsValidLocation)
        {
          cg.EmitPosition(_start, _end);
        }
      }

        /// <summary>
        /// Generates code for this expression in a value position.
        /// This method will leave the value of the expression
        /// on the top of the stack typed as Type.
        /// </summary>
        /// <param name="cg">Where to generate the code.</param>
        public abstract void Emit(CodeGen cg);

        /// <summary>
        /// Generates the code for the expression, leaving it on
        /// the stack typed as object.
        /// </summary>
        /// <param name="cg">Where to emit the code.</param>
        public void EmitAsObject(CodeGen cg) {
            this.Emit(cg);  // emit as Type
            cg.EmitBoxing(Type);
        }

        static readonly FieldInfo True = typeof(RuntimeHelpers).GetField("True");
        static readonly FieldInfo False = typeof(RuntimeHelpers).GetField("False");

        /// <summary>
        /// Generates code for this expression in a value position.  This will leave
        /// the value of the expression on the top of the stack typed as asType.
        /// </summary>
        /// <param name="cg">Where to generate the code.</param>
        /// <param name="asType">The type to leave on top of the stack.</param>
        internal void EmitAs(CodeGen cg, Type asType) {
          if (this is ConstantExpression && asType == typeof(object) && Type == typeof(bool))
          {
            if (IsConstant(true))
            {
              cg.EmitFieldGet(True);
            }
            else
            {
              cg.EmitFieldGet(False);
            }
          }
          else
          {
            if (this is UnaryExpression)
            {
              UnaryExpression ue = this as UnaryExpression;
              if (ue.NodeType == AstNodeType.Convert)
              {
                if (ue.Operand.Type == asType)
                {
                  ue.Operand.Emit(cg);
                  return;
                }
              }
            }
            this.Emit(cg);  // emit as Type
            if (asType.IsValueType || !IsConstant(null) && Type != typeof(SymbolId))
            {
              cg.EmitConvert(Type, asType);
            }
          }
        }

        /// <summary>
        /// Generates this expression as a bool and then branches to label
        /// if the resulting bool is false.  This can be overriden by 
        /// comparison expressions to generate tighter code by using
        /// the opcodes that include both a test and a branch in one.
        /// </summary>
        /// <param name="cg">Where to generate the code.</param>
        /// <param name="label">Where to branch if this expression is false.</param>
        public virtual void EmitBranchFalse(CodeGen cg, Label label) {
            this.EmitAs(cg, typeof(bool));
            cg.EmitSequencePointNone();
            cg.Emit(OpCodes.Brfalse, label);
            cg.EmitSequencePointNone();
        }

        /// <summary>
        /// This is the dual of EmitBranchFalse.
        /// </summary>
        public virtual void EmitBranchTrue(CodeGen cg, Label label) {
            this.EmitAs(cg, typeof(bool));
            cg.Emit(OpCodes.Brtrue, label);
        }

        /// <summary>
        /// Tests to see if this expression is a constant with the given value.
        /// </summary>
        /// <param name="value">The constant value to check for.</param>
        public virtual bool IsConstant(object value) {
            return false;
        }

        internal virtual void EmitAddress(CodeGen cg, Type asType) {
            EmitAs(cg, asType);
            Slot tmp = cg.GetLocalTmp(asType);
            tmp.EmitSet(cg);
            tmp.EmitGetAddr(cg);
        }

        public virtual Type Type {
            get { return typeof(object); }
        }

        Type inferredtype = typeof(object);

        public virtual Type InferredType
        {
          get { return inferredtype; }
          set { inferredtype = value; }
        }
    }
}
