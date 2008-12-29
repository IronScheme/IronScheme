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

namespace Microsoft.Scripting.Ast {
    /// <summary>
    /// Walker class - The DLR AST Walker
    /// </summary>
    partial class Walker {

        #region Generated DLR AST Walker

      // *** BEGIN GENERATED CODE ***


#if FULL
        // ActionExpression
        protected internal virtual bool Walk(ActionExpression node) { return true; }
        protected internal virtual void PostWalk(ActionExpression node) { } 
#endif


      // ArrayIndexAssignment
        protected internal virtual bool Walk(ArrayIndexAssignment node) { return true; }
        protected internal virtual void PostWalk(ArrayIndexAssignment node) { }

        // ArrayIndexExpression
        protected internal virtual bool Walk(ArrayIndexExpression node) { return true; }
        protected internal virtual void PostWalk(ArrayIndexExpression node) { }

        // BinaryExpression
        protected internal virtual bool Walk(BinaryExpression node) { return true; }
        protected internal virtual void PostWalk(BinaryExpression node) { }

        // BoundAssignment
        protected internal virtual bool Walk(BoundAssignment node) { return true; }
        protected internal virtual void PostWalk(BoundAssignment node) { }

        // WriteStatement
        protected internal virtual bool Walk(WriteStatement node) { return true; }
        protected internal virtual void PostWalk(WriteStatement node) { }

        // BoundExpression
        protected internal virtual bool Walk(BoundExpression node) { return true; }
        protected internal virtual void PostWalk(BoundExpression node) { }

        // CodeBlockExpression
        protected internal virtual bool Walk(CodeBlockExpression node) { return true; }
        protected internal virtual void PostWalk(CodeBlockExpression node) { }

        // CodeContextExpression
        protected internal virtual bool Walk(CodeContextExpression node) { return true; }
        protected internal virtual void PostWalk(CodeContextExpression node) { }

        // CommaExpression
        protected internal virtual bool Walk(CommaExpression node) { return true; }
        protected internal virtual void PostWalk(CommaExpression node) { }

        // ConditionalExpression
        protected internal virtual bool Walk(ConditionalExpression node) { return true; }
        protected internal virtual void PostWalk(ConditionalExpression node) { }

        // ConstantExpression
        protected internal virtual bool Walk(ConstantExpression node) { return true; }
        protected internal virtual void PostWalk(ConstantExpression node) { }

        // DeleteUnboundExpression
        protected internal virtual bool Walk(DeleteUnboundExpression node) { return true; }
        protected internal virtual void PostWalk(DeleteUnboundExpression node) { }


#if FULL
        // DynamicConversionExpression
        protected internal virtual bool Walk(DynamicConversionExpression node) { return true; }
        protected internal virtual void PostWalk(DynamicConversionExpression node) { } 
#endif


        // EnvironmentExpression
        protected internal virtual bool Walk(EnvironmentExpression node) { return true; }
        protected internal virtual void PostWalk(EnvironmentExpression node) { }

        // MemberAssignment
        protected internal virtual bool Walk(MemberAssignment node) { return true; }
        protected internal virtual void PostWalk(MemberAssignment node) { }

        // MemberExpression
        protected internal virtual bool Walk(MemberExpression node) { return true; }
        protected internal virtual void PostWalk(MemberExpression node) { }

        // MethodCallExpression
        protected internal virtual bool Walk(MethodCallExpression node) { return true; }
        protected internal virtual void PostWalk(MethodCallExpression node) { }

        // NewArrayExpression
        protected internal virtual bool Walk(NewArrayExpression node) { return true; }
        protected internal virtual void PostWalk(NewArrayExpression node) { }

        // NewExpression
        protected internal virtual bool Walk(NewExpression node) { return true; }
        protected internal virtual void PostWalk(NewExpression node) { }

        // ParamsExpression
        protected internal virtual bool Walk(ParamsExpression node) { return true; }
        protected internal virtual void PostWalk(ParamsExpression node) { }

        // ParenthesizedExpression
        protected internal virtual bool Walk(ParenthesizedExpression node) { return true; }
        protected internal virtual void PostWalk(ParenthesizedExpression node) { }

        // TypeBinaryExpression
        protected internal virtual bool Walk(TypeBinaryExpression node) { return true; }
        protected internal virtual void PostWalk(TypeBinaryExpression node) { }

        // UnaryExpression
        protected internal virtual bool Walk(UnaryExpression node) { return true; }
        protected internal virtual void PostWalk(UnaryExpression node) { }

        // UnboundAssignment
        protected internal virtual bool Walk(UnboundAssignment node) { return true; }
        protected internal virtual void PostWalk(UnboundAssignment node) { }

        // UnboundExpression
        protected internal virtual bool Walk(UnboundExpression node) { return true; }
        protected internal virtual void PostWalk(UnboundExpression node) { }

        // VoidExpression
        protected internal virtual bool Walk(VoidExpression node) { return true; }
        protected internal virtual void PostWalk(VoidExpression node) { }

        // BlockStatement
        protected internal virtual bool Walk(BlockStatement node) { return true; }
        protected internal virtual void PostWalk(BlockStatement node) { }

        // BreakStatement
        protected internal virtual bool Walk(BreakStatement node) { return true; }
        protected internal virtual void PostWalk(BreakStatement node) { }

        // ContinueStatement
        protected internal virtual bool Walk(ContinueStatement node) { return true; }
        protected internal virtual void PostWalk(ContinueStatement node) { }

        // DebugStatement
        protected internal virtual bool Walk(DebugStatement node) { return true; }
        protected internal virtual void PostWalk(DebugStatement node) { }

        // DeleteStatement
        protected internal virtual bool Walk(DeleteStatement node) { return true; }
        protected internal virtual void PostWalk(DeleteStatement node) { }

        // DoStatement
        protected internal virtual bool Walk(DoStatement node) { return true; }
        protected internal virtual void PostWalk(DoStatement node) { }

        // EmptyStatement
        protected internal virtual bool Walk(EmptyStatement node) { return true; }
        protected internal virtual void PostWalk(EmptyStatement node) { }

        // ExpressionStatement
        protected internal virtual bool Walk(ExpressionStatement node) { return true; }
        protected internal virtual void PostWalk(ExpressionStatement node) { }

        // IfStatement
        protected internal virtual bool Walk(IfStatement node) { return true; }
        protected internal virtual void PostWalk(IfStatement node) { }

        // LabeledStatement
        protected internal virtual bool Walk(LabeledStatement node) { return true; }
        protected internal virtual void PostWalk(LabeledStatement node) { }

        // LoopStatement
        protected internal virtual bool Walk(LoopStatement node) { return true; }
        protected internal virtual void PostWalk(LoopStatement node) { }

        // ReturnStatement
        protected internal virtual bool Walk(ReturnStatement node) { return true; }
        protected internal virtual void PostWalk(ReturnStatement node) { }

        // ScopeStatement
        protected internal virtual bool Walk(ScopeStatement node) { return true; }
        protected internal virtual void PostWalk(ScopeStatement node) { }

        // SwitchStatement
        protected internal virtual bool Walk(SwitchStatement node) { return true; }
        protected internal virtual void PostWalk(SwitchStatement node) { }

        // ThrowStatement
        protected internal virtual bool Walk(ThrowStatement node) { return true; }
        protected internal virtual void PostWalk(ThrowStatement node) { }

        // TryStatement
        protected internal virtual bool Walk(TryStatement node) { return true; }
        protected internal virtual void PostWalk(TryStatement node) { }

        // YieldStatement
        protected internal virtual bool Walk(YieldStatement node) { return true; }
        protected internal virtual void PostWalk(YieldStatement node) { }

        // CatchBlock
        protected internal virtual bool Walk(CatchBlock node) { return true; }
        protected internal virtual void PostWalk(CatchBlock node) { }

        // CodeBlock
        protected internal virtual bool Walk(CodeBlock node) { return true; }
        protected internal virtual void PostWalk(CodeBlock node) { }

        // GeneratorCodeBlock
        protected internal virtual bool Walk(GeneratorCodeBlock node) { return true; }
        protected internal virtual void PostWalk(GeneratorCodeBlock node) { }

        // IfStatementTest
        protected internal virtual bool Walk(IfStatementTest node) { return true; }
        protected internal virtual void PostWalk(IfStatementTest node) { }

        // SwitchCase
        protected internal virtual bool Walk(SwitchCase node) { return true; }
        protected internal virtual void PostWalk(SwitchCase node) { }

        // *** END GENERATED CODE ***

        #endregion

    }
}
