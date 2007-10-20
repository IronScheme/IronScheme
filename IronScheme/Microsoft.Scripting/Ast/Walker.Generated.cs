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

using System.Diagnostics;

namespace Microsoft.Scripting.Ast {
    #region Generated Scripting AST Walker

    // *** BEGIN GENERATED CODE ***

    /// <summary>
    /// Walker class - The Scripting AST Walker (default result is true)
    /// </summary>
    public class Walker {
        // ActionExpression
        public virtual bool Walk(ActionExpression node) { return true; }
        public virtual void PostWalk(ActionExpression node) { }

        // ArrayIndexAssignment
        public virtual bool Walk(ArrayIndexAssignment node) { return true; }
        public virtual void PostWalk(ArrayIndexAssignment node) { }

        // ArrayIndexExpression
        public virtual bool Walk(ArrayIndexExpression node) { return true; }
        public virtual void PostWalk(ArrayIndexExpression node) { }

        // BinaryExpression
        public virtual bool Walk(BinaryExpression node) { return true; }
        public virtual void PostWalk(BinaryExpression node) { }

        // BoundAssignment
        public virtual bool Walk(BoundAssignment node) { return true; }
        public virtual void PostWalk(BoundAssignment node) { }

        // BoundExpression
        public virtual bool Walk(BoundExpression node) { return true; }
        public virtual void PostWalk(BoundExpression node) { }

        // CallWithThisExpression
        public virtual bool Walk(CallWithThisExpression node) { return true; }
        public virtual void PostWalk(CallWithThisExpression node) { }

        // CodeBlockExpression
        public virtual bool Walk(CodeBlockExpression node) { return true; }
        public virtual void PostWalk(CodeBlockExpression node) { }

        // CodeContextExpression
        public virtual bool Walk(CodeContextExpression node) { return true; }
        public virtual void PostWalk(CodeContextExpression node) { }

        // CommaExpression
        public virtual bool Walk(CommaExpression node) { return true; }
        public virtual void PostWalk(CommaExpression node) { }

        // ConditionalExpression
        public virtual bool Walk(ConditionalExpression node) { return true; }
        public virtual void PostWalk(ConditionalExpression node) { }

        // ConstantExpression
        public virtual bool Walk(ConstantExpression node) { return true; }
        public virtual void PostWalk(ConstantExpression node) { }

        // DeleteUnboundExpression
        public virtual bool Walk(DeleteUnboundExpression node) { return true; }
        public virtual void PostWalk(DeleteUnboundExpression node) { }

        // DynamicConversionExpression
        public virtual bool Walk(DynamicConversionExpression node) { return true; }
        public virtual void PostWalk(DynamicConversionExpression node) { }

        // EnvironmentExpression
        public virtual bool Walk(EnvironmentExpression node) { return true; }
        public virtual void PostWalk(EnvironmentExpression node) { }

        // MemberAssignment
        public virtual bool Walk(MemberAssignment node) { return true; }
        public virtual void PostWalk(MemberAssignment node) { }

        // MemberExpression
        public virtual bool Walk(MemberExpression node) { return true; }
        public virtual void PostWalk(MemberExpression node) { }

        // MethodCallExpression
        public virtual bool Walk(MethodCallExpression node) { return true; }
        public virtual void PostWalk(MethodCallExpression node) { }

        // NewArrayExpression
        public virtual bool Walk(NewArrayExpression node) { return true; }
        public virtual void PostWalk(NewArrayExpression node) { }

        // NewExpression
        public virtual bool Walk(NewExpression node) { return true; }
        public virtual void PostWalk(NewExpression node) { }

        // ParamsExpression
        public virtual bool Walk(ParamsExpression node) { return true; }
        public virtual void PostWalk(ParamsExpression node) { }

        // ParenthesizedExpression
        public virtual bool Walk(ParenthesizedExpression node) { return true; }
        public virtual void PostWalk(ParenthesizedExpression node) { }

        // ShortCircuitExpression
        public virtual bool Walk(ShortCircuitExpression node) { return true; }
        public virtual void PostWalk(ShortCircuitExpression node) { }

        // TypeBinaryExpression
        public virtual bool Walk(TypeBinaryExpression node) { return true; }
        public virtual void PostWalk(TypeBinaryExpression node) { }

        // UnaryExpression
        public virtual bool Walk(UnaryExpression node) { return true; }
        public virtual void PostWalk(UnaryExpression node) { }

        // UnboundAssignment
        public virtual bool Walk(UnboundAssignment node) { return true; }
        public virtual void PostWalk(UnboundAssignment node) { }

        // UnboundExpression
        public virtual bool Walk(UnboundExpression node) { return true; }
        public virtual void PostWalk(UnboundExpression node) { }

        // VoidExpression
        public virtual bool Walk(VoidExpression node) { return true; }
        public virtual void PostWalk(VoidExpression node) { }

        // BlockStatement
        public virtual bool Walk(BlockStatement node) { return true; }
        public virtual void PostWalk(BlockStatement node) { }

        // BreakStatement
        public virtual bool Walk(BreakStatement node) { return true; }
        public virtual void PostWalk(BreakStatement node) { }

        // ContinueStatement
        public virtual bool Walk(ContinueStatement node) { return true; }
        public virtual void PostWalk(ContinueStatement node) { }

        // DebugStatement
        public virtual bool Walk(DebugStatement node) { return true; }
        public virtual void PostWalk(DebugStatement node) { }

        // DeleteStatement
        public virtual bool Walk(DeleteStatement node) { return true; }
        public virtual void PostWalk(DeleteStatement node) { }

        // DoStatement
        public virtual bool Walk(DoStatement node) { return true; }
        public virtual void PostWalk(DoStatement node) { }

        // EmptyStatement
        public virtual bool Walk(EmptyStatement node) { return true; }
        public virtual void PostWalk(EmptyStatement node) { }

        // ExpressionStatement
        public virtual bool Walk(ExpressionStatement node) { return true; }
        public virtual void PostWalk(ExpressionStatement node) { }

        // IfStatement
        public virtual bool Walk(IfStatement node) { return true; }
        public virtual void PostWalk(IfStatement node) { }

        // LabeledStatement
        public virtual bool Walk(LabeledStatement node) { return true; }
        public virtual void PostWalk(LabeledStatement node) { }

        // LoopStatement
        public virtual bool Walk(LoopStatement node) { return true; }
        public virtual void PostWalk(LoopStatement node) { }

        // ReturnStatement
        public virtual bool Walk(ReturnStatement node) { return true; }
        public virtual void PostWalk(ReturnStatement node) { }

        // ScopeStatement
        public virtual bool Walk(ScopeStatement node) { return true; }
        public virtual void PostWalk(ScopeStatement node) { }

        // SwitchStatement
        public virtual bool Walk(SwitchStatement node) { return true; }
        public virtual void PostWalk(SwitchStatement node) { }

        // ThrowStatement
        public virtual bool Walk(ThrowStatement node) { return true; }
        public virtual void PostWalk(ThrowStatement node) { }

        // TryStatement
        public virtual bool Walk(TryStatement node) { return true; }
        public virtual void PostWalk(TryStatement node) { }

        // YieldStatement
        public virtual bool Walk(YieldStatement node) { return true; }
        public virtual void PostWalk(YieldStatement node) { }

        // Arg
        public virtual bool Walk(Arg node) { return true; }
        public virtual void PostWalk(Arg node) { }

        // CatchBlock
        public virtual bool Walk(CatchBlock node) { return true; }
        public virtual void PostWalk(CatchBlock node) { }

        // CodeBlock
        public virtual bool Walk(CodeBlock node) { return true; }
        public virtual void PostWalk(CodeBlock node) { }

        // GeneratorCodeBlock
        public virtual bool Walk(GeneratorCodeBlock node) { return true; }
        public virtual void PostWalk(GeneratorCodeBlock node) { }

        // IfStatementTest
        public virtual bool Walk(IfStatementTest node) { return true; }
        public virtual void PostWalk(IfStatementTest node) { }
    }

    // *** END GENERATED CODE ***

    #endregion
}
