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

using System.Collections.Generic;

namespace Microsoft.Scripting.Ast {
    public partial class Walker {

        public void WalkNode(Node node) {
            if (node == null) {
                return;
            }

            switch (node.NodeType) {
                case AstNodeType.Add:
                case AstNodeType.AddChecked:
                case AstNodeType.And:
                case AstNodeType.AndAlso:
                case AstNodeType.Divide:
                case AstNodeType.Equal:
                case AstNodeType.ExclusiveOr:
                case AstNodeType.GreaterThan:
                case AstNodeType.GreaterThanOrEqual:
                case AstNodeType.LeftShift:
                case AstNodeType.LessThan:
                case AstNodeType.LessThanOrEqual:
                case AstNodeType.Modulo:
                case AstNodeType.Multiply:
                case AstNodeType.MultiplyChecked:
                case AstNodeType.NotEqual:
                case AstNodeType.Or:
                case AstNodeType.OrElse:
                case AstNodeType.RightShift:
                case AstNodeType.Subtract:
                case AstNodeType.SubtractChecked:
                    DefaultWalk((BinaryExpression)node);
                    break;
                case AstNodeType.Call:
                    DefaultWalk((MethodCallExpression)node);
                    break;
                case AstNodeType.Conditional:
                    DefaultWalk((ConditionalExpression)node);
                    break;
                case AstNodeType.Constant:
                    DefaultWalk((ConstantExpression)node);
                    break;
                case AstNodeType.Convert:
                case AstNodeType.Negate:
                case AstNodeType.Not:
                case AstNodeType.OnesComplement:
                    DefaultWalk((UnaryExpression)node);
                    break;
                case AstNodeType.New:
                    DefaultWalk((NewExpression)node);
                    break;
                case AstNodeType.TypeIs:
                    DefaultWalk((TypeBinaryExpression)node);
                    break;
                case AstNodeType.ArrayIndexAssignment:
                    DefaultWalk((ArrayIndexAssignment)node);
                    break;
                case AstNodeType.ArrayIndexExpression:
                    DefaultWalk((ArrayIndexExpression)node);
                    break;
                case AstNodeType.BlockStatement:
                    DefaultWalk((BlockStatement)node);
                    break;
                case AstNodeType.BoundAssignment:
                    DefaultWalk((BoundAssignment)node);
                    break;
                case AstNodeType.WriteStatement:
                    DefaultWalk((WriteStatement)node);
                    break;
                case AstNodeType.BoundExpression:
                    DefaultWalk((BoundExpression)node);
                    break;
                case AstNodeType.CatchBlock:
                    DefaultWalk((CatchBlock)node);
                    break;
                case AstNodeType.CodeBlock:
                    DefaultWalk((CodeBlock)node);
                    break;
                case AstNodeType.CodeBlockExpression:
                    DefaultWalk((CodeBlockExpression)node);
                    break;
                case AstNodeType.CodeContextExpression:
                    DefaultWalk((CodeContextExpression)node);
                    break;
                case AstNodeType.CommaExpression:
                    DefaultWalk((CommaExpression)node);
                    break;
                case AstNodeType.ContinueStatement:
                    DefaultWalk((ContinueStatement)node);
                    break;
                case AstNodeType.EmptyStatement:
                    DefaultWalk((EmptyStatement)node);
                    break;
                case AstNodeType.ExpressionStatement:
                    DefaultWalk((ExpressionStatement)node);
                    break;
                case AstNodeType.IfStatement:
                    DefaultWalk((IfStatement)node);
                    break;
                case AstNodeType.IfStatementTest:
                    DefaultWalk((IfStatementTest)node);
                    break;
                case AstNodeType.LabeledStatement:
                    DefaultWalk((LabeledStatement)node);
                    break;
                case AstNodeType.MemberAssignment:
                    DefaultWalk((MemberAssignment)node);
                    break;
                case AstNodeType.MemberExpression:
                    DefaultWalk((MemberExpression)node);
                    break;
                case AstNodeType.NewArrayExpression:
                    DefaultWalk((NewArrayExpression)node);
                    break;
                case AstNodeType.ReturnStatement:
                    DefaultWalk((ReturnStatement)node);
                    break;
                case AstNodeType.ThrowStatement:
                    DefaultWalk((ThrowStatement)node);
                    break;
                case AstNodeType.TryStatement:
                    DefaultWalk((TryStatement)node);
                    break;
                case AstNodeType.UnboundExpression:
                    DefaultWalk((UnboundExpression)node);
                    break;
                case AstNodeType.VoidExpression:
                    DefaultWalk((VoidExpression)node);
                    break;
            }
        }

        // ArrayIndexAssignment
        private void DefaultWalk(ArrayIndexAssignment node) {
            if (Walk(node)) {
                WalkNode(node.Array);
                WalkNode(node.Index);
                WalkNode(node.Value);
            }
            PostWalk(node);
        }

        // ArrayIndexExpression
        private void DefaultWalk(ArrayIndexExpression node) {
            if (Walk(node)) {
                WalkNode(node.Array);
                WalkNode(node.Index);
            }
            PostWalk(node);
        }

        // BinaryExpression
        private void DefaultWalk(BinaryExpression node) {
            if (Walk(node)) {
                WalkNode(node.Left);
                WalkNode(node.Right);
            }
            PostWalk(node);
        }

        // BoundAssignment
        private void DefaultWalk(BoundAssignment node) {
            if (Walk(node)) {
                WalkNode(node.Value);
            }
            PostWalk(node);
        }

        // WriteStatement
        private void DefaultWalk(WriteStatement node)
        {
          if (Walk(node))
          {
            WalkNode(node.Value);
          }
          PostWalk(node);
        }

        // BoundExpression
        private void DefaultWalk(BoundExpression node) {
            Walk(node);
            PostWalk(node);
        }

        // CodeBlockExpression
        private void DefaultWalk(CodeBlockExpression node) {
            if (node.IsDeclarative && Walk(node)) {
                WalkNode(node.Block);
            }
            PostWalk(node);
        }

        // CodeContextExpression
        private void DefaultWalk(CodeContextExpression node) {
            Walk(node);
            PostWalk(node);
        }

        // CommaExpression
        private void DefaultWalk(CommaExpression node) {
            if (Walk(node)) {
                foreach (Expression e in node.Expressions) {
                    WalkNode(e);
                }
            }
            PostWalk(node);
        }

        // ConditionalExpression
        private void DefaultWalk(ConditionalExpression node) {
            if (Walk(node)) {
                WalkNode(node.Test);
                WalkNode(node.IfTrue);
                WalkNode(node.IfFalse);
            }
            PostWalk(node);
        }

        // ConstantExpression
        private void DefaultWalk(ConstantExpression node) {
            Walk(node);
            PostWalk(node);
        }

        // MemberAssignment
        private void DefaultWalk(MemberAssignment node) {
            if (Walk(node)) {
                WalkNode(node.Expression);
                WalkNode(node.Value);
            }
            PostWalk(node);
        }

        // MemberExpression
        private void DefaultWalk(MemberExpression node) {
            if (Walk(node)) {
                WalkNode(node.Expression);
            }
            PostWalk(node);
        }

        // MethodCallExpression
        private void DefaultWalk(MethodCallExpression node) {
            if (Walk(node)) {
                WalkNode(node.Instance);
                IList<Expression> args = node.Arguments;
                if (args != null) {
                    foreach (Expression e in args) {
                        WalkNode(e);
                    }
                }
            }
            PostWalk(node);
        }

        // NewArrayExpression
        private void DefaultWalk(NewArrayExpression node) {
            if (Walk(node)) {
                foreach (Expression expr in node.Expressions) {
                    WalkNode(expr);
                }
            }
            PostWalk(node);
        }

        // NewExpression
        private void DefaultWalk(NewExpression node) {
            if (Walk(node)) {
                IList<Expression> args = node.Arguments;
                if (args != null) {
                    foreach (Expression e in args) {
                        WalkNode(e);
                    }
                }
            }
            PostWalk(node);
        }

        // TypeBinaryExpression
        private void DefaultWalk(TypeBinaryExpression node) {
            if (Walk(node)) {
                WalkNode(node.Expression);
            }
            PostWalk(node);
        }

        // UnaryExpression
        private void DefaultWalk(UnaryExpression node) {
            if (Walk(node)) {
                WalkNode(node.Operand);
            }
            PostWalk(node);
        }

        // UnboundExpression
        private void DefaultWalk(UnboundExpression node) {
            Walk(node);
            PostWalk(node);
        }

        // VoidExpression
        private void DefaultWalk(VoidExpression node) {
            if (Walk(node)) {
                WalkNode(node.Statement);
            }
            PostWalk(node);
        }

        // BlockStatement
        private void DefaultWalk(BlockStatement node) {
            if (Walk(node)) {
                foreach (Statement stmt in node.Statements) {
                    WalkNode(stmt);
                }
            }
            PostWalk(node);
        }


        // ContinueStatement
        private void DefaultWalk(ContinueStatement node) {
            Walk(node);
            PostWalk(node);
        }
        // EmptyStatement
        private void DefaultWalk(EmptyStatement node) {
            Walk(node);
            PostWalk(node);
        }

        // ExpressionStatement
        private void DefaultWalk(ExpressionStatement node) {
            if (Walk(node)) {
                WalkNode(node.Expression);
            }
            PostWalk(node);
        }

        // IfStatement
        private void DefaultWalk(IfStatement node) {
            if (Walk(node)) {
                foreach (IfStatementTest t in node.Tests) {
                    WalkNode(t);
                }
                WalkNode(node.ElseStatement);
            }
            PostWalk(node);
        }

        // LabeledStatement
        private void DefaultWalk(LabeledStatement node) {
            if (Walk(node)) {
                WalkNode(node.Statement);
            }
            PostWalk(node);
        }

        // ReturnStatement
        private void DefaultWalk(ReturnStatement node) {
            if (Walk(node)) {
                WalkNode(node.Expression);
            }
            PostWalk(node);
        }

        // ThrowStatement
        private void DefaultWalk(ThrowStatement node) {
            if (Walk(node)) {
                WalkNode(node.Value);
            }
            PostWalk(node);
        }

        // TryStatement
        private void DefaultWalk(TryStatement node) {
            if (Walk(node)) {
                WalkNode(node.Body);
                if (node.Handlers != null) {
                    foreach (CatchBlock handler in node.Handlers) {
                        WalkNode(handler);
                    }
                }
                WalkNode(node.FinallyStatement);
            }
            PostWalk(node);
        }

        // CatchBlock
        private void DefaultWalk(CatchBlock node) {
            if (Walk(node)) {
                WalkNode(node.Body);
            }
            PostWalk(node);
        }

        // CodeBlock
        private void DefaultWalk(CodeBlock node) {
            if (Walk(node)) {
                WalkNode(node.Body);
            }
            PostWalk(node);
        }

        // IfStatementTest
        private void DefaultWalk(IfStatementTest node) {
            if (Walk(node)) {
                WalkNode(node.Test);
                WalkNode(node.Body);
            }
            PostWalk(node);
        }
    }
}
