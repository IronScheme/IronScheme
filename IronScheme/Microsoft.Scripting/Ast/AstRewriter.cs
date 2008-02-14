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
using System.Collections.Generic;
using System.Collections.ObjectModel;

using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Actions;

namespace Microsoft.Scripting.Ast {
    class AstRewriter {
        private abstract class TempMaker {
            /// <summary>
            /// Current temporary variable
            /// </summary>
            private int _temp;

            /// <summary>
            /// List of free temporary variables
            /// </summary>
            private List<Variable> _freeTemps;

            /// <summary>
            /// Stack of currently active temporary variables.
            /// </summary>
            private Stack<Variable> _usedTemps;

            internal Variable Temp(Type type) {
                Variable temp;
                if (_freeTemps != null) {
                    for (int i = _freeTemps.Count - 1; i >= 0; i--) {
                        temp = _freeTemps[i];
                        if (temp.Type == type) {
                            _freeTemps.RemoveAt(i);
                            return UseTemp(temp);
                        }
                    }
                }
                temp = MakeTemp("$temp$" + _temp++, type);
                return UseTemp(temp);
            }

            private Variable UseTemp(Variable temp) {
                Debug.Assert(_freeTemps == null || !_freeTemps.Contains(temp));
                Debug.Assert(_usedTemps == null || !_usedTemps.Contains(temp));

                if (_usedTemps == null) {
                    _usedTemps = new Stack<Variable>();
                }
                _usedTemps.Push(temp);
                return temp;
            }

            private void FreeTemp(Variable temp) {
                Debug.Assert(_freeTemps == null || !_freeTemps.Contains(temp));
                if (_freeTemps == null) {
                    _freeTemps = new List<Variable>();
                }
                _freeTemps.Add(temp);
            }

            internal int Mark() {
                return _usedTemps != null ? _usedTemps.Count : 0;
            }

            internal void Free(int mark) {
                // (_usedTemps != null) ==> (mark <= _usedTemps.Count)
                Debug.Assert(_usedTemps == null || mark <= _usedTemps.Count);
                // (_usedTemps == null) ==> (mark == 0)
                Debug.Assert(mark == 0 || _usedTemps != null);

                if (_usedTemps != null) {
                    while (mark < _usedTemps.Count) {
                        FreeTemp(_usedTemps.Pop());
                    }
                }
            }

            [Conditional("DEBUG")]
            internal void VerifyTemps() {
                Debug.Assert(_usedTemps == null || _usedTemps.Count == 0);
            }

            protected internal abstract Variable MakeTemp(string name, Type type);
        }

        private class BlockTempMaker : TempMaker {
            /// <summary>
            /// CodeBlock the body of which is being rewritten
            /// </summary>
            private readonly CodeBlock _block;

            internal BlockTempMaker(CodeBlock block) {
                Debug.Assert(block != null);
                _block = block;
            }
            protected internal override Variable MakeTemp(string name, Type type) {
                return _block.CreateTemporaryVariable(SymbolTable.StringToId(name), type);
            }
        }

        private class RuleTempMaker : TempMaker {
            /// <summary>
            /// Rule which is being rewritten
            /// </summary>
            private readonly StandardRule _rule;

            internal RuleTempMaker(StandardRule rule) {
                Debug.Assert(rule != null);
                _rule = rule;
            }

            protected internal override Variable MakeTemp(string name, Type type) {
                return _rule.GetTemporary(type, name);
            }
        }

        /// <summary>
        /// The source of temporary variables, either BlockTempMaker or RuleTempMaker
        /// </summary>
        private readonly TempMaker _tm;

        /// <summary>
        /// Mapping of statements before and after rewrite.
        /// This is to handle break and continue statements
        /// which must be redirected to the new statements.
        /// </summary>
        private Dictionary<Statement, Statement> _map;

        /// <summary>
        /// List of break statements in the AST
        /// </summary>
        private List<BreakStatement> _break;

        /// <summary>
        /// List of continue statements in the AST
        /// </summary>
        private List<ContinueStatement> _continue;

        #region Rewriter entry points

        public static void RewriteBlock(CodeBlock block) {
            AstRewriter ar = new AstRewriter(new BlockTempMaker(block));
            ar.Rewrite(block);
        }

        public static void RewriteRule(StandardRule rule) {
            AstRewriter ar = new AstRewriter(new RuleTempMaker(rule));
            ar.Rewrite(rule);
        }

        #endregion

        private AstRewriter(TempMaker tm) {
            _tm = tm;
        }

        private void Rewrite(CodeBlock block) {
            VerifyTemps();

            Statement body = RewriteStatement(block.Body);

            VerifyTemps();

            if ((object)body != (object)block.Body) {
                FixBreakAndContinue();
                block.Body = body;
            }
        }

        private void Rewrite(StandardRule rule) {
            VerifyTemps();

            Expression test = RewriteExpressionFreeTemps(rule.Test);
            Statement target = RewriteStatement(rule.Target);

            VerifyTemps();

            if (((object)test != (object)rule.Test) ||
                ((object)target != (object)rule.Target)) {
                FixBreakAndContinue();

                if ((object)test != (object)rule.Test) {
                    rule.RewriteTest(test);
                }
                if ((object)target != (object)rule.Target) {
                    rule.RewriteTarget(target);
                }
            }
        }

        #region Temps

        private Variable Temp(Type type) {
            return _tm.Temp(type);
        }

        private int Mark() {
            return _tm.Mark();
        }

        private void Free(int mark) {
            _tm.Free(mark);
        }

        [Conditional("DEBUG")]
        private void VerifyTemps() {
            _tm.VerifyTemps();
        }

        #endregion

        /// <summary>
        /// Will perform:
        ///     save: temp = expression
        ///     return value: temp
        /// </summary>
        private Expression ToTemp(Expression expression, out Expression save) {
            Variable temp = Temp(expression.Type);
            save = Ast.Assign(temp, expression);
            return Ast.Read(temp);
        }

        #region Rewritten statement mapping

        private Statement Map(Statement original, Statement rewritten) {
            if ((object)original != (object)rewritten) {
                if (_map == null) {
                    _map = new Dictionary<Statement, Statement>();
                }
                _map.Add(original, rewritten);
            }
            return rewritten;
        }

        private bool TryFindMapping(Statement original, out Statement rewritten) {
            if (_map != null) {
                return _map.TryGetValue(original, out rewritten);
            } else {
                rewritten = null;
                return false;
            }
        }

        private void FixBreakAndContinue() {
            Statement mapped;

            if (_break != null) {
                foreach (BreakStatement b in _break) {
                    if (TryFindMapping(b.Statement, out mapped)) {
                        b.Statement = mapped;
                    }
                }
            }
            if (_continue != null) {
                foreach (ContinueStatement c in _continue) {
                    if (TryFindMapping(c.Statement, out mapped)) {
                        c.Statement = mapped;
                    }
                }
            }
        }

        #endregion

        #region Expressions

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1800:DoNotCastUnnecessarily")]
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Maintainability", "CA1502:AvoidExcessiveComplexity")]
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Maintainability", "CA1506:AvoidExcessiveClassCoupling")]
        private Expression RewriteExpression(Expression node) {
            if (node == null) {
                return null;
            }

            switch (node.NodeType) {
                case AstNodeType.AndAlso:
                case AstNodeType.OrElse:
                    return RewriteLogical((BinaryExpression)node);
                case AstNodeType.Add:
                case AstNodeType.AddChecked:
                case AstNodeType.And:
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
                case AstNodeType.RightShift:
                case AstNodeType.Subtract:
                case AstNodeType.SubtractChecked:
                    return Rewrite((BinaryExpression)node);

                case AstNodeType.Call:
                    return Rewrite((MethodCallExpression)node);

                case AstNodeType.Conditional:
                    return Rewrite((ConditionalExpression)node);

                case AstNodeType.Constant:
                    return Rewrite((ConstantExpression)node);

                case AstNodeType.Convert:
                case AstNodeType.Negate:
                case AstNodeType.Not:
                case AstNodeType.OnesComplement:
                    return Rewrite((UnaryExpression)node);

                case AstNodeType.New:
                    return Rewrite((NewExpression)node);

                case AstNodeType.TypeIs:
                    return Rewrite((TypeBinaryExpression)node);

                case AstNodeType.ActionExpression:
                    return Rewrite((ActionExpression)node);

                case AstNodeType.ArrayIndexAssignment:
                    return Rewrite((ArrayIndexAssignment)node);

                case AstNodeType.ArrayIndexExpression:
                    return Rewrite((ArrayIndexExpression)node);

                case AstNodeType.BoundAssignment:
                    return Rewrite((BoundAssignment)node);

                case AstNodeType.BoundExpression:
                    return Rewrite((BoundExpression)node);

                case AstNodeType.CodeBlockExpression:
                    return Rewrite((CodeBlockExpression)node);

                case AstNodeType.CodeContextExpression:
                    return Rewrite((CodeContextExpression)node);

                case AstNodeType.CommaExpression:
                    return Rewrite((CommaExpression)node);

                case AstNodeType.DeleteUnboundExpression:
                    return Rewrite((DeleteUnboundExpression)node);

                case AstNodeType.DynamicConversionExpression:
                    return Rewrite((DynamicConversionExpression)node);

                case AstNodeType.EnvironmentExpression:
                    return Rewrite((EnvironmentExpression)node);

                case AstNodeType.MemberAssignment:
                    return Rewrite((MemberAssignment)node);

                case AstNodeType.MemberExpression:
                    return Rewrite((MemberExpression)node);

                case AstNodeType.NewArrayExpression:
                    return Rewrite((NewArrayExpression)node);

                case AstNodeType.ParamsExpression:
                    return Rewrite((ParamsExpression)node);

                case AstNodeType.ParenthesizedExpression:
                    return Rewrite((ParenthesizedExpression)node);

                case AstNodeType.UnboundAssignment:
                    return Rewrite((UnboundAssignment)node);

                case AstNodeType.UnboundExpression:
                    return Rewrite((UnboundExpression)node);

                case AstNodeType.VoidExpression:
                    return Rewrite((VoidExpression)node);

                default:
                    throw new InvalidOperationException();
            }
        }

        private Expression RewriteExpressionFreeTemps(Expression expression) {
            int mark = Mark();
            expression = RewriteExpression(expression);
            Free(mark);
            return expression;
        }

        // ActionExpression
        private Expression Rewrite(ActionExpression node) {
            Expression[] clone, comma;
            if (RewriteExpressions(node.Arguments, out clone, out comma)) {
                comma[comma.Length - 1] =
                    Ast.Action.ActionExpression(node.Action, clone, node.Type);
                return Ast.Comma(comma);
            } else {
                return node;
            }
        }

        // ArrayIndexAssignment
        private Expression Rewrite(ArrayIndexAssignment node) {
            Expression value = RewriteExpression(node.Value);
            Expression array = RewriteExpression(node.Array);
            Expression index = RewriteExpression(node.Index);

            // Did any of them change?
            if (((object)value != (object)node.Value) ||
                ((object)array != (object)node.Array) ||
                ((object)index != (object)node.Index)) {

                Expression saveValue, saveArray, saveIndex;

                value = ToTemp(value, out saveValue);
                array = ToTemp(array, out saveArray);
                index = ToTemp(index, out saveIndex);

                return Ast.Comma(
                    saveValue,
                    saveArray,
                    saveIndex,
                    Ast.AssignArrayIndex(array, index, value)
                );
            } else {
                return node;
            }
        }

        // ArrayIndexExpression
        private Expression Rewrite(ArrayIndexExpression node) {
            Expression array = RewriteExpression(node.Array);
            Expression index = RewriteExpression(node.Index);

            if (((object)array != (object)node.Array) ||
                ((object)index != (object)node.Index)) {

                Expression saveArray, saveIndex;

                array = ToTemp(array, out saveArray);
                index = ToTemp(index, out saveIndex);

                return Ast.Comma(
                    saveArray,
                    saveIndex,
                    Ast.ArrayIndex(array, index)
                );
            } else {
                return node;
            }
        }

        // BinaryExpression: AndAlso, OrElse
        private Expression RewriteLogical(BinaryExpression node) {
            Expression left = RewriteExpression(node.Left);
            Expression right = RewriteExpression(node.Right);

            if (((object)left != (object)node.Left) ||
                ((object)right != (object)node.Right)) {
                return new BinaryExpression(node.NodeType, left, right);
            } else {
                return node;
            }
        }

        // BinaryExpression
        private Expression Rewrite(BinaryExpression node) {
            Expression left = RewriteExpression(node.Left);
            Expression right = RewriteExpression(node.Right);

            if (((object)left != (object)node.Left) ||
                ((object)right != (object)node.Right)) {
                Expression saveLeft, saveRight;

                left = ToTemp(left, out saveLeft);
                right = ToTemp(right, out saveRight);

                return Ast.Comma(
                    saveLeft,
                    saveRight,
                    new BinaryExpression(node.NodeType, left, right)
                );
            } else {
                return node;
            }
        }

        // BoundAssignment
        private Expression Rewrite(BoundAssignment node) {
            Expression value = RewriteExpression(node.Value);
            if ((object)value != (object)node.Value) {
                return Ast.Assign(node.Variable, value);
            } else {
                return node;
            }
        }

        // BoundExpression
        private Expression Rewrite(BoundExpression node) {
            // No action necessary
            return node;
        }

        // CodeBlockExpression
        private Expression Rewrite(CodeBlockExpression node) {
            // No action necessary
            return node;
        }

        // CodeContextExpression
        private Expression Rewrite(CodeContextExpression node) {
            // No action necessary
            return node;
        }

        // CommaExpression
        private Expression Rewrite(CommaExpression node) {
            ReadOnlyCollection<Expression> expressions = node.Expressions;
            int index = node.ValueIndex;
            Expression[] clone = null;
            Expression result = null;

            for (int i = 0; i < expressions.Count; i++) {
                Expression expression = expressions[i];
                Expression rewritten = RewriteExpression(expression);

                if ((object)expression != (object)rewritten) {
                    if (clone == null) {
                        int size = expressions.Count;
                        // If the result is not at the end of the comma,
                        // we need an extra element for the result temp.
                        if (index < expressions.Count - 1) {
                            size++;
                        }
                        clone = new Expression[size];
                        for (int j = 0; j < i; j++) {
                            Expression expr = expressions[j];
                            if (j == index) {
                                // This expression is not the last (j < i < expressions.Count)
                                Debug.Assert(j < expressions.Count - 1);
                                result = ToTemp(expr, out expr);
                            }
                            clone[j] = expr;
                        }
                    }
                }

                if (clone != null) {
                    if (i == index && index < expressions.Count - 1) {
                        result = ToTemp(rewritten, out rewritten);
                    }
                    clone[i] = rewritten;
                }
            }

            if (clone != null) {
                if (result != null) {
                    Debug.Assert(index < expressions.Count - 1);
                    Debug.Assert(clone[clone.Length - 1] == null);
                    clone[clone.Length - 1] = result;
                }
                return Ast.Comma(clone);
            } else {
                return node;
            }
        }

        // ConditionalExpression
        private Expression Rewrite(ConditionalExpression node) {
            Expression test = RewriteExpression(node.Test);
            Expression ifTrue = RewriteExpression(node.IfTrue);
            Expression ifFalse = RewriteExpression(node.IfFalse);

            if (((object)test != (object)node.Test) ||
                ((object)ifTrue != (object)node.IfTrue) ||
                ((object)ifFalse != (object)node.IfFalse)) {
                return Ast.Condition(test, ifTrue, ifFalse);
            } else {
                return node;
            }
        }

        // ConstantExpression
        private Expression Rewrite(ConstantExpression node) {
            // No action necessary
            return node;
        }

        // DeleteUnboundExpression
        private Expression Rewrite(DeleteUnboundExpression node) {
            // No action necessary
            return node;
        }

        // DynamicConversionExpression
        private Expression Rewrite(DynamicConversionExpression node) {
            // No action necessary
            return node;
        }

        // EnvironmentExpression
        private Expression Rewrite(EnvironmentExpression node) {
            // No action necessary
            return node;
        }

        // MemberAssignment
        private Expression Rewrite(MemberAssignment node) {
            Expression expression = RewriteExpression(node.Expression);
            Expression value = RewriteExpression(node.Value);

            if (((object)expression != (object)node.Expression) ||
                ((object)value != (object)node.Value)) {

                if (expression != null) {
                    Expression saveExpression, saveValue;
                    expression = ToTemp(expression, out saveExpression);
                    value = ToTemp(value, out saveValue);

                    return Ast.Comma(
                        saveExpression,
                        saveValue,
                        new MemberAssignment(node.Member, expression, value)
                    );
                } else {
                    // Expression is null, value gets an empty stack
                    return new MemberAssignment(node.Member, expression, value);
                }
            } else {
                return node;
            }
        }

        // MemberExpression
        private Expression Rewrite(MemberExpression node) {
            Expression expression = RewriteExpression(node.Expression);
            if ((object)expression != (object)node.Expression) {
                return new MemberExpression(node.Member, expression);
            } else {
                return node;
            }
        }

        // MethodCallExpression
        // TODO: ref parameters!!!
        private Expression Rewrite(MethodCallExpression node) {
            Expression instance = RewriteExpression(node.Instance);
            ReadOnlyCollection<Expression> args = node.Arguments;
            Expression[] clone = null;
            Expression[] comma = null;
            int ci = 0; // comma array fill index

            if (args != null) {
                for (int i = 0; i < args.Count; i++) {
                    Expression arg = args[i];
                    Expression rarg = RewriteExpression(arg);

                    if ((object)arg != (object)rarg) {
                        if (clone == null) {
                            clone = new Expression[args.Count];
                            if (instance != null) {
                                comma = new Expression[args.Count + 2]; // + instance + the call
                                instance = ToTemp(instance, out comma[ci++]);
                            } else {
                                comma = new Expression[args.Count + 1];
                            }

                            for (int j = 0; j < i; j++) {
                                clone[j] = ToTemp(args[j], out comma[ci++]);
                            }
                        }
                    }

                    if (clone != null) {
                        clone[i] = ToTemp(rarg, out comma[ci++]);
                    }
                }
            }

            if (clone != null) {
                comma[ci] = Ast.Call(instance, node.Method, clone);
                return Ast.Comma(comma);
            } else if ((object)instance != (object)node.Instance) {
                return Ast.Call(
                    instance,
                    node.Method,
                    node.Arguments
                );
            } else {
                return node;
            }
        }

        // NewArrayExpression
        private Expression Rewrite(NewArrayExpression node) {
            Expression[] clone, comma;

            if (RewriteExpressions(node.Expressions, out clone, out comma)) {
                comma[comma.Length - 1] = Ast.NewArray(node.Type, clone);
                return Ast.Comma(comma);
            } else {
                return node;
            }
        }

        // NewExpression
        private Expression Rewrite(NewExpression node) {
            Expression[] clone, comma;
            if (RewriteExpressions(node.Arguments, out clone, out comma)) {
                comma[comma.Length - 1] =
                    Ast.New(node.Constructor, clone);
                return Ast.Comma(comma);
            } else {
                return node;
            }
        }

        // ParamsExpression
        private Expression Rewrite(ParamsExpression node) {
            // No action necessary
            return node;
        }

        // ParenthesizedExpression
        private Expression Rewrite(ParenthesizedExpression node) {
            Expression expression = RewriteExpression(node.Expression);
            if ((object)expression != (object)node.Expression) {
                return Ast.Parenthesize(expression);
            } else {
                return node;
            }
        }

        // TypeBinaryExpression
        private Expression Rewrite(TypeBinaryExpression node) {
            Expression expression = RewriteExpression(node.Expression);
            if ((object)expression != (object)node.Expression) {
                return Ast.TypeIs(expression, node.TypeOperand);
            } else {
                return node;
            }
        }

        // UnaryExpression
        private Expression Rewrite(UnaryExpression node) {
            Expression expression = RewriteExpression(node.Operand);
            if ((object)expression != (object)node.Operand) {
                return new UnaryExpression(node.NodeType, expression, node.Type);
            } else {
                return node;
            }
        }

        // UnboundAssignment
        private Expression Rewrite(UnboundAssignment node) {
            Expression expression = RewriteExpression(node.Value);
            if ((object)expression != (object)node.Value) {
                return Ast.Assign(node.Name, expression);
            } else {
                return node;
            }
        }

        // UnboundExpression
        private Expression Rewrite(UnboundExpression node) {
            // No action necessary
            return node;
        }

        // VoidExpression
        private Expression Rewrite(VoidExpression node) {
            Statement statement = RewriteStatement(node.Statement);
            return Ast.Void(statement);
        }

        #endregion

        #region Statements

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Maintainability", "CA1502:AvoidExcessiveComplexity")]
        private Statement RewriteStatement(Statement node) {
            if (node == null) {
                return null;
            }

            Statement result;

            switch (node.NodeType) {
                case AstNodeType.BlockStatement:
                    result = Rewrite((BlockStatement)node);
                    break;

                case AstNodeType.BreakStatement:
                    result = Rewrite((BreakStatement)node);
                    break;

                case AstNodeType.ContinueStatement:
                    result = Rewrite((ContinueStatement)node);
                    break;

                case AstNodeType.DebugStatement:
                    result = Rewrite((DebugStatement)node);
                    break;

                case AstNodeType.DeleteStatement:
                    result = Rewrite((DeleteStatement)node);
                    break;

                case AstNodeType.DoStatement:
                    result = Rewrite((DoStatement)node);
                    break;

                case AstNodeType.EmptyStatement:
                    result = Rewrite((EmptyStatement)node);
                    break;

                case AstNodeType.ExpressionStatement:
                    result = Rewrite((ExpressionStatement)node);
                    break;

                case AstNodeType.IfStatement:
                    result = Rewrite((IfStatement)node);
                    break;

                case AstNodeType.LabeledStatement:
                    result = Rewrite((LabeledStatement)node);
                    break;

                case AstNodeType.LoopStatement:
                    result = Rewrite((LoopStatement)node);
                    break;

                case AstNodeType.ReturnStatement:
                    result = Rewrite((ReturnStatement)node);
                    break;

                case AstNodeType.ScopeStatement:
                    result = Rewrite((ScopeStatement)node);
                    break;

                case AstNodeType.SwitchStatement:
                    result = Rewrite((SwitchStatement)node);
                    break;

                case AstNodeType.ThrowStatement:
                    result = Rewrite((ThrowStatement)node);
                    break;

                case AstNodeType.TryStatement:
                    result = Rewrite((TryStatement)node);
                    break;

                case AstNodeType.YieldStatement:
                    result = Rewrite((YieldStatement)node);
                    break;

                default:
                    throw new InvalidOperationException();
            }

            // Store the mapping to resolve the break and continue later
            return Map(node, result);
        }

        // BlockStatement
        private Statement Rewrite(BlockStatement node) {
            ReadOnlyCollection<Statement> statements = node.Statements;
            Statement[] clone = null;

            for (int i = 0; i < statements.Count; i++) {
                Statement statement = statements[i];
                Statement rewritten = RewriteStatement(statement);

                if (((object)rewritten != (object)statement) && (clone == null)) {
                    clone = Clone(statements, i);
                }

                if (clone != null) {
                    clone[i] = rewritten;
                }
            }

            if (clone != null) {
                return Ast.Block(node.Span, clone);
            } else {
                return node;
            }
        }

        // BreakStatement
        private Statement Rewrite(BreakStatement node) {
            if (node.Statement != null) {
                if (_break == null) {
                    _break = new List<BreakStatement>();
                }
                _break.Add(node);
            }

            // No further action necessary
            return node;
        }

        // ContinueStatement
        private Statement Rewrite(ContinueStatement node) {
            if (node.Statement != null) {
                if (_continue == null) {
                    _continue = new List<ContinueStatement>();
                }
                _continue.Add(node);
            }

            // No further action necessary
            return node;
        }

        // DebugStatement
        private Statement Rewrite(DebugStatement node) {
            // No action necessary
            return node;
        }

        // DeleteStatement
        private Statement Rewrite(DeleteStatement node) {
            // No action necessary
            return node;
        }

        // DoStatement
        private Statement Rewrite(DoStatement node) {
            Statement body = RewriteStatement(node.Body);
            Expression test = RewriteExpressionFreeTemps(node.Test);

            if (((object)body != (object)node.Body) ||
                ((object)test != (object)node.Test)) {
                return new DoStatement(node.Span, node.Header, test, body);
            } else {
                return node;
            }
        }

        // EmptyStatement
        private Statement Rewrite(EmptyStatement node) {
            // No action necessary
            return node;
        }

        // ExpressionStatement
        private Statement Rewrite(ExpressionStatement node) {
            Expression expression = RewriteExpressionFreeTemps(node.Expression);
            if ((object)expression != (object)node.Expression) {
                return Ast.Statement(node.Span, expression);
            } else {
                return node;
            }
        }

        // IfStatement
        private Statement Rewrite(IfStatement node) {
            ReadOnlyCollection<IfStatementTest> tests = node.Tests;
            IfStatementTest[] clone = null;

            for (int i = 0; i < tests.Count; i++) {
                IfStatementTest test = tests[i];
                IfStatementTest rtest = Rewrite(test);

                if (((object)test != (object)rtest) && (clone == null)) {
                    clone = Clone(tests, i);
                }

                if (clone != null) {
                    clone[i] = rtest;
                }
            }

            Statement @else = RewriteStatement(node.ElseStatement);
            
            // Did we rewrite anything?
            if (clone != null) {
                return new IfStatement(node.Span, CollectionUtils.ToReadOnlyCollection(clone), @else);
            } else if ((object)@else != (object)node.ElseStatement) {
                return new IfStatement(node.Span, CollectionUtils.ToReadOnlyCollection(tests), @else);
            } else {
                return node;
            }
        }

        // LabeledStatement
        private Statement Rewrite(LabeledStatement node) {
            Statement statement = RewriteStatement(node.Statement);
            if ((object)statement != (object)node.Statement) {
                return Ast.Labeled(node.Span, statement);
            } else {
                return node;
            }
        }

        // LoopStatement
        private Statement Rewrite(LoopStatement node) {
            Expression test = RewriteExpressionFreeTemps(node.Test);
            Expression incr = RewriteExpressionFreeTemps(node.Increment);
            Statement body = RewriteStatement(node.Body);
            Statement @else = RewriteStatement(node.ElseStatement);

            if (((object)test != (object)node.Test) ||
                ((object)incr != (object)node.Increment) ||
                ((object)body != (object)node.Body) ||
                ((object)@else != (object)node.ElseStatement)) {
                return Ast.Loop(node.Span, node.Header, test, incr, body, @else);
            } else {
                return node;
            }
        }

        // ReturnStatement
        private Statement Rewrite(ReturnStatement node) {
            Expression expression = RewriteExpressionFreeTemps(node.Expression);
            if ((object)expression != (object)node.Expression) {
                return Ast.Return(node.Span, expression);
            } else {
                return node;
            }
        }

        // ScopeStatement
        private Statement Rewrite(ScopeStatement node) {
            Expression scope = RewriteExpressionFreeTemps(node.Scope);
            Statement body = RewriteStatement(node.Body);

            if (((object)scope != (object)node.Scope) ||
                ((object)body != (object)node.Body)) {
                return Ast.Scope(node.Span, scope, body);
            } else {
                return node;
            }
        }

        // SwitchStatement
        private Statement Rewrite(SwitchStatement node) {
            Expression test = RewriteExpressionFreeTemps(node.TestValue);
            ReadOnlyCollection<SwitchCase> cases = node.Cases;
            SwitchCase[] clone = null;

            for (int i = 0; i < cases.Count; i++) {
                SwitchCase @case = cases[i];
                SwitchCase rcase = Rewrite(@case);

                if (((object)rcase != (object)@case) && (clone == null)) {
                    clone = Clone(cases, i);
                }

                if (clone != null) {
                    clone[i] = rcase;
                }
            }

            if (clone != null) {
                return Ast.Switch(node.Span, node.Header, test, clone);
            } else if ((object)test != (object)node.TestValue) {
                return Ast.Switch(node.Span, node.Header, test, ArrayUtils.ToArray(node.Cases));
            } else {
                return node;
            }
        }

        // ThrowStatement
        private Statement Rewrite(ThrowStatement node) {
            Expression value = RewriteExpressionFreeTemps(node.Value);
            if ((object)value != (object)node.Value) {
                return Ast.Throw(node.Span, value);
            } else {
                return node;
            }
        }

        // TryStatement
        private Statement Rewrite(TryStatement node) {
            Statement body = RewriteStatement(node.Body);
            ReadOnlyCollection<CatchBlock> handlers = node.Handlers;
            CatchBlock[] clone = null;

            if (handlers != null) {
                for (int i = 0; i < handlers.Count; i++) {
                    CatchBlock handler = handlers[i];
                    CatchBlock rhandler = Rewrite(handler);

                    if (((object)rhandler != (object)handler) && (clone == null)) {
                        clone = Clone(handlers, i);
                    }

                    if (clone != null) {
                        clone[i] = rhandler;
                    }
                }
            }

            Statement @finally = RewriteStatement(node.FinallyStatement);

            if ((clone != null) ||
                ((object)body != (object)node.Body) ||
                ((object)@finally != (object)node.FinallyStatement)) {

                if (clone != null) {
                    handlers = CollectionUtils.ToReadOnlyCollection(clone);
                }

                return new TryStatement(node.Span, node.Header, body, handlers, @finally);
            } else {
                return node;
            }
        }

        // YieldStatement
        private Statement Rewrite(YieldStatement node) {
            Expression expression = RewriteExpressionFreeTemps(node.Expression);

            // Yield needs empty stack too, so rewrite it always
            return Ast.Yield(node.Span, expression);
        }

        #endregion

        #region Nodes

        // CatchBlock
        private CatchBlock Rewrite(CatchBlock node) {
            Statement body = RewriteStatement(node.Body);

            if ((object)body != (object)node.Body) {
                return Ast.Catch(node.Span, node.Header, node.Test, node.Variable, body);
            } else {
                return node;
            }
        }

        // IfStatementTest
        private IfStatementTest Rewrite(IfStatementTest node) {
            Expression test = RewriteExpressionFreeTemps(node.Test);
            Statement body = RewriteStatement(node.Body);

            if (((object)test != (object)node.Test) ||
                ((object)body != (object)node.Body)) {
                return new IfStatementTest(node.Span, node.Header, test, body);
            } else {
                return node;
            }
        }

        // SwitchCase
        private SwitchCase Rewrite(SwitchCase node) {
            Statement body = RewriteStatement(node.Body);

            if ((object)body != (object)node.Body) {
                return new SwitchCase(node.Header, node.IsDefault, node.Value, body);
            } else {
                return node;
            }
        }

        #endregion

        #region Cloning

        /// <summary>
        /// Will clone an IList into an array of the same size, and copy
        /// all vaues up to (and NOT including) the max index
        /// </summary>
        /// <returns>The cloned array.</returns>
        private static T[] Clone<T>(ReadOnlyCollection<T>/*!*/ roc, int max) {
            Debug.Assert(roc != null);
            Debug.Assert(max < roc.Count);

            T[] clone = new T[roc.Count];
            for (int j = 0; j < max; j++) {
                clone[j] = roc[j];
            }
            return clone;
        }

        /// <summary>
        /// Rewrites all rexpressions in the collecation. If any of them changes,
        /// will allocate the cloned array and an array of initialization expressions
        /// for the resulting comma.
        /// </summary>
        /// <returns>Cloned array or null, if no change encountered.</returns>
        private bool RewriteExpressions(ReadOnlyCollection<Expression>/*!*/ expressions, out Expression[] clone, out Expression[] comma) {
            Debug.Assert(expressions != null);

            clone = comma = null;

            for (int i = 0, count = expressions.Count; i < count; i++) {
                Expression arg = expressions[i];
                Expression exp = RewriteExpression(arg);

                // The expression has been rewritten, rewrite this too.
                if (((object)arg != (object)exp) && (clone == null)) {
                    clone = new Expression[count];
                    comma = new Expression[count + 1];
                    for (int j = 0; j < i; j++) {
                        clone[j] = ToTemp(expressions[j], out comma[j]);
                    }
                }

                if (clone != null) {
                    clone[i] = ToTemp(exp, out comma[i]);
                }
            }

            return clone != null;
        }

        #endregion
    }
}
