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
using System.IO;
using System.Text;

using Microsoft.Scripting;
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Generation;

#if DEBUG
namespace Microsoft.Scripting.Ast {
    class AstWriter {
        [Flags]
        private enum Flow {
            None,
            Space,
            NewLine,

            Break = 0x8000      // newline if column > MaxColumn
        };

        private struct Block {
            CodeBlock _block;
            int _id;

            public Block(CodeBlock block, int id) {
                _block = block;
                _id = id;
            }

            public CodeBlock CodeBlock {
                get { return _block; }
            }
            public int Id {
                get { return _id; }
            }
        }

        private struct Alignment {
            private readonly Statement _statement;
            private readonly int _depth;

            public Alignment(Statement statement, int depth) {
                _statement = statement;
                _depth = depth;
            }

            public Statement Statement {
                get { return _statement; }
            }
            public int Depth {
                get { return _depth; }
            }
        }

        private const int Tab = 4;
        private const int MaxColumn = 80;

        private TextWriter _out;
        private int _column;

        private Queue<Block> _blocks;
        private int _blockid;
        private Stack<Alignment> _stack = new Stack<Alignment>();
        private int _delta;
        private Flow _flow;

        private AstWriter() {
        }

        private int Base {
            get {
                return _stack.Count > 0 ? _stack.Peek().Depth : 0;
            }
        }

        private int Delta {
            get { return _delta; }
        }

        private int Depth {
            get { return Base + Delta; }
        }

        private void Indent() {
            _delta += Tab;
        }
        private void Dedent() {
            _delta -= Tab;
        }

        private void NewLine() {
            _flow = Flow.NewLine;
        }

#if !SILVERLIGHT
        private static ConsoleColor GetAstColor() {
            if (Console.BackgroundColor == ConsoleColor.White) {
                return ConsoleColor.DarkCyan;
            } else {
                return ConsoleColor.Cyan;
            }
        }
#endif

        /// <summary>
        /// Write out the given AST (only if ShowASTs or DumpASTs is enabled)
        /// </summary>
        public static void Dump(CodeBlock/*!*/ block, string descr) {
            Debug.Assert(block != null);

            if (descr == null) {
                descr = "unknown_ast";
            }

            if (ScriptDomainManager.Options.ShowASTs) {
#if !SILVERLIGHT
                ConsoleColor color = Console.ForegroundColor;
                try {
                    Console.ForegroundColor = GetAstColor();
#endif
                    Dump(block, descr, System.Console.Out);
#if !SILVERLIGHT
                } finally {
                    Console.ForegroundColor = color;
                }
#endif
            } else if (ScriptDomainManager.Options.DumpASTs) {
              StreamWriter sw = new StreamWriter(GetFilePath(descr), descr == "unknown_ast");
                using (sw) {
                    Dump(block, descr, sw);
                }
            }
        }


#if FULL
        /// <summary>
        /// Write out the given rule's AST (only if ShowRules is enabled)
        /// </summary>
        public static void Dump<T>(StandardRule<T> rule) {
            if (ScriptDomainManager.Options.ShowRules) {
#if !SILVERLIGHT
                ConsoleColor color = Console.ForegroundColor;
                try {
                    Console.ForegroundColor = GetAstColor();
#endif
                    AstWriter.Dump(rule.Test, "Rule.Test", System.Console.Out);
                    AstWriter.Dump(rule.Target, "Rule.Target", System.Console.Out);
#if !SILVERLIGHT
                } finally {
                    Console.ForegroundColor = color;
                }
#endif
            }
        } 
#endif


        /// <summary>
        /// Write out the given AST
        /// </summary>
        internal static void Dump(Node/*!*/ node, string/*!*/ descr, TextWriter/*!*/ writer) {
            Debug.Assert(node != null);
            Debug.Assert(descr != null);
            Debug.Assert(writer != null);

            AstWriter dv = new AstWriter();
            dv.DoDump(node, descr, writer);
        }

        private static string GetFilePath(string/*!*/ path) {
            Debug.Assert(path != null);

#if !SILVERLIGHT // GetInvalidFileNameChars does not exist in CoreCLR
            char[] invalid = System.IO.Path.GetInvalidFileNameChars();
            foreach (char ch in invalid) {
                path = path.Replace(ch, '_');
            }
#endif
            return path + ".ast";
        }

        private void DoDump(Node node, string name, TextWriter outFile) {
            _out = outFile;

            WriteLine("//");
            WriteLine("// AST {0}", name);
            WriteLine("//");
            WriteLine();

            WalkNode(node);
            Debug.Assert(_stack.Count == 0);

            //while (_blocks != null && _blocks.Count > 0) {
            //    Block b = _blocks.Dequeue();
            //    WriteLine();
            //    WriteLine("//");
            //    WriteLine("// CODE BLOCK: {0} ({1})", b.CodeBlock.Name, b.Id);
            //    WriteLine("//");
            //    WriteLine();

            //    WalkNode(b.CodeBlock);

            //    Debug.Assert(_stack.Count == 0);
            //}

            WriteLine();
        }

        private int Enqueue(CodeBlock block) {
            if (_blocks == null) {
                _blocks = new Queue<Block>();
            }
            _blocks.Enqueue(new Block(block, ++_blockid));
            return _blockid;
        }

        #region The printing code

        private void Out(string s) {
            Out(Flow.None, s, Flow.None);
        }

        private void Out(Flow before, string s) {
            Out(before, s, Flow.None);
        }

        private void Out(string s, Flow after) {
            Out(Flow.None, s, after);
        }

        private void Out(Flow before, string s, Flow after) {
            switch (GetFlow(before)) {
                case Flow.None:
                    break;
                case Flow.Space:
                    Write(" ");
                    break;
                case Flow.NewLine:
                    WriteLine();
                    Write(new String(' ', Depth));
                    break;
            }
            Write(s);
            _flow = after;
        }

        private void WriteLine() {
            _out.WriteLine();
            _column = 0;
        }
        private void WriteLine(string s) {
            _out.WriteLine(s);
            _column = 0;
        }
        private void WriteLine(string format, object arg0) {
            string s = String.Format(format, arg0);
            WriteLine(s);
        }
        private void WriteLine(string format, object arg0, object arg1) {
            string s = String.Format(format, arg0, arg1);
            WriteLine(s);
        }
        private void Write(string s) {
            _out.Write(s);
            _column += s.Length;
        }

        private Flow GetFlow(Flow flow) {
            Flow last;

            last = CheckBreak(_flow);
            flow = CheckBreak(flow);

            // Get the biggest flow that is requested None < Space < NewLine
            return (Flow)System.Math.Max((int)last, (int)flow);
        }

        private Flow CheckBreak(Flow flow) {
            if ((flow & Flow.Break) != 0) {
                if (_column > (MaxColumn + Depth)) {
                    flow = Flow.NewLine;
                } else {
                    flow &= ~Flow.Break;
                }
            }
            return flow;
        }

        #endregion

        #region The AST Output

        private void WalkNode(Node node) {
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
                    Dump((BinaryExpression)node);
                    break;
                case AstNodeType.Call:
                    Dump((MethodCallExpression)node);
                    break;
                case AstNodeType.Conditional:
                    Dump((ConditionalExpression)node);
                    break;
                case AstNodeType.Constant:
                    Dump((ConstantExpression)node);
                    break;
                case AstNodeType.Convert:
                case AstNodeType.Negate:
                case AstNodeType.Not:
                case AstNodeType.OnesComplement:
                    Dump((UnaryExpression)node);
                    break;
                case AstNodeType.New:
                    Dump((NewExpression)node);
                    break;
                case AstNodeType.TypeIs:
                    Dump((TypeBinaryExpression)node);
                    break;

#if FULL
                case AstNodeType.ActionExpression:
                    Dump((ActionExpression)node);
                    break; 
#endif

                case AstNodeType.ArrayIndexAssignment:
                    Dump((ArrayIndexAssignment)node);
                    break;
                case AstNodeType.ArrayIndexExpression:
                    Dump((ArrayIndexExpression)node);
                    break;
                case AstNodeType.BlockStatement:
                    Dump((BlockStatement)node);
                    break;
                case AstNodeType.BoundAssignment:
                    Dump((BoundAssignment)node);
                    break;
                case AstNodeType.WriteStatement:
                    Dump((WriteStatement)node);
                    break;
                case AstNodeType.BoundExpression:
                    Dump((BoundExpression)node);
                    break;
                case AstNodeType.BreakStatement:
                    Dump((BreakStatement)node);
                    break;
                case AstNodeType.CodeBlock:
                    Dump((CodeBlock)node);
                    break;
                case AstNodeType.CodeBlockExpression:
                    Dump((CodeBlockExpression)node);
                    break;
                case AstNodeType.CodeContextExpression:
                    Dump((CodeContextExpression)node);
                    break;
                case AstNodeType.CommaExpression:
                    Dump((CommaExpression)node);
                    break;
                case AstNodeType.ContinueStatement:
                    Dump((ContinueStatement)node);
                    break;
                case AstNodeType.DebugStatement:
                    Dump((DebugStatement)node);
                    break;
                case AstNodeType.DeleteStatement:
                    Dump((DeleteStatement)node);
                    break;
                case AstNodeType.DeleteUnboundExpression:
                    Dump((DeleteUnboundExpression)node);
                    break;
                case AstNodeType.DoStatement:
                    Dump((DoStatement)node);
                    break;

#if FULL
                case AstNodeType.DynamicConversionExpression:
                    Dump((DynamicConversionExpression)node);
                    break; 
#endif

                case AstNodeType.EmptyStatement:
                    Dump((EmptyStatement)node);
                    break;
                case AstNodeType.EnvironmentExpression:
                    Dump((EnvironmentExpression)node);
                    break;
                case AstNodeType.ExpressionStatement:
                    Dump((ExpressionStatement)node);
                    break;
                case AstNodeType.GeneratorCodeBlock:
                    Dump((GeneratorCodeBlock)node);
                    break;
                case AstNodeType.IfStatement:
                    Dump((IfStatement)node);
                    break;
                case AstNodeType.LabeledStatement:
                    Dump((LabeledStatement)node);
                    break;
                case AstNodeType.LoopStatement:
                    Dump((LoopStatement)node);
                    break;
                case AstNodeType.MemberAssignment:
                    Dump((MemberAssignment)node);
                    break;
                case AstNodeType.MemberExpression:
                    Dump((MemberExpression)node);
                    break;
                case AstNodeType.NewArrayExpression:
                    Dump((NewArrayExpression)node);
                    break;
                case AstNodeType.ParamsExpression:
                    Dump((ParamsExpression)node);
                    break;
                case AstNodeType.ParenthesizedExpression:
                    Dump((ParenthesizedExpression)node);
                    break;
                case AstNodeType.ReturnStatement:
                    Dump((ReturnStatement)node);
                    break;
                case AstNodeType.ScopeStatement:
                    Dump((ScopeStatement)node);
                    break;
                case AstNodeType.SwitchStatement:
                    Dump((SwitchStatement)node);
                    break;
                case AstNodeType.ThrowStatement:
                    Dump((ThrowStatement)node);
                    break;
                case AstNodeType.TryStatement:
                    Dump((TryStatement)node);
                    break;
                case AstNodeType.UnboundAssignment:
                    Dump((UnboundAssignment)node);
                    break;
                case AstNodeType.UnboundExpression:
                    Dump((UnboundExpression)node);
                    break;
                case AstNodeType.VoidExpression:
                    Dump((VoidExpression)node);
                    break;
                case AstNodeType.YieldStatement:
                    Dump((YieldStatement)node);
                    break;

                // These are handled within their respective statements
                case AstNodeType.CatchBlock:
                case AstNodeType.IfStatementTest:
                case AstNodeType.SwitchCase:
                default:
                    throw new InvalidOperationException("Unexpected node type: " + node.NodeType.ToString());
            }
        }


#if FULL

        // More proper would be to make this a virtual method on Action
        private static string FormatAction(DynamicAction action) {
            DoOperationAction doa;
            GetMemberAction gma;
            SetMemberAction sma;
            InvokeMemberAction ima;
            ConvertToAction cta;
            CallAction cla;

            if ((doa = action as DoOperationAction) != null) {
                return "Do " + doa.Operation.ToString();
            } else if ((gma = action as GetMemberAction) != null) {
                return "GetMember " + SymbolTable.IdToString(gma.Name);
            } else if ((sma = action as SetMemberAction) != null) {
                return "SetMember " + SymbolTable.IdToString(sma.Name);
            } else if ((ima = action as InvokeMemberAction) != null) {
                return "InvokeMember " + ima.Name;
            } else if ((cta = action as ConvertToAction) != null) {
                return "ConvertTo " + cta.ToType.ToString();
            } else if ((cla = action as CallAction) != null) {
                return "Call";
            } else {
                return "UnknownAction (" + action.Kind.ToString() + ")";
            }
        }

        // ActionExpression
        private void Dump(ActionExpression node) {
            Out(".action", Flow.Space);
            Out(FormatAction(node.Action));
            Out("(");
            Indent();
            NewLine();
            foreach (Expression arg in node.Arguments) {
                WalkNode(arg);
                NewLine();
            }
            Dedent();
            Out(")");
        } 
#endif


        // ArrayIndexAssignment
        private void Dump(ArrayIndexAssignment node) {
            WalkNode(node.Array);
            Out("[");
            WalkNode(node.Index);
            Out("] = ");
            WalkNode(node.Value);
        }

        // ArrayIndexExpression
        private void Dump(ArrayIndexExpression node) {
            WalkNode(node.Array);
            Out("[");
            WalkNode(node.Index);
            Out("]");
        }

        // BinaryExpression
        private void Dump(BinaryExpression node) {
            WalkNode(node.Left);
            string op;
            switch (node.NodeType) {
                case AstNodeType.Equal: op = "=="; break;
                case AstNodeType.NotEqual: op = "!="; break;
                case AstNodeType.AndAlso: op = "&&"; break;
                case AstNodeType.OrElse: op = "||"; break;
                case AstNodeType.GreaterThan: op = ">"; break;
                case AstNodeType.LessThan: op = "<"; break;
                case AstNodeType.GreaterThanOrEqual: op = ">="; break;
                case AstNodeType.LessThanOrEqual: op = "<="; break;
                case AstNodeType.AddChecked:
                case AstNodeType.Add: op = "+"; break;
                case AstNodeType.SubtractChecked:
                case AstNodeType.Subtract: op = "-"; break;
                case AstNodeType.Divide: op = "/"; break;
                case AstNodeType.Modulo: op = "%"; break;
                case AstNodeType.MultiplyChecked:
                case AstNodeType.Multiply: op = "*"; break;
                case AstNodeType.LeftShift: op = "<<"; break;
                case AstNodeType.RightShift: op = ">>"; break;
                case AstNodeType.And: op = "&"; break;
                case AstNodeType.Or: op = "|"; break;
                case AstNodeType.ExclusiveOr: op = "^"; break;
                default:
                    throw new InvalidOperationException();
            }
            Out(Flow.Space, op, Flow.Space | Flow.Break);
            WalkNode(node.Right);
        }

        // BoundAssignment
        private void Dump(BoundAssignment node) {
            Out("(.bound " + SymbolTable.IdToString(node.Variable.Name) + ") = ");
            WalkNode(node.Value);
        }

        // WriteStatement
        private void Dump(WriteStatement node)
        {
          Out("(.bound " + SymbolTable.IdToString(node.Variable.Name) + ") = ");
          WalkNode(node.Value);
          Out(";");
        }

        // BoundExpression
        private void Dump(BoundExpression node) {
            Out("(.bound ");
            Out(SymbolTable.IdToString(node.Name));
            Out(")");
        }

        // CodeBlockExpression
        private void Dump(CodeBlockExpression node) {
            int id = Enqueue(node.Block);
            Out(String.Format(".block ({0} #{1}", node.Block.Name, id));
            Indent();
            bool nl = false;
            if (node.ForceWrapperMethod) { nl = true; Out(Flow.NewLine, "ForceWrapper"); }
            if (node.IsStronglyTyped) { nl = true; Out(Flow.NewLine, "StronglyTyped"); }
            if (node.IsDeclarative) { nl = true; Out(Flow.NewLine, "Declarative"); }
            Dedent();
            Out(nl ? Flow.NewLine : Flow.None, ")");
        }

        // CodeContextExpression
        private void Dump(CodeContextExpression node) {
            Out(".context");
        }

        // CommaExpression
        private void Dump(CommaExpression node) {
            Out(String.Format(".comma ({0}) {{", node.ValueIndex), Flow.NewLine);
            Indent();
            for (int i = 0; i < node.Expressions.Count; i++) {
                WalkNode(node.Expressions[i]);
                Out(",", Flow.NewLine);
            }
            Dedent();
            Out("}", Flow.NewLine);
        }

        // ConditionalExpression
        private void Dump(ConditionalExpression node) {
            Out("(");
            WalkNode(node.Test);
            Out(" ? ");
            WalkNode(node.IfTrue);
            Out(" : ");
            WalkNode(node.IfFalse);
            Out(")");
        }

        private static string Constant(object value) {
            if (value == null) {
                return ".null";
            }

            CompilerConstant cc;
            if ((cc = value as CompilerConstant) != null) {
                value = cc.Create();

#if FULL
                if (value is ITemplatedValue) {
                    return ".template (" + ((ITemplatedValue)value).ObjectValue.ToString() + ")";
                } 
#endif

            }

            Type t;
            if ((t = value as Type) != null) {
                return "((Type)" + t.Name + ")";
            }
            string s;
            if ((s = value as string) != null) {
                return "\"" + s + "\"";
            }
            if (value is int || value is double) {
                return String.Format("{0:G}", value);
            }
            return "(" + value.GetType().Name + ")" + value.ToString();
        }

        // ConstantExpression
        private void Dump(ConstantExpression node) {
            Out(Constant(node.Value));
        }

        // DeleteUnboundExpression
        private void Dump(DeleteUnboundExpression node) {
            Out(String.Format(".delname({0})", SymbolTable.IdToString(node.Name)));
        }


#if FULL
        // DynamicConversionExpression
        private void Dump(DynamicConversionExpression node) {
            Out(String.Format("(.convert.d {0})(", node.Type));
            WalkNode(node.Expression);
            Out(")");
        } 
#endif


        // EnvironmentExpression
        private void Dump(EnvironmentExpression node) {
            Out(".env");
        }

        // MemberAssignment
        private void Dump(MemberAssignment node) {
            WalkNode(node.Expression);
            Out(".");
            Out(node.Member.Name);
            Out(" = ");
            WalkNode(node.Value);
        }

        // MemberExpression
        private void Dump(MemberExpression node) {
            WalkNode(node.Expression);
            Out(".");
            Out(node.Member.Name);
        }

        // MethodCallExpression
        private void Dump(MethodCallExpression node) {
            if (node.Instance != null) {
                Out("(");
                WalkNode(node.Instance);
                Out(").");
            }
            Out("(" + (node.TailCall ? "*" : "")  + node.Method.ReflectedType.Name + "." + node.Method.Name + ")(");
            if (node.Arguments != null && node.Arguments.Count > 0) {
                NewLine(); Indent();
                foreach (Expression e in node.Arguments) {
                    WalkNode(e);
                    Out(",", Flow.NewLine);
                }
                Dedent();
            }
            Out(")");
        }

        // NewArrayExpression
        private void Dump(NewArrayExpression node) {
            Out(".new " + node.Type.Name + " = {");
            if (node.Expressions != null && node.Expressions.Count > 0) {
                NewLine(); Indent();
                foreach (Expression e in node.Expressions) {
                    WalkNode(e);
                    Out(",", Flow.NewLine);
                }
                Dedent();
            }
            Out("}");
        }

        // NewExpression
        private void Dump(NewExpression node) {
            Out(".new " + node.Type.Name + "(");
            if (node.Arguments != null && node.Arguments.Count > 0) {
                NewLine(); Indent();
                foreach (Expression e in node.Arguments) {
                    WalkNode(e);
                    Out(",", Flow.NewLine);
                }
                Dedent();
            }
            Out(")");
        }

        // ParamsExpression
        private void Dump(ParamsExpression node) {
            Out(".params");
        }

        // ParenthesizedExpression
        private void Dump(ParenthesizedExpression node) {
            Out("(");
            WalkNode(node.Expression);
            Out(")");
        }

        // TypeBinaryExpression
        private void Dump(TypeBinaryExpression node) {
            WalkNode(node.Expression);
            Out(Flow.Space, ".is", Flow.Space);
            Out(node.TypeOperand.Name);
        }

        // UnaryExpression
        private void Dump(UnaryExpression node) {
            switch (node.NodeType) {
                case AstNodeType.Convert:
                    Out("(" + node.Type.Name + ")");
                    break;
                case AstNodeType.Not:
                    Out(node.Type == typeof(bool) ? "!" : "~");
                    break;
                case AstNodeType.Negate:
                    Out("-");
                    break;
                case AstNodeType.OnesComplement:
                    Out("~");
                    break;
            }

            WalkNode(node.Operand);
        }

        // UnboundAssignment
        private void Dump(UnboundAssignment node) {
            Out(SymbolTable.IdToString(node.Name));
            Out(" := ");
            WalkNode(node.Value);
        }

        // UnboundExpression
        private void Dump(UnboundExpression node) {
            Out(".unbound " + SymbolTable.IdToString(node.Name));
        }

        // VoidExpression
        private void Dump(VoidExpression node) {
            Out(".void {");
            Indent();
            WalkNode(node.Statement);
            Dedent();
            Out("}");
        }

        // BlockStatement
        private void Dump(BlockStatement node) {
            Out("{");
            NewLine(); Indent();
            foreach (Statement s in node.Statements) {
                WalkNode(s);
                NewLine();
            }
            Dedent();
            Out("}", Flow.NewLine);
        }

        // BreakStatement
        private void Dump(BreakStatement node) {
            Out(".break;", Flow.NewLine);
        }

        // ContinueStatement
        private void Dump(ContinueStatement node) {
            Out(".continue;", Flow.NewLine);
        }

        // DebugStatement
        private void Dump(DebugStatement node) {
            Out(".debug(" + node.Marker + ");", Flow.NewLine);
        }

        // DeleteStatement
        private void Dump(DeleteStatement node) {
            Out(".del");
            if (node.Variable != null) {
                Out(Flow.Space, SymbolTable.IdToString(node.Variable.Name));
            }
            NewLine();
        }

        // DoStatement
        private void Dump(DoStatement node) {
            Out(".do {", Flow.NewLine);
            Indent();
            WalkNode(node.Body);
            Dedent();
            Out(Flow.NewLine, "} .while (");
            WalkNode(node.Test);
            Out(");");
        }

        // EmptyStatement
        private void Dump(EmptyStatement node) {
            Out(";", Flow.NewLine);
        }

        // ExpressionStatement
        private void Dump(ExpressionStatement node) {
            WalkNode(node.Expression);
            Out(";", Flow.NewLine);
        }

        // IfStatement
        private void Dump(IfStatement node) {
            for (int i = 0; i < node.Tests.Count; i++) {
                IfStatementTest test = node.Tests[i];
                Out(i == 0 ? ".if (" : "} .elif (");
                WalkNode(test.Test);
                Out(") {", Flow.NewLine);
                Indent();
                WalkNode(test.Body);
                Dedent();
            }

            if (node.ElseStatement != null) {
                Out("} .else {", Flow.NewLine);
                Indent();
                WalkNode(node.ElseStatement);
                Dedent();
            }
            Out("}", Flow.NewLine);
        }

        // LabeledStatement
        private void Dump(LabeledStatement node) {
            Out(".labeled {", Flow.NewLine);
            Indent();
            WalkNode(node.Statement);
            Dedent();
            Out(Flow.NewLine, "}");
        }

        // LoopStatement
        private void Dump(LoopStatement node) {
            Out(".for (; ");
            WalkNode(node.Test);
            Out("; ");
            WalkNode(node.Increment);
            Out(") {", Flow.NewLine);
            Indent();
            WalkNode(node.Body);
            Dedent();
            Out(Flow.NewLine, "}");
        }

        // ReturnStatement
        private void Dump(ReturnStatement node) {
            Out(".return", Flow.Space);
            WalkNode(node.Expression);
            Out(";", Flow.NewLine);
        }

        // ScopeStatement
        private void Dump(ScopeStatement node) {
            Out(".scope (");
            WalkNode(node.Scope);
            Out(") {", Flow.NewLine);
            Indent();
            WalkNode(node.Body);
            Dedent();
            Out("}", Flow.NewLine);
        }

        // SwitchStatement
        private void Dump(SwitchStatement node) {
            Out(".switch (");
            WalkNode(node.TestValue);
            Out(") {", Flow.NewLine);
            foreach (SwitchCase sc in node.Cases) {
                if (sc.IsDefault) {
                    Out(".default");
                } else {
                    Out(".case " + sc.Value);
                }
                Out(":", Flow.NewLine);
                Indent(); Indent();
                WalkNode(sc.Body);
                Dedent(); Dedent();
                NewLine();
            }
            Out("}", Flow.NewLine);
        }

        // ThrowStatement
        private void Dump(ThrowStatement node) {
            Out(Flow.NewLine, ".throw (");
            WalkNode(node.Exception);
            Out(")", Flow.NewLine);
        }

        // TryStatement
        private void Dump(TryStatement node) {
            Out(".try {", Flow.NewLine);
            Indent();
            WalkNode(node.Body);
            Dedent();
            if (node.Handlers != null && node.Handlers.Count > 0) {
                foreach (CatchBlock cb in node.Handlers) {
                    Out("} .catch ( " + cb.Test.Name);
                    if (cb.Variable != null) {
                        Out(Flow.Space, SymbolTable.IdToString(cb.Variable.Name));
                    }
                    Out(") {", Flow.NewLine);
                    Indent();
                    WalkNode(cb.Body);
                    Dedent();
                }
            }
            if (node.FinallyStatement != null) {
                Out("} .finally {", Flow.NewLine);
                Indent();
                WalkNode(node.FinallyStatement);
                Dedent();
            }
            Out("}", Flow.NewLine);
        }

        // YieldStatement
        private void Dump(YieldStatement node) {
            Out(".yield ");
            WalkNode(node.Expression);
            Out(";", Flow.NewLine);
        }

        private static string GetCodeBlockInfo(CodeBlock block) {
            string info = String.Format("{0} {1} (", block.ReturnType.Name, block.Name);
            if (block.IsGlobal) {
                info += " global,";
            }
            if (!block.IsVisible) {
                info += " hidden,";
            }
            if (block.IsClosure) {
                info += " closure,";
            }
            if (block.ParameterArray) {
                info += " param array,";
            }
            if (block.HasEnvironment) {
                info += " environment,";
            }
            if (block.EmitLocalDictionary) {
                info += " local dict";
            }
            info += ")";
            return info;
        }

        private void DumpVariable(Variable v) {
            string descr = String.Format("{2} {0} ({1}", SymbolTable.IdToString(v.Name), v.Kind.ToString(), v.Type.Name);
            if (v.Lift) {
                descr += ",Lift";
            }
            if (v.InParameterArray) {
                descr += ",InParameterArray";
            }
            descr += ")";
            Out(descr);
            if (v.DefaultValue != null) {
                Out(" = ");
                WalkNode(v.DefaultValue);
            }
            NewLine();
        }

        private void DumpBlock(CodeBlock node) {
            Out(GetCodeBlockInfo(node));
            Out("(");
            Indent();
            foreach (Variable v in node.Parameters) {
                Out(Flow.NewLine, ".arg", Flow.Space);
                DumpVariable(v);
            }
            Dedent();
            Out(") {");
            Indent();
            foreach (Variable v in node.Variables) {
                Out(Flow.NewLine, ".var", Flow.Space);
                DumpVariable(v);
            }
            Out(Flow.NewLine, "", Flow.NewLine);
            WalkNode(node.Body);
            Dedent();
            Out("}");
        }

        // CodeBlock
        private void Dump(CodeBlock node) {
            Out(".codeblock", Flow.Space);
            DumpBlock(node);
        }

        // GeneratorCodeBlock
        private void Dump(GeneratorCodeBlock node) {
            Out(".generator", Flow.Space);
            DumpBlock(node);
        }

        #endregion
    }
}
#endif
