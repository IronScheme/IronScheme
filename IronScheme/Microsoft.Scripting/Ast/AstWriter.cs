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
    class AstWriter : Walker {
        private enum Flow {
            None,
            Space,
            NewLine
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

        private TextWriter _out;
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

        /// <summary>
        /// Write out the given AST (only if ShowASTs or DumpASTs is enabled)
        /// </summary>
        public static void Dump(Node node, CompilerContext context) {
            string descr = context.SourceUnit.Id;
            if (ScriptDomainManager.Options.ShowASTs) {
                AstWriter.ForceDump(node, descr, System.Console.Out);
            } else if (ScriptDomainManager.Options.DumpASTs) {
                AstWriter.ForceDump(node, descr, new StreamWriter(FixPath(descr) + ".ast", true));
            }
        }

        /// <summary>
        /// Write out the given rule's AST (only if ShowRules is enabled)
        /// </summary>
        public static void DumpRule<T>(StandardRule<T> rule) {
            if (ScriptDomainManager.Options.ShowRules) {
                AstWriter.ForceDump(rule.Test, rule.ToString() + ".Test", System.Console.Out);
                AstWriter.ForceDump(rule.Target, rule.ToString() + ".Target", System.Console.Out);
            }
        }

        /// <summary>
        /// Write out the given AST
        /// </summary>
        public static void ForceDump(Node node, string descr, TextWriter outFile) {
            if (node != null) {
                if (String.IsNullOrEmpty(descr)) descr = "<unknown>";
                AstWriter dv = new AstWriter();
                dv.DoDump(node, descr, outFile);
            }
        }

        private static string FixPath(string path) {
#if !SILVERLIGHT // GetInvalidFileNameChars does not exist in CoreCLR
            char[] invalid = System.IO.Path.GetInvalidFileNameChars();

            foreach (char ch in invalid) {
                path = path.Replace(ch, '_');
            }
#endif
            return path;
        }

        private void DoDump(Node node, string name, TextWriter outFile) {
            _out = outFile;
            _out.WriteLine("\n\n//\n// AST {0}\n//", name);

            node.Walk(this);
            Debug.Assert(_stack.Count == 0);

            while (_blocks != null && _blocks.Count > 0) {
                Block b = _blocks.Dequeue();
                Write(String.Format("\n\n//\n// CODE BLOCK: {0} ({1})\n//\n", b.CodeBlock.Name, b.Id));
                b.CodeBlock.Walk(this);
                Write("");

                Debug.Assert(_stack.Count == 0);
            }
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
        }
        private void Write(string s) {
            _out.Write(s);
        }

        private Flow GetFlow(Flow flow) {
            // Get the biggest flow that is requested None < Space < NewLine
            return (Flow)System.Math.Max((int)_flow, (int)flow);
        }

        private void WalkChild(Node n) {
            if (n != null) n.Walk(this);
        }

        #endregion

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
        public override bool Walk(ActionExpression node) {
            Out(".action", Flow.Space);
            Out(FormatAction(node.Action));
            Out("(");
            Indent();
            NewLine();
            foreach (Expression arg in node.Arguments) {
                WalkChild(arg);
                NewLine();
            }
            Dedent();
            Out(")");
            return false;
        }

        // ArrayIndexAssignment
        public override bool Walk(ArrayIndexAssignment node) {
            WalkChild(node.Array);
            Out("[");
            WalkChild(node.Index);
            Out("] = ");
            WalkChild(node.Value);
            return false;
        }

        // ArrayIndexExpression
        public override bool Walk(ArrayIndexExpression node) {
            WalkChild(node.Array);
            Out("[");
            WalkChild(node.Index);
            Out("]");
            return false;
        }

        // BinaryExpression
        public override bool Walk(BinaryExpression node) {
            WalkChild(node.Left);
            string op;
            switch (node.Operator) {
                case BinaryOperators.Equal: op = "="; break;
                case BinaryOperators.NotEqual: op = "!="; break;
                case BinaryOperators.AndAlso: op = "&&"; break;
                case BinaryOperators.OrElse: op = "||"; break;
                case BinaryOperators.GreaterThan: op = ">"; break;
                case BinaryOperators.LessThan: op = "<"; break;
                case BinaryOperators.GreaterThanEquals: op = ">="; break;
                case BinaryOperators.LessThanEquals: op = "<="; break;
                case BinaryOperators.Add: op = "+"; break;
                case BinaryOperators.Subtract: op = "-"; break;
                case BinaryOperators.Divide: op = "/"; break;
                case BinaryOperators.Modulo: op = "%"; break;
                case BinaryOperators.Multiply: op = "*"; break;
                case BinaryOperators.LeftShift: op = "<<"; break;
                case BinaryOperators.RightShift: op = ">>"; break;
                case BinaryOperators.BitwiseAnd: op = "&"; break;
                case BinaryOperators.BitwiseOr: op = "|"; break;
                case BinaryOperators.ExclusiveOr: op = "^"; break;
                default:
                    throw new InvalidOperationException();
            }
            Out(Flow.Space, op, Flow.Space);
            WalkChild(node.Right);
            return false;
        }

        // BoundAssignment
        public override bool Walk(BoundAssignment node) {
            Out("(.bound " + SymbolTable.IdToString(node.Variable.Name) + ") = ");
            WalkChild(node.Value);
            return false;
        }

        // BoundExpression
        public override bool Walk(BoundExpression node) {
            Out("(.bound ");
            Out(SymbolTable.IdToString(node.Name));
            Out(")");
            return false;
        }

        // CallWithThisExpression
        public override bool Walk(CallWithThisExpression node) { throw NotImplemented(node); }
        public override void PostWalk(CallWithThisExpression node) { }

        // CodeBlockExpression
        public override bool Walk(CodeBlockExpression node) {
            int id = Enqueue(node.Block);
            Out(String.Format(".block ({0} #{1}", node.Block.Name, id));
            Indent();
            bool nl = false;
            if (node.ForceWrapperMethod) { nl = true; Out(Flow.NewLine, "ForceWrapper"); }
            if (node.IsStronglyTyped) { nl = true; Out(Flow.NewLine, "StronglyTyped"); }
            if (node.IsDeclarative) { nl = true; Out(Flow.NewLine, "Declarative"); }
            Dedent();
            Out(nl ? Flow.NewLine : Flow.None, ")");
            return false;
        }

        // CodeContextExpression
        public override bool Walk(CodeContextExpression node) {
            Out(".context");
            return false;
        }

        // CommaExpression
        public override bool Walk(CommaExpression node) {
            Out(String.Format(".comma ({0}) {{", node.ValueIndex), Flow.NewLine);
            Indent();
            for (int i = 0; i < node.Expressions.Count; i++) {
                WalkChild(node.Expressions[i]);
                Out(",", Flow.NewLine);
            }
            Dedent();
            Out("}", Flow.NewLine);
            return false;
        }

        // ConditionalExpression
        public override bool Walk(ConditionalExpression node) {
            Out("(");
            WalkChild(node.Test);
            Out(" ? ");
            WalkChild(node.TrueExpression);
            Out(" : ");
            WalkChild(node.FalseExpression);
            Out(")");
            return false;
        }

        private static string Constant(object value) {
            if (value == null) {
                return ".null";
            }

            CompilerConstant cc;
            if ((cc = value as CompilerConstant) != null) {
                value = cc.Create();
                if (value is ITemplatedValue) {
                    return ".template (" + ((ITemplatedValue)value).ObjectValue.ToString() + ")";
                }
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
        public override bool Walk(ConstantExpression node) {
            Out(Constant(node.Value));
            return false;
        }

        // DeleteUnboundExpression
        public override bool Walk(DeleteUnboundExpression node) {
            Out(String.Format(".delname({0})", SymbolTable.IdToString(node.Name)));
            return false;
        }

        // DynamicConversionExpression
        public override bool Walk(DynamicConversionExpression node) {
            Out(String.Format(".convert.d ({0})", node.Type));
            WalkChild(node.Expression);
            Out(")");
            return false;
        }

        // EnvironmentExpression
        public override bool Walk(EnvironmentExpression node) {
            Out(".env");
            return false;
        }

        // MemberAssignment
        public override bool Walk(MemberAssignment node) {
            WalkChild(node.Expression);
            Out(".");
            Out(node.Member.Name);
            Out(" = ");
            WalkChild(node.Value);
            return false;
        }

        // MemberExpression
        public override bool Walk(MemberExpression node) {
            WalkChild(node.Expression);
            Out(".");
            Out(node.Member.Name);
            return false;
        }

        // MethodCallExpression
        public override bool Walk(MethodCallExpression node) {
            if (node.Instance != null) {
                Out("(");
                WalkChild(node.Instance);
                Out(").");
            }
            Out("(" + node.Method.ReflectedType.Name + "." + node.Method.Name + ")(");
            if (node.Arguments != null && node.Arguments.Count > 0) {
                NewLine(); Indent();
                foreach (Expression e in node.Arguments) {
                    WalkChild(e);
                    Out(",", Flow.NewLine);
                }
                Dedent();
            }
            Out(")");
            return false;
        }

        // NewArrayExpression
        public override bool Walk(NewArrayExpression node) {
            Out(".new " + node.Type.Name + "[] = {");
            if (node.Expressions != null && node.Expressions.Count > 0) {
                NewLine(); Indent();
                foreach (Expression e in node.Expressions) {
                    WalkChild(e);
                    Out(",", Flow.NewLine);
                }
                Dedent();
            }
            Out("}");
            return false;
        }

        // NewExpression
        public override bool Walk(NewExpression node) {
            Out(".new " + node.Type.Name + "(");
            if (node.Arguments != null && node.Arguments.Count > 0) {
                NewLine(); Indent();
                foreach (Expression e in node.Arguments) {
                    WalkChild(e);
                    Out(",", Flow.NewLine);
                }
                Dedent();
            }
            Out(")");
            return false;
        }

        // ParamsExpression
        public override bool Walk(ParamsExpression node) {
            Out(".params");
            return false;
        }

        // ParenthesizedExpression
        public override bool Walk(ParenthesizedExpression node) {
            Out("(");
            WalkChild(node.Expression);
            Out(")");
            return false;
        }

        // ShortCircuitExpression
        public override bool Walk(ShortCircuitExpression node) {
            throw NotImplemented(node);
        }

        // TypeBinaryExpression
        public override bool Walk(TypeBinaryExpression node) {
            WalkChild(node.Expression);
            Out(Flow.Space, ".is", Flow.Space);
            Out(node.Type.Name);
            return false;
        }

        // UnaryExpression
        public override bool Walk(UnaryExpression node) {
            switch (node.Operator) {
                case UnaryOperators.Convert:
                    Out("(" + node.Type.Name + ")");
                    break;
                case UnaryOperators.Not:
                    Out(node.Type == typeof(bool) ? "!" : "~");
                    break;
                case UnaryOperators.Negate:
                    Out("-");
                    break;
                case UnaryOperators.OnesComplement:
                    Out("~");
                    break;
            }

            WalkChild(node.Operand);
            return false;
        }

        // UnboundAssignment
        public override bool Walk(UnboundAssignment node) {
            Out(SymbolTable.IdToString(node.Name));
            Out(" := ");
            WalkChild(node.Value);
            return false;
        }

        // UnboundExpression
        public override bool Walk(UnboundExpression node) {
            Out(".unbound " + SymbolTable.IdToString(node.Name));
            return false;
        }

        // VoidExpression
        public override bool Walk(VoidExpression node) {
            Out(".void {");
            Indent();
            WalkChild(node.Statement);
            Dedent();
            Out("}");
            return false;
        }

        // BlockStatement
        public override bool Walk(BlockStatement node) {
            Out("{");
            NewLine(); Indent();
            foreach (Statement s in node.Statements) {
                WalkChild(s);
                NewLine();
            }
            Dedent();
            Out("}", Flow.NewLine);
            return false;
        }

        // BreakStatement
        public override bool Walk(BreakStatement node) {
            Out(".break;", Flow.NewLine);
            return false;
        }

        // ContinueStatement
        public override bool Walk(ContinueStatement node) {
            Out(".continue;", Flow.NewLine);
            return false;
        }

        // DebugStatement
        public override bool Walk(DebugStatement node) {
            Out(".debug(" + node.Marker + ");", Flow.NewLine);
            return false;
        }

        // DeleteStatement
        public override bool Walk(DeleteStatement node) {
            Out(".del");
            if (node.Variable != null) {
                Out(Flow.Space, SymbolTable.IdToString(node.Variable.Name));
            }
            NewLine();
            return false;
        }

        // DoStatement
        public override bool Walk(DoStatement node) {
            Out(".do {", Flow.NewLine);
            Indent();
            WalkChild(node.Body);
            Dedent();
            Out(Flow.NewLine, "} .while (");
            WalkChild(node.Test);
            Out(");");
            return false;
        }

        // EmptyStatement
        public override bool Walk(EmptyStatement node) {
            Out(";", Flow.NewLine);
            return false;
        }

        // ExpressionStatement
        public override bool Walk(ExpressionStatement node) {
            WalkChild(node.Expression);
            Out(";", Flow.NewLine);
            return false;
        }

        // IfStatement
        public override bool Walk(IfStatement node) {
            for (int i = 0; i < node.Tests.Count; i++) {
                IfStatementTest test = node.Tests[i];
                Out(i == 0 ? ".if (" : "} .elif (");
                WalkChild(test.Test);
                Out(") {", Flow.NewLine);
                Indent();
                WalkChild(test.Body);
                Dedent();
            }

            if (node.ElseStatement != null) {
                Out("} .else {", Flow.NewLine);
                Indent();
                WalkChild(node.ElseStatement);
                Dedent();
            }
            Out("}");
            return false;
        }

        // LabeledStatement
        public override bool Walk(LabeledStatement node) {
            Out(".labeled {", Flow.NewLine);
            Indent();
            WalkChild(node.Statement);
            Dedent();
            Out(Flow.NewLine, "}");
            return false;
        }

        // LoopStatement
        public override bool Walk(LoopStatement node) {
            Out(".for (; ");
            WalkChild(node.Test);
            Out("; ");
            WalkChild(node.Increment);
            Out(") {", Flow.NewLine);
            Indent();
            WalkChild(node.Body);
            Dedent();
            Out(Flow.NewLine, "}");
            return false;
        }

        // ReturnStatement
        public override bool Walk(ReturnStatement node) {
            Out(".return", Flow.Space);
            WalkChild(node.Expression);
            Out(";", Flow.NewLine);
            return false;
        }

        // ScopeStatement
        public override bool Walk(ScopeStatement node) {
            Out(".scope (");
            WalkChild(node.Scope);
            Out(") {", Flow.NewLine);
            Indent();
            WalkChild(node.Body);
            Dedent();
            Out("}", Flow.NewLine);
            return false;
        }

        // SwitchStatement
        public override bool Walk(SwitchStatement node) {
            Out(".switch (");
            WalkChild(node.TestValue);
            Out(") {", Flow.NewLine);
            foreach (SwitchCase sc in node.Cases) {
                Out(".case ");
                WalkChild(sc.Value);
                Out(":", Flow.NewLine);
                Indent(); Indent();
                WalkChild(sc.Body);
                Dedent(); Dedent();
                NewLine();
            }
            Out("}", Flow.NewLine);
            return false;
        }

        // ThrowStatement
        public override bool Walk(ThrowStatement node) {
            Out(Flow.NewLine, ".throw (");
            WalkChild(node.Exception);
            Out(")", Flow.NewLine);
            return false;
        }

        // TryStatement
        public override bool Walk(TryStatement node) {
            Out(".try {", Flow.NewLine);
            Indent();
            WalkChild(node.Body);
            Dedent();
            if (node.Handlers != null && node.Handlers.Count > 0) {
                foreach (CatchBlock cb in node.Handlers) {
                    Out("} .catch ( " + cb.Test.Name);
                    if (cb.Variable != null) {
                        Out(Flow.Space, SymbolTable.IdToString(cb.Variable.Name));
                    }
                    Out(") {", Flow.NewLine);
                    Indent();
                    WalkChild(cb.Body);
                    Dedent();
                }
            }
            if (node.FinallyStatement != null) {
                Out("} .finally {", Flow.NewLine);
                Indent();
                WalkChild(node.FinallyStatement);
                Dedent();
            }
            Out("}", Flow.NewLine);
            return false;
        }

        // YieldStatement
        public override bool Walk(YieldStatement node) {
            Out(".yield ");
            WalkChild(node.Expression);
            Out(";", Flow.NewLine);
            return false;
        }

        // Arg
        public override bool Walk(Arg node) { throw NotImplemented(node); }

        // CatchBlock
        public override bool Walk(CatchBlock node) { throw NotImplemented(node); }

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
                WalkChild(v.DefaultValue);
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
            WalkChild(node.Body);
            Dedent();
            Out("}");
        }

        // CodeBlock
        public override bool Walk(CodeBlock node) {
            Out(".codeblock", Flow.Space);
            DumpBlock(node);
            return false;
        }

        // GeneratorCodeBlock
        public override bool Walk(GeneratorCodeBlock node) {
            Out(".generator", Flow.Space);
            DumpBlock(node);
            return false;
        }

        // IfStatementTest
        public override bool Walk(IfStatementTest node) { throw NotImplemented(node); }

        private static Exception NotImplemented(Node node) {
            return new NotImplementedException("Not implemented: " + node.GetType().Name);
        }
    }
}
#endif
