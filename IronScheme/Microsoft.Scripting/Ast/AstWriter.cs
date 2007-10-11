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

        private int _depth = 0;

        private TextWriter _outFile;
        private Queue<Block> _blocks;

        private int _blockid;

        private AstWriter() {
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
            _outFile = outFile;
            _depth = 0;

            _outFile.WriteLine("#\n# AST {0}\n#", name);

            node.Walk(this);
            Debug.Assert(_depth == 0);


            while (_blocks != null && _blocks.Count > 0) {
                Block b = _blocks.Dequeue();
                Write("#");
                Write(String.Format("# CODE BLOCK: {0} (# {1})", b.CodeBlock.Name, b.Id));
                Write("#");

                b.CodeBlock.Walk(this);

                Write("");

                Debug.Assert(_depth == 0);
            }
        }


        private void Push(string label) {
            Write(label);
            _depth++;
        }

        private void Pop(string label) {
            _depth--;
            Write(label);
        }

        private void Pop() {
            _depth--;
            Debug.Assert(_depth >= 0);
        }

        private int Enqueue(CodeBlock block) {
            if (_blocks == null) {
                _blocks = new Queue<Block>();
            }
            _blocks.Enqueue(new Block(block, ++_blockid));
            return _blockid;
        }

        public void Write(string s) {
            _outFile.WriteLine(new string(' ', _depth * 3) + s);
        }

        private void Child(Node node) {
            if (node != null) node.Walk(this);
        }

        // Unfortunately, overload resolution happens in our parent class, so we have to explicitly call this default case
        public bool DefaultWalk(Node node, string name) {
            Push(name);
            return (node != null);
        }

        // More proper would be to make this a virtual method on Action
        private string FormatAction(DynamicAction action) {
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
            Push(".action " + FormatAction(node.Action) + "(");
            return true;
        }
        public override void PostWalk(ActionExpression node) {
            Pop(")");
        }

        // ArrayIndexAssignment
        public override bool Walk(ArrayIndexAssignment node) {
            return DefaultWalk(node, "< [] = >");
        }
        public override void PostWalk(ArrayIndexAssignment node) {
            Pop();
        }

        // ArrayIndexExpression
        public override bool Walk(ArrayIndexExpression node) {
            return DefaultWalk(node, "< [] >");
        }
        public override void PostWalk(ArrayIndexExpression node) {
            Pop();
        }

        // BinaryExpression
        public override bool Walk(BinaryExpression node) {
            return DefaultWalk(node, "<binaryexpr> Op:" + node.Operator.ToString());
        }
        public override void PostWalk(BinaryExpression node) {
            Pop();
        }

        // StaticUnaryExpression
        public override bool Walk(UnaryExpression node) {
            switch (node.Operator) {
                case UnaryOperators.Convert:
                    Push("(" + node.Type.ToString() + ") (");
                    break;
                case UnaryOperators.Negate:
                    Push("- (");
                    break;
                case UnaryOperators.Not:
                    Push(node.Type == typeof(bool) ? "! (" : "~ (");
                    break;
                case UnaryOperators.OnesComplement:
                    Push("~ (");
                    break;
            }
            return true;
        }
        public override void PostWalk(UnaryExpression node) {
            Pop(")");
        }

        // DynamicConversionExpression
        public override bool Walk(DynamicConversionExpression node) {
            Push(".dynamic (" + node.Type.ToString() + ")");
            return true;
        }
        public override void PostWalk(DynamicConversionExpression node) {
            Pop();
        }

        // BoundAssignment
        public override bool Walk(BoundAssignment node) {
            Push(String.Format(".bound {0} = ...", SymbolTable.IdToString(node.Variable.Name)));
            return true;
        }
        public override void PostWalk(BoundAssignment node) {
            Pop();
        }

        // BoundExpression
        public override bool Walk(BoundExpression node) {
            Write(".bound (" + node.Name.ToString() + ")");
            return true;
        }

        // UnboundAssignment
        public override bool Walk(UnboundAssignment node) {
            Push(String.Format(".unbound {0} = ...", SymbolTable.IdToString(node.Name)));
            return true;
        }
        public override void PostWalk(UnboundAssignment node) {
            Pop();
        }

        // UnboundExpression
        public override bool Walk(UnboundExpression node) {
            Write(".unbound (" + node.Name.ToString() + ")");
            return true;
        }

        // CallWithThisExpression
        public override bool Walk(CallWithThisExpression node) {
            return NotImplemented(node);
        }

        private void DumpCodeBlockExpression(CodeBlockExpression node) {
            int id = Enqueue(node.Block);
            Write(String.Format(".block {0} ({1}   #{2}) {3}{4}{5}", 
                node.DelegateType != null ? node.DelegateType.Name : "no-type", 
                node.Block.Name, 
                id,
                node.ForceWrapperMethod ? " ForceWrapperMethod" : "",
                node.IsStronglyTyped ? " IsStronglyTyped" : "",
                node.IsDeclarative ? " IsDeclarative" : ""));
        }

        // CodeBlockExpression
        public override bool Walk(CodeBlockExpression node) {
            // Walk gets called only if IsDeclarative==true
            // So we do the real work in PostWalk
            Debug.Assert(node.IsDeclarative);
            return false;
        }

        public override void PostWalk(CodeBlockExpression node) {
            DumpCodeBlockExpression(node);
        }

        // CodeContextExpression
        public override bool Walk(CodeContextExpression node) {
            return DefaultWalk(node, ".context");
        }
        public override void PostWalk(CodeContextExpression node) {
            Pop();
        }

        // CommaExpression
        public override bool Walk(CommaExpression node) {
            return DefaultWalk(node, ".comma (" + node.ValueIndex + ") {");
        }
        public override void PostWalk(CommaExpression node) {
            Pop("}");
        }

        // ConditionalExpression
        public override bool Walk(ConditionalExpression node) {
            return DefaultWalk(node, "<?:>");
        }
        public override void PostWalk(ConditionalExpression node) {
            Pop();
        }

        // ConstantExpression
        public override bool Walk(ConstantExpression node) {
            return DefaultWalk(node, GetConstantDisplay(node));
        }

        private static string GetConstantDisplay(ConstantExpression node) {
            if (node.Value == null) return "(null)";

            CompilerConstant cc = node.Value as CompilerConstant;

            if (cc != null) {
                object value = cc.Create();
                if (value is ITemplatedValue) {
                    return "TemplateVal: " + ((ITemplatedValue)value).ObjectValue.ToString();
                }

                return cc.Name + " " + cc.Type + " " + value.ToString();
            } else if (node.Value is Type) {
                return "Type: " + ((Type)node.Value).FullName;
            }

            string cs = node.Value as string;
            if (cs != null) {
                return "(string) \"" + cs + "\"";
            }

            return "(" + node.Value.GetType().Name + ") " + node.Value.ToString();
        }

        public override void PostWalk(ConstantExpression node) {
            Pop();
        }

        // EnvironmentExpression
        public override bool Walk(EnvironmentExpression node) {
            return DefaultWalk(node, "<$env>");
        }
        public override void PostWalk(EnvironmentExpression node) {
            Pop();
        }

        // MemberExpression
        public override bool Walk(MemberExpression node) {
            return DefaultWalk(node, "<member> Name:" + node.Member.Name);
        }
        public override void PostWalk(MemberExpression node) {
            Pop();
        }

        // MethodCallExpression
        public override bool Walk(MethodCallExpression node) {
            Push(".call " + node.Method.ReflectedType.Name + "." + node.Method.Name + " (");
            if (node.Instance != null) {
                Push(".inst (");
                Child(node.Instance);
                Pop(")");
            }
            Push(".args (");
            foreach (Expression e in node.Arguments) {
                Child(e);
            }
            Pop(")");
            Pop(")");
            return false;
        }


        // NewArrayExpression
        public override bool Walk(NewArrayExpression node) {
            Push(".new (" + node.Type.FullName + ") = {");
            return true;
        }
        public override void PostWalk(NewArrayExpression node) {
            Pop("}");
        }

        // NewExpression
        public override bool Walk(NewExpression node) {
            Push("<new> Name:" + node.Constructor.DeclaringType.FullName);
            Push("args:");
            foreach (Expression e in node.Arguments) {
                Child(e);
            }
            Pop();
            Pop();
            return false;
        }


        // ParamsExpression
        public override bool Walk(ParamsExpression node) {
            return DefaultWalk(node, "<$params$>");
        }
        public override void PostWalk(ParamsExpression node) {
            Pop();
        }

        // ParenthesisExpression
        public override bool Walk(ParenthesizedExpression node) {
            Push("(");
            return true;
        }
        public override void PostWalk(ParenthesizedExpression node) {
            Pop(")");
        }

        // ShortCircuitExpression
        public override bool Walk(ShortCircuitExpression node) {
            return NotImplemented(node);
        }

        // VoidExpression
        public override bool Walk(VoidExpression node) {
            Push(".void (");
            return true;
        }
        public override void PostWalk(VoidExpression node) {
            Pop(")");
        }

        // BlockStatement
        public override bool Walk(BlockStatement node) {
            Push("{");
            return true;
        }
        public override void PostWalk(BlockStatement node) {
            Pop("}");
        }

        // BreakStatement
        public override bool Walk(BreakStatement node) {
            Write(".break");
            return false;
        }

        // ContinueStatement
        public override bool Walk(ContinueStatement node) {
            return DefaultWalk(node, ".continue;");
        }
        public override void PostWalk(ContinueStatement node) {
            Pop();
        }

        // DebugStatement
        public override bool Walk(DebugStatement node) {
            Write(".trace \"" + node.Marker + "\"");
            return false;
        }

        // DelStatement
        public override bool Walk(DeleteStatement node) {
            string descr = ".del";
            if (node.Variable != null)
                descr += " " + SymbolTable.IdToString(node.Variable.Name);
            return DefaultWalk(node, descr);
        }
        public override void PostWalk(DeleteStatement node) {
            Pop();
        }

        // DoStatement
        public override bool Walk(DoStatement node) {
            Push(".do {");
            node.Body.Walk(this);
            Pop();
            Push("} .while (");
            node.Test.Walk(this);
            Pop(");");
            return false;
        }

        // LoopStatement
        public override bool Walk(LoopStatement node) {
            Push(".loop {");
            return true;
        }
        public override void PostWalk(LoopStatement node) {
            Pop("}");
        }

        // EmptyStatement
        public override bool Walk(EmptyStatement node) {
            Write(";");
            return false;
        }

        // IfStatement
        public override bool Walk(IfStatement node) {
            for (int i = 0; i < node.Tests.Count; i++) {
                Push(i == 0 ? ".if (" : "} .else if (");
                node.Tests[i].Test.Walk(this);
                Write(") {");
                node.Tests[i].Body.Walk(this);
                Pop();
            }
            if (node.ElseStatement != null) {
                Push("} .else {");
                Child(node.ElseStatement);
                Pop("}");
            } else {
                Write("}");
            }
            return false;
        }

        // LabeledStatement
        public override bool Walk(LabeledStatement node) {
            return DefaultWalk(node, "<label>");
        }
        public override void PostWalk(LabeledStatement node) {
            Pop();
        }

        // ReturnStatement
        public override bool Walk(ReturnStatement node) {
            return DefaultWalk(node, ".return (");
        }
        public override void PostWalk(ReturnStatement node) {
            Pop(")");
        }

        // SwitchStatement
        public override bool Walk(SwitchStatement node) {
            Push(". switch (");
            Child(node.TestValue);
            Pop(")");
            Push("}");
            foreach (SwitchCase sc in node.Cases) {
                DumpCase(sc);
            }
            Pop("}");
            return false;
        }

        private void DumpCase(SwitchCase sc) {
            Push(".case (");
            Child(sc.Value);
            Pop(")");
            Push("{");
            Child(sc.Body);
            Pop("}");
        }

        // ThrowStatement
        public override bool Walk(ThrowExpression node) {
            return DefaultWalk(node, ".throw (");
        }
        public override void PostWalk(ThrowExpression node) {
            Pop(")");
        }

        // TryStatement
        public override bool Walk(TryStatement node) {
            Push(".try {");
            node.Body.Walk(this);
            if (node.Handlers != null) {
                StringBuilder sb = new StringBuilder("} .catch (");

                foreach (CatchBlock cb in node.Handlers) {
                    Pop();
                    sb.Append(cb.Test.Name);
                    if (cb.Variable != null) {
                        sb.Append(" ");
                        sb.Append(cb.Variable.Name);
                    }
                    sb.Append(") {");

                    Push(sb.ToString());
                    cb.Body.Walk(this);
                    sb.Length = 10;
                }
            }

            if (node.FinallyStatement != null) {
                Pop();
                Push("} .finally {");
                node.FinallyStatement.Walk(this);
            }

            Pop("}");
            return false;
        }

        // CatchBlock
        public override bool Walk(CatchBlock node) {
            Push(". catch (");
            return true;
        }
        public override void PostWalk(CatchBlock node) {
            Pop(")");
        }

        // YieldStatement
        public override bool Walk(YieldStatement node) {
            Push(".yield");
            return true;
        }
        public override void PostWalk(YieldStatement node) {
            Pop();
        }

        // Arg
        public override bool Walk(Arg node) {
            Push(".arg");
            if (node.Name != SymbolId.Empty) {
                Push(SymbolTable.IdToString(node.Name));
                Pop();
            }
            Child(node.Expression);
            Pop();
            return false;
        }

        private string GetCodeBlockInfo(CodeBlock block) {
            string info = String.Format("{0} {1} (", block.ReturnType.Name, block.Name);

            if (block.IsGlobal) {
                info += "IsGlobal,";
            }

            if (!block.IsVisible) {
                info += "IsVisible=false,";
            }

            if (block.IsClosure) {
                info += "IsClosure,";
            }

            if (block.ParameterArray) {
                info += "ParameterArray,";
            }

            if (block.HasEnvironment) {
                info += "HasEnvironment,";
            }

            if (block.EmitLocalDictionary) {
                info += "EmitLocalDictionary,";
            }

            info += ")";

            return info;
        }

        // CodeBlock
        public override bool Walk(CodeBlock node) {
            Push(".codeblock " + GetCodeBlockInfo(node) + " (");
            foreach (Variable v in node.Parameters) {
                DumpVariable(v);
            }
            Write(")");
            
            Push(".vars {");
            foreach (Variable v in node.Variables) {
                DumpVariable(v);
            }
            Pop("}");

            Child(node.Body);
            Pop("}");
            return false;
        }

        private void DumpVariable(Variable v) {
            string descr = String.Format("{2} {0} ({1}", SymbolTable.IdToString(v.Name), v.Kind.ToString(), v.Type.Name);
            if (v.Lift) { descr += ",Lift"; }
            if (v.InParameterArray) { descr += ",InParameterArray"; }
            descr += ")";
            Push(descr);

            if (v.DefaultValue != null) {
                Child(v.DefaultValue);
            }
            Pop();
        }

        // GeneratorCodeBlock
        public override bool Walk(GeneratorCodeBlock node) {
            Push(".generator " + GetCodeBlockInfo(node) + " (");
            foreach (Variable v in node.Parameters) {
                DumpVariable(v);
            }
            Pop(")");
            Push(".vars {");
            foreach (Variable v in node.Variables) {
                DumpVariable(v);
            }
            Pop("}");
            Push("{");
            Child(node.Body);
            Pop("}");
            return false;
        }


        // IfStatementTest
        public override bool Walk(IfStatementTest node) {
            return DefaultWalk(node, "<cond>");
        }
        public override void PostWalk(IfStatementTest node) {
            Pop();
        }

        public override bool Walk(ScopeStatement node) {
            return DefaultWalk(node, "<scope>");
        }
        public override void PostWalk(ScopeStatement node) {
            Pop();
        }

        private bool NotImplemented(Node node) {
            Push("NOT IMPLEMENTED: " + node.GetType().Name);
            Pop();
            return false;
        }
    }
}
#endif
