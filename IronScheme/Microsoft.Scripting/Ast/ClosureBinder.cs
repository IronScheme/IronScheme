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

using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Ast {
    /// <summary>
    /// Ths ClosureBinder takes as an input a bound AST tree in which each Reference is initialized with respective Definition.
    /// The ClosureBinder will then resolve Reference->Definition relationships which span multiple scopes and ensure that the
    /// functions are properly marked with IsClosure and HasEnvironment flags.
    /// </summary>
    class ClosureBinder : Walker {
        class Block {
            private CodeBlock _block;
            private Dictionary<Variable, VariableReference> _references;

            internal Block(CodeBlock block) {
                _block = block;
            }

            internal CodeBlock CodeBlock {
                get { return _block; }
            }

            internal IList<VariableReference> GetReferences() {
                if (_references != null) {
                    Dictionary<Variable, VariableReference>.ValueCollection values = _references.Values;
                    VariableReference[] refs = new VariableReference[values.Count];
                    int index = 0;
                    foreach (VariableReference vr in values) {
                        refs[index++] = vr;
                    }
                    return refs;
                } else {
                    return new VariableReference[0];
                }
            }

            internal VariableReference Reference(Variable variable) {
                if (_references == null) {
                    _references = new Dictionary<Variable, VariableReference>();
                }
                VariableReference reference;
                if (!_references.TryGetValue(variable, out reference)) {
                    _references[variable] = reference = new VariableReference(variable);
                }
                return reference;
            }

            internal void PublishReferences() {
                _block.References = GetReferences();
            }

            internal void AddGeneratorTemps(int count) {
                _block.AddGeneratorTemps(count);
            }
        }

        /// <summary>
        ///  The global codeblock
        /// </summary>
        private readonly CodeBlock _global;

        /// <summary>
        /// List to store all context statements for further processing - storage allocation
        /// </summary>
        private readonly List<CodeBlock> _blocks = new List<CodeBlock>();
        private readonly Stack<Block> _stack = new Stack<Block>();

        public static void Bind(CodeBlock ast) {
            ClosureBinder cb = new ClosureBinder(ast);
            cb.Bind();
        }

        private ClosureBinder(CodeBlock global) {
            _global = global;
        }

        #region AstWalker overrides

        protected internal override bool Walk(BoundAssignment node) {
            node.Ref = Reference(node.Variable);
            return true;
        }

        protected internal override bool Walk(WriteStatement node)
        {
          node.Ref = Reference(node.Variable);
          return true;
        }

        protected internal override bool Walk(BoundExpression node) {
            node.Ref = Reference(node.Variable);
            return true;
        }

        protected internal override bool Walk(DeleteStatement node) {
            node.Ref = Reference(node.Variable);
            return true;
        }

        protected internal override bool Walk(CatchBlock node) {
            // CatchBlock is not required to have target variable
            if (node.Variable != null) {
                node.Ref = Reference(node.Variable);
            }
            return true;            
        }

        protected internal override bool Walk(CodeBlock node) {
          node.ResetBindings();
  
          Push(node);
          
            return true;
        }
        protected internal override void PostWalk(CodeBlock node) {
            ProcessAndPop(node);
        }

        protected internal override bool Walk(GeneratorCodeBlock node) {
            Push(node);
            return true;
        }

        protected internal override void PostWalk(GeneratorCodeBlock node) {
            int temps = node.BuildYieldTargets();
            AddGeneratorTemps(temps);
            ProcessAndPop(node);
        }

        private void Push(CodeBlock block) {
            _stack.Push(new Block(block));
        }

        private void ProcessAndPop(CodeBlock block) {
            _blocks.Add(block);
            Block top = _stack.Pop();
            Debug.Assert(top.CodeBlock == block);
            top.PublishReferences();
        }

        private void AddGeneratorTemps(int count) {
            Debug.Assert(_stack.Count > 0);
            _stack.Peek().AddGeneratorTemps(count);
        }

        #endregion

        private void Bind() {
            // Collect the context statements
            WalkNode(_global);
            BindTheScopes();
        }

        private VariableReference Reference(Variable variable) {
            Debug.Assert(variable != null);
            return _stack.Peek().Reference(variable);
        }

        // TODO: Alternatively, this can be virtual method on ScopeStatement
        // or also implemented directly in the walker (PostWalk)
        private void BindTheScopes() {
            for (int i = 0; i < _blocks.Count; i++) {
                CodeBlock block = _blocks[i];
                if (!block.IsGlobal) {
                    BindCodeBlock(block);
                }
            }
        }

        private void BindCodeBlock(CodeBlock block) {
            // If the function is generator or needs custom frame,
            // lift locals to closure
            if (block is GeneratorCodeBlock || block.EmitLocalDictionary) {
                LiftLocalsToClosure(block);
            }
            ResolveClosure(block);
        }

        private static void LiftLocalsToClosure(CodeBlock block) {
            // Lift all parameters
            foreach (Variable p in block.Parameters) {
                p.LiftToClosure();
            }
            // Lift all locals
            foreach (Variable d in block.Variables) {
                if (d.Kind == Variable.VariableKind.Local) {
                    d.LiftToClosure();
                }
            }
            block.HasEnvironment = true;
        }

        private void ResolveClosure(CodeBlock block) {
          if (block.Name == "anon#1#2##anon#1#2#3#4")
          {
            ;
          }
            foreach (VariableReference r in block.References) {
                Debug.Assert(r.Variable != null);

                if (r.Variable.Block == block) {
                    // local reference => no closure
                    continue;
                }

                // Global variables as local
                if (r.Variable.Kind == Variable.VariableKind.Global || 
                    (r.Variable.Kind == Variable.VariableKind.Local && r.Variable.Block.IsGlobal)) {
                    //Debug.Assert(r.Variable.Block == _global);
                    continue;
                }

                // Lift the variable into the closure
                r.Variable.LiftToClosure();

                // Mark all parent scopes between the use and the definition
                // as closures/environment
                CodeBlock current = block;
                do {
                    current.IsClosure = true;

                    CodeBlock parent = current.Parent;
                    if (parent == null) {
                        throw new ArgumentException(
                            String.Format(
                                "Cannot resolve variable '{0}' " +
                                "referenced from code block '{1}' " +
                                "and defined in code block {2}).\n" +
                                "Is CodeBlock.Parent set correctly?",
                                SymbolTable.IdToString(r.Variable.Name),
                                block.Name ?? "<unnamed>",
                                r.Variable.Block != null ? (r.Variable.Block.Name ?? "<unnamed>") : "<unknown>"
                            )
                        );
                    }

                    //parent.HasEnvironment = true;
                    current = parent;
                } while (current != r.Variable.Block);
            }
        }
    }
}
