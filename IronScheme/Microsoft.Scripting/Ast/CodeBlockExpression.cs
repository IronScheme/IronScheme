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

using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;
using System.Diagnostics;
using System.Text;
using System.Threading;

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {
    /// <summary>
    /// An expression that will return a reference to a block of code.
    /// Currently these references are created by emitting a delegate of the 
    /// requested type.
    /// </summary>
    public class CodeBlockExpression : Expression {
        private readonly CodeBlock _block;
        private readonly bool _forceWrapperMethod;
        private readonly bool _stronglyTyped;
        private readonly Type _delegateType;
        private readonly bool _isDeclarative;
        
#if DEBUG
        internal bool ForceWrapperMethod {
            get { return _forceWrapperMethod; }
        }

        internal bool IsStronglyTyped {
            get { return _stronglyTyped; }
        }

#endif
        internal Type DelegateType {
            get { return _delegateType; }
        }

        /// <summary>
        /// Whether this expression declares the block. If so, the declaring code's variable are accessible from within the block (a closure is created).
        /// Otherwise, the block is only referred to by the containing block and do not share it's scope.
        /// </summary>
        public bool IsDeclarative {
            get { return _isDeclarative; }
        }

        internal CodeBlockExpression(CodeBlock block, bool forceWrapperMethod, bool stronglyTyped, bool isDeclarative, Type delegateType) {
            Assert.NotNull(block);

            if (isDeclarative) {
                block.DeclarativeReferenceAdded();
            }

            _block = block;
            _forceWrapperMethod = forceWrapperMethod;
            _stronglyTyped = stronglyTyped;
            _isDeclarative = isDeclarative;
            _delegateType = delegateType;
        }

        public CodeBlock Block {
            get { return _block; }
        }

        public override void Walk(Walker walker) {
            // do not walk into the referenced code block - we have already been there:
            if (_isDeclarative && walker.Walk(this)) {
                _block.Walk(walker);
            }
            walker.PostWalk(this);
        }

        public override Type Type {
            get {
                return typeof(Delegate);
            }
        }

        protected override object DoEvaluate(CodeContext context) {
            Debug.Assert(_delegateType == null); // We currently cannot create a strongly-typed delegate
            return _block.GetDelegateForInterpreter(context, _forceWrapperMethod);
        }

        public override void Emit(CodeGen cg) {
            _block.EmitDelegateConstruction(cg, _forceWrapperMethod, _stronglyTyped, _delegateType);
        }
    }

    public static partial class Ast {

        // TODO: rename to CodeBlockDeclaration?

        public static CodeBlockExpression CodeBlockExpression(CodeBlock block, bool forceWrapper) {
            return new CodeBlockExpression(block, forceWrapper, false, true, null);
        }

        public static CodeBlockExpression CodeBlockExpression(CodeBlock block, bool forceWrapper, bool stronglyTyped) {
            return new CodeBlockExpression(block, forceWrapper, stronglyTyped, true, null);
        }

        public static CodeBlockExpression CodeBlockExpression(CodeBlock block, bool stronglyTyped, Type delegateType) {
            return new CodeBlockExpression(block, false, stronglyTyped, true, delegateType);
        }

        public static CodeBlockExpression CodeBlockReference(CodeBlock block, Type delegateType) {
            return new CodeBlockExpression(block, false, true, false, delegateType);
        }
    }
}
