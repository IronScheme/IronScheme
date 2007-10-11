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
using Microsoft.Scripting.Hosting;

namespace Microsoft.Scripting {
    /// <summary>
    /// Singleton LanguageContext which represents a language-neutral LanguageContext
    /// </summary>
    public class InvariantContext : LanguageContext {
        public static InvariantContext Instance;
        public static CodeContext CodeContext;

        static InvariantContext() {
            Instance = new InvariantContext();
            ModuleContext moduleContext = new ModuleContext(null);
            moduleContext.ShowCls = true;
            CodeContext = new CodeContext(new Scope(new SymbolDictionary()), Instance, moduleContext);
        }
        
        private InvariantContext()
            : base() {
        }

        public override Microsoft.Scripting.Ast.CodeBlock ParseSourceCode(CompilerContext context) {
            throw new Exception("The method or operation is not implemented.");
        }
    }
}
