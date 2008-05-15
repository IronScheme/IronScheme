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
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Types;

namespace Microsoft.Scripting {
    [Flags]
    public enum CallType {
        None = 0,
        ImplicitInstance,
    }

    public abstract partial class FastCallable : DynamicTypeSlot, ICallableWithCodeContext {        
        protected FastCallable() { }

        public abstract object Call(CodeContext context, params object[] args);
        public abstract object CallInstance(CodeContext context, object instance, params object[] args);

        internal static Exception BadArgumentError(string name, int minArgs, int maxArgs, CallType callType, int argCount) {
            if (callType == CallType.ImplicitInstance) {
                if (maxArgs > 0) {
                    argCount -= 1;
                    minArgs -= 1;
                    maxArgs -= 1;
                } 
            }

            // This generates Python style error messages assuming that all arg counts in between min and max are allowed
            //It's possible that discontinuous sets of arg counts will produce a weird error message
            return RuntimeHelpers.TypeErrorForIncorrectArgumentCount(name, maxArgs, maxArgs - minArgs, argCount);
        }

        protected static object[] PrependInstance(object instance, object[] args) {
            object[] nargs = new object[args.Length + 1];
            nargs[0] = instance;
            for (int i = 0; i < args.Length; i++) {
                nargs[i + 1] = args[i];
            }
            return nargs;
        }
    }

    // Swaps the first two arguments to the wrapped fast callable
    // This is used to implement __r*__ functions from .NET methods
    public partial class ReversedFastCallableWrapper : FastCallable {
        private FastCallable target;
        public ReversedFastCallableWrapper(FastCallable target) {
            this.target = target;
        }

        public override object Call(CodeContext context, params object[] args) {
            if (args.Length == 0) {
                return Call(context);
            }
            if (args.Length == 1) {
                return Call(context, args[0]);
            }
            if (args.Length == 2) {
                return Call(context, args[0], args[1]);
            }

            object[] newArgs = new object[args.Length];
            newArgs[0] = args[1];
            newArgs[1] = args[0];
            for (int i = 2; i < args.Length; i++) {
                newArgs[i] = args[i];
            }

            return target.Call(context, newArgs);
        }

        public override object CallInstance(CodeContext context, object instance, params object[] args) {
            if (args.Length == 0) {
                return CallInstance(context, instance);
            }
            if (args.Length == 1) {
                return CallInstance(context, instance, args[0]);
            }

            object[] newArgs = new object[args.Length];
            newArgs[0] = instance;
            instance = args[0];
            for (int i = 1; i < args.Length; i++) {
                newArgs[i] = args[i];
            }

            return target.Call(context, newArgs);
        }
    }
}
