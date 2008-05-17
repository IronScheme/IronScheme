
#if FULL
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
using System.Collections.Generic;
using System.Reflection.Emit;
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Generation {
    public static class StubGenerator {

        public enum CallType {
            None = 0,
            ArgumentList = 1,
            KeywordDictionary = 2,
        }

        /// <summary>
        /// Generates stub to receive the CLR call and then call the dynamic language code.
        /// </summary>
        public static void  EmitClrCallStub(CodeGen cg, Slot callTarget, int firstArg, CallType functionAttributes) {
            List<ReturnFixer> fixers = new List<ReturnFixer>(0);
            IList<Slot> args = cg.ArgumentSlots;
            int nargs = args.Count - firstArg;
            
            CallAction action;
            if ((functionAttributes & CallType.ArgumentList) != 0) {
                ArgumentInfo[] infos = CompilerHelpers.MakeRepeatedArray(ArgumentInfo.Simple, nargs);
                infos[nargs - 1] = new ArgumentInfo(ArgumentKind.List);

                action = CallAction.Make(new CallSignature(infos));
            } else {
                action = CallAction.Make(nargs);
            }

            bool fast;
            Slot site = cg.CreateDynamicSite(action, 
                CompilerHelpers.MakeRepeatedArray(typeof(object), nargs + 2), 
                out fast);

            site.EmitGet(cg);
            if (!fast) cg.EmitCodeContext();

            if (DynamicSiteHelpers.IsBigTarget(site.Type)) {
                cg.EmitTuple(site.Type.GetGenericArguments()[0], args.Count + 1, delegate(int index) {
                    if (index == 0) {
                        callTarget.EmitGet(cg);
                    } else {
                        ReturnFixer rf = ReturnFixer.EmitArgument(cg, args[index - 1]);
                        if (rf != null) fixers.Add(rf);
                    }
                });
            } else {
                callTarget.EmitGet(cg);

                for (int i = firstArg; i < args.Count; i++) {
                    ReturnFixer rf = ReturnFixer.EmitArgument(cg, args[i]);
                    if (rf != null) fixers.Add(rf);
                }
            }

            cg.EmitCall(site.Type, "Invoke"); 

            foreach (ReturnFixer rf in fixers) {
                rf.FixReturn(cg);
            }
            cg.EmitReturnFromObject();
        }

        private static Type[] CreateSignatureWithContext(int count) {
            Type[] array = new Type[count + 1];
            while (count > 0) {
                array[count--] = typeof(object);
            }
            array[0] = typeof(CodeContext);
            return array;
        }
    }
}

#endif	
