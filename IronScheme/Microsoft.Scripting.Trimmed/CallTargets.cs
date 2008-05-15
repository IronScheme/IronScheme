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
using System.Text;

namespace Microsoft.Scripting {
    public static class CallTargets {
        #region Generated MaximumCallArgs

        // *** BEGIN GENERATED CODE ***

        public const int MaximumCallArgs = 5;

        // *** END GENERATED CODE ***

        #endregion

        public static Type GetTargetType(bool needsContext, int nargs, bool needsThis) {
            if (needsThis) {
                if (needsContext) {
                    switch (nargs) {
                        case 0: return typeof(CallTargetWithContextAndThis0);
                        case 1: return typeof(CallTargetWithContextAndThis1);
                        case 2: return typeof(CallTargetWithContextAndThis2);
                        case 3: return typeof(CallTargetWithContextAndThis3);
                        case 4: return typeof(CallTargetWithContextAndThis4);
                        case 5: return typeof(CallTargetWithContextAndThis5);
                    }
                } else {
                    switch (nargs) {
                        case 0: return typeof(CallTargetWithThis0);
                        case 1: return typeof(CallTargetWithThis1);
                        case 2: return typeof(CallTargetWithThis2);
                        case 3: return typeof(CallTargetWithThis3);
                        case 4: return typeof(CallTargetWithThis4);
                        case 5: return typeof(CallTargetWithThis5);
                    }
                }
            } else {
                return GetTargetType(needsContext, nargs);
            }
            throw new NotImplementedException();
        }

        private static Type GetTargetType(bool needsContext, int nargs) {
            if (needsContext) {
                switch (nargs) {
                    case 0: return typeof(CallTargetWithContext0);
                    case 1: return typeof(CallTargetWithContext1);
                    case 2: return typeof(CallTargetWithContext2);
                    case 3: return typeof(CallTargetWithContext3);
                    case 4: return typeof(CallTargetWithContext4);
                    case 5: return typeof(CallTargetWithContext5);
                }
            } else {
                switch (nargs) {
                    case 0: return typeof(CallTarget0);
                    case 1: return typeof(CallTarget1);
                    case 2: return typeof(CallTarget2);
                    case 3: return typeof(CallTarget3);
                    case 4: return typeof(CallTarget4);
                    case 5: return typeof(CallTarget5);
                }
            }
            throw new NotImplementedException();
        }
    }


    public delegate object CallTargetN(params object[] args);
    public delegate object CallTargetWithContextN(CodeContext context, params object[] args);
    public delegate object CallTargetWithThisN(object instance, params object[] args);
    public delegate object CallTargetWithContextAndThisN(CodeContext context, object instance, params object[] args);

    #region Generated Contextless CallTargets

    // *** BEGIN GENERATED CODE ***


    public delegate object CallTarget0();
    public delegate object CallTarget1(object arg0);
    public delegate object CallTarget2(object arg0, object arg1);
    public delegate object CallTarget3(object arg0, object arg1, object arg2);
    public delegate object CallTarget4(object arg0, object arg1, object arg2, object arg3);
    public delegate object CallTarget5(object arg0, object arg1, object arg2, object arg3, object arg4);


    // *** END GENERATED CODE ***

    #endregion

    #region Generated CallTargets

    // *** BEGIN GENERATED CODE ***


    public delegate object CallTargetWithContext0(CodeContext context);
    public delegate object CallTargetWithContext1(CodeContext context, object arg0);
    public delegate object CallTargetWithContext2(CodeContext context, object arg0, object arg1);
    public delegate object CallTargetWithContext3(CodeContext context, object arg0, object arg1, object arg2);
    public delegate object CallTargetWithContext4(CodeContext context, object arg0, object arg1, object arg2, object arg3);
    public delegate object CallTargetWithContext5(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4);


    // *** END GENERATED CODE ***

    #endregion

    #region Generated CallTargets WithThis

    // *** BEGIN GENERATED CODE ***


    public delegate object CallTargetWithThis0(object instance);
    public delegate object CallTargetWithThis1(object instance, object arg0);
    public delegate object CallTargetWithThis2(object instance, object arg0, object arg1);
    public delegate object CallTargetWithThis3(object instance, object arg0, object arg1, object arg2);
    public delegate object CallTargetWithThis4(object instance, object arg0, object arg1, object arg2, object arg3);
    public delegate object CallTargetWithThis5(object instance, object arg0, object arg1, object arg2, object arg3, object arg4);


    // *** END GENERATED CODE ***

    #endregion

    #region Generated CallTargets WithContextAndThis

    // *** BEGIN GENERATED CODE ***


    public delegate object CallTargetWithContextAndThis0(CodeContext context, object instance);
    public delegate object CallTargetWithContextAndThis1(CodeContext context, object instance, object arg0);
    public delegate object CallTargetWithContextAndThis2(CodeContext context, object instance, object arg0, object arg1);
    public delegate object CallTargetWithContextAndThis3(CodeContext context, object instance, object arg0, object arg1, object arg2);
    public delegate object CallTargetWithContextAndThis4(CodeContext context, object instance, object arg0, object arg1, object arg2, object arg3);
    public delegate object CallTargetWithContextAndThis5(CodeContext context, object instance, object arg0, object arg1, object arg2, object arg3, object arg4);


    // *** END GENERATED CODE ***

    #endregion

}
