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
using System.Reflection;
using System.Collections.Generic;
using System.Text;

namespace Microsoft.Scripting {
    public abstract partial class FastCallable {
        #region Generated FastCallableMembers

        // *** BEGIN GENERATED CODE ***

        public static FastCallable Make(string name, bool needsContext, int nargs, Delegate target) {
            if (needsContext) {
                switch (nargs) {
                    case 0: return new FastCallableWithContext0(name, (CallTargetWithContext0)target);
                    case 1: return new FastCallableWithContext1(name, (CallTargetWithContext1)target);
                    case 2: return new FastCallableWithContext2(name, (CallTargetWithContext2)target);
                    case 3: return new FastCallableWithContext3(name, (CallTargetWithContext3)target);
                    case 4: return new FastCallableWithContext4(name, (CallTargetWithContext4)target);
                    case 5: return new FastCallableWithContext5(name, (CallTargetWithContext5)target);
                }
            } else {
                switch (nargs) {
                    case 0: return new FastCallable0(name, (CallTarget0)target);
                    case 1: return new FastCallable1(name, (CallTarget1)target);
                    case 2: return new FastCallable2(name, (CallTarget2)target);
                    case 3: return new FastCallable3(name, (CallTarget3)target);
                    case 4: return new FastCallable4(name, (CallTarget4)target);
                    case 5: return new FastCallable5(name, (CallTarget5)target);
                }
            }
            throw new NotImplementedException();
        }
        public abstract object Call(CodeContext context);
        public abstract object Call(CodeContext context, object arg0);
        public abstract object CallInstance(CodeContext context, object arg0);
        public abstract object Call(CodeContext context, object arg0, object arg1);
        public abstract object CallInstance(CodeContext context, object arg0, object arg1);
        public abstract object Call(CodeContext context, object arg0, object arg1, object arg2);
        public abstract object CallInstance(CodeContext context, object arg0, object arg1, object arg2);
        public abstract object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3);
        public abstract object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3);
        public abstract object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4);
        public abstract object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4);
        public object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5) {
            return CallInstance(context, arg0, new object[] { arg1, arg2, arg3, arg4, arg5 });
        }

        // *** END GENERATED CODE ***

        #endregion
    }

    public partial class ReversedFastCallableWrapper : FastCallable {
        #region Generated ReversedFastCallableWrapper Members

        // *** BEGIN GENERATED CODE ***

        public override object Call(CodeContext context) {
            return target.Call(context);
        }
        public override object Call(CodeContext context, object arg0) {
            return target.Call(context, arg0);
        }
        public override object CallInstance(CodeContext context, object arg0) {
            return target.CallInstance(context, arg0);
        }
        public override object Call(CodeContext context, object arg0, object arg1) {
            return target.Call(context, arg1, arg0);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1) {
            return target.CallInstance(context, arg1, arg0);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2) {
            return target.Call(context, arg1, arg0, arg2);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2) {
            return target.CallInstance(context, arg1, arg0, arg2);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            return target.Call(context, arg1, arg0, arg2, arg3);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            return target.CallInstance(context, arg1, arg0, arg2, arg3);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            return target.Call(context, arg1, arg0, arg2, arg3, arg4);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            return target.CallInstance(context, arg1, arg0, arg2, arg3, arg4);
        }

        // *** END GENERATED CODE ***

        #endregion
    }


    #region Generated ConcreteFastCallables

    // *** BEGIN GENERATED CODE ***

    public class FastCallableAny : FastCallable {
        public CallTarget0 target0;
        public CallTarget1 target1;
        public CallTarget2 target2;
        public CallTarget3 target3;
        public CallTarget4 target4;
        public CallTarget5 target5;
        public CallTargetN targetN;
        private string name;
        private int minArgs, maxArgs;
        public FastCallableAny(string name, int minArgs, int maxArgs) {
            this.name = name;
            this.minArgs = minArgs;
            this.maxArgs = maxArgs;
        }
        public override object Call(CodeContext context) {
            if (target0 != null) return target0();
            throw BadArgumentError(CallType.None, 0);
        }
        public override object Call(CodeContext context, object arg0) {
            if (target1 != null) return target1(arg0);
            throw BadArgumentError(CallType.None, 1);
        }
        public override object Call(CodeContext context, object arg0, object arg1) {
            if (target2 != null) return target2(arg0, arg1);
            throw BadArgumentError(CallType.None, 2);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2) {
            if (target3 != null) return target3(arg0, arg1, arg2);
            throw BadArgumentError(CallType.None, 3);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            if (target4 != null) return target4(arg0, arg1, arg2, arg3);
            throw BadArgumentError(CallType.None, 4);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            if (target5 != null) return target5(arg0, arg1, arg2, arg3, arg4);
            throw BadArgumentError(CallType.None, 5);
        }
        public override object CallInstance(CodeContext context, object arg0) {
            if (target1 != null) return target1(arg0);
            throw BadArgumentError(CallType.ImplicitInstance, 1);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1) {
            if (target2 != null) return target2(arg0, arg1);
            throw BadArgumentError(CallType.ImplicitInstance, 2);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2) {
            if (target3 != null) return target3(arg0, arg1, arg2);
            throw BadArgumentError(CallType.ImplicitInstance, 3);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            if (target4 != null) return target4(arg0, arg1, arg2, arg3);
            throw BadArgumentError(CallType.ImplicitInstance, 4);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            if (target5 != null) return target5(arg0, arg1, arg2, arg3, arg4);
            throw BadArgumentError(CallType.ImplicitInstance, 5);
        }
        public override object Call(CodeContext context, params object[] args) {
            switch (args.Length) {
                case 0: return Call(context);
                case 1: return Call(context, args[0]);
                case 2: return Call(context, args[0], args[1]);
                case 3: return Call(context, args[0], args[1], args[2]);
                case 4: return Call(context, args[0], args[1], args[2], args[3]);
                case 5: return Call(context, args[0], args[1], args[2], args[3], args[4]);
            }
            if (targetN != null) return targetN(args);
            throw BadArgumentError(CallType.None, args.Length);
        }
        public override object CallInstance(CodeContext context, object instance, params object[] args) {
            switch (args.Length) {
                case 0: return CallInstance(context, instance);
                case 1: return CallInstance(context, instance, args[0]);
                case 2: return CallInstance(context, instance, args[0], args[1]);
                case 3: return CallInstance(context, instance, args[0], args[1], args[2]);
                case 4: return CallInstance(context, instance, args[0], args[1], args[2], args[3]);
            }
            if (targetN != null) return targetN(PrependInstance(instance, args));
            throw BadArgumentError(CallType.None, args.Length + 1);
        }
        private Exception BadArgumentError(CallType callType, int nargs) {
            return BadArgumentError(name, minArgs, maxArgs, callType, nargs);
        }
    }
    public class FastCallableWithContextAny : FastCallable {
        public CallTargetWithContext0 target0;
        public CallTargetWithContext1 target1;
        public CallTargetWithContext2 target2;
        public CallTargetWithContext3 target3;
        public CallTargetWithContext4 target4;
        public CallTargetWithContext5 target5;
        public CallTargetWithContextN targetN;
        private string name;
        private int minArgs, maxArgs;
        public FastCallableWithContextAny(string name, int minArgs, int maxArgs) {
            this.name = name;
            this.minArgs = minArgs;
            this.maxArgs = maxArgs;
        }
        public override object Call(CodeContext context) {
            if (target0 != null) return target0(context);
            throw BadArgumentError(CallType.None, 0);
        }
        public override object Call(CodeContext context, object arg0) {
            if (target1 != null) return target1(context, arg0);
            throw BadArgumentError(CallType.None, 1);
        }
        public override object Call(CodeContext context, object arg0, object arg1) {
            if (target2 != null) return target2(context, arg0, arg1);
            throw BadArgumentError(CallType.None, 2);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2) {
            if (target3 != null) return target3(context, arg0, arg1, arg2);
            throw BadArgumentError(CallType.None, 3);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            if (target4 != null) return target4(context, arg0, arg1, arg2, arg3);
            throw BadArgumentError(CallType.None, 4);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            if (target5 != null) return target5(context, arg0, arg1, arg2, arg3, arg4);
            throw BadArgumentError(CallType.None, 5);
        }
        public override object CallInstance(CodeContext context, object arg0) {
            if (target1 != null) return target1(context, arg0);
            throw BadArgumentError(CallType.ImplicitInstance, 1);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1) {
            if (target2 != null) return target2(context, arg0, arg1);
            throw BadArgumentError(CallType.ImplicitInstance, 2);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2) {
            if (target3 != null) return target3(context, arg0, arg1, arg2);
            throw BadArgumentError(CallType.ImplicitInstance, 3);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            if (target4 != null) return target4(context, arg0, arg1, arg2, arg3);
            throw BadArgumentError(CallType.ImplicitInstance, 4);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            if (target5 != null) return target5(context, arg0, arg1, arg2, arg3, arg4);
            throw BadArgumentError(CallType.ImplicitInstance, 5);
        }
        public override object Call(CodeContext context, params object[] args) {
            switch (args.Length) {
                case 0: return Call(context);
                case 1: return Call(context, args[0]);
                case 2: return Call(context, args[0], args[1]);
                case 3: return Call(context, args[0], args[1], args[2]);
                case 4: return Call(context, args[0], args[1], args[2], args[3]);
                case 5: return Call(context, args[0], args[1], args[2], args[3], args[4]);
            }
            if (targetN != null) return targetN(context, args);
            throw BadArgumentError(CallType.None, args.Length);
        }
        public override object CallInstance(CodeContext context, object instance, params object[] args) {
            switch (args.Length) {
                case 0: return CallInstance(context, instance);
                case 1: return CallInstance(context, instance, args[0]);
                case 2: return CallInstance(context, instance, args[0], args[1]);
                case 3: return CallInstance(context, instance, args[0], args[1], args[2]);
                case 4: return CallInstance(context, instance, args[0], args[1], args[2], args[3]);
            }
            if (targetN != null) return targetN(context, PrependInstance(instance, args));
            throw BadArgumentError(CallType.None, args.Length + 1);
        }
        private Exception BadArgumentError(CallType callType, int nargs) {
            return BadArgumentError(name, minArgs, maxArgs, callType, nargs);
        }
    }
    public class FastCallable0 : FastCallable {
        public CallTarget0 target0;
        private string name;
        public FastCallable0(string name, CallTarget0 target) {
            this.target0 = target;
            this.name = name;
        }
        public override object Call(CodeContext context) {
            return target0();
        }
        public override object Call(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.None, 1);
        }
        public override object CallInstance(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.ImplicitInstance, 1);
        }
        public override object Call(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.None, 2);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.ImplicitInstance, 2);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.None, 3);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.ImplicitInstance, 3);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.None, 4);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.ImplicitInstance, 4);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.None, 5);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.ImplicitInstance, 5);
        }
        public override object Call(CodeContext context, params object[] args) {
            if (args.Length == 0) return Call(context);
            throw BadArgumentError(CallType.None, args.Length);
        }
        public override object CallInstance(CodeContext context, object instance, params object[] args) {
            throw BadArgumentError(CallType.ImplicitInstance, args.Length);
        }
        private Exception BadArgumentError(CallType callType, int nargs) {
            return BadArgumentError(name, 0, 0, callType, nargs);
        }
    }
    public class FastCallableWithContext0 : FastCallable {
        public CallTargetWithContext0 target0;
        private string name;
        public FastCallableWithContext0(string name, CallTargetWithContext0 target) {
            this.target0 = target;
            this.name = name;
        }
        public override object Call(CodeContext context) {
            return target0(context);
        }
        public override object Call(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.None, 1);
        }
        public override object CallInstance(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.ImplicitInstance, 1);
        }
        public override object Call(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.None, 2);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.ImplicitInstance, 2);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.None, 3);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.ImplicitInstance, 3);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.None, 4);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.ImplicitInstance, 4);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.None, 5);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.ImplicitInstance, 5);
        }
        public override object Call(CodeContext context, params object[] args) {
            if (args.Length == 0) return Call(context);
            throw BadArgumentError(CallType.None, args.Length);
        }
        public override object CallInstance(CodeContext context, object instance, params object[] args) {
            throw BadArgumentError(CallType.ImplicitInstance, args.Length);
        }
        private Exception BadArgumentError(CallType callType, int nargs) {
            return BadArgumentError(name, 0, 0, callType, nargs);
        }
    }
    public class FastCallable1 : FastCallable {
        public CallTarget1 target1;
        private string name;
        public FastCallable1(string name, CallTarget1 target) {
            this.target1 = target;
            this.name = name;
        }
        public override object Call(CodeContext context) {
            throw BadArgumentError(CallType.None, 0);
        }
        public override object Call(CodeContext context, object arg0) {
            return target1(arg0);
        }
        public override object CallInstance(CodeContext context, object arg0) {
            return target1(arg0);
        }
        public override object Call(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.None, 2);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.ImplicitInstance, 2);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.None, 3);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.ImplicitInstance, 3);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.None, 4);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.ImplicitInstance, 4);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.None, 5);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.ImplicitInstance, 5);
        }
        public override object Call(CodeContext context, params object[] args) {
            if (args.Length == 1) return Call(context, args[0]);
            throw BadArgumentError(CallType.None, args.Length);
        }
        public override object CallInstance(CodeContext context, object instance, params object[] args) {
            if (args.Length == 0) return CallInstance(context, instance);
            throw BadArgumentError(CallType.ImplicitInstance, args.Length);
        }
        private Exception BadArgumentError(CallType callType, int nargs) {
            return BadArgumentError(name, 1, 1, callType, nargs);
        }
    }
    public class FastCallableWithContext1 : FastCallable {
        public CallTargetWithContext1 target1;
        private string name;
        public FastCallableWithContext1(string name, CallTargetWithContext1 target) {
            this.target1 = target;
            this.name = name;
        }
        public override object Call(CodeContext context) {
            throw BadArgumentError(CallType.None, 0);
        }
        public override object Call(CodeContext context, object arg0) {
            return target1(context, arg0);
        }
        public override object CallInstance(CodeContext context, object arg0) {
            return target1(context, arg0);
        }
        public override object Call(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.None, 2);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.ImplicitInstance, 2);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.None, 3);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.ImplicitInstance, 3);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.None, 4);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.ImplicitInstance, 4);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.None, 5);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.ImplicitInstance, 5);
        }
        public override object Call(CodeContext context, params object[] args) {
            if (args.Length == 1) return Call(context, args[0]);
            throw BadArgumentError(CallType.None, args.Length);
        }
        public override object CallInstance(CodeContext context, object instance, params object[] args) {
            if (args.Length == 0) return CallInstance(context, instance);
            throw BadArgumentError(CallType.ImplicitInstance, args.Length);
        }
        private Exception BadArgumentError(CallType callType, int nargs) {
            return BadArgumentError(name, 1, 1, callType, nargs);
        }
    }
    public class FastCallable2 : FastCallable {
        public CallTarget2 target2;
        private string name;
        public FastCallable2(string name, CallTarget2 target) {
            this.target2 = target;
            this.name = name;
        }
        public override object Call(CodeContext context) {
            throw BadArgumentError(CallType.None, 0);
        }
        public override object Call(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.None, 1);
        }
        public override object CallInstance(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.ImplicitInstance, 1);
        }
        public override object Call(CodeContext context, object arg0, object arg1) {
            return target2(arg0, arg1);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1) {
            return target2(arg0, arg1);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.None, 3);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.ImplicitInstance, 3);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.None, 4);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.ImplicitInstance, 4);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.None, 5);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.ImplicitInstance, 5);
        }
        public override object Call(CodeContext context, params object[] args) {
            if (args.Length == 2) return Call(context, args[0], args[1]);
            throw BadArgumentError(CallType.None, args.Length);
        }
        public override object CallInstance(CodeContext context, object instance, params object[] args) {
            if (args.Length == 1) return CallInstance(context, instance, args[0]);
            throw BadArgumentError(CallType.ImplicitInstance, args.Length);
        }
        private Exception BadArgumentError(CallType callType, int nargs) {
            return BadArgumentError(name, 2, 2, callType, nargs);
        }
    }
    public class FastCallableWithContext2 : FastCallable {
        public CallTargetWithContext2 target2;
        private string name;
        public FastCallableWithContext2(string name, CallTargetWithContext2 target) {
            this.target2 = target;
            this.name = name;
        }
        public override object Call(CodeContext context) {
            throw BadArgumentError(CallType.None, 0);
        }
        public override object Call(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.None, 1);
        }
        public override object CallInstance(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.ImplicitInstance, 1);
        }
        public override object Call(CodeContext context, object arg0, object arg1) {
            return target2(context, arg0, arg1);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1) {
            return target2(context, arg0, arg1);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.None, 3);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.ImplicitInstance, 3);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.None, 4);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.ImplicitInstance, 4);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.None, 5);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.ImplicitInstance, 5);
        }
        public override object Call(CodeContext context, params object[] args) {
            if (args.Length == 2) return Call(context, args[0], args[1]);
            throw BadArgumentError(CallType.None, args.Length);
        }
        public override object CallInstance(CodeContext context, object instance, params object[] args) {
            if (args.Length == 1) return CallInstance(context, instance, args[0]);
            throw BadArgumentError(CallType.ImplicitInstance, args.Length);
        }
        private Exception BadArgumentError(CallType callType, int nargs) {
            return BadArgumentError(name, 2, 2, callType, nargs);
        }
    }
    public class FastCallable3 : FastCallable {
        public CallTarget3 target3;
        private string name;
        public FastCallable3(string name, CallTarget3 target) {
            this.target3 = target;
            this.name = name;
        }
        public override object Call(CodeContext context) {
            throw BadArgumentError(CallType.None, 0);
        }
        public override object Call(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.None, 1);
        }
        public override object CallInstance(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.ImplicitInstance, 1);
        }
        public override object Call(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.None, 2);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.ImplicitInstance, 2);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2) {
            return target3(arg0, arg1, arg2);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2) {
            return target3(arg0, arg1, arg2);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.None, 4);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.ImplicitInstance, 4);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.None, 5);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.ImplicitInstance, 5);
        }
        public override object Call(CodeContext context, params object[] args) {
            if (args.Length == 3) return Call(context, args[0], args[1], args[2]);
            throw BadArgumentError(CallType.None, args.Length);
        }
        public override object CallInstance(CodeContext context, object instance, params object[] args) {
            if (args.Length == 2) return CallInstance(context, instance, args[0], args[1]);
            throw BadArgumentError(CallType.ImplicitInstance, args.Length);
        }
        private Exception BadArgumentError(CallType callType, int nargs) {
            return BadArgumentError(name, 3, 3, callType, nargs);
        }
    }
    public class FastCallableWithContext3 : FastCallable {
        public CallTargetWithContext3 target3;
        private string name;
        public FastCallableWithContext3(string name, CallTargetWithContext3 target) {
            this.target3 = target;
            this.name = name;
        }
        public override object Call(CodeContext context) {
            throw BadArgumentError(CallType.None, 0);
        }
        public override object Call(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.None, 1);
        }
        public override object CallInstance(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.ImplicitInstance, 1);
        }
        public override object Call(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.None, 2);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.ImplicitInstance, 2);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2) {
            return target3(context, arg0, arg1, arg2);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2) {
            return target3(context, arg0, arg1, arg2);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.None, 4);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.ImplicitInstance, 4);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.None, 5);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.ImplicitInstance, 5);
        }
        public override object Call(CodeContext context, params object[] args) {
            if (args.Length == 3) return Call(context, args[0], args[1], args[2]);
            throw BadArgumentError(CallType.None, args.Length);
        }
        public override object CallInstance(CodeContext context, object instance, params object[] args) {
            if (args.Length == 2) return CallInstance(context, instance, args[0], args[1]);
            throw BadArgumentError(CallType.ImplicitInstance, args.Length);
        }
        private Exception BadArgumentError(CallType callType, int nargs) {
            return BadArgumentError(name, 3, 3, callType, nargs);
        }
    }
    public class FastCallable4 : FastCallable {
        public CallTarget4 target4;
        private string name;
        public FastCallable4(string name, CallTarget4 target) {
            this.target4 = target;
            this.name = name;
        }
        public override object Call(CodeContext context) {
            throw BadArgumentError(CallType.None, 0);
        }
        public override object Call(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.None, 1);
        }
        public override object CallInstance(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.ImplicitInstance, 1);
        }
        public override object Call(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.None, 2);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.ImplicitInstance, 2);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.None, 3);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.ImplicitInstance, 3);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            return target4(arg0, arg1, arg2, arg3);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            return target4(arg0, arg1, arg2, arg3);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.None, 5);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.ImplicitInstance, 5);
        }
        public override object Call(CodeContext context, params object[] args) {
            if (args.Length == 4) return Call(context, args[0], args[1], args[2], args[3]);
            throw BadArgumentError(CallType.None, args.Length);
        }
        public override object CallInstance(CodeContext context, object instance, params object[] args) {
            if (args.Length == 3) return CallInstance(context, instance, args[0], args[1], args[2]);
            throw BadArgumentError(CallType.ImplicitInstance, args.Length);
        }
        private Exception BadArgumentError(CallType callType, int nargs) {
            return BadArgumentError(name, 4, 4, callType, nargs);
        }
    }
    public class FastCallableWithContext4 : FastCallable {
        public CallTargetWithContext4 target4;
        private string name;
        public FastCallableWithContext4(string name, CallTargetWithContext4 target) {
            this.target4 = target;
            this.name = name;
        }
        public override object Call(CodeContext context) {
            throw BadArgumentError(CallType.None, 0);
        }
        public override object Call(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.None, 1);
        }
        public override object CallInstance(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.ImplicitInstance, 1);
        }
        public override object Call(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.None, 2);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.ImplicitInstance, 2);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.None, 3);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.ImplicitInstance, 3);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            return target4(context, arg0, arg1, arg2, arg3);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            return target4(context, arg0, arg1, arg2, arg3);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.None, 5);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            throw BadArgumentError(CallType.ImplicitInstance, 5);
        }
        public override object Call(CodeContext context, params object[] args) {
            if (args.Length == 4) return Call(context, args[0], args[1], args[2], args[3]);
            throw BadArgumentError(CallType.None, args.Length);
        }
        public override object CallInstance(CodeContext context, object instance, params object[] args) {
            if (args.Length == 3) return CallInstance(context, instance, args[0], args[1], args[2]);
            throw BadArgumentError(CallType.ImplicitInstance, args.Length);
        }
        private Exception BadArgumentError(CallType callType, int nargs) {
            return BadArgumentError(name, 4, 4, callType, nargs);
        }
    }
    public class FastCallable5 : FastCallable {
        public CallTarget5 target5;
        private string name;
        public FastCallable5(string name, CallTarget5 target) {
            this.target5 = target;
            this.name = name;
        }
        public override object Call(CodeContext context) {
            throw BadArgumentError(CallType.None, 0);
        }
        public override object Call(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.None, 1);
        }
        public override object CallInstance(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.ImplicitInstance, 1);
        }
        public override object Call(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.None, 2);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.ImplicitInstance, 2);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.None, 3);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.ImplicitInstance, 3);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.None, 4);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.ImplicitInstance, 4);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            return target5(arg0, arg1, arg2, arg3, arg4);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            return target5(arg0, arg1, arg2, arg3, arg4);
        }
        public override object Call(CodeContext context, params object[] args) {
            if (args.Length == 5) return Call(context, args[0], args[1], args[2], args[3], args[4]);
            throw BadArgumentError(CallType.None, args.Length);
        }
        public override object CallInstance(CodeContext context, object instance, params object[] args) {
            if (args.Length == 4) return CallInstance(context, instance, args[0], args[1], args[2], args[3]);
            throw BadArgumentError(CallType.ImplicitInstance, args.Length);
        }
        private Exception BadArgumentError(CallType callType, int nargs) {
            return BadArgumentError(name, 5, 5, callType, nargs);
        }
    }
    public class FastCallableWithContext5 : FastCallable {
        public CallTargetWithContext5 target5;
        private string name;
        public FastCallableWithContext5(string name, CallTargetWithContext5 target) {
            this.target5 = target;
            this.name = name;
        }
        public override object Call(CodeContext context) {
            throw BadArgumentError(CallType.None, 0);
        }
        public override object Call(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.None, 1);
        }
        public override object CallInstance(CodeContext context, object arg0) {
            throw BadArgumentError(CallType.ImplicitInstance, 1);
        }
        public override object Call(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.None, 2);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1) {
            throw BadArgumentError(CallType.ImplicitInstance, 2);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.None, 3);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2) {
            throw BadArgumentError(CallType.ImplicitInstance, 3);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.None, 4);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3) {
            throw BadArgumentError(CallType.ImplicitInstance, 4);
        }
        public override object Call(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            return target5(context, arg0, arg1, arg2, arg3, arg4);
        }
        public override object CallInstance(CodeContext context, object arg0, object arg1, object arg2, object arg3, object arg4) {
            return target5(context, arg0, arg1, arg2, arg3, arg4);
        }
        public override object Call(CodeContext context, params object[] args) {
            if (args.Length == 5) return Call(context, args[0], args[1], args[2], args[3], args[4]);
            throw BadArgumentError(CallType.None, args.Length);
        }
        public override object CallInstance(CodeContext context, object instance, params object[] args) {
            if (args.Length == 4) return CallInstance(context, instance, args[0], args[1], args[2], args[3]);
            throw BadArgumentError(CallType.ImplicitInstance, args.Length);
        }
        private Exception BadArgumentError(CallType callType, int nargs) {
            return BadArgumentError(name, 5, 5, callType, nargs);
        }
    }

    // *** END GENERATED CODE ***

    #endregion

}
