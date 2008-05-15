/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. A 
 * copy of the license can be found in the License.html file at the root of this distribution. If 
 * you cannot locate the  Microsoft Public License, please send an email to 
 * ironpy@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 *
 *
 * ***************************************************************************/

using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.Diagnostics;

namespace Microsoft.Scripting.Utils {
    #region Generated Reflected Caller

    // *** BEGIN GENERATED CODE ***

    public partial class ReflectedCaller {
        private const int MaxHelpers = 7;
        private const int MaxArgs = 3;

        public virtual object InvokeInstance(object instance, params object[] args) {
            switch(args.Length) {
                case 0: return Invoke(instance);
                case 1: return Invoke(instance, args[0]);
                case 2: return Invoke(instance, args[0], args[1]);
                case 3: return Invoke(instance, args[0], args[1], args[2]);
                case 4: return Invoke(instance, args[0], args[1], args[2], args[3]);
                case 5: return Invoke(instance, args[0], args[1], args[2], args[3], args[4]);
                default: throw new InvalidOperationException();
            }
        }

        public virtual object Invoke(params object[] args) {
            switch(args.Length) {
                case 0: return Invoke();
                case 1: return Invoke(args[0]);
                case 2: return Invoke(args[0], args[1]);
                case 3: return Invoke(args[0], args[1], args[2]);
                case 4: return Invoke(args[0], args[1], args[2], args[3]);
                case 5: return Invoke(args[0], args[1], args[2], args[3], args[4]);
                case 6: return Invoke(args[0], args[1], args[2], args[3], args[4], args[5]);
                default: throw new InvalidOperationException();
            }
        }

        public virtual object Invoke() { throw new InvalidOperationException(); }
        public virtual object Invoke(object arg0) { throw new InvalidOperationException(); }
        public virtual object Invoke(object arg0, object arg1) { throw new InvalidOperationException(); }
        public virtual object Invoke(object arg0, object arg1, object arg2) { throw new InvalidOperationException(); }
        public virtual object Invoke(object arg0, object arg1, object arg2, object arg3) { throw new InvalidOperationException(); }
        public virtual object Invoke(object arg0, object arg1, object arg2, object arg3, object arg4) { throw new InvalidOperationException(); }
        public virtual object Invoke(object arg0, object arg1, object arg2, object arg3, object arg4, object arg5) { throw new InvalidOperationException(); }

        /// <summary>
        /// Fast creation works if we have a known primitive types for the entire
        /// method siganture.  If we have any non-primitive types then FastCreate
        /// falls back to SlowCreate which works for all types.
        /// 
        /// Fast creation is fast because it avoids using reflection (MakeGenericType
        /// and Activator.CreateInstance) to create the types.  It does this through
        /// calling a series of generic methods picking up each strong type of the
        /// signature along the way.  When it runs out of types it news up the 
        /// appropriate ReflectedCaller with the strong-types that have been built up.
        /// 
        /// One relaxation is that for return types which are non-primitive types
        /// we can fallback to object due to relaxed delegates.
        /// </summary>
        private static ReflectedCaller FastCreate(MethodInfo target, ParameterInfo[] pi) {
            Type t = TryGetParameterOrReturnType(target, pi, 0);
            if (t == null) {
                return new ActionHelper(target);
            }

            if (t.IsEnum) return SlowCreate(target, pi);
            switch (Type.GetTypeCode(t)) {
                case TypeCode.Object: {
                    if (t != typeof(object) && (IndexIsNotReturnType(0, target, pi) || t.IsValueType)) {
                        // if we're on the return type relaxed delegates makes it ok to use object
                        goto default;
                    }
                    return FastCreate<Object>(target, pi);
                }
                case TypeCode.Int16: return FastCreate<Int16>(target, pi);
                case TypeCode.Int32: return FastCreate<Int32>(target, pi);
                case TypeCode.Int64: return FastCreate<Int64>(target, pi);
                case TypeCode.Boolean: return FastCreate<Boolean>(target, pi);
                case TypeCode.Char: return FastCreate<Char>(target, pi);
                case TypeCode.Byte: return FastCreate<Byte>(target, pi);
                case TypeCode.Decimal: return FastCreate<Decimal>(target, pi);
                case TypeCode.DateTime: return FastCreate<DateTime>(target, pi);
                case TypeCode.Double: return FastCreate<Double>(target, pi);
                case TypeCode.Single: return FastCreate<Single>(target, pi);
                case TypeCode.UInt16: return FastCreate<UInt16>(target, pi);
                case TypeCode.UInt32: return FastCreate<UInt32>(target, pi);
                case TypeCode.UInt64: return FastCreate<UInt64>(target, pi);
                case TypeCode.String: return FastCreate<String>(target, pi);
                case TypeCode.SByte: return FastCreate<SByte>(target, pi);
                default: return SlowCreate(target, pi);
            }
        }

        private static ReflectedCaller FastCreate<T0>(MethodInfo target, ParameterInfo[] pi) {
            Type t = TryGetParameterOrReturnType(target, pi, 1);
            if (t == null) {
                if (target.ReturnType == typeof(void)) {
                    return new ActionHelper<T0>(target);
                }
                return new InvokeHelper<T0>(target);
            }

            if (t.IsEnum) return SlowCreate(target, pi);
            switch (Type.GetTypeCode(t)) {
                case TypeCode.Object: {
                    if (t != typeof(object) && (IndexIsNotReturnType(1, target, pi) || t.IsValueType)) {
                        // if we're on the return type relaxed delegates makes it ok to use object
                        goto default;
                    }
                    return FastCreate<T0, Object>(target, pi);
                }
                case TypeCode.Int16: return FastCreate<T0, Int16>(target, pi);
                case TypeCode.Int32: return FastCreate<T0, Int32>(target, pi);
                case TypeCode.Int64: return FastCreate<T0, Int64>(target, pi);
                case TypeCode.Boolean: return FastCreate<T0, Boolean>(target, pi);
                case TypeCode.Char: return FastCreate<T0, Char>(target, pi);
                case TypeCode.Byte: return FastCreate<T0, Byte>(target, pi);
                case TypeCode.Decimal: return FastCreate<T0, Decimal>(target, pi);
                case TypeCode.DateTime: return FastCreate<T0, DateTime>(target, pi);
                case TypeCode.Double: return FastCreate<T0, Double>(target, pi);
                case TypeCode.Single: return FastCreate<T0, Single>(target, pi);
                case TypeCode.UInt16: return FastCreate<T0, UInt16>(target, pi);
                case TypeCode.UInt32: return FastCreate<T0, UInt32>(target, pi);
                case TypeCode.UInt64: return FastCreate<T0, UInt64>(target, pi);
                case TypeCode.String: return FastCreate<T0, String>(target, pi);
                case TypeCode.SByte: return FastCreate<T0, SByte>(target, pi);
                default: return SlowCreate(target, pi);
            }
        }

        private static ReflectedCaller FastCreate<T0, T1>(MethodInfo target, ParameterInfo[] pi) {
            Type t = TryGetParameterOrReturnType(target, pi, 2);
            if (t == null) {
                if (target.ReturnType == typeof(void)) {
                    return new ActionHelper<T0, T1>(target);
                }
                return new InvokeHelper<T0, T1>(target);
            }

            if (t.IsEnum) return SlowCreate(target, pi);
            switch (Type.GetTypeCode(t)) {
                case TypeCode.Object: {
                    Debug.Assert(pi.Length == 2);
                    if (t.IsValueType) goto default;

                    return new InvokeHelper<T0, T1, Object>(target);
                }
                case TypeCode.Int16: return new InvokeHelper<T0, T1, Int16>(target);
                case TypeCode.Int32: return new InvokeHelper<T0, T1, Int32>(target);
                case TypeCode.Int64: return new InvokeHelper<T0, T1, Int64>(target);
                case TypeCode.Boolean: return new InvokeHelper<T0, T1, Boolean>(target);
                case TypeCode.Char: return new InvokeHelper<T0, T1, Char>(target);
                case TypeCode.Byte: return new InvokeHelper<T0, T1, Byte>(target);
                case TypeCode.Decimal: return new InvokeHelper<T0, T1, Decimal>(target);
                case TypeCode.DateTime: return new InvokeHelper<T0, T1, DateTime>(target);
                case TypeCode.Double: return new InvokeHelper<T0, T1, Double>(target);
                case TypeCode.Single: return new InvokeHelper<T0, T1, Single>(target);
                case TypeCode.UInt16: return new InvokeHelper<T0, T1, UInt16>(target);
                case TypeCode.UInt32: return new InvokeHelper<T0, T1, UInt32>(target);
                case TypeCode.UInt64: return new InvokeHelper<T0, T1, UInt64>(target);
                case TypeCode.String: return new InvokeHelper<T0, T1, String>(target);
                case TypeCode.SByte: return new InvokeHelper<T0, T1, SByte>(target);
                default: return SlowCreate(target, pi);
            }
        }

        private static Type GetHelperType(MethodInfo info, Type[] arrTypes) {
            Type t;
            if (info.ReturnType == typeof(void)) {
                switch (arrTypes.Length) {
                    case 0: t = typeof(ActionHelper); break;
                    case 1: t = typeof(ActionHelper<>).MakeGenericType(arrTypes); break;
                    case 2: t = typeof(ActionHelper<,>).MakeGenericType(arrTypes); break;
                    case 3: t = typeof(ActionHelper<,,>).MakeGenericType(arrTypes); break;
                    case 4: t = typeof(ActionHelper<,,,>).MakeGenericType(arrTypes); break;
                    case 5: t = typeof(ActionHelper<,,,,>).MakeGenericType(arrTypes); break;
                    case 6: t = typeof(ActionHelper<,,,,,>).MakeGenericType(arrTypes); break;
                    default: throw new InvalidOperationException();
                }
            } else {
                switch (arrTypes.Length) {
                    case 1: t = typeof(InvokeHelper<>).MakeGenericType(arrTypes); break;
                    case 2: t = typeof(InvokeHelper<,>).MakeGenericType(arrTypes); break;
                    case 3: t = typeof(InvokeHelper<,,>).MakeGenericType(arrTypes); break;
                    case 4: t = typeof(InvokeHelper<,,,>).MakeGenericType(arrTypes); break;
                    case 5: t = typeof(InvokeHelper<,,,,>).MakeGenericType(arrTypes); break;
                    case 6: t = typeof(InvokeHelper<,,,,,>).MakeGenericType(arrTypes); break;
                    case 7: t = typeof(InvokeHelper<,,,,,,>).MakeGenericType(arrTypes); break;
                    default: throw new InvalidOperationException();
                }
            }
            return t;
        }
    }

    sealed class ActionHelper : ReflectedCaller {
        private Action _target;

        public ActionHelper(MethodInfo target) {
            _target = (Action)Delegate.CreateDelegate(typeof(Action), target);
        }

        public override object Invoke() {
            _target();
            return null;
        }
    }

    sealed class ActionHelper<T0> : ReflectedCaller {
        private Action<T0> _target;

        public ActionHelper(MethodInfo target) {
            _target = (Action<T0>)Delegate.CreateDelegate(typeof(Action<T0>), target);
        }

        public override object Invoke(object arg0) {
            _target(arg0 != null ? (T0)arg0 : default(T0));
            return null;
        }
    }

    sealed class ActionHelper<T0, T1> : ReflectedCaller {
        private Action<T0, T1> _target;

        public ActionHelper(MethodInfo target) {
            _target = (Action<T0, T1>)Delegate.CreateDelegate(typeof(Action<T0, T1>), target);
        }

        public override object Invoke(object arg0, object arg1) {
            _target(arg0 != null ? (T0)arg0 : default(T0), arg1 != null ? (T1)arg1 : default(T1));
            return null;
        }
    }

    sealed class ActionHelper<T0, T1, T2> : ReflectedCaller {
        private Action<T0, T1, T2> _target;

        public ActionHelper(MethodInfo target) {
            _target = (Action<T0, T1, T2>)Delegate.CreateDelegate(typeof(Action<T0, T1, T2>), target);
        }

        public override object Invoke(object arg0, object arg1, object arg2) {
            _target(arg0 != null ? (T0)arg0 : default(T0), arg1 != null ? (T1)arg1 : default(T1), arg2 != null ? (T2)arg2 : default(T2));
            return null;
        }
    }

    sealed class ActionHelper<T0, T1, T2, T3> : ReflectedCaller {
        private Action<T0, T1, T2, T3> _target;

        public ActionHelper(MethodInfo target) {
            _target = (Action<T0, T1, T2, T3>)Delegate.CreateDelegate(typeof(Action<T0, T1, T2, T3>), target);
        }

        public override object Invoke(object arg0, object arg1, object arg2, object arg3) {
            _target(arg0 != null ? (T0)arg0 : default(T0), arg1 != null ? (T1)arg1 : default(T1), arg2 != null ? (T2)arg2 : default(T2), arg3 != null ? (T3)arg3 : default(T3));
            return null;
        }
    }

    sealed class ActionHelper<T0, T1, T2, T3, T4> : ReflectedCaller {
        private Action<T0, T1, T2, T3, T4> _target;

        public ActionHelper(MethodInfo target) {
            _target = (Action<T0, T1, T2, T3, T4>)Delegate.CreateDelegate(typeof(Action<T0, T1, T2, T3, T4>), target);
        }

        public override object Invoke(object arg0, object arg1, object arg2, object arg3, object arg4) {
            _target(arg0 != null ? (T0)arg0 : default(T0), arg1 != null ? (T1)arg1 : default(T1), arg2 != null ? (T2)arg2 : default(T2), arg3 != null ? (T3)arg3 : default(T3), arg4 != null ? (T4)arg4 : default(T4));
            return null;
        }
    }

    sealed class ActionHelper<T0, T1, T2, T3, T4, T5> : ReflectedCaller {
        private Action<T0, T1, T2, T3, T4, T5> _target;

        public ActionHelper(MethodInfo target) {
            _target = (Action<T0, T1, T2, T3, T4, T5>)Delegate.CreateDelegate(typeof(Action<T0, T1, T2, T3, T4, T5>), target);
        }

        public override object Invoke(object arg0, object arg1, object arg2, object arg3, object arg4, object arg5) {
            _target(arg0 != null ? (T0)arg0 : default(T0), arg1 != null ? (T1)arg1 : default(T1), arg2 != null ? (T2)arg2 : default(T2), arg3 != null ? (T3)arg3 : default(T3), arg4 != null ? (T4)arg4 : default(T4), arg5 != null ? (T5)arg5 : default(T5));
            return null;
        }
    }

    sealed class InvokeHelper<TRet> : ReflectedCaller {
        private Function<TRet> _target;

        public InvokeHelper(MethodInfo target) {
            _target = (Function<TRet>)Delegate.CreateDelegate(typeof(Function<TRet>), target);
        }

        public override object Invoke() {
            return _target();
        }
    }

    sealed class InvokeHelper<T0, TRet> : ReflectedCaller {
        private Function<T0, TRet> _target;

        public InvokeHelper(MethodInfo target) {
            _target = (Function<T0, TRet>)Delegate.CreateDelegate(typeof(Function<T0, TRet>), target);
        }

        public override object Invoke(object arg0) {
            return _target(arg0 != null ? (T0)arg0 : default(T0));
        }
    }

    sealed class InvokeHelper<T0, T1, TRet> : ReflectedCaller {
        private Function<T0, T1, TRet> _target;

        public InvokeHelper(MethodInfo target) {
            _target = (Function<T0, T1, TRet>)Delegate.CreateDelegate(typeof(Function<T0, T1, TRet>), target);
        }

        public override object Invoke(object arg0, object arg1) {
            return _target(arg0 != null ? (T0)arg0 : default(T0), arg1 != null ? (T1)arg1 : default(T1));
        }
    }

    sealed class InvokeHelper<T0, T1, T2, TRet> : ReflectedCaller {
        private Function<T0, T1, T2, TRet> _target;

        public InvokeHelper(MethodInfo target) {
            _target = (Function<T0, T1, T2, TRet>)Delegate.CreateDelegate(typeof(Function<T0, T1, T2, TRet>), target);
        }

        public override object Invoke(object arg0, object arg1, object arg2) {
            return _target(arg0 != null ? (T0)arg0 : default(T0), arg1 != null ? (T1)arg1 : default(T1), arg2 != null ? (T2)arg2 : default(T2));
        }
    }

    sealed class InvokeHelper<T0, T1, T2, T3, TRet> : ReflectedCaller {
        private Function<T0, T1, T2, T3, TRet> _target;

        public InvokeHelper(MethodInfo target) {
            _target = (Function<T0, T1, T2, T3, TRet>)Delegate.CreateDelegate(typeof(Function<T0, T1, T2, T3, TRet>), target);
        }

        public override object Invoke(object arg0, object arg1, object arg2, object arg3) {
            return _target(arg0 != null ? (T0)arg0 : default(T0), arg1 != null ? (T1)arg1 : default(T1), arg2 != null ? (T2)arg2 : default(T2), arg3 != null ? (T3)arg3 : default(T3));
        }
    }

    sealed class InvokeHelper<T0, T1, T2, T3, T4, TRet> : ReflectedCaller {
        private Function<T0, T1, T2, T3, T4, TRet> _target;

        public InvokeHelper(MethodInfo target) {
            _target = (Function<T0, T1, T2, T3, T4, TRet>)Delegate.CreateDelegate(typeof(Function<T0, T1, T2, T3, T4, TRet>), target);
        }

        public override object Invoke(object arg0, object arg1, object arg2, object arg3, object arg4) {
            return _target(arg0 != null ? (T0)arg0 : default(T0), arg1 != null ? (T1)arg1 : default(T1), arg2 != null ? (T2)arg2 : default(T2), arg3 != null ? (T3)arg3 : default(T3), arg4 != null ? (T4)arg4 : default(T4));
        }
    }

    sealed class InvokeHelper<T0, T1, T2, T3, T4, T5, TRet> : ReflectedCaller {
        private Function<T0, T1, T2, T3, T4, T5, TRet> _target;

        public InvokeHelper(MethodInfo target) {
            _target = (Function<T0, T1, T2, T3, T4, T5, TRet>)Delegate.CreateDelegate(typeof(Function<T0, T1, T2, T3, T4, T5, TRet>), target);
        }

        public override object Invoke(object arg0, object arg1, object arg2, object arg3, object arg4, object arg5) {
            return _target(arg0 != null ? (T0)arg0 : default(T0), arg1 != null ? (T1)arg1 : default(T1), arg2 != null ? (T2)arg2 : default(T2), arg3 != null ? (T3)arg3 : default(T3), arg4 != null ? (T4)arg4 : default(T4), arg5 != null ? (T5)arg5 : default(T5));
        }
    }

    sealed partial class SlowReflectedCaller : ReflectedCaller {
        public override object Invoke() {
            return InvokeWorker();
        }
        public override object Invoke(object arg0) {
            return InvokeWorker(arg0);
        }
        public override object Invoke(object arg0, object arg1) {
            return InvokeWorker(arg0, arg1);
        }
    }

    // *** END GENERATED CODE ***

    #endregion
}
