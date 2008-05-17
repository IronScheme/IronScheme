
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
using System.Reflection;
using System.Diagnostics;
using System.Collections.Generic;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Actions {
    public static partial class DynamicSiteHelpers {
        private static Dictionary<Type, CreateSite> _siteCtors = new Dictionary<Type, CreateSite>();
        private static Dictionary<Type, CreateFastSite> _fastSiteCtors = new Dictionary<Type, CreateFastSite>();

        /// <summary>
        /// Checks to see if a given delegate is used as a "fast" target.  A fast target
        /// does not include a CodeContext in its parameter list but is able to retrieve
        /// such a delegate from it's DynamicSite if needed.
        /// </summary>
        public static bool IsFastTarget(Type type) {
            return type.Name.StartsWith("Fast") || type.Name.StartsWith("BigFast");
        }

        public static bool IsBigTarget(Type type) {
            return type.Name.StartsWith("Big");
        }

        private delegate object CreateSite(DynamicAction action);
        private delegate object CreateFastSite(CodeContext context, DynamicAction action);

        public static DynamicSite MakeSite(DynamicAction action, Type siteType) {
            CreateSite ctor;
            lock (_siteCtors) {
                if (!_siteCtors.TryGetValue(siteType, out ctor)) {
                    _siteCtors[siteType] = ctor = (CreateSite)Delegate.CreateDelegate(typeof(CreateSite), siteType.GetMethod("Create"));
                }
            }

            return (DynamicSite)ctor(action);
        }

        public static FastDynamicSite MakeFastSite(CodeContext context, DynamicAction action, Type siteType) {
            Debug.Assert(typeof(FastDynamicSite).IsAssignableFrom(siteType.BaseType));
            CreateFastSite ctor;
            lock (_fastSiteCtors) {
                if (!_fastSiteCtors.TryGetValue(siteType, out ctor)) {
                    _fastSiteCtors[siteType] = ctor = (CreateFastSite)Delegate.CreateDelegate(typeof(CreateFastSite), siteType.GetMethod("Create"));
                }
            }

            return (FastDynamicSite)ctor(context, action);
        }

        //
        // Initialization of dynamic sites stored in static fields 
        //

        public static void InitializeFields(CodeContext context, Type type) {
            InitializeFields(context, type, false);
        }

        public static void InitializeFields(CodeContext context, Type type, bool reusable) {
            if (type == null) return;

            const string slotStorageName = "#SlotStorage";
            foreach (FieldInfo fi in type.GetFields()) {
                if (fi.Name.StartsWith(slotStorageName)) {
                    object value;
                    if (reusable) {
                        value = ConstantPool.GetConstantDataReusable(Int32.Parse(fi.Name.Substring(slotStorageName.Length)));
                    } else {
                        value = ConstantPool.GetConstantData(Int32.Parse(fi.Name.Substring(slotStorageName.Length)));
                    }
                    Debug.Assert(value != null);
                    FastDynamicSite fds = value as FastDynamicSite;
                    if (fds != null) {
                        fds.Context = context;
                    }
                    fi.SetValue(null, value);
                }                
            }
        }

        
        private static Type MakeBigDynamicSiteType(params Type[] types) {
            if (types.Length < 2) throw new ArgumentException("must have at least 2 types");

            Type tupleType = Tuple.MakeTupleType(ArrayUtils.RemoveLast(types));

            return typeof(BigDynamicSite<,>).MakeGenericType(tupleType, types[types.Length - 1]);            
        }


        public static Type MakeBigFastDynamicSiteType(params Type[] types) {
            if (types.Length < 2) throw new ArgumentException("must have at least 2 types");

            Type tupleType = Tuple.MakeTupleType(ArrayUtils.RemoveLast(types));

            return typeof(BigFastDynamicSite<,>).MakeGenericType(tupleType, types[types.Length - 1]);
        }

        private class BigUninitializedTargetHelper<T0, Tret> where T0 : Tuple {
            public Tret BigInvoke(BigDynamicSite<T0, Tret> site, CodeContext context, T0 arg0) {
                return site.UpdateBindingAndInvoke(context, arg0);
            }

            public Tret BigFastInvoke(BigFastDynamicSite<T0, Tret> site, T0 arg0) {
                return site.UpdateBindingAndInvoke(arg0);
            }
        }

        public static Delegate MakeUninitializedTarget(Type targetType) {
            Type[] args = new Type[DynamicSiteHelpers.MaximumArity];
            Type[] existing = targetType.GetGenericArguments();
            for (int i = 0; i < existing.Length - 1; i++) args[i] = existing[i];
            for (int i = existing.Length - 1; i < args.Length - 1; i++) args[i] = typeof(object);
            args[args.Length - 1] = existing[existing.Length - 1];
            Type dType = typeof(UninitializedTargetHelper<,,,,,,>).MakeGenericType(args);
            return Delegate.CreateDelegate(targetType, Activator.CreateInstance(dType), "Invoke" + (existing.Length - 1).ToString());
        }

        public static Delegate MakeUninitializedFastTarget(Type targetType) {
            Type[] args = new Type[DynamicSiteHelpers.MaximumArity];
            Type[] existing = targetType.GetGenericArguments();
            for (int i = 0; i < existing.Length - 1; i++) args[i] = existing[i];
            for (int i = existing.Length - 1; i < args.Length - 1; i++) args[i] = typeof(object);
            args[args.Length - 1] = existing[existing.Length - 1];
            Type dType = typeof(UninitializedTargetHelper<,,,,,,>).MakeGenericType(args);
            return Delegate.CreateDelegate(targetType, Activator.CreateInstance(dType), "FastInvoke" + (existing.Length - 1).ToString());
        }

        public static Delegate MakeUninitializedBigTarget(Type targetType) {
            Type dType = typeof(BigUninitializedTargetHelper<,>).MakeGenericType(targetType.GetGenericArguments());
            return Delegate.CreateDelegate(targetType, Activator.CreateInstance(dType), "BigInvoke");
        }

        public static Delegate MakeUninitializedBigFastTarget(Type targetType) {
            Type dType = typeof(BigUninitializedTargetHelper<,>).MakeGenericType(targetType.GetGenericArguments());
            return Delegate.CreateDelegate(targetType, Activator.CreateInstance(dType), "BigFastInvoke");
        }

        internal static CodeContext GetEvaluationContext<T>(CodeContext context, ref object []args) {
            context = new CodeContext(new Scope(context.Scope, null), context.LanguageContext, context.ModuleContext);
            if (DynamicSiteHelpers.IsBigTarget(typeof(T))) {
                args = new object[] { Tuple.MakeTuple(typeof(T).GetGenericArguments()[0], args) };
            }
            return context;
        }

        internal static void UpdateSite<T>(CodeContext callerContext, object site, ref T target, ref RuleSet<T> rules, StandardRule<T> rule) {
            lock (site) {
                bool monomorphic = rules.HasMonomorphicTarget(target);

                rules = rules.AddRule(rule);
                if (monomorphic || rules == EmptyRuleSet<T>.FixedInstance) {
                    target = rule.MonomorphicRuleSet.GetOrMakeTarget(callerContext);
                } else {
                    target = rules.GetOrMakeTarget(callerContext);
                }
            }
        }
    }
}

#endif	
