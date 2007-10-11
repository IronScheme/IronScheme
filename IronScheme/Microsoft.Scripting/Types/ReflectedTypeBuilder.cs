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
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;
using System.Threading;
using System.Runtime.CompilerServices;

using Microsoft.Scripting.Math;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Actions;

namespace Microsoft.Scripting.Types {
    public delegate DynamicType TypeBuilderDelegate(Type t);

    /// <summary>
    /// ReflectedTypeBuilder creates DynamicType's for staticly defined .NET types.  These include:
    /// 1. Built-in types supported intrinsically by the engine (and implemented as CLI types in the engine)
    /// 2. Non-Python types imported from CLI assemblies
    /// 
    /// ReflectedType's are read-only once they are created as the user is not allowed to assign 
    /// properties to them.  Languages can still get a DynamicTypeBUilder for the dynamic types to
    /// extend them with language specific features.
    /// </summary>
    public partial class ReflectedTypeBuilder : CoreReflectedTypeBuilder {
        class Members {
            public object Value;
            public Dictionary<ContextId, object> CtxValues;
        }

        private Dictionary<SymbolId, Members> _dict;
        private Type _type;
        private ExtensionNameTransformer _transform;
        private Dictionary<OperatorMapping, OperatorInfo> _operImpl;
        private static List<TypeBuilderDelegate> _altBuilders;

        /// <summary>Table from the .NET operator names to OperatorMapping which includes operator and arity info</summary>
        internal static Dictionary<string, OperatorMapping> _operatorTable = InitializeOperatorTable();

        public ReflectedTypeBuilder() {
        }

        public override string ToString() {
            return "ReflectedTypeBuilder for " + _type;
        }

        /// <summary>
        /// Builds a ReflectedType
        /// </summary>
        public static DynamicType Build(Type t) {
            PerfTrack.NoteEvent(PerfTrack.Categories.ReflectedTypes, t);

#if !SILVERLIGHT // COM
            if (_altBuilders != null) {
                lock (_altBuilders) {
                    foreach (TypeBuilderDelegate tb in _altBuilders) {
                        DynamicType res = tb(t);
                        if (res != null) {
                            return res;
                        }
                    }
                }
            }
#endif

            ReflectedTypeBuilder rtb = new ReflectedTypeBuilder();
            return rtb.DoBuild(t);
        }

        public static void RegisterAlternateBuilder(TypeBuilderDelegate builder) {
            if (_altBuilders == null) {
                Interlocked.CompareExchange<List<TypeBuilderDelegate>>(ref _altBuilders, new List<TypeBuilderDelegate>(), null);
            }

            lock (_altBuilders) {
                _altBuilders.Add(builder);
            }
        }

        protected DynamicType DoBuild(Type t) {
            object[] attrs = t.GetCustomAttributes(typeof(ScriptTypeAttribute), false);

            if (attrs != null && attrs.Length > 0) {
                Type impersonate = null;
                string name = t.Name;
                ScriptTypeAttribute pta = attrs[0] as ScriptTypeAttribute;
                name = pta.Name;
                impersonate = pta.ImpersonateType;

                return DoBuild(pta.Name, t, null, pta.ImpersonateType, ((ScriptTypeAttribute)attrs[0]).Context);
            } else { 
                Builder = new DynamicTypeBuilder(t.Name, t);
            }

            return DoBuildWorker(t);
        }

        protected DynamicType DoBuild(Type t, Type extensionType) {
            object[] attrs = t.GetCustomAttributes(typeof(ScriptTypeAttribute), false);
            string name = t.Name;   
            Type impersonate = null;
            ContextId ctx = ContextId.Empty;
            if (attrs != null && attrs.Length > 0) {
                ScriptTypeAttribute pta = attrs[0] as ScriptTypeAttribute;
                name = pta.Name;
                impersonate = pta.ImpersonateType;
                ctx = pta.Context;
            }

            return DoBuild(name, t, extensionType, impersonate, ctx);
        }

        protected DynamicType DoBuild(string name, Type t, Type extensionType, ContextId context) {
            return DoBuild(name, t, extensionType, null, context);
        }

        protected DynamicType DoBuild(string name, Type t, Type extensionType, Type impersonationType, ContextId context) {
            Builder = new DynamicTypeBuilder(name, t, extensionType);
            Builder.SetTypeContext(context);
            if (impersonationType != null) {
                Builder.SetImpersonationType(impersonationType);
            }

            return DoBuildWorker(t);
        }

        private DynamicType DoBuildWorker(Type t) {
            _type = t;
            _dict = new Dictionary<SymbolId, Members>();

            Builder.AddInitializer(InitializeOne);
            Builder.IsSystemType = true;

            return Builder.UnfinishedType;
        }

        protected void InitializeOne(DynamicMixinBuilder tb) {
            Debug.Assert(tb == Builder);

            bool isOps = ExtensionTypeAttribute.IsExtensionType(_type);
            lock (this) {
                // if we're an ops type don't add our members to ourself
                // (this happens if the user gets ahold of FloatOps and then
                // trys to call a method on it).
                if (isOps) {
                    return;
                }

                MemberInfo[] defaultMembers = _type.GetDefaultMembers();
                BindingFlags bf = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic;

                AddBases();

                EnsureTransformDelegate();


                // if the type's not public you can't access members on it, only members
                // on it's bases which might be private
                foreach(Type type in GetTypesForAdd()) {
                    foreach (MethodInfo mi in type.GetMethods(bf)) {
                        AddReflectedMethod(mi, defaultMembers);
                    }

                    foreach (FieldInfo fi in type.GetFields(bf)) {
                        AddReflectedField(fi);
                    }

                    foreach (PropertyInfo pi in type.GetProperties(bf)) {
                        AddReflectedProperty(pi, defaultMembers);
                    }

                    foreach (EventInfo pi in type.GetEvents()) {
                        AddReflectedEvent(pi);
                    }

                    foreach (Type ty in type.GetNestedTypes(bf)) {
                        AddNestedType(ty);
                    }

                    if (_type == type) {
                        AddConstructors();
                    }

                    AddInheritedStaticMethods(defaultMembers, bf);
                }

                AddOps();

                if (_type.IsSubclassOf(typeof(Delegate))) {
                    AddOperator(new DynamicTypeDelegateCallSlot(), new OperatorMapping(Operators.Call, false, true, false));
                }
            }

            PublishDictionary();

            Builder.Finish(!_type.IsDefined(typeof(MutableTypeAttribute), true));
        }

        public class DynamicTypeDelegateCallSlot : DynamicTypeSlot {
            private BuiltinFunction _invoker;

            private void CreateInvoker(DynamicType dt) {
                MethodInfo delegateInfo = dt.UnderlyingSystemType.GetMethod("Invoke");
                Debug.Assert(delegateInfo != null);
                _invoker = BuiltinFunction.MakeMethod("invoke", delegateInfo, FunctionType.Function | FunctionType.AlwaysVisible);
            }

            public override bool TryGetValue(CodeContext context, object instance, DynamicMixin owner, out object value) {
                if (_invoker == null) CreateInvoker((DynamicType)owner);

                value = new DelegateInvoker(instance, _invoker);
                return true;
            }
        }

        public class DelegateInvoker : ICallableWithCodeContext {
            private BuiltinFunction _invoker;
            private object _instance;

            public DelegateInvoker(object instance, BuiltinFunction invoker) {
                _instance = instance;
                _invoker = invoker;
            }

            #region ICallableWithCodeContext Members

            [SpecialName]
            public object Call(CodeContext context, object[] args) {
                return _invoker.CallInstance(context, _instance, args);
            }

            #endregion
        }

        private IList<Type> GetTypesForAdd() {
            // only add interface members for private types.
            if (ShouldAddTypesMembers()) return new Type[] { _type };
            
            return _type.GetInterfaces();
        }

        private bool ShouldAddTypesMembers() {
            return _type.IsPublic || 
                (_type.DeclaringType != null && _type.IsNestedPublic) || 
                ScriptDomainManager.Options.PrivateBinding;
        }

        private static object NewDict() {
            return new SymbolDictionary();
        }

        private void PublishDictionary() {
            if (_operImpl != null) {
                foreach(KeyValuePair<OperatorMapping, OperatorInfo> kvp in _operImpl) {
                    AddOperator(kvp.Value.Callable as DynamicTypeSlot ?? CreateValueSlot(kvp.Value.Callable), kvp.Key);
                }
            }

            foreach (KeyValuePair<SymbolId, Members> kvp in _dict) {
                if (kvp.Value.Value != null) {
                    DynamicTypeSlot dts = kvp.Value.Value as DynamicTypeSlot ?? CreateValueSlot(kvp.Value.Value);

                    Builder.AddSlot(kvp.Key, dts);
                }

                if (kvp.Value.CtxValues != null) {
                    foreach(KeyValuePair<ContextId, object> ctxKvp in kvp.Value.CtxValues) {
                        DynamicTypeSlot dts = ctxKvp.Value as DynamicTypeSlot ?? CreateValueSlot(ctxKvp.Value);

                        Builder.AddSlot(ctxKvp.Key, kvp.Key, dts);
                    }
                }
            }
        }

        protected virtual DynamicTypeSlot CreateValueSlot(object value) {
            return new DynamicTypeValueSlot(value);
        }

        protected virtual void AddOps() {
        }

        protected override bool TryGetContextValue(SymbolId name, ContextId context, out object value) {
            Members mem;

            if (!Dictionary.TryGetValue(name, out mem)) {
                value = null;
                return false;
            }

            if (mem.CtxValues != null) {
                return mem.CtxValues.TryGetValue(context, out value);
            }
            value = null;
            return false;
        }

        protected override bool TryGetValue(SymbolId name, ContextId context, out object value) {
            Members mem;

            if (!Dictionary.TryGetValue(name, out mem)) {
                value = null;
                return false;
            }

            if (mem.CtxValues != null && context != ContextId.Empty) {
                if (mem.CtxValues.TryGetValue(context, out value)) return true;
            }

            if (mem.Value != null) {
                value = mem.Value;
                return true;
            }

            value = null;
            return false;
        }

        protected override void SetValue(SymbolId name, ContextId context, object value) {
            Members mem;
            if (!Dictionary.TryGetValue(name, out mem)) {
                mem = new Members();
                Dictionary[name] = mem;
            }
            if (context == ContextId.Empty) {
                mem.Value = value;
            } else {
                if (mem.CtxValues == null) mem.CtxValues = new Dictionary<ContextId, object>();
                mem.CtxValues[context] = value;
            }
        }

        protected override void RemoveValue(SymbolId name, ContextId context) {
            Dictionary.Remove(name);
        }

        #region Internal API Surface

        protected DynamicTypeSlot StoreReflectedMethod(string name, ContextId context, MethodInfo mi, FunctionType ft) {
            ft |= CompilerHelpers.IsStatic(mi) ? FunctionType.Function : FunctionType.Method;

            // avoid duplicates of interfaces...
            if (mi.IsAbstract) return StoreMethodNoConflicts(name, context, mi, ft);

            return StoreMethod(SymbolTable.StringToId(name), name, context, mi, ft);
        }

        protected override void AddImplicitConversion(MethodInfo mi) {            
            Builder.AddConversion(mi.GetParameters()[0].ParameterType,
                mi.ReturnType,
                delegate(object from) {
                    //!!! generate a delegate
                    return mi.Invoke(null, new object[] { from });
                });
        }           

        #endregion

        #region Private APIs

        /// <summary>
        /// Creates a __new__ method for the type.  If the type defines interesting constructors
        /// then the __new__ method will call that.  Otherwise if it has only a single argless
        /// </summary>
        protected void AddConstructors() {
            if (typeof(Delegate).IsAssignableFrom(_type)) {
                Builder.SetConstructor(new DelegateBuilder(_type));
            } else if (!_type.IsAbstract) {
                BuiltinFunction reflectedCtors = GetConstructors();
                if (reflectedCtors == null) return; // no ctors, no __new__

                Builder.SetConstructor(reflectedCtors);                
            }
        }

        private void AddBases() {
            if (_type.BaseType != null) {
                if (_type.BaseType == Builder.UnfinishedType.ImpersonationType) {
                    // we're impersonating, add our impersonation type as the base.
                    Builder.AddBaseType(DynamicHelpers.GetDynamicTypeFromType(Builder.UnfinishedType.ImpersonationType));
                } else if (_type.BaseType == typeof(ValueType)) {
                    Builder.AddBaseType(DynamicHelpers.GetDynamicTypeFromType(typeof(object)));
                } else {
                    Builder.AddBaseType(DynamicHelpers.GetDynamicTypeFromType(_type.BaseType));
                }

                IList<DynamicMixin> mro = new List<DynamicMixin>();
                Type curType = _type;
                while (curType != null) {
                    mro.Add(DynamicHelpers.GetDynamicTypeFromType(curType));
                    curType = curType.BaseType;
                }

                Builder.SetResolutionOrder(mro);
            } else if (_type.IsInterface) {
                List<DynamicMixin> mro = new List<DynamicMixin>();
                mro.Add(Builder.UnfinishedType);
                foreach (Type i in _type.GetInterfaces()) {
                    DynamicType it = DynamicHelpers.GetDynamicTypeFromType(i);
                    mro.Add(it);
                    Builder.AddBaseType(it);
                }
                Builder.SetResolutionOrder(mro);
            }
        }

        private static bool ShouldInclude(MethodInfo getter) {
            return getter != null && 
                (getter.IsFamily || getter.IsFamilyOrAssembly || getter.IsPublic || ScriptDomainManager.Options.PrivateBinding);
        }

        private static bool ShouldInclude(Type type) {
            return type != null &&
                (type.IsNestedFamily|| type.IsNestedFamORAssem || type.IsPublic || ScriptDomainManager.Options.PrivateBinding);
        }

        private static bool ShouldInclude(FieldInfo field) {
            return field != null &&
                (field.IsFamily || field.IsFamilyOrAssembly || field.IsPublic || ScriptDomainManager.Options.PrivateBinding);
        }
        
        private void AddReflectedProperty(PropertyInfo info, MemberInfo[] defaultMembers) {
            if (info.GetIndexParameters().Length > 0) {
                foreach (MemberInfo member in defaultMembers) {
                    if (member == info) {
                        AddIndexer(info);
                        return;
                    }
                }

                // named indexer that isn't a default.  This occurs if VB or COM components
                // define a property that takes parameters.  
                AddNamedIndexer(info);
            } else {
                // properties can have conflicting accessibility.  Generate public or private
                // accessors as necessary.
                MethodInfo getter = info.GetGetMethod(true), setter = info.GetSetMethod(true);

                if (!ShouldInclude(getter)) getter = null;
                if (!ShouldInclude(setter)) setter = null;

                if (getter != null || setter != null) {
                    foreach (TransformedName name in _transform(info, TransformReason.Property)) {
                        if (name.Context != ContextId.Empty || _transform == DefaultNameTransform) {
                            SetValue(SymbolTable.StringToId(name.Name), name.Context, new ReflectedProperty(info, getter, setter, NameType.PythonProperty));
                        } else {
                            SetValue(SymbolTable.StringToId(name.Name), name.Context, new ReflectedProperty(info, getter, setter, NameType.Property));
                        }
                    }
                }
            }
        }

        private void AddIndexer(PropertyInfo pi) {
            MethodInfo method;

            method = pi.GetGetMethod();
            if (method != null) {
                AddOperator(method, FunctionType.Method);
            }
            method = pi.GetSetMethod();
            if (method != null) {
                AddOperator(method, FunctionType.Method);
            }
        }

        private void AddNamedIndexer(PropertyInfo pi) {
            MethodInfo getter = pi.GetGetMethod(true), setter = pi.GetSetMethod(true);

            if (!ShouldInclude(getter)) getter = null;
            if (!ShouldInclude(setter)) setter = null;
            
            if(getter != null || setter != null) {
                SetValue(SymbolTable.StringToId(pi.Name), ContextId.Empty, new ReflectedIndexer(pi, NameType.Property));                                                      
            }
        }

        private void AddInheritedStaticMethods(MemberInfo[] defaultMembers, BindingFlags bf) {
            if (_type != typeof(object)) {
                Type curType = _type.BaseType;
                while (curType != null && curType != typeof(object) && curType != typeof(ValueType)) {
                    foreach (MethodInfo mi in curType.GetMethods(bf & ~BindingFlags.Instance)) {
                        // Only inherit op_Implicit.  
                        if (!mi.IsSpecialName || mi.Name != "op_Implicit") continue;

                        foreach (TransformedName tn in _transform(mi, TransformReason.Method)) {
                            if(tn.Name != null) {
                                AddReflectedMethod(mi, defaultMembers);
                            }
                        }
                    }
                    curType = curType.BaseType;
                }
            }
        }

        private void AddReflectedEvent(EventInfo info) {
            SetValue(SymbolTable.StringToId(info.Name), ContextId.Empty, new ReflectedEvent(info, IsPythonType));
        }

        private void AddNestedType(Type type) {
            if (type.IsPublic || type.IsNestedPublic || type.IsNestedFamily || type.IsNestedFamORAssem) {
                EnsureTransformDelegate();

                foreach (TransformedName tn in _transform(type, TransformReason.NestedType)) {
                    if (tn.Name != null) {
                        object typeEntity = DynamicHelpers.GetDynamicTypeFromType(type);
                        string name = TypeGroup.GetNormalizedTypeName(tn.Name);
                        SymbolId nameId = SymbolTable.StringToId(name);

                        // Check for colliding generic types with the same name
                        object existingType;
                        if (TryGetValue(nameId, tn.Context, out existingType)) {
                            DynamicType existingDt = existingType as DynamicType;
                            if (existingDt != null) {
                                existingType = ReflectionCache.GetTypeTracker(existingDt.UnderlyingSystemType);
                            }

                            typeEntity = TypeGroup.UpdateTypeEntity((TypeTracker)existingType, ReflectionCache.GetTypeTracker(type));
                        }

                        SetValue(nameId, tn.Context, typeEntity);
                    }
                }
            }
        }

        private void AddReflectedField(FieldInfo fi) {
            Debug.Assert(fi != null);

            if (ShouldInclude(fi)) {
                foreach (TransformedName tn in _transform(fi, TransformReason.Field)) {
                    if (tn.Context != ContextId.Empty || _transform == DefaultNameTransform) {
                        SetValue(SymbolTable.StringToId(tn.Name), tn.Context, new ReflectedField(fi, NameType.PythonField));
                    } else {
                        SetValue(SymbolTable.StringToId(tn.Name), tn.Context, new ReflectedField(fi, NameType.Field));
                    }
                }
            }
        }

        private void AddReflectedMethod(MethodInfo mi, MemberInfo[] defaultMembers) {
            Debug.Assert(mi != null && defaultMembers != null);

            if (IsExplicitInterfaceImpl(mi)) {
                // explicit interface methods will be added independently.
                return;
            }

            if (ShouldInclude(mi)) {
                if (_transform != DefaultNameTransform) {
                    StoreReflectedMethod(mi.Name, ContextId.Empty, mi, FunctionType.None);
                } else {
                    StoreReflectedMethod(mi.Name, ContextId.Empty, mi, FunctionType.AlwaysVisible);
                }
            }

            if (mi.IsSpecialName) {
                if (GetPropertyFromMethod(mi, defaultMembers) != null) {
                    // properties don't get added as methods
                    return;
                }
                AddOperator(mi, FunctionType.Method);
            } else if (_transform != DefaultNameTransform) {
                foreach (TransformedName tn in _transform(mi, TransformReason.Method)) {
                    if (tn.CustomTransformer != null) {
                        // transformer is asking to place custom value here.
                        DynamicTypeSlot prev = null;
                        object tmp;
                        if(TryGetValue(SymbolTable.StringToId(tn.Name), tn.Context, out tmp)) {
                            prev = tmp as DynamicTypeSlot;
                            if(prev == null) prev = new DynamicTypeValueSlot(tmp);
                        }
                        SetValue(SymbolTable.StringToId(tn.Name), tn.Context, tn.CustomTransformer(mi, prev));
                    } else if (tn.Context != ContextId.Empty) {
                        StoreReflectedMethod(tn.Name, tn.Context, mi, FunctionType.AlwaysVisible);
                    } else {
                        StoreReflectedMethod(tn.Name, tn.Context, mi, FunctionType.None);
                    }
                }
            }
            
        }

        protected void AddOperator(MethodInfo mi, FunctionType baseFunctionType) {
            if (mi.Name == "op_Implicit") {
                AddImplicitConversion(mi);
                return;
            }
            
            EnsureTransformDelegate();

            FunctionType funcType = baseFunctionType | FunctionType.AlwaysVisible;
            
            ParameterInfo[] pis = mi.GetParameters();
            if (pis.Length == 3 && pis[0].ParameterType == typeof(CodeContext)) funcType |= FunctionType.BinaryOperator;
            else if (pis.Length == 2) funcType |= FunctionType.BinaryOperator;

            // TODO: Unify w/ DynamicTypeExtender.StoreOperator
            foreach (TransformedName name in _transform(mi, TransformReason.Operator)) {
                OperatorInfo info;
                OperatorMapping op = name.Operator;
                FunctionType ft = baseFunctionType;

                if (op != null) {
                    if (_operImpl == null) _operImpl = new Dictionary<OperatorMapping, OperatorInfo>();

                    if (op.IsReversed) ft |= FunctionType.ReversedOperator;
                    if (!_operImpl.TryGetValue(op, out info)) {
                        object bf = BuiltinFunction.MakeMethod(mi.Name, mi, ft).GetDescriptor();

                        _operImpl[op] = new OperatorInfo(name, bf, name.Context);
                    } else {
                        info.Callable = ExtendMethod(mi, ft, info.Callable);
                    }
                }

                if (name.Name != null) {
                    if (name.Context != ContextId.Empty) {
                        StoreMethod(name.Name, name.Context, mi, ft | FunctionType.AlwaysVisible);
                    } else {
                        StoreMethod(name.Name, name.Context, mi, ft );
                    }
                }
            }
        }

        private void EnsureOperImpl() {
            if (_operImpl == null) _operImpl = new Dictionary<OperatorMapping, OperatorInfo>();
        }

        private void EnsureTransformDelegate() {
            if (_transform == null) {
                object[] attrs = _type.GetCustomAttributes(typeof(ScriptTypeAttribute), false);
                if (attrs != null && attrs.Length > 0) {
                    _transform = ((ScriptTypeAttribute)attrs[0]).GetTransformer(Builder.UnfinishedType);
                }

                if (_transform == null) {
                    _transform = DefaultNameTransform;
                }
            }
        }

        class OperatorInfo {
            public ContextId Context;
            public object Callable;
            public TransformedName TransformedName;

            public OperatorInfo(TransformedName name, object callable, ContextId context) {
                Callable = callable;
                Context = context;
                TransformedName = name;
            }
        }

        /// <summary>
        /// The default name transform just returns the member's .NET name within
        /// the default context.
        /// </summary>
        private IEnumerable<TransformedName> DefaultNameTransform(MemberInfo member, TransformReason reason) {
            switch(reason) {
                case TransformReason.Method:
                case TransformReason.Field:
                case TransformReason.Property:
                case TransformReason.Event:
                case TransformReason.NestedType:
                    return DefaultMemberTransformer(member);
                case TransformReason.Operator:
                    return DefaultOperatorTransform((MethodInfo)member);
                default:
                    throw new InvalidOperationException();
            }
        }

        private IEnumerable<TransformedName> EmptyTransformer() {
            yield break;
        }

        private IEnumerable<TransformedName> DefaultMemberTransformer(MemberInfo mi) {
            yield return new TransformedName(mi.Name, ContextId.Empty);
        }

        private IEnumerable<TransformedName> DefaultOperatorTransform(MethodInfo mi) {
            OperatorMapping method;
            if (!_operatorTable.TryGetValue(mi.Name, out method)) {
                yield break;
            }

            bool instance = !mi.IsStatic;

            ParameterInfo[] parms = mi.GetParameters();
            int nparams = parms.Length + (instance ? 1 : 0);
            int ctxOffset = (parms.Length > 0 && parms[0].ParameterType == typeof(CodeContext)) ? 1 : 0;
            nparams -= ctxOffset;
            if (nparams < method.MinArgs || nparams > method.MaxArgs) {
                yield break;
            }

            if (instance) {
                yield return new TransformedName(mi.Name, method, ContextId.Empty);

            } else {
                // a method can be both forward and reverse.  This will happen if the method
                // is operating on it's own types for both parameters.  in this case we publish
                // it under forward & reverse versions and the reverse versions can be called
                // if there is a known conversion to the type.
                bool regular = parms.Length > 0 && parms[0 + ctxOffset].ParameterType == _type;
                bool reverse = !CompilerHelpers.IsComparisonOperator(method.Operator) && parms.Length > 1 && parms[1 + ctxOffset].ParameterType == _type;

                if (regular || ReversedComparison(parms, ctxOffset, reverse)) {
                    yield return new TransformedName(mi.Name, method, ContextId.Empty);
                }

                if (reverse) {
                    Operators revOp = CompilerHelpers.OperatorToReverseOperator(method.Operator);
                    if (method.IsReversable && (!regular || revOp != method.Operator)) {
                        OperatorMapping revmap = method.GetReversed();

                        yield return new TransformedName(mi.Name, revmap, ContextId.Empty);
                    }
                }
            }
        }

        private void StoreOperator(string name, ContextId context, MethodInfo mi, FunctionType type) {
            StoreMethod(name, context, mi, type);
        }

        private bool ReversedComparison(ParameterInfo[] parms, int ctxOffset, bool reverse) {
            return (!reverse && parms.Length > 1 && parms[1 + ctxOffset].ParameterType == _type);
        }

        internal static bool IsExplicitInterfaceImpl(MethodInfo mi) {
            return mi.IsFinal && mi.IsHideBySig && mi.IsPrivate && mi.IsVirtual;
        }

        private PropertyInfo GetPropertyFromMethod(MethodInfo mi, MemberInfo[] defaultMembers) {
            foreach (MemberInfo member in defaultMembers) {
                if (member.MemberType == MemberTypes.Property) {
                    PropertyInfo property = member as PropertyInfo;
                    if (mi == property.GetGetMethod() ||
                        mi == property.GetSetMethod()) {

                        return property;
                    }
                }
            }

            // check both the declaring type, and our type.  We need to check
            // both to deal w/ interfaces & abstract classes where the property
            // is defined in the interface/abstract class, and the methods are
            // declared in the concrete type.
            if (mi.DeclaringType != this._type) {
                PropertyInfo pi = SearchTypeForProperty(mi, mi.DeclaringType);
                if (pi != null) {
                    return pi;
                }
            }

            return SearchTypeForProperty(mi, _type);
        }
        
        private Dictionary<SymbolId, Members> Dictionary {
            get {
                return _dict;
            }
        }

        protected virtual bool IsPythonType {
            get {
                return false;
                //return _type.IsDefined(typeof(PythonTypeAttribute), true);
            }
        }

        public static Dictionary<string, OperatorMapping> OperatorTable {
            get {
                return _operatorTable;
            }
        }

        // We cache the PropertyInfos since SearchTypeForProperty is called for every method. If it called Type.GetProperties every time,
        // it creates a new array of PropertyInfos everytime, which is expensive.
        static Dictionary<Type, PropertyInfo[]> _PropertyInfos = new Dictionary<Type, PropertyInfo[]>();

        static PropertyInfo[] GetTypeProperties(Type type) {
            if (_PropertyInfos.ContainsKey(type)) {
                return _PropertyInfos[type];
            }

            PropertyInfo[] infos = type.GetProperties(BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic);
            _PropertyInfos[type] = infos;
            return infos;
        }

        private static PropertyInfo SearchTypeForProperty(MethodInfo mi, Type type) {
            foreach (PropertyInfo prop in GetTypeProperties(type)) {
                if ((prop.GetGetMethod(true) == mi) ||
                    (prop.GetSetMethod(true) == mi)) {
                    return prop;
                }
            }
            return null;
        }

        private static Dictionary<string, OperatorMapping> InitializeOperatorTable() {
            Dictionary<string, OperatorMapping> ot = new Dictionary<string, OperatorMapping>(28);
            ot["op_Addition"] = new OperatorMapping(Operators.Add, false, true, false, true);
            ot["op_Subtraction"] = new OperatorMapping(Operators.Subtract, false, true, false, true);
            ot["op_Multiply"] = new OperatorMapping(Operators.Multiply, false, true, false, true);
            ot["op_Division"] = new OperatorMapping(Operators.Divide, false, true, false, true);
            ot["op_Modulus"] = new OperatorMapping(Operators.Mod, false, true, false, true);
            ot["op_ExclusiveOr"] = new OperatorMapping(Operators.Xor, false, true, false, true);
            ot["op_BitwiseAnd"] = new OperatorMapping(Operators.BitwiseAnd, false, true, false, true);
            ot["op_BitwiseOr"] = new OperatorMapping(Operators.BitwiseOr, false, true, false, true);
            ot["op_LeftShift"] = new OperatorMapping(Operators.LeftShift, false, true, false, true);
            ot["op_RightShift"] = new OperatorMapping(Operators.RightShift, false, true, false, true);
            ot["op_Equality"] = new OperatorMapping(Operators.Equals, false, true, false, true);
            ot["op_GreaterThan"] = new OperatorMapping(Operators.GreaterThan, false, true, false, true);
            ot["op_LessThan"] = new OperatorMapping(Operators.LessThan, false, true, false, true);
            ot["op_Inequality"] = new OperatorMapping(Operators.NotEquals, false, true, false, true);
            ot["op_GreaterThanOrEqual"] = new OperatorMapping(Operators.GreaterThanOrEqual, false, true, false, true);
            ot["op_LessThanOrEqual"] = new OperatorMapping(Operators.LessThanOrEqual, false, true, false, true);

            ot["op_AdditionAssignment"] = new OperatorMapping(Operators.InPlaceAdd, false, true, false);
            ot["op_SubtractionAssignment"] = new OperatorMapping(Operators.InPlaceSubtract, false, true, false);
            ot["op_MultiplicationAssignment"] = new OperatorMapping(Operators.InPlaceMultiply, false, true, false);
            ot["op_DivisionAssignment"] = new OperatorMapping(Operators.InPlaceDivide, false, true, false);
            ot["op_ModulusAssignment"] = new OperatorMapping(Operators.InPlaceMod, false, true, false);
            ot["op_LeftShiftAssignment"] = new OperatorMapping(Operators.InPlaceLeftShift, false, true, false);
            ot["op_RightShiftAssignment"] = new OperatorMapping(Operators.InPlaceRightShift, false, true, false);
            ot["op_BitwiseAndAssignment"] = new OperatorMapping(Operators.InPlaceBitwiseAnd, false, true, false);
            ot["op_ExclusiveOrAssignment"] = new OperatorMapping(Operators.InPlaceXor, false, true, false);
            ot["op_BitwiseOrAssignment"] = new OperatorMapping(Operators.InPlaceBitwiseOr, false, true, false);

            ot["op_UnaryNegation"] = new OperatorMapping(Operators.Negate, true, false, false);
            ot["op_OnesComplement"] = new OperatorMapping(Operators.OnesComplement, true, false, false);
            ot["op_UnaryPlus"] = new OperatorMapping(Operators.Positive, true, false, false);
            //ot["op_Implicit"] = null;
            //ot["op_Explicit"] = null;
            //ot["op_LogicalAnd"] = null;
            //ot["op_LogicalOr"] = null;
            //ot["op_Assign"] = null;
            //ot["op_SignedRightShift"] = null;
            //ot["op_Comma"] = null;
            //ot["op_Decrement"] = null;
            //ot["op_Increment"] = null;
            //ot["op_UnaryPlus"] = null;

            //These are the recommended public "alternative" names for overloaded operators
            //THese are used with the OperatorMethod attribute
            ot["Add"] = ot["op_Addition"];
            ot["Subtract"] = ot["op_Subtraction"];
            ot["Multiply"] = ot["op_Multiply"];
            ot["Divide"] = ot["op_Division"];
            ot["Mod"] = ot["op_Modulus"];
            ot["LeftShift"] = ot["op_LeftShift"];
            ot["RightShift"] = ot["op_RightShift"];
            ot["BitwiseAnd"] = ot["op_BitwiseAnd"];
            ot["BitwiseOr"] = ot["op_BitwiseOr"];
            ot["ExclusiveOr"] = ot["op_ExclusiveOr"];


            ot["InPlaceAdd"] = ot["op_AdditionAssignment"];
            ot["InPlaceSubtract"] = ot["op_SubtractionAssignment"];
            ot["InPlaceMultiply"] = ot["op_MultiplicationAssignment"];
            ot["InPlaceDivide"] = ot["op_DivisionAssignment"];
            ot["InPlaceMod"] = ot["op_ModulusAssignment"];
            ot["InPlaceLeftShift"] = ot["op_LeftShiftAssignment"];
            ot["InPlaceRightShift"] = ot["op_RightShiftAssignment"];
            ot["InPlaceBitwiseAnd"] = ot["op_BitwiseAndAssignment"];
            ot["InPlaceBitwiseOr"] = ot["op_BitwiseOrAssignment"];
            ot["InPlaceExclusiveOr"] = ot["op_ExclusiveOrAssignment"];

            ot["Equals"] = ot["op_Equality"];
            ot["NotEquals"] = ot["op_Inequality"];
            ot["GreaterThan"] = ot["op_GreaterThan"];
            ot["LessThan"] = ot["op_LessThan"];
            ot["GreaterThanOrEqual"] = ot["op_GreaterThanOrEqual"];
            ot["LessThanOrEqual"] = ot["op_LessThanOrEqual"];

            ot["Plus"] = ot["op_UnaryPlus"];
            ot["Negate"] = ot["op_UnaryNegation"];
            ot["OnesComplement"] = ot["op_OnesComplement"];

            //These types reflect operators from Python that other languages are likely to have
            //They are not part of the CLS defined operators today
            ot["TrueDivide"] = new OperatorMapping(Operators.TrueDivide, false, true, false, true);
            ot["FloorDivide"] = new OperatorMapping(Operators.FloorDivide, false, true, false, true);

            ot["Abs"] = new OperatorMapping(Operators.AbsoluteValue, true, false, false);

            ot["Power"] = new OperatorMapping(Operators.Power, false, true, true, true);
            ot["ToString"] = new OperatorMapping(Operators.ConvertToString, true, false, false);
            ot["GetItem"] = new OperatorMapping(Operators.GetItem, false, true, false);
            ot["SetItem"] = new OperatorMapping(Operators.SetItem, false, false, true);

            // TODO: remove these ugly versions and detect them w/ GetIndexParameters
            ot["get_Item"] = ot["GetItem"];
            ot["set_Item"] = ot["SetItem"];

            ot["Call"] = new OperatorMapping(Operators.Call, false, true, false);
            ot["Contains"] = new OperatorMapping(Operators.Contains, false, true, false);
            //ot["Compare"] = new OperatorMapping(Operators.Compare, false, true, false);

            ot["GetMember"] = new OperatorMapping(Operators.GetMember, false, true, false);
            ot["GetBoundMember"] = new OperatorMapping(Operators.GetBoundMember, false, true, false);
            ot["SetMember"] = new OperatorMapping(Operators.SetMember, false, false, true);
            ot["DeleteMember"] = new OperatorMapping(Operators.DeleteMember, false, false, true);
            ot["GetMemberNames"] = new OperatorMapping(Operators.GetMemberNames, true, false, false);

            return ot;

        }
        #endregion

        class DelegateBuilder : ICallableWithCodeContext {
            private Type _type;

            public DelegateBuilder(Type type) {
                _type = type;
            }

            #region ICallableWithCodeContext Members

            public object Call(CodeContext context, object[] args) {
                Assert.NotNull(args);
                return DynamicHelpers.GetDelegate(args[0], _type, null);
            }

            #endregion
        }
    }
}

