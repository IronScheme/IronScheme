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
using System.Text;
using System.Reflection;
using System.Diagnostics;
using System.Threading;

using Microsoft.Scripting;
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Types {
    /// <summary>
    /// Implements core functionality used by anyone building from a reflected type.
    /// </summary>
    public abstract partial class CoreReflectedTypeBuilder {
        private DynamicTypeBuilder _builder;

        protected virtual bool TryGetContextValue(SymbolId name, ContextId context, out object value) {
            DynamicTypeSlot dts;
            if (_builder.UnfinishedType.TryLookupContextSlot(SimpleContext.Create(context),
                name,
                out dts)) {
                value = dts;
                return true;
            }

            value = null;
            return false;
        }

        protected virtual bool TryGetValue(SymbolId name, ContextId context, out object value) {
            DynamicTypeSlot dts;
            if (_builder.UnfinishedType.TryLookupSlot(SimpleContext.Create(context),
                name,
                out dts)) {
                value = dts;
                return true;
            }

            value = null;
            return false;
        }
        protected virtual void SetValue(SymbolId name, ContextId context, object value) {
            DynamicTypeSlot dts = value as DynamicTypeSlot;
            if (dts == null)
                dts = new DynamicTypeValueSlot(value);

            _builder.AddSlot(context, name, dts);
        }
        
        protected virtual void RemoveValue(SymbolId name, ContextId context) {
            _builder.RemoveSlot(context, name);
        }

        protected abstract void AddImplicitConversion(MethodInfo mi);        

        protected DynamicTypeSlot StoreMethod(string name, ContextId context, MethodInfo mi, FunctionType ft) {
            return StoreMethod(SymbolTable.StringToId(name), name, context, mi, ft);
        }

        /// <summary> Generic helper for doing the different types of method stores. </summary>
        protected virtual DynamicTypeSlot StoreMethod(SymbolId methodId, string name, ContextId context, MethodInfo mi, FunctionType ft) {
            object existingMember;
            BuiltinFunction rm = null;
            DynamicTypeSlot ret = null;
            bool success = false;

            if (context == ContextId.Empty) {
                success = TryGetValue(methodId, context, out existingMember);
            } else {
                success = TryGetContextValue(methodId, context, out existingMember);
            }

            if (success) {
                BuiltinMethodDescriptor bimd = existingMember as BuiltinMethodDescriptor;
                ret = existingMember as DynamicTypeSlot;

                if (bimd != null) rm = bimd.template as BuiltinFunction;
                else rm = existingMember as BuiltinFunction; 
                
                if (rm != null) {
                    rm.FunctionType |= ft;
                    rm.AddMethod(mi);

                    DynamicTypeSlot newDescriptor;
                    if (bimd == null && (newDescriptor = rm.GetDescriptor()) != rm) {
                        // previously we didn't need a descriptor, but now we do.  This
                        // happens if we added a static function & then an instance function
                        // w/ the same name.  We'll replace the function w/ a descriptor.
                        SetValue(methodId, context, newDescriptor);
                        ret = newDescriptor;
                    }
                }
            }

            if (rm == null) {
                // This is destructive, Assert
                Debug.Assert(existingMember == null, String.Format("Replacing {0} with new BuiltinFunction", existingMember));
                //if (name == "__init__") name = _builder.UnfinishedType.Name;
                rm = BuiltinFunction.MakeMethod(name, mi, ft);
                ret = rm.GetDescriptor();
                SetValue(methodId, context, ret);
            }

            return ret;            
        }

        protected DynamicTypeSlot StoreMethodNoConflicts(string name, ContextId context, MethodInfo mi, FunctionType ft) {
            object tmp;
            bool success;
            if (context != ContextId.Empty) {
                success = TryGetContextValue(SymbolTable.StringToId(name), context, out tmp);
            } else {
                success = TryGetValue(SymbolTable.StringToId(name), context, out tmp);
            }

            if (success) {
                BuiltinFunction bf = tmp as BuiltinFunction;
                if (bf == null) {
                    BuiltinMethodDescriptor bmd = tmp as BuiltinMethodDescriptor;
                    if (bmd != null) {
                        bf = bmd.Template;
                    }
                }

                if (bf != null) {
                    Type[] types = CompilerHelpers.GetTypesWithThis(mi);
                    for (int i = 0; i < bf.Targets.Length; i++) {
                        Type[] otherTypes = CompilerHelpers.GetTypesWithThis(bf.Targets[i]);
                        if (types.Length != otherTypes.Length) continue;

                        bool mismatch = false;
                        for (int j = (mi.DeclaringType.IsInterface || bf.Targets[i].DeclaringType.IsInterface) ? 1 : 0; j < types.Length; j++) {
                            if (types[j] != otherTypes[j]) {
                                mismatch = true;
                                break;
                            }
                        }

                        if (!mismatch) return null;
                    }
                }
            }

            return StoreMethod(name, context, mi, ft);
        }

        protected void AddOperator(DynamicTypeSlot callable, OperatorMapping op) {
            AddOperator(ContextId.Empty, callable, op);
        }

        /// <summary>
        /// Adds an operator that is bound to a specific value.  If the type change
        /// someone needs to update the operators.
        /// </summary>
        protected void AddOperator(ContextId contextId, DynamicTypeSlot callable, OperatorMapping op) {
            DynamicTypeBuilder builder = Builder;
            DynamicType dt = _builder.UnfinishedType;
            if (op.Operator == Operators.Call) {
                builder.AddOperator(contextId,
                    Operators.Call,
                    delegate(CodeContext context, object self, object other, out object ret) {
                        object value;
                        if (!callable.TryGetValue(context, self, dt, out value)) {
                            ret = null;
                            return false;
                        }

                        Debug.Assert(other != null);

                        if (other.GetType() == typeof(KwCallInfo)) {
                            KwCallInfo kwinfo = (KwCallInfo)other;
                            ret = context.LanguageContext.CallWithKeywordArgs(context, 
                                value,
                                kwinfo.Arguments,
                                kwinfo.Names);
                            return true;
                        } else {
                            object[] arroth = (object[])other;
                            ret = context.LanguageContext.Call(context,
                                value,
                                arroth);
                            return true;
                        }
                        
                    });
                return;
            }

            if (op.IsUnary) {
                builder.AddOperator(contextId, op.Operator, UnarySite.Make(dt, callable, contextId).Invoke);
            }

            if (op.IsBinary) {
                builder.AddOperator(contextId, op.Operator, BinarySite.Make(dt, callable, contextId).Invoke);
            }

            if (op.IsTernary) {
                builder.AddOperator(contextId, op.Operator, TernarySite.Make(dt, callable, contextId).Invoke);
            }
        }

        #region Operator sites

        protected abstract class UnarySite {
            private DynamicTypeSlot _callable;
            private DynamicType _type;

            protected UnarySite(DynamicType type, DynamicTypeSlot callable) {
                _callable = callable;
                _type = type;
            }

            public static UnarySite Make(DynamicType type, DynamicTypeSlot callable, ContextId context) {
                if (context == ContextId.Empty) {
                    return new MultiUnarySite(type, callable);
                }

                return new SingleUnarySite(type, callable);
            }

            public bool Invoke(CodeContext context, object self, out object ret) {
                object value;
                _callable.TryGetValue(context, self, _type, out value);

                ret = GetSite(context).Invoke(context, value);
                return true;
            }

            public bool InvokeNotImpl(CodeContext context, object self, out object ret) {
                if (Invoke(context, self, out ret)) {
                    return ret != OperationFailed.Value;
                }
                return false;
            }

            protected abstract DynamicSite<object, object> GetSite(CodeContext context);           
        }

        private class MultiUnarySite : UnarySite {
            private Dictionary<Type, DynamicSite<object, object>> _sites;

            public MultiUnarySite(DynamicType type, DynamicTypeSlot callable)
                : base(type, callable) {
            }

            protected override DynamicSite<object, object> GetSite(CodeContext context) {
                if (_sites == null) {
                    Interlocked.CompareExchange<Dictionary<Type, DynamicSite<object, object>>>(ref _sites,
                        new Dictionary<Type, DynamicSite<object, object>>(),
                        null);
                }

                lock (_sites) {
                    Type t = context.LanguageContext.GetType();
                    DynamicSite<object, object> res;
                    if (!_sites.TryGetValue(t, out res)) {
                        _sites[t] = res = RuntimeHelpers.CreateSimpleCallSite<object, object>();
                    }
                    return res;
                }
            }
        }

        private class SingleUnarySite : UnarySite {
            private DynamicSite<object, object> _site;

            public SingleUnarySite(DynamicType type, DynamicTypeSlot callable)
                : base(type, callable) {
            }

            protected override DynamicSite<object, object> GetSite(CodeContext context) {
                return RuntimeHelpers.CreateSimpleCallSite<object, object>(ref _site);
            }
        }

        protected abstract class BinarySite {
            private DynamicTypeSlot _callable;
            private DynamicType _type;

            protected BinarySite(DynamicType type, DynamicTypeSlot callable) {
                _callable = callable;
                _type = type;
            }

            public static BinarySite Make(DynamicType type, DynamicTypeSlot callable, ContextId context) {
                if (context == ContextId.Empty) {
                    return new MultiBinarySite(type, callable);
                }

                return new SingleBinarySite(type, callable);
            }

            public bool Invoke(CodeContext context, object self, object other, out object ret) {
                object value;
                _callable.TryGetValue(context, self, _type, out value);

                ret = GetSite(context).Invoke(context, value, other);
                return true;
            }

            public bool InvokeNotImpl(CodeContext context, object self, object other, out object ret) {
                if (Invoke(context, self, other, out ret)) {
                    return ret != OperationFailed.Value;
                }
                return false;
            }

            protected abstract DynamicSite<object, object, object> GetSite(CodeContext context);
        }

        private class MultiBinarySite : BinarySite {
            private Dictionary<Type, DynamicSite<object, object, object>> _sites;

            public MultiBinarySite(DynamicType type, DynamicTypeSlot callable)
                : base(type, callable) {
            }

            protected override DynamicSite<object, object, object> GetSite(CodeContext context) {
                if (_sites == null) {
                    Interlocked.CompareExchange<Dictionary<Type, DynamicSite<object, object, object>>>(ref _sites,
                        new Dictionary<Type, DynamicSite<object, object, object>>(),
                        null);
                }

                lock (_sites) {
                    Type t = context.LanguageContext.GetType();
                    DynamicSite<object, object, object> res;
                    if (!_sites.TryGetValue(t, out res)) {
                        _sites[t] = res = RuntimeHelpers.CreateSimpleCallSite<object, object, object>();
                    }
                    return res;
                }
            }
        }

        private class SingleBinarySite : BinarySite {
            private DynamicSite<object, object, object> _site;

            public SingleBinarySite(DynamicType type, DynamicTypeSlot callable)
                : base(type, callable) {
            }

            protected override DynamicSite<object, object, object> GetSite(CodeContext context) {
                return RuntimeHelpers.CreateSimpleCallSite(ref _site);
            }
        }

        protected abstract class TernarySite {
            private DynamicTypeSlot _callable;
            private DynamicType _type;

            protected TernarySite(DynamicType type, DynamicTypeSlot callable) {
                _callable = callable;
                _type = type;
            }

            public static TernarySite Make(DynamicType type, DynamicTypeSlot callable, ContextId context) {
                if (context == ContextId.Empty) {
                    return new MultiTernarySite(type, callable);
                }

                return new SingleTernarySite(type, callable);
            }

            public bool Invoke(CodeContext context, object self, object value1, object value2, out object ret) {
                object value;
                _callable.TryGetValue(context, self, _type, out value);

                ret = GetSite(context).Invoke(context, value, value1, value2);
                return true;
            }

            public bool InvokeNotImpl(CodeContext context, object self, object value1, object value2, out object ret) {
                if (Invoke(context, self, value1, value2, out ret)) {
                    return ret != OperationFailed.Value;
                }
                return false;
            }

            protected abstract DynamicSite<object, object, object, object> GetSite(CodeContext context);
        }

        private class MultiTernarySite : TernarySite {
            private Dictionary<Type, DynamicSite<object, object, object, object>> _sites;

            public MultiTernarySite(DynamicType type, DynamicTypeSlot callable)
                : base(type, callable) {
            }

            protected override DynamicSite<object, object, object, object> GetSite(CodeContext context) {
                if (_sites == null) {
                    Interlocked.CompareExchange<Dictionary<Type, DynamicSite<object, object, object, object>>>(ref _sites,
                        new Dictionary<Type, DynamicSite<object, object, object, object>>(),
                        null);
                }

                lock (_sites) {
                    Type t = context.LanguageContext.GetType();
                    DynamicSite<object, object, object, object> res;
                    if (!_sites.TryGetValue(t, out res)) {
                        _sites[t] = res = RuntimeHelpers.CreateSimpleCallSite<object, object, object, object>();
                    }
                    return res;
                }
            }
        }

        private class SingleTernarySite : TernarySite {
            private DynamicSite<object, object, object, object> _site;

            public SingleTernarySite(DynamicType type, DynamicTypeSlot callable)
                : base(type, callable) {
            }

            protected override DynamicSite<object, object, object, object> GetSite(CodeContext context) {
                return RuntimeHelpers.CreateSimpleCallSite(ref _site);
            }
        }
        #endregion

        protected BuiltinFunction GetConstructors() {
            BuiltinFunction reflectedCtors = null;
            bool hasDefaultConstructor = false;
            Type type = Builder.UnfinishedType.UnderlyingSystemType;
            foreach (ConstructorInfo ci in type.GetConstructors(BindingFlags.Public | BindingFlags.Instance)) {
                if (ci.IsPublic) {
                    if (ci.GetParameters().Length == 0) hasDefaultConstructor = true;
                    reflectedCtors = BuiltinFunction.MakeOrAdd(reflectedCtors, Builder.UnfinishedType.Name, ci, FunctionType.Function);
                }
            }
            if (type.IsValueType && !hasDefaultConstructor && type != typeof(void)) {
                try {
                    MethodInfo mi = typeof(RuntimeHelpers).GetMethod("CreateInstance", ArrayUtils.EmptyTypes).MakeGenericMethod(type);

                    reflectedCtors = BuiltinFunction.MakeOrAdd(reflectedCtors, Builder.UnfinishedType.Name, mi, FunctionType.Function);
                } catch (BadImageFormatException) {
                    // certain types (e.g. ArgIterator) won't survive the above call.
                    // we won't let you create instances of these types.
                }
            }
            return reflectedCtors;
        }

        protected static object ExtendMethod(MethodInfo mi, FunctionType ft, object existingMember) {
            BuiltinFunction rm = null;
            BuiltinMethodDescriptor bimd = existingMember as BuiltinMethodDescriptor;
            if (bimd != null) rm = bimd.template as BuiltinFunction;
            else rm = existingMember as BuiltinFunction;

            if (rm != null) {
                rm.FunctionType |= ft;
                rm.AddMethod(mi);
                // we'll return either the original method descriptor which we've updated,
                // or the reflected's methods descriptor (which will be it's self if it's static)

                return bimd ?? rm.GetDescriptor();
            }
            return null;
        }


        public class OperatorMethod {
            public string method;
            public string rmethod;
            public int args;
            public int maxArgs;

            public OperatorMethod(string method, string rmethod, int args) : this(method, rmethod, args, args) { }
            public OperatorMethod(string method, string rmethod, int args, int maxArgs) {
                this.method = method;
                this.rmethod = rmethod;
                this.args = args;
                this.maxArgs = maxArgs;
            }
        }

        protected DynamicTypeBuilder Builder {
            get {
                return _builder;
            }
            set {
                _builder = value;
            }
        }

        /// <summary>
        /// Simple class which implements CodeContext for a specific
        /// context-ID.  Used for applying context-updates while building the 
        /// Ops type.
        /// </summary>
        protected class SimpleContext : LanguageContext {
            private ContextId _context;
            //private bool _showCls = true;

            private static ScriptModule SimpleModule = ScriptDomainManager.CurrentManager.CreateModule("<empty>");

            public static CodeContext Create(ContextId contextId) {
                SimpleContext context = new SimpleContext(contextId);
                ModuleContext moduleContext = context.EnsureModuleContext(SimpleModule);
                moduleContext.ShowCls = true;
                return new CodeContext(SimpleModule.Scope, context, moduleContext);
            }

            private SimpleContext(ContextId context)
                : base() {
                _context = context;
            }

            public override ActionBinder Binder {
                get {
                    return null;
                }
            }

            public override ContextId ContextId {
                get {
                    return _context;
                }
            }

            public override Microsoft.Scripting.Ast.CodeBlock ParseSourceCode(CompilerContext context) {
                throw new NotSupportedException();
            }
        }

    }
}
