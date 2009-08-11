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
using System.Diagnostics;
using System.Threading;
using System.Reflection;
using System.Collections;

using Microsoft.Scripting.Shell;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Actions;

namespace Microsoft.Scripting {
    /// <summary>
    /// These are some generally useful helper methods.
    /// Currently the only methods are those to cached boxed representations of commonly
    /// used primitive types so that they can be shared.  This is useful to most dynamic
    /// languages that use object as a universal type.
    /// </summary>
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Maintainability", "CA1506:AvoidExcessiveClassCoupling")]
    [DebuggerNonUserCode]
    public static partial class RuntimeHelpers {
        private const int MIN_CACHE = -100;
        private const int MAX_CACHE = 1000;
        private static readonly object[] cache = MakeCache();
        private static readonly string[] chars = MakeSingleCharStrings();

        /// <summary> Singleton boxed instance of True.  We should never box additional instances. </summary>
        public static readonly object True = true;
        /// <summary> Singleton boxed instance of False  We should never box additional instances. </summary>
        public static readonly object False = false;

#if FULL
        private static TopNamespaceTracker _topNamespace; 
#endif


        /// <summary> Table of dynamicly generated delegates which are shared based upon method signature. </summary>

#if FULL
        private static Publisher<DelegateSignatureInfo, DelegateInfo> _dynamicDelegateCache = new Publisher<DelegateSignatureInfo, DelegateInfo>(); 
#endif

        private static Dictionary<Type, List<Type>> _extensionTypes = new Dictionary<Type, List<Type>>();

        private static object[] MakeCache() {
            object[] result = new object[MAX_CACHE - MIN_CACHE];

            for (int i = 0; i < result.Length; i++) {
                result[i] = (object)(i + MIN_CACHE);
            }

            return result;
        }

        private static string[] MakeSingleCharStrings() {
            string[] result = new string[255];

            for (char ch = (char)0; ch < result.Length; ch++) {
                result[ch] = new string(ch, 1);
            }

            return result;
        }

        [DebuggerHidden]
        [DebuggerStepThrough]
        public static string CharToString(char ch) {
            if (ch < 255) return chars[ch];
            return new string(ch, 1);
        }

        [DebuggerHidden]
        [DebuggerStepThrough]
        public static object BooleanToObject(bool value) {
            return value ? True : False;
        }

        [DebuggerHidden]
        [DebuggerStepThrough]
        public static object Int32ToObject(Int32 value) {
            // caches improves pystone by ~5-10% on MS .Net 1.1, this is a very integer intense app
            if (value < MAX_CACHE && value >= MIN_CACHE) {
                return cache[value - MIN_CACHE];
            }
            return (object)value;
        }

        // formalNormalArgumentCount - does not include FuncDefFlags.ArgList and FuncDefFlags.KwDict
        // defaultArgumentCount - How many arguments in the method declaration have a default value?
        // providedArgumentCount - How many arguments are passed in at the call site?
        // hasArgList - Is the method declaration of the form "foo(*argList)"?
        // keywordArgumentsProvided - Does the call site specify keyword arguments?
        public static ArgumentTypeException TypeErrorForIncorrectArgumentCount(
            string methodName,
            int formalNormalArgumentCount,
            int defaultArgumentCount,
            int providedArgumentCount,
            bool hasArgList,
            bool keywordArgumentsProvided) {
            return TypeErrorForIncorrectArgumentCount(methodName, formalNormalArgumentCount, formalNormalArgumentCount, defaultArgumentCount, providedArgumentCount, hasArgList, keywordArgumentsProvided);
        }

        public static ArgumentTypeException TypeErrorForIncorrectArgumentCount(
            string methodName,
            int minFormalNormalArgumentCount,
            int maxFormalNormalArgumentCount,
            int defaultArgumentCount,
            int providedArgumentCount,
            bool hasArgList,
            bool keywordArgumentsProvided) {

            int formalCount;
            string formalCountQualifier;
            string nonKeyword = keywordArgumentsProvided ? "non-keyword " : "";

            if (defaultArgumentCount > 0 || hasArgList || minFormalNormalArgumentCount != maxFormalNormalArgumentCount) {
                if (providedArgumentCount < minFormalNormalArgumentCount) {
                    formalCountQualifier = "at least";
                    formalCount = minFormalNormalArgumentCount - defaultArgumentCount;
                } else {
                    formalCountQualifier = "at most";
                    formalCount = maxFormalNormalArgumentCount;
                }
            } else {
                formalCountQualifier = "exactly";
                formalCount = minFormalNormalArgumentCount;
            }

            return RuntimeHelpers.SimpleTypeError(string.Format(
                "{0}() takes {1} {2} {3}argument{4} ({5} given)",
                                methodName, // 0
                                formalCountQualifier, // 1
                                formalCount, // 2
                                nonKeyword, // 3
                                formalCount == 1 ? "" : "s", // 4
                                providedArgumentCount)); // 5
        }

        public static ArgumentTypeException TypeErrorForIncorrectArgumentCount(string name, int formalNormalArgumentCount, int defaultArgumentCount, int providedArgumentCount) {
            return TypeErrorForIncorrectArgumentCount(name, formalNormalArgumentCount, defaultArgumentCount, providedArgumentCount, false, false);
        }

        public static ArgumentTypeException TypeErrorForIncorrectArgumentCount(string name, int expected, int received) {
            return TypeErrorForIncorrectArgumentCount(name, expected, 0, received);
        }

        public static ArgumentTypeException TypeErrorForExtraKeywordArgument(string name, string argumentName) {
            return SimpleTypeError(String.Format("{0}() got an unexpected keyword argument '{1}'", name, argumentName));
        }

        public static ArgumentTypeException SimpleTypeError(string message) {
            return new ArgumentTypeException(message);
        }

        public static Exception CannotConvertError(Type toType, object value) {
            return SimpleTypeError(String.Format("Cannot convert {0}({1}) to {2}", CompilerHelpers.GetType(value).Name, value, toType.Name));
        }

        public static Exception SimpleAttributeError(string message) {
            return new MissingMemberException(message);
        }

        public delegate object AssertHandler(object who, object msg, params object[] irritants);

        public static AssertHandler Assert;

        public static void ThrowUnboundLocalError(SymbolId name) {
          if (Assert != null)
          {
            Assert(False, "variable not initialized", Ast.Variable.UnGenSym(name));
          }
          else
          {
            throw new UnboundLocalException(string.Format("local variable '{0}' referenced before assignment", SymbolTable.IdToString(name)));
          }
        }

        /// <summary>
        /// Called from generated code, helper to do name lookup
        /// </summary>
        public static object LookupName(CodeContext context, SymbolId name) {
          return context.Scope.LookupName(name);
        }

        /// <summary>
        /// Called from generated code, helper to do name assignment.
        /// Order of parameters matches the codegen flow.
        /// </summary>
        public static object SetNameReorder(object value, CodeContext context, SymbolId name) {
            context.LanguageContext.SetName(context, name, value);
            return value;
        }

        /// <summary>
        /// Called from generated code, helper to do name assignment
        /// </summary>
        public static void SetName(CodeContext context, SymbolId name, object value) {
            context.LanguageContext.SetName(context, name, value);
        }

        public static void SetNameBoxed(CodeContext context, object name, object value)
        {
          context.LanguageContext.SetName(context, (SymbolId) name, value);
        }

        /// <summary>
        /// Called from generated code, helper to remove a name
        /// </summary>
        public static object RemoveName(CodeContext context, SymbolId name) {
            context.LanguageContext.RemoveName(context, name);
            return null;
        }
        /// <summary>
        /// Called from generated code, helper to do a global name lookup
        /// </summary>
        public static object LookupGlobalName(CodeContext context, SymbolId name) {
          return context.Scope.ModuleScope.LookupName(name);
            // TODO: could we get rid of new context creation:
            //CodeContext moduleScopedContext = new CodeContext(context.Scope.ModuleScope, context.LanguageContext, context.ModuleContext);
            //return context.LanguageContext.LookupName(moduleScopedContext, name);
        }

        /// <summary>
        /// Called from generated code, helper to do global name assignment
        /// </summary>
        public static void SetGlobalName(CodeContext context, SymbolId name, object value) {
          context.Scope.ModuleScope.SetName(name, value);
            // TODO: could we get rid of new context creation:
            //CodeContext moduleScopedContext = new CodeContext(context.Scope.ModuleScope, context.LanguageContext, context.ModuleContext);
            //context.LanguageContext.SetName(moduleScopedContext, name, value);
        }

        /// <summary>
        /// Called from generated code, helper to remove a global name
        /// </summary>
        public static void RemoveGlobalName(CodeContext context, SymbolId name) {
            // TODO: could we get rid of new context creation:
            CodeContext moduleScopedContext = new CodeContext(context.Scope.ModuleScope, context.LanguageContext, context.ModuleContext);
            context.LanguageContext.RemoveName(moduleScopedContext, name);
        }

        public static void InitializeModuleFieldBoxed(CodeContext context, object name, ref ModuleGlobalWrapper wrapper)
        {
          var s = (SymbolId)name;
          ModuleGlobalCache mgc = context.LanguageContext.GetModuleCache(s);

          wrapper = new ModuleGlobalWrapper(context, mgc, s);
        }

        public static void InitializeModuleField(CodeContext context, SymbolId name, ref ModuleGlobalWrapper wrapper) {
            ModuleGlobalCache mgc = context.LanguageContext.GetModuleCache(name);

            wrapper = new ModuleGlobalWrapper(context, mgc, name);
        }

        public static TupleType GetTupleDictionaryData<TupleType>(Scope scope) where TupleType : Tuple {
            return ((TupleDictionary<TupleType>)scope.Dict).TupleData;
        }

        // The locals dictionary must be first so that we have the benefit of an emtpy stack when we emit the value
        // in the ScopeExpression
        public static CodeContext CreateNestedCodeContext(IAttributesCollection locals, CodeContext context, bool visible) {
            return new CodeContext(new Scope(context.Scope, locals, visible), context.LanguageContext, context.ModuleContext);
        }

#if FULL

        #region Dynamic Sites Construction Helpers // TODO: generate this

        public static DynamicSite<T0, R> CreateSimpleCallSite<T0, R>() {
            return new DynamicSite<T0, R>(CallAction.Make(0));
        }

        public static DynamicSite<T0, T1, R> CreateSimpleCallSite<T0, T1, R>() {
            return new DynamicSite<T0, T1, R>(CallAction.Make(1));
        }

        public static DynamicSite<T0, T1, T2, R> CreateSimpleCallSite<T0, T1, T2, R>() {
            return new DynamicSite<T0, T1, T2, R>(CallAction.Make(2));
        }

        public static DynamicSite<T0, T1, T2, T3, R> CreateSimpleCallSite<T0, T1, T2, T3, R>() {
            return new DynamicSite<T0, T1, T2, T3, R>(CallAction.Make(3));
        }

        public static DynamicSite<T0, T1, T2, T3, T4, R> CreateSimpleCallSite<T0, T1, T2, T3, T4, R>() {
            return new DynamicSite<T0, T1, T2, T3, T4, R>(CallAction.Make(4));
        }

        public static FastDynamicSite<T0, R> CreateSimpleCallSite<T0, R>(CodeContext context) {
            return new FastDynamicSite<T0, R>(context, CallAction.Make(0));
        }

        public static FastDynamicSite<T0, T1, R> CreateSimpleCallSite<T0, T1, R>(CodeContext context) {
            return new FastDynamicSite<T0, T1, R>(context, CallAction.Make(1));
        }

        public static FastDynamicSite<T0, T1, T2, R> CreateSimpleCallSite<T0, T1, T2, R>(CodeContext context) {
            return new FastDynamicSite<T0, T1, T2, R>(context, CallAction.Make(2));
        }

        public static FastDynamicSite<T0, T1, T2, T3, R> CreateSimpleCallSite<T0, T1, T2, T3, R>(CodeContext context) {
            return new FastDynamicSite<T0, T1, T2, T3, R>(context, CallAction.Make(3));
        }

        public static FastDynamicSite<T0, T1, T2, T3, T4, R> CreateSimpleCallSite<T0, T1, T2, T3, T4, R>(CodeContext context) {
            return new FastDynamicSite<T0, T1, T2, T3, T4, R>(context, CallAction.Make(4));
        }

        public static FastDynamicSite<T0, T1, T2, T3, T4, T5, R> CreateSimpleCallSite<T0, T1, T2, T3, T4, T5, R>(CodeContext context) {
            return new FastDynamicSite<T0, T1, T2, T3, T4, T5, R>(context, CallAction.Make(5));
        }

        public static DynamicSite<T0, R> CreateSimpleCallSite<T0, R>(ref DynamicSite<T0, R> site) {
            if (site == null) {
                Interlocked.CompareExchange<DynamicSite<T0, R>>(ref site, CreateSimpleCallSite<T0, R>(), null);
            }
            return site;
        }

        public static DynamicSite<T0, T1, R> CreateSimpleCallSite<T0, T1, R>(ref DynamicSite<T0, T1, R> site) {
            if (site == null) {
                Interlocked.CompareExchange<DynamicSite<T0, T1, R>>(ref site, CreateSimpleCallSite<T0, T1, R>(), null);
            }
            return site;
        }

        public static DynamicSite<T0, T1, T2, R> CreateSimpleCallSite<T0, T1, T2, R>(ref DynamicSite<T0, T1, T2, R> site) {
            if (site == null) {
                Interlocked.CompareExchange<DynamicSite<T0, T1, T2, R>>(ref site, CreateSimpleCallSite<T0, T1, T2, R>(), null);
            }
            return site;
        }

        public static DynamicSite<T0, T1, T2, T3, R> CreateSimpleCallSite<T0, T1, T2, T3, R>(ref DynamicSite<T0, T1, T2, T3, R> site) {
            if (site == null) {
                Interlocked.CompareExchange<DynamicSite<T0, T1, T2, T3, R>>(ref site, CreateSimpleCallSite<T0, T1, T2, T3, R>(), null);
            }
            return site;
        }

        public static DynamicSite<T0, T1, T2, T3, T4, R> CreateSimpleCallSite<T0, T1, T2, T3, T4, R>(ref DynamicSite<T0, T1, T2, T3, T4, R> site) {
            if (site == null) {
                Interlocked.CompareExchange<DynamicSite<T0, T1, T2, T3, T4, R>>(ref site, CreateSimpleCallSite<T0, T1, T2, T3, T4, R>(), null);
            }
            return site;
        }

        public static FastDynamicSite<T0, R> CreateSimpleCallSite<T0, R>(CodeContext context, ref FastDynamicSite<T0, R> site) {
            if (site == null) {
                Interlocked.CompareExchange<FastDynamicSite<T0, R>>(ref site, CreateSimpleCallSite<T0, R>(context), null);
            }
            return site;
        }

        public static FastDynamicSite<T0, T1, R> CreateSimpleCallSite<T0, T1, R>(CodeContext context, ref FastDynamicSite<T0, T1, R> site) {
            if (site == null) {
                Interlocked.CompareExchange<FastDynamicSite<T0, T1, R>>(ref site, CreateSimpleCallSite<T0, T1, R>(context), null);
            }
            return site;
        }

        public static FastDynamicSite<T0, T1, T2, R> CreateSimpleCallSite<T0, T1, T2, R>(CodeContext context, ref FastDynamicSite<T0, T1, T2, R> site) {
            if (site == null) {
                Interlocked.CompareExchange<FastDynamicSite<T0, T1, T2, R>>(ref site, CreateSimpleCallSite<T0, T1, T2, R>(context), null);
            }
            return site;
        }

        public static FastDynamicSite<T0, T1, T2, T3, R> CreateSimpleCallSite<T0, T1, T2, T3, R>(CodeContext context, ref FastDynamicSite<T0, T1, T2, T3, R> site) {
            if (site == null) {
                Interlocked.CompareExchange<FastDynamicSite<T0, T1, T2, T3, R>>(ref site, CreateSimpleCallSite<T0, T1, T2, T3, R>(context), null);
            }
            return site;
        }

        public static FastDynamicSite<T0, T1, T2, T3, T4, R> CreateSimpleCallSite<T0, T1, T2, T3, T4, R>(CodeContext context, ref FastDynamicSite<T0, T1, T2, T3, T4, R> site) {
            if (site == null) {
                Interlocked.CompareExchange<FastDynamicSite<T0, T1, T2, T3, T4, R>>(ref site, CreateSimpleCallSite<T0, T1, T2, T3, T4, R>(context), null);
            }
            return site;
        }

        #endregion

#endif

        public static IAttributesCollection GetLocalDictionary(CodeContext context) {
            return context.Scope.Dict;
        }

        /// <summary>
        /// Initializes all but the 1st member of a environement tuple to Uninitialized.Instance
        /// 
        /// Called from generated code for environment initialization.
        /// </summary>
        public static void UninitializeEnvironmentTuple(Tuple tuple, int capacity) {
            Debug.Assert(tuple != null);

            for (int i = 1; i < capacity; i++) {
                tuple.SetValue(i, Uninitialized.Instance);
            }
        }

        public static ArgumentTypeException BadArgumentsForOperation(Operators op, params object[] args) {
            StringBuilder message = new StringBuilder("unsupported operand type(s) for operation ");
            message.Append(op.ToString());
            message.Append(": ");
            string comma = "";

            foreach (object o in args) {
                message.Append(comma);
                message.Append(CompilerHelpers.GetType(o));
                comma = ", ";
            }

            throw new ArgumentTypeException(message.ToString());
        }

        public static object ReadOnlyAssignError(bool field, string fieldName) {
            throw SimpleAttributeError(String.Format("{0} {1} is read-only", field ? "Field" : "Property", fieldName));
        }


#if FULL
        public static DynamicStackFrame[] GetDynamicStackFrames(Exception e) {
            return GetDynamicStackFrames(e, true);
        }

        public static DynamicStackFrame[] GetDynamicStackFrames(Exception e, bool filter) {
            List<DynamicStackFrame> frames = e.Data[typeof(DynamicStackFrame)] as List<DynamicStackFrame>;

            if (frames == null) {
                // we may have missed a dynamic catch, and our host is looking
                // for the exception...
                frames = ExceptionHelpers.AssociateDynamicStackFrames(e);
                ExceptionHelpers.ClearDynamicStackFrames();
            }

            if (frames == null) {
                return new DynamicStackFrame[0];
            }

            if (!filter) return frames.ToArray();
#if !SILVERLIGHT
            frames = new List<DynamicStackFrame>(frames);
            List<DynamicStackFrame> res = new List<DynamicStackFrame>();

            // the list of _stackFrames we build up in RuntimeHelpers can have
            // too many frames if exceptions are thrown from script code and
            // caught outside w/o calling GetDynamicStackFrames.  Therefore we
            // filter down to only script frames which we know are associated
            // w/ the exception here.
            try {
                StackTrace outermostTrace = new StackTrace(e);
                IList<StackTrace> otherTraces = ExceptionHelpers.GetExceptionStackTraces(e) ?? new List<StackTrace>();
                List<StackFrame> clrFrames = new List<StackFrame>();
                foreach (StackTrace trace in otherTraces) {
                    clrFrames.AddRange(trace.GetFrames());
                }
                clrFrames.AddRange(outermostTrace.GetFrames());

                int lastFound = 0;
                foreach (StackFrame clrFrame in clrFrames) {
                    MethodBase method = clrFrame.GetMethod();

                    for (int j = lastFound; j < frames.Count; j++) {
                        MethodBase other = frames[j].GetMethod();
                        // method info's don't always compare equal, check based
                        // upon name/module/declaring type which will always be a correct
                        // check for dynamic methods.
                        if (method.Module == other.Module &&
                            method.DeclaringType == other.DeclaringType &&
                            method.Name == other.Name) {
                            res.Add(frames[j]);
                            frames.RemoveAt(j);
                            lastFound = j;
                            break;
                        }
                    }
                }
            } catch (MemberAccessException) {
                // can't access new StackTrace(e) due to security
            }
            return res.ToArray();
#else 
            return frames.ToArray();
#endif
        } 
#endif

#if FULL

        public static TopNamespaceTracker TopNamespace {
            get {
                if (_topNamespace == null)
                    Interlocked.CompareExchange<TopNamespaceTracker>(ref _topNamespace, new TopNamespaceTracker(), null);

                return _topNamespace;
            }
        }

#endif

        /// <summary>
        /// Creates a delegate with a given signature that could be used to invoke this object from non-dynamic code (w/o code context).
        /// A stub is created that makes appropriate conversions/boxing and calls the object.
        /// The stub should be executed within a context of this object's language.
        /// </summary>
        /// <returns>The delegate or a <c>null</c> reference if the object is not callable.</returns>
        public static Delegate GetDelegate(object callableObject, Type delegateType) {
            Contract.RequiresNotNull(delegateType, "delegateType");

            Delegate result = callableObject as Delegate;
            if (result != null) {
                if (!delegateType.IsAssignableFrom(result.GetType())) {
                    throw RuntimeHelpers.SimpleTypeError(String.Format("Cannot cast {0} to {1}.", result.GetType(), delegateType));
                }

                return result;
            }


#if FULL
            IDynamicObject dynamicObject = callableObject as IDynamicObject;
            if (dynamicObject != null) {

                MethodInfo invoke;

                if (!typeof(Delegate).IsAssignableFrom(delegateType) || (invoke = delegateType.GetMethod("Invoke")) == null) {
                    throw RuntimeHelpers.SimpleTypeError("A specific delegate type is required.");
                }

                // using IDynamicObject.LanguageContext for now, we need todo better
                Debug.Assert(dynamicObject.LanguageContext != null, "Invalid implementation");

                ParameterInfo[] parameters = invoke.GetParameters();

                dynamicObject.LanguageContext.CheckCallable(dynamicObject, parameters.Length);

                DelegateSignatureInfo signatureInfo = new DelegateSignatureInfo(
                    dynamicObject.LanguageContext.Binder,
                    invoke.ReturnType,
                    parameters
                );

                DelegateInfo delegateInfo = _dynamicDelegateCache.GetOrCreateValue(signatureInfo,
                    delegate() {
                        // creation code
                        return signatureInfo.GenerateDelegateStub();
                    });


                result = delegateInfo.CreateDelegate(delegateType, dynamicObject);
                if (result != null) {
                    return result;
                }
            } 
#endif


            throw RuntimeHelpers.SimpleTypeError("Object is not callable.");
        }

        /// <summary>
        /// Registers a set of extension methods from the provided assemly.
        /// </summary>
        public static void RegisterAssembly(Assembly assembly) {
            object[] attrs = assembly.GetCustomAttributes(typeof(ExtensionTypeAttribute), false);
            foreach (ExtensionTypeAttribute et in attrs) {
                RegisterOneExtension(et.Extends, et.Type);
            }
        }

        private static void RegisterOneExtension(Type extending, Type extension) {
            lock (_extensionTypes) {
                List<Type> extensions;
                if (!_extensionTypes.TryGetValue(extending, out extensions)) {
                    _extensionTypes[extending] = extensions = new List<Type>();
                }
                extensions.Add(extension);
            }

            ExtensionTypeAttribute.RegisterType(extending, extension);

            FireExtensionEvent(extending, extension);
        }

        private static void FireExtensionEvent(Type extending, Type extension) {
            EventHandler<TypeExtendedEventArgs> ev = _extended;
            if (ev != null) {
                ev(null, new TypeExtendedEventArgs(extending, extension));
            }
        }

        public class TypeExtendedEventArgs : EventArgs {
            public TypeExtendedEventArgs(Type extending, Type extension) {
                Extending = extending;
                Extension = extension;
            }

            public Type Extending;
            public Type Extension;
        }

        /// <summary>
        /// Provides a notification when a language agnostic extension event has been registered.
        /// 
        /// Maybe just a work around until Python can pull out the extension types on-demand or 
        /// if we require extension to be registered w/ an engine.
        /// </summary>
        public static event EventHandler<TypeExtendedEventArgs> TypeExtended {
            add {
                List<KeyValuePair<Type, Type>> existing = new List<KeyValuePair<Type, Type>>();
                lock (_extensionTypes) {
                    _extended += value;

                    foreach (KeyValuePair<Type, List<Type>> kvp in _extensionTypes) {
                        foreach (Type t in kvp.Value) {
                            existing.Add(new KeyValuePair<Type, Type>(kvp.Key, t));
                        }
                    }
                }
                foreach (KeyValuePair<Type, Type> extended in existing) {
                    FireExtensionEvent(extended.Key, extended.Value);
                }
            }
            remove {
                _extended -= value;
            }
        }

        private static EventHandler<TypeExtendedEventArgs> _extended;

        internal static Type[] GetExtensionTypes(Type t) {
            lock (_extensionTypes) {
                List<Type> res;
                if (_extensionTypes.TryGetValue(t, out res)) {
                    return res.ToArray();
                }
            }

            return ArrayUtils.EmptyTypes;
        }
    }
}
