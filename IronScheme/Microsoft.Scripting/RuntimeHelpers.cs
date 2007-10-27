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
using System.Diagnostics;
using System.Threading;
using System.Reflection;
using System.Collections;

using Microsoft.Scripting.Shell;
using Microsoft.Scripting.Types;
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
    public static partial class RuntimeHelpers {
        private const int MIN_CACHE = -100;
        private const int MAX_CACHE = 1000;
        private static readonly object[] cache = MakeCache();
        private static readonly string[] chars = MakeSingleCharStrings();

        /// <summary> Singleton boxed instance of True.  We should never box additional instances. </summary>
        public static readonly object True = true;
        /// <summary> Singleton boxed instance of False  We should never box additional instances. </summary>
        public static readonly object False = false;

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

        public static string CharToString(char ch) {
            if (ch < 255) return chars[ch];
            return new string(ch, 1);
        }

        public static object BooleanToObject(bool value) {
            return value ? True : False;
        }

        public static object Int32ToObject(Int32 value) {
            // caches improves pystone by ~5-10% on MS .Net 1.1, this is a very integer intense app
            if (value < MAX_CACHE && value >= MIN_CACHE) {
                return cache[value - MIN_CACHE];
            }
            return (object)value;
        }

        /// <summary>
        /// Helper method for DynamicSite rules that check the version of their dynamic object
        /// TODO - Remove this method for more direct field accesses
        /// </summary>
        /// <param name="o"></param>
        /// <param name="version"></param>
        /// <returns></returns>
        public static bool CheckTypeVersion(object o, int version) {
            return ((ISuperDynamicObject)o).DynamicType.Version == version;
        }

        /// <summary>
        /// Helper method for DynamicSite rules that check the version of their dynamic object
        /// TODO - Remove this method for more direct field accesses
        /// </summary>
        /// <param name="o"></param>
        /// <param name="version"></param>
        /// <returns></returns>
        public static bool CheckAlternateTypeVersion(object o, int version) {
            return ((ISuperDynamicObject)o).DynamicType.AlternateVersion == version;
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

        public static Exception SimpleAttributeError(string message) {
            return new MissingMemberException(message);
        }

        public static void ThrowUnboundLocalError(SymbolId name) {
            throw new UnboundLocalException(string.Format("local variable '{0}' referenced before assignment", SymbolTable.IdToString(name)));
        }

        /// <summary>
        /// Called from generated code, helper to do name lookup
        /// </summary>
        public static object LookupName(CodeContext context, SymbolId name) {
            return context.LanguageContext.LookupName(context, name);
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
            // TODO: could we get rid of new context creation:
            CodeContext moduleScopedContext = new CodeContext(context.Scope.ModuleScope, context.LanguageContext, context.ModuleContext);
            return context.LanguageContext.LookupName(moduleScopedContext, name);
        }

        /// <summary>
        /// Called from generated code, helper to do global name assignment
        /// </summary>
        public static void SetGlobalName(CodeContext context, SymbolId name, object value) {
            // TODO: could we get rid of new context creation:
            CodeContext moduleScopedContext = new CodeContext(context.Scope.ModuleScope, context.LanguageContext, context.ModuleContext);
            context.LanguageContext.SetName(moduleScopedContext, name, value);
        }

        /// <summary>
        /// Called from generated code, helper to remove a global name
        /// </summary>
        public static void RemoveGlobalName(CodeContext context, SymbolId name) {
            // TODO: could we get rid of new context creation:
            CodeContext moduleScopedContext = new CodeContext(context.Scope.ModuleScope, context.LanguageContext, context.ModuleContext);
            context.LanguageContext.RemoveName(moduleScopedContext, name);
        }

        /// <summary>
        /// Called from the generated code, a helper to call a function with "this" and array of arguments.
        /// </summary>
        public static object CallWithThis(CodeContext context, object function, object instance, object[] args) {
            return context.LanguageContext.CallWithThis(context, function, instance, args);
        }

        public static void InitializeModuleField(CodeContext context, SymbolId name, ref ModuleGlobalWrapper wrapper) {
            ModuleGlobalCache mgc = context.LanguageContext.GetModuleCache(name);

            wrapper = new ModuleGlobalWrapper(context, mgc, name);
        }

        public static TupleType GetTupleDictionaryData<TupleType>(Scope scope) where TupleType : Tuple {
            return ((TupleDictionary<TupleType>)scope.Dict).TupleData;
        }

        public static CodeContext CreateNestedCodeContext(CodeContext context, IAttributesCollection locals, bool visible) {
            return new CodeContext(new Scope(context.Scope, locals, visible), context.LanguageContext, context.ModuleContext);
        }

        /// <summary>
        /// Helper method to create an instance.  Work around for Silverlight where Activator.CreateInstance
        /// is SecuritySafeCritical.
        /// </summary>
        public static T CreateInstance<T>() {
            return default(T);
        }

        public static T[] CreateArray<T>(int args) {
            return new T[args];
        }

        public static T CreateDelegate<T>(object callable) {
            return (T)(object)DynamicHelpers.GetDelegate(callable, typeof(T), null);
        }

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
       
        public static ReflectedEvent.BoundEvent MakeBoundEvent(ReflectedEvent eventObj, object instance, Type type) {
            return new ReflectedEvent.BoundEvent(eventObj, instance, DynamicHelpers.GetDynamicTypeFromType(type));
        }

        /// <summary>
        /// Helper function to combine an object array with a sequence of additional parameters that has been splatted for a function call.
        /// </summary>
        public static object[] GetCombinedParameters(object[] initialArgs, object additionalArgs) {
            IList listArgs = additionalArgs as IList;
            if (listArgs == null) {
                IEnumerable ie = additionalArgs as IEnumerable;
                if (ie == null) {
                    throw new InvalidOperationException("args must be iterable");
                }
                listArgs = new List<object>();
                foreach (object o in ie) {
                    listArgs.Add(o);
                }
            }

            object[] res = new object[initialArgs.Length + listArgs.Count];
            Array.Copy(initialArgs, res, initialArgs.Length);
            listArgs.CopyTo(res, initialArgs.Length);
            return res;
        }

        public static object[] GetCombinedKeywordParameters(object[] initialArgs, IAttributesCollection additionalArgs, ref string[] extraNames) {
            List<object> args = new List<object>(initialArgs);
            List<string> newNames = extraNames == null ? new List<string>(additionalArgs.Count) : new List<string>(extraNames);
            foreach(KeyValuePair<object, object> kvp in additionalArgs) {
                if (kvp.Key is string) {
                    newNames.Add((string)kvp.Key);
                    args.Add(kvp.Value);
                }
            }
            extraNames = newNames.ToArray();
            return args.ToArray();
        }

        public static SymbolDictionary MakeSymbolDictionary(SymbolId[] names, object[] values) {
            SymbolDictionary res = new SymbolDictionary();
            for (int i = 0; i < names.Length; i++) {
                ((IAttributesCollection)res)[names[i]] = values[i];
            }
            return res;
        }

        public static object IncorrectBoxType(Type expected, object received) {
            throw new ArgumentTypeException(String.Format("Expected type {0}, got {1}", expected, CompilerHelpers.GetType(received)));
        }

        public static void UpdateBox<T>(StrongBox<T> box, T value) {
            box.Value = value;
        }

        public static T GetBox<T>(StrongBox<T> box) {
            return box.Value;
        }

        public static bool CheckDictionaryMembers(IDictionary dict, string[] names) {
            if (dict.Count != names.Length) return false;

            foreach (string name in names) {
                if(!dict.Contains(name)) {
                    return false;
                }
            }
            return true;
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
    }
}
