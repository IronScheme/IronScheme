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

        /// <summary> Singleton boxed instance of True.  We should never box additional instances. </summary>
        public static readonly object True = true;
        /// <summary> Singleton boxed instance of False  We should never box additional instances. </summary>
        public static readonly object False = false;

        public static FieldInfo Unspecified;

        /// <summary> Table of dynamicly generated delegates which are shared based upon method signature. </summary>
        private static object[] MakeCache() {
            object[] result = new object[MAX_CACHE - MIN_CACHE];

            for (int i = 0; i < result.Length; i++) {
                result[i] = (object)(i + MIN_CACHE);
            }

            return result;
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
          return context.Scope.LookupName(context.LanguageContext, name);
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

        public static void InitializeFieldBoxed<T>(CodeContext context, object name, ref T wrapper)
        {
          var s = (SymbolId)name;
          object val;
          if (context.Scope.ModuleScope.TryLookupName(s, out val))
          {
            wrapper = (T)val;
          }
          else
          {
            wrapper = default(T);
          }
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

      [DebuggerStepThrough]
      [DebuggerHidden]
        public static StorageType GetStorageData<StorageType>(Scope scope) where StorageType : class, new()
        {
          return ((Storage<StorageType>)scope.Dict).Data;
        }

        // The locals dictionary must be first so that we have the benefit of an emtpy stack when we emit the value
        // in the ScopeExpression
        public static CodeContext CreateNestedCodeContext(IAttributesCollection locals, CodeContext context)
        {
          return new CodeContext(new Scope(context.Scope, locals), context.LanguageContext, context.ModuleContext);
        }

        public static IAttributesCollection GetLocalDictionary(CodeContext context) {
            return context.Scope.Dict;
        }

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
            throw RuntimeHelpers.SimpleTypeError("Object is not callable.");
        }
    }
}
