
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
using System.Reflection;
using System.Reflection.Emit;
using System.Text;
using System.Diagnostics;

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting {
    /// <summary>
    /// Helper class for performing keyword argument binding over multiple methods.
    /// 
    /// Class is constructed w/ args and keynames.  Binds can then be attempted for multiple
    /// delegates or function information by calling the DoBind overrides.  If a bind is 
    /// successful the .NET object array for calling the parameter is returned.  On failure
    /// null is returned and GetError() can be called to get the binding failure.
    /// </summary>
    public class KwArgBinder {
        private object[] _arguments;
        private string[] _kwNames;       // keyword argument names provided at the call site
        private object[] _realArgs;
        private bool[] _haveArg;
        private int _kwDictIndex = -1;
        private int _paramArrayIndex = -1;
        private bool _targetsCls;       // true if we target a CLS method, false if we target a Python function
        private string _methodName = "unknown";
        private bool _fAllowUnboundArgs;
        private List<UnboundArgument> _unboundArgs;
        private Exception _error;
        private CodeContext _ctx;
        private object _instance;

        public KwArgBinder(CodeContext context, object instance, object[] args, string[] keyNames)
            : this(context, instance, args, keyNames, false) {
        }

        public KwArgBinder(CodeContext context, object instance, object[] args, string[] keyNames, bool allowUnboundArgs) {
            _arguments = args;
            _kwNames = keyNames;
            Debug.Assert(keyNames.Length <= args.Length);
            _fAllowUnboundArgs = allowUnboundArgs;
            _ctx = context;
            _instance = instance;
        }

        /// <summary>
        /// Bind a MethodBase using the args and keyword names specified in the constructor
        /// </summary>
        public object[] DoBind(MethodBase target, string name) {
            ParameterInfo[] pis = target.GetParameters();
            // TODO: Only CodeContext
            if (pis.Length > 0 && (pis[0].ParameterType == typeof(CodeContext))) {
                // calling a context aware method, remove context from the parameters
                // for the purpose of the bind.
                pis = ArrayUtils.RemoveFirst(pis);
            }

            string[] argNames = new string[pis.Length];
            object[] defaultVals = new object[pis.Length];
            _methodName = target.Name;
            _targetsCls = true;
            int kwDict = -1, paramsArray = -1;

            if (pis.Length > 0) {
                // populate argument information
                for (int i = 0; i < pis.Length; i++) {
                    argNames[i] = pis[i].Name;
                    defaultVals[i] = pis[i].DefaultValue;
                }

                if (pis[pis.Length - 1].IsDefined(typeof(ParamArrayAttribute), false)) {
                    paramsArray = pis.Length - 1;
                    if (pis.Length > 1 &&
                        pis[pis.Length - 2].IsDefined(typeof(ParamDictionaryAttribute), false)) {
                        kwDict = pis.Length - 2;
                    }
                } else if (pis[pis.Length - 1].IsDefined(typeof(ParamDictionaryAttribute), false)) {
                    kwDict = pis.Length - 1;
                }
            }

            object[] oldArgs = _arguments;
            if (_instance != null && CompilerHelpers.IsStatic(target)) {
                // instance provided for non-instance method, combine.
                _arguments = ArrayUtils.Insert(_instance, _arguments);
            }

            try {
                return DoBind(name, argNames, defaultVals, kwDict, paramsArray);
            } finally {
                _arguments = oldArgs;
            }
        }

        /// <summary>
        /// provide the binding result for the specified arguments, values, and positions for kw dict and param array.
        /// </summary>
        /// argNames - Names of all arguments, including abnormal arguments like FuncDefFlags.ArgList or FuncDefFlags.KwDict
        /// defaultVals - There is one entry per argument. The entry will be DBNull.Value if there is no default value for the argument
        public object[] DoBind(string methName, string[] argNames, object[] defaultVals, int kwDict, int paramArray) {
            _methodName = methName;
            _realArgs = new object[argNames.Length];
            _haveArg = new bool[argNames.Length];
            _unboundArgs = null;
            _kwDictIndex = kwDict;
            _paramArrayIndex = paramArray;

            Debug.Assert(kwDict == -1 || kwDict < argNames.Length);
            Debug.Assert(paramArray == -1 || paramArray < argNames.Length);
            Debug.Assert(defaultVals.Length == argNames.Length);

            if (BindNormalArgs(kwDict, paramArray) &&
                BindKWArgs(argNames, kwDict, paramArray) &&
                IsValidCall(argNames, defaultVals)) {
                return _realArgs;
            }

            return null;
        }

        private int GetNormalArgumentsCount() {
            int normalArgumentsCount = _realArgs.Length;
            if (_kwDictIndex != -1) normalArgumentsCount--;
            if (_paramArrayIndex != -1) normalArgumentsCount--;
            return normalArgumentsCount;
        }

        /// <summary>
        /// Gets the error that caused the bind to fail, or null
        /// if no errors occured during the binding process.
        /// </summary>
        public Exception GetError() {
            return (_error);
        }

        public bool AllowUnboundArgs {
            get {
                return (_fAllowUnboundArgs);
            }
        }

        public List<UnboundArgument> UnboundArgs {
            get {
                return (_unboundArgs);
            }
        }

        private static int FindParamIndex(string[] argNames, string name) {
            for (int i = 0; i < argNames.Length; i++) {
                if (argNames[i] == name) {
                    return (i);
                }
            }
            return (-1);
        }

        private bool BindNormalArgs(int kwDict, int paramArrayIndex) {
            int maxNormalArgs = _arguments.Length - _kwNames.Length;

            for (int i = 0; i < maxNormalArgs; i++) {
                if (i == paramArrayIndex || (i == kwDict && paramArrayIndex != -1)) {
                    _haveArg[paramArrayIndex] = true;
                    object[] paramArray = new object[maxNormalArgs - i];
                    for (int j = i; j < maxNormalArgs; j++) {
                        paramArray[j - i] = _arguments[j];
                    }
                    _realArgs[paramArrayIndex] = _targetsCls ? (object)paramArray : (object)_ctx.LanguageContext.Binder.GetByRefArray(paramArray);
                    return true;
                } else if (i == kwDict) {
                    // we shouldn't bind to the kwDict during normal arg binding
                    _error = RuntimeHelpers.SimpleTypeError(String.Format("{0}() takes exactly {1} argument ({2} given)", _methodName, i, maxNormalArgs));
                    return false;
                } else if (i >= _realArgs.Length) {
                    _error = RuntimeHelpers.SimpleTypeError(String.Format("{0}() takes exactly {1} argument ({2} given)", _methodName, i, maxNormalArgs));
                    return false;
                }

                _haveArg[i] = true;
                _realArgs[i] = _arguments[i];
            }
            if (paramArrayIndex != -1 && !_haveArg[paramArrayIndex]) {
                _realArgs[paramArrayIndex] = _targetsCls ? (object)ArrayUtils.EmptyObjects : (object)_ctx.LanguageContext.Binder.GetByRefArray(ArrayUtils.EmptyObjects);
                _haveArg[paramArrayIndex] = true;
            }
            return true;
        }

        private bool BindKWArgs(string[] argNames, int kwDictIndex, int paramArray) {
            bool fHasDict = false;
            IAttributesCollection kwDict = null;
            if (kwDictIndex != -1) {
                // append kw value to dictionary
                _realArgs[kwDictIndex] = kwDict = new SymbolDictionary();
                _haveArg[kwDictIndex] = true;
                fHasDict = true;
            }
            for (int i = 0; i < _kwNames.Length; i++) {
                int index = FindParamIndex(argNames, _kwNames[i]);
                int argumentsIndex = i + _arguments.Length - _kwNames.Length;
                if (index != -1 && index != kwDictIndex && index != paramArray) {
                    // attempt to bind to a real arg
                    if (_haveArg[index]) {
                        if (index == paramArray)
                            _error = RuntimeHelpers.SimpleTypeError(String.Format("{0}() got an unexpected keyword argument '{1}'", _methodName, _kwNames[i]));
                        else
                            _error = RuntimeHelpers.SimpleTypeError(String.Format("got multiple values for keyword argument {0}", _kwNames[i]));
                        return false;
                    }

                    _haveArg[index] = true;
                    _realArgs[index] = _arguments[argumentsIndex];
                } else if (fHasDict) {
                    // append kw value to dictionary
                    kwDict[SymbolTable.StringToId(_kwNames[i])] = _arguments[argumentsIndex];
                } else if (AllowUnboundArgs) {
                    if (_unboundArgs == null) {
                        _unboundArgs = new List<UnboundArgument>();
                    }
                    _unboundArgs.Add(new UnboundArgument(_kwNames[i], _arguments[argumentsIndex]));
                } else {
                    _error = RuntimeHelpers.SimpleTypeError(String.Format("{0}() got an unexpected keyword argument '{1}'", _methodName, _kwNames[i]));
                    return false;
                }
            }
            return true;
        }

        private bool IsValidCall(string[] argNames, object[] defaultVals) {
            for (int i = 0; i < argNames.Length; i++) {
                if (!_haveArg[i]) {
                    if (defaultVals != null && i < defaultVals.Length && defaultVals[i] != DBNull.Value) {
                        _realArgs[i] = defaultVals[i];
                        _haveArg[i] = true;
                    } else {
                        int realDefaultsCount = 0;
                        for (int d = 0; d < GetNormalArgumentsCount(); d++) {
                            if (defaultVals[d] != DBNull.Value) realDefaultsCount++;
                        }

                        _error = RuntimeHelpers.TypeErrorForIncorrectArgumentCount(_methodName, GetNormalArgumentsCount(), realDefaultsCount, _arguments.Length - _kwNames.Length, _paramArrayIndex != -1, true);
                        return false;
                    }
                }
            }
            return true;
        }
    }

    public class UnboundArgument {
        public UnboundArgument(string name, object value) {
            Name = name;
            Value = value;
        }

        public string Name;
        public object Value;
    }

}

#endif	
