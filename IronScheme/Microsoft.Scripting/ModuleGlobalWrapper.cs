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
using System.Diagnostics;

namespace Microsoft.Scripting
{
    /// <summary>
    /// Provides cached global variable for modules to enable optimized access to
    /// module globals.  Both the module global value and the cached value can be held
    /// onto and the cached value can be invalidated by the providing LanguageContext.
    /// 
    /// The cached value is provided by the LanguageContext.GetModuleCache API.
    /// </summary>
    [DebuggerDisplay("{Display}")]
    public sealed class ModuleGlobalWrapper {
        private object _value;
        private ModuleGlobalCache _global;
        private SymbolId _name;
        private CodeContext _context;

        internal ModuleGlobalWrapper(CodeContext context, ModuleGlobalCache global, SymbolId name) {
            _value = Uninitialized.Instance;
            _context = context; 
            _global = global;
            _name = name;
        }

        public static void SetValue(object value, ModuleGlobalWrapper wrapper)
        {
          wrapper.CurrentValue = value;
        }

        public object CurrentValue {
#if !DEBUG
          [DebuggerHidden]
          [DebuggerStepThrough]
#endif
            get {
                if (_value != Uninitialized.Instance) return _value;                

                return GetCachedValue();
            }
#if !DEBUG
          [DebuggerHidden]
          [DebuggerStepThrough]
#endif
            set {
                if (value == Uninitialized.Instance && _value == Uninitialized.Instance) {
                    throw _context.LanguageContext.MissingName(_name);

                }
                _context.Scope.SetName(_name, value);
                _value = value;
            }
        }

        private object GetCachedValue() {
            if (_global.IsCaching) {
                if (_global.HasValue) return _global.Value;
            } else {
                object value;
                // HACK: Shouldn't look in the GlobalScope here, but need to until JSGlobalObject
                // unifies w/ module dictionary.
                if (_context.Scope.ModuleScope.TryGetName(_context.LanguageContext, _name, out value)) {

                  return _value = value;
                }

                if (_context.LanguageContext.TryLookupGlobal(_context, _name, out value)) {
                    return _value = value;
                }
            }

            // TODO: support returning undefined
            throw _context.LanguageContext.MissingName(_name);
        }

        public object RawValue {
            get {
                return _value;
            }
        }

        public override string ToString() {
            return String.Format("ModuleGlobal: {0} Value: {1} ({2})",
                _name,
                _value,
                RawValue == Uninitialized.Instance ? "Module Local" : "Global");
        }
    }  
}
