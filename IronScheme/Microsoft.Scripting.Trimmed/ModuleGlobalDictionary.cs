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
using Microsoft.Scripting;
using System.Diagnostics;

namespace Microsoft.Scripting {
    /// <summary>
    /// Intended for internal use to initialization optimized module dictionaries.  Exposed publicly because 
    /// generated types implement this interface.
    /// </summary>
    public interface IModuleDictionaryInitialization {
        void InitializeModuleDictionary(CodeContext context);
    }

    /// <summary>
    /// Dictionary backed by tuples used for collectable modules globals.    All members are wrapped in ModuleGlobalWrapper's
    /// which are updated when the builtin module changes.
    /// </summary>
    /// <typeparam name="TupleType"></typeparam>
    public class ModuleGlobalDictionary<TupleType> : TupleDictionary<TupleType>, IModuleDictionaryInitialization
        where TupleType : Tuple {

        public ModuleGlobalDictionary(TupleType data, SymbolId[] names)
            : base(data, names) {
        }

        protected internal override bool TryGetExtraValue(SymbolId key, out object value) {
            if (base.TryGetExtraValue(key, out value)) {
                ModuleGlobalWrapper wrapper = value as ModuleGlobalWrapper;
                Debug.Assert(wrapper != null);

                if (wrapper.RawValue != Uninitialized.Instance) {
                    value = wrapper.RawValue;
                    return true;
                }
            }
            return false;
        }

        protected internal override bool TrySetExtraValue(SymbolId key, object value) {
            object prevVal;
            if (base.TryGetExtraValue(key, out prevVal)) {
                ModuleGlobalWrapper wrapper = prevVal as ModuleGlobalWrapper;

                Debug.Assert(wrapper != null);

                wrapper.CurrentValue = value;

                return true;
            }
            return false;
        }

        void IModuleDictionaryInitialization.InitializeModuleDictionary(CodeContext context) {
            if (Extra.Length == 0) return;

            object val;
            if (base.TryGetExtraValue(Extra[0], out val) && val != null)
                throw new InvalidOperationException("already initialized");

            foreach (SymbolId id in base.Extra) {
                bool res = base.TrySetExtraValue(id, new ModuleGlobalWrapper(context, context.LanguageContext.GetModuleCache(id), id));
                Debug.Assert(res);
            }
        }
    }
}
