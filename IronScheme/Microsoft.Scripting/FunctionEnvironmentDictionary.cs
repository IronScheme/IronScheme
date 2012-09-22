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
using System.Collections.Generic;

using Microsoft.Scripting;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting
{
  /// <summary>
    /// Base class for FunctionEnvironment's which use a Tuple for the underlying storage.
    /// </summary>
    /// <typeparam name="TupleType"></typeparam>
  [DebuggerNonUserCode]
  public sealed class FunctionEnvironmentDictionary<TupleType> : TupleDictionary<TupleType>
 where TupleType : Tuple
  {
        public FunctionEnvironmentDictionary(TupleType data) :
            base(data) {
        }

        public FunctionEnvironmentDictionary(TupleType data, SymbolId[] names) :
          base(data, names)
        {
        }

        protected internal override bool TrySetExtraValue(SymbolId key, object value) {
          return false;
        }

        protected internal override bool TryGetExtraValue(SymbolId key, out object value) {
          value = null;
          return false;
        }

   }
}
