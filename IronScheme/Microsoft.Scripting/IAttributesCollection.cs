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

namespace Microsoft.Scripting
{
    /// <summary>
    /// This interface represents a dictionary that can be accessed using symbols and also arbitrary objects.
    /// This should conceptually inherit from IDictionary&lt;object, object&gt;, but we do not do that as we want the default indexer
    /// property to be indexed by SymbolId, not by object.
    /// </summary>
    public interface IAttributesCollection : IEnumerable<KeyValuePair<object, object>> {
        bool TryGetValue(SymbolId name, out object value);
        bool Remove(SymbolId name);
        object this[SymbolId name] { get; set; }
        IEnumerable<SymbolId> Keys { get; }
    }
}
