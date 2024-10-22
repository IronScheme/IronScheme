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
    /// Base class for SymbolId dictionaries.  
    /// 
    /// SymbolId dictionaries are fast dictionaries used for looking up members of classes, 
    /// function environments, function locals, and other places which are typically indexed by 
    /// string names.  
    /// 
    /// SymbolId dictionaries support both keying by SymbolId (the common case) and object keys 
    /// (supporting late bound access to the dictionary as a normal Dictionary&lt;object, object&gt; 
    /// when exposed directly to user code).  When indexed by objects null is a valid value for the
    /// key.
    /// </summary>
    public abstract class BaseSymbolDictionary
    {
        /// <summary>
        /// Creates a new SymbolIdDictBase from the specified creating context which will be
        /// used for comparisons.
        /// </summary>
        protected BaseSymbolDictionary() {
        }

        #region ICollection Members

        public void CopyTo(Array array, int index) {
            throw new NotImplementedException("The method or operation is not implemented.");
        }

        public virtual bool IsSynchronized {
            get { return false; }
        }

        public virtual object SyncRoot {
            get { return null; }
        }

        #endregion        
    }
}