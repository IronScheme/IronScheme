
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
using System.Text;

namespace Microsoft.Scripting {

    /// <summary>
    /// Indicates the type of change to a dynamic type - whether a new member was added, or an existing member
    /// was removed.
    /// </summary>
    public enum ChangeType {
        Added,
        Removed
    }


    /// <summary>
    /// Contains information about how a PythonType was changed.
    /// </summary>
    public class PythonTypeChangedEventArgs : EventArgs {
        private SymbolId _changed;
        private ChangeType _type;
        private object _previous;
        private object _newValue;
        private CodeContext _context;

        public PythonTypeChangedEventArgs(CodeContext context, SymbolId changed, ChangeType type, object previous, object newValue) {
            _context = context;
            _changed = changed;
            _type = type;
            _previous = previous;
            _newValue = newValue;
        }

        /// <summary>
        /// The entry name that was changed in the PythonType
        /// </summary>
        public SymbolId Symbol {
            get {
                return _changed;
            }
        }

        /// <summary>
        /// The way the entry was changed (Added or Removed)
        /// </summary>
        public ChangeType ChangeType {
            get {
                return _type;
            }
        }

        /// <summary>
        /// The value stored in the entry before it was changed
        /// </summary>
        public object PreviousValue {
            get {
                return _previous;
            }
        }

        /// <summary>
        /// The value stored in the entry after it was changed (only applicable
        /// for adds)
        /// </summary>
        public object NewValue {
            get {
                return _newValue;
            }
        }

        /// <summary>
        /// Gets the code context of the caller who triggered the event.
        /// </summary>
        public CodeContext Context {
            get {
                return _context;
            }
        }
    }
}

#endif	
