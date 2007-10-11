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

namespace Microsoft.Scripting.Types {
    /// <summary>
    /// TODO: Move back to IronPython?
    /// </summary>
    public interface ISuperDynamicObject {
        IAttributesCollection Dict {
            get;
        }

        bool HasDictionary {
            get;
        }

        /// <summary>
        /// Thread-safe dictionary set.  Returns the dictionary set or the previous value if already set or 
        /// null if the dictionary set isn't supported.
        /// </summary>
        /// <param name="dict"></param>
        /// <returns></returns>
        IAttributesCollection SetDict(IAttributesCollection dict);
        /// <summary>
        /// Dictionary replacement.  Returns true if replaced, false if the dictionary set isn't supported.
        /// </summary>
        /// <param name="dict"></param>
        /// <returns></returns>
        bool ReplaceDict(IAttributesCollection dict);

        DynamicType DynamicType {
            get;
        }

        void SetDynamicType(DynamicType newType);  
    }
}
