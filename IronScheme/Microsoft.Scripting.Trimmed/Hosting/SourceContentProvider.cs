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
using System.IO;

namespace Microsoft.Scripting.Hosting {

    [Serializable]
    public abstract class SourceContentProvider {
        public abstract TextReader GetReader();

        /// <summary>
        /// Reloads the content from its source if it could be out of sync.
        /// </summary>
        public virtual void Reload() {
            // no-op
        }
    }
}
