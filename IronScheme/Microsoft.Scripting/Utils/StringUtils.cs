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
using System.Globalization;

namespace Microsoft.Scripting.Utils {
    public static class StringUtils {       
        public static string GetSuffix(string str, char separator, bool includeSeparator) {
            Contract.RequiresNotNull(str, "str");
            int last = str.LastIndexOf(separator);
            return (last != -1) ? str.Substring(includeSeparator ? last : last + 1) : null;
        }
    }
}
