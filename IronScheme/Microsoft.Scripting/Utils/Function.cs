
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

namespace Microsoft.Scripting.Utils {
    // Orcas: to be replaced by Func<>

    public delegate R Function<R>();
    public delegate R Function<T, R>(T arg);
    public delegate R Function<T1, T2, R>(T1 arg1, T2 arg2);
    public delegate R Function<T1, T2, T3, R>(T1 arg1, T2 arg2, T3 arg3);
    public delegate R Function<T1, T2, T3, T4, R>(T1 arg1, T2 arg2, T3 arg3, T4 arg4);
    public delegate R Function<T1, T2, T3, T4, T5, R>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5);
    public delegate R Function<T1, T2, T3, T4, T5, T6, R>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6);
    public delegate R Function<T1, T2, T3, T4, T5, T6, T7, R>(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5, T6 arg6, T7 arg7);
}

#endif	
