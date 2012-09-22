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
using System.Runtime.Remoting;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Hosting {
    public interface IRemotable {
        Type GetType();
    }

    internal interface ILocalObject : IRemotable
    {

    }

    internal sealed class RemoteWrapper {
        internal static T WrapRemotable<T>(IRemotable remotable, IRemotable host) 
            where T : class, IRemotable {

            return (T)remotable;
        }

        internal static object WrapObject(object value, IScriptHost host) {
            return value;
        }

        internal static object UnwrapObject(object value) {
            return value;
        }

        internal static T TryGetLocal<T>(object remotable) {
            return (T)remotable;
        }

        internal static T GetLocalArgument<T>(object remotableArgument, string argumentName) {
            return (T)remotableArgument;
        }
    }

}
