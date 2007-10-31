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
using System.Runtime.Remoting;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Hosting {
    public interface IRemotable {
        Type GetType();
    }

    internal interface ILocalObject : IRemotable {
#if !SILVERLIGHT
        RemoteWrapper Wrap();
#endif
    }

#if SILVERLIGHT

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

#else

    internal abstract class RemoteWrapper : MarshalByRefObject, IRemotable {
        public abstract ILocalObject LocalObject { get; }

        public new Type GetType() {
            return LocalObject.GetType();
        }

        internal static T/*?*/ TryGetLocal<T>(object remotable) 
            where T : class, IRemotable {

            RemoteWrapper remote_wrapper = remotable as RemoteWrapper;
            if (remote_wrapper == null) return (T)remotable;
            if (!Utilities.IsRemote(remote_wrapper)) return (T)remote_wrapper.LocalObject;

            return null;
        }

        internal static T GetLocalArgument<T>(object remotableArgument, string argumentName) 
            where T : class, IRemotable {

            if (remotableArgument == null) throw new ArgumentNullException(argumentName);
            
            T result = TryGetLocal<T>(remotableArgument);
            if (result == null) {
                throw new ArgumentException(Resources.RemoteInstanceMisused, argumentName);
            }
            return (T)result;
        }

        internal static T WrapRemotable<T>(object remotable) 
            where T : class, IRemotable {

            return WrapRemotable<T>(remotable, null);
        }

        internal static T WrapRemotable<T>(object remotable, IRemotable host)
            where T : class, IRemotable {

            ILocalObject local;

            if ((host == null || host is RemoteWrapper) && (local = remotable as ILocalObject) != null) {
                return (T)(object)local.Wrap();
            } else {
                return (T)remotable;
            }
        }
    }
#endif
}
