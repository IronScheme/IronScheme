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
using System.Reflection;
using System.Runtime.CompilerServices;
using Microsoft.Scripting.Utils;

using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting {
    /// <summary>
    /// Provides a cache of reflection members.  Only one set of values is ever handed out per a 
    /// specific request.
    /// </summary>
    public class ReflectionCache {
        private static Dictionary<MethodBaseCache, MethodGroup> _functions = new Dictionary<MethodBaseCache, MethodGroup>();
        private static Dictionary<Type, TypeTracker> _typeCache = new Dictionary<Type, TypeTracker>();

        /// <summary>
        /// Gets a singleton method group from the provided type.
        /// 
        /// The provided method group will be unique based upon the methods defined, not based upon the type/name
        /// combination.  In other words calling GetMethodGroup on a base type and a derived type that introduces
        /// no new methods under a given name will result in the same method group for both types.
        /// </summary>
        public static MethodGroup GetMethodGroup(Type type, string name) {
            Contract.RequiresNotNull(type, "type");
            Contract.RequiresNotNull(name, "name");

            MemberInfo[] mems = type.FindMembers(MemberTypes.Method,
                BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static | BindingFlags.InvokeMethod,
                delegate(MemberInfo mem, object filterCritera) {
                    return mem.Name == name;
                },
                null);

            MethodGroup res = null;
            if (mems.Length != 0) {
                MethodInfo[] methods = ArrayUtils.ConvertAll<MemberInfo, MethodInfo>(
                    mems,
                    delegate(MemberInfo x) { return (MethodInfo)x; }
                );
                res = GetMethodGroup(name, methods);
            }
            return res;
        }

        public static MethodGroup GetMethodGroup(string name, MethodBase[] methods) {
            MethodGroup res = null;
            MethodBaseCache cache = new MethodBaseCache(name, methods);
            lock (_functions) {
                if (!_functions.TryGetValue(cache, out res)) {
                    _functions[cache] = res = new MethodGroup(
                        ArrayUtils.ConvertAll<MethodBase, MethodTracker>(
                            methods,
                            delegate(MethodBase x) {
                                return (MethodTracker)MemberTracker.FromMemberInfo(x);
                            }
                        )
                    );
                }
            }
            return res;
        }

        public static MethodGroup GetMethodGroup(string name, MemberGroup mems) {
            MethodGroup res = null;

            MethodBase[] bases = new MethodBase[mems.Count];
            MethodTracker[] trackers = new MethodTracker[mems.Count];
            for (int i = 0; i < bases.Length; i++) {
                trackers[i] = (MethodTracker)mems[i];
                bases[i] = trackers[i].Method;
            }

            if (mems.Count != 0) {
                MethodBaseCache cache = new MethodBaseCache(name, bases);
                lock (_functions) {
                    if (!_functions.TryGetValue(cache, out res)) {
                        _functions[cache] = res = new MethodGroup(trackers);
                    }
                }
            }

            return res;
        }

        public static TypeTracker GetTypeTracker(Type type) {
            TypeTracker res;

            lock (_typeCache) {
                if (!_typeCache.TryGetValue(type, out res)) {
                    _typeCache[type] = res = new NestedTypeTracker(type);
                }
            }

            return res;
        }

        /// <summary>
        /// TODO: Make me private again
        /// </summary>
        public class MethodBaseCache {
            private MethodBase[] _members;
            private string _name;

            public MethodBaseCache(string name, MethodBase[] members) {
                // sort by token so that the Equals / GetHashCode doesn't have
                // to line up members if reflection returns them in different orders.
                Array.Sort<MethodBase>(members, delegate(MethodBase x, MethodBase y) {
                    long res = x.MethodHandle.Value.ToInt64() - y.MethodHandle.Value.ToInt64();
                    if (res == 0) return 0;
                    if (res < 0) return -1;
                    return 1;
                });
                _name = name;
                _members = members;
            }

            public override bool Equals(object obj) {
                MethodBaseCache other = obj as MethodBaseCache;
                if (other == null || _members.Length != other._members.Length) return false;
                if (other._name != _name) return false;

                for (int i = 0; i < _members.Length; i++) {
                    if (_members[i].DeclaringType != other._members[i].DeclaringType ||
                        _members[i].MetadataToken != other._members[i].MetadataToken ||
                        _members[i].IsGenericMethod != other._members[i].IsGenericMethod) {
                        return false;
                    }

                    if (_members[i].IsGenericMethod) {
                        Type[] args = _members[i].GetGenericArguments();
                        Type[] otherArgs = other._members[i].GetGenericArguments();

                        if (args.Length != otherArgs.Length) {
                            return false;
                        }

                        for (int j = 0; j < args.Length; j++) {
                            if (args[j] != otherArgs[j]) return false;
                        }
                    }
                }

                return true;
            }

            public override int GetHashCode() {
                int res = 6551;
                foreach (MemberInfo mi in _members) {
                    res ^= res << 5 ^ mi.DeclaringType.GetHashCode() ^ mi.MetadataToken;
                }
                res ^= _name.GetHashCode();

                return res;
            }
        }
    }
}
