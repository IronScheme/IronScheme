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
using System.Reflection;
using System.Runtime.CompilerServices;
using Microsoft.Scripting.Utils;

using Microsoft.Scripting.Types;
using Microsoft.Scripting.Actions;

namespace Microsoft.Scripting {
    /// <summary>
    /// Provides a cache of reflection members.  Only one set of values is ever handed out per a 
    /// specific request.
    /// </summary>
    public class ReflectionCache {
        private static Dictionary<MethodBaseCache, BuiltinFunction> _functions = new Dictionary<MethodBaseCache,BuiltinFunction>();
        private static Dictionary<EventInfo, ReflectedEvent> _eventCache = new Dictionary<EventInfo, ReflectedEvent>();
        private static Dictionary<PropertyInfo, ReflectedIndexer> _indexerCache = new Dictionary<PropertyInfo, ReflectedIndexer>();
        private static Dictionary<Type, TypeTracker> _typeCache = new Dictionary<Type, TypeTracker>();
        private static Dictionary<FieldInfo, ReflectedField> _fieldCache = new Dictionary<FieldInfo, ReflectedField>();

        /// <summary>
        /// Gets a singleton method group from the provided type.
        /// 
        /// The provided method group will be unique based upon the methods defined, not based upon the type/name
        /// combination.  In other words calling GetMethodGroup on a base type and a derived type that introduces
        /// no new methods under a given name will result in the same method group for both types.
        /// </summary>
        public static BuiltinFunction GetMethodGroup(Type type, string name) {
            Contract.RequiresNotNull(type, "type");
            Contract.RequiresNotNull(name, "name");

            MemberInfo[] mems = type.FindMembers(MemberTypes.Method,
                BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static | BindingFlags.InvokeMethod,
                delegate(MemberInfo mem, object filterCritera) {
                    return mem.Name == name;
                },
                null);

            return GetMethodGroup(type, name, mems);
        }

        public static BuiltinFunction GetMethodGroup(Type type, string name, MemberInfo[] mems) {
            BuiltinFunction res = null;

            MethodBase[] bases = ReflectionUtils.GetMethodInfos(mems);

            if (mems.Length != 0) {
                MethodBaseCache cache = new MethodBaseCache(name, bases);
                lock (_functions) {
                    if (!_functions.TryGetValue(cache, out res)) {
                        _functions[cache] = res = BuiltinFunction.MakeMethod(
                            name,
                            bases,
                            GetMethodFunctionType(type, bases));
                    }
                }
            }

            return res;
        }

        public static ReflectedIndexer GetReflectedIndexer(PropertyInfo info) {
            ReflectedIndexer res;

            lock (_indexerCache) {
                if (!_indexerCache.TryGetValue(info, out res)) {
                    _indexerCache[info] = res = new ReflectedIndexer(info, NameType.PythonProperty);
                }
            }

            return res;
        }

        public static ReflectedEvent GetReflectedEvent(EventInfo info) {
            ReflectedEvent res;

            lock (_eventCache) {
                if (!_eventCache.TryGetValue(info, out res)) {
                    _eventCache[info] = res = new ReflectedEvent(info, false);
                }
            }

            return res;
        }

        public static ReflectedField GetReflectedField(FieldInfo info) {
            ReflectedField res;

            lock (_fieldCache) {
                if (!_fieldCache.TryGetValue(info, out res)) {
                    _fieldCache[info] = res = new ReflectedField(info, NameType.Field);
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

        private static FunctionType GetMethodFunctionType(Type type, MethodBase[] methods) {
            FunctionType ft = FunctionType.None;
            foreach (MethodInfo mi in methods) {
                if (mi.IsStatic && mi.IsSpecialName) {

                    ParameterInfo[] pis = mi.GetParameters();
                    if (pis.Length == 2 || (pis.Length == 3 && pis[0].ParameterType == typeof(CodeContext))) {
                        ft |= FunctionType.BinaryOperator;

                        if (pis[pis.Length - 2].ParameterType != type && pis[pis.Length - 1].ParameterType == type) {
                            ft |= FunctionType.ReversedOperator;
                        }
                    }
                }

                if (mi.IsStatic && mi.DeclaringType.IsAssignableFrom(type)) {
                    ft |= FunctionType.Function;
                } else {
                    ft |= FunctionType.Method;
                }
            }

            return ft;
        }

        private class MethodBaseCache {
            private MethodBase[] _members;
            private string _name;

            public MethodBaseCache(string name, MethodBase [] members) {
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
