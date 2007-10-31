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
using System.Diagnostics;
using System.Threading;
using System.Reflection;
using System.Collections;

using Microsoft.Scripting.Shell;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Actions;

namespace Microsoft.Scripting {
    /// <summary>
    /// Helper methods that calls are generated to from the default DLR binders.
    /// </summary>
    public static partial class BinderOps {
        /// <summary>
        /// Helper method to create an instance.  Work around for Silverlight where Activator.CreateInstance
        /// is SecuritySafeCritical.
        /// </summary>
        public static T CreateInstance<T>() {
            return default(T);
        }

        public static T[] CreateArray<T>(int args) {
            return new T[args];
        }

        public static T CreateDelegate<T>(object callable) {
            return (T)(object)RuntimeHelpers.GetDelegate(callable, typeof(T));
        }

        /// <summary>
        /// Helper function to combine an object array with a sequence of additional parameters that has been splatted for a function call.
        /// </summary>
        public static object[] GetCombinedParameters(object[] initialArgs, object additionalArgs) {
            IList listArgs = additionalArgs as IList;
            if (listArgs == null) {
                IEnumerable ie = additionalArgs as IEnumerable;
                if (ie == null) {
                    throw new InvalidOperationException("args must be iterable");
                }
                listArgs = new List<object>();
                foreach (object o in ie) {
                    listArgs.Add(o);
                }
            }

            object[] res = new object[initialArgs.Length + listArgs.Count];
            Array.Copy(initialArgs, res, initialArgs.Length);
            listArgs.CopyTo(res, initialArgs.Length);
            return res;
        }

        public static object[] GetCombinedKeywordParameters(object[] initialArgs, IAttributesCollection additionalArgs, ref string[] extraNames) {
            List<object> args = new List<object>(initialArgs);
            List<string> newNames = extraNames == null ? new List<string>(additionalArgs.Count) : new List<string>(extraNames);
            foreach(KeyValuePair<object, object> kvp in additionalArgs) {
                if (kvp.Key is string) {
                    newNames.Add((string)kvp.Key);
                    args.Add(kvp.Value);
                }
            }
            extraNames = newNames.ToArray();
            return args.ToArray();
        }

        public static SymbolDictionary MakeSymbolDictionary(SymbolId[] names, object[] values) {
            SymbolDictionary res = new SymbolDictionary();
            for (int i = 0; i < names.Length; i++) {
                ((IAttributesCollection)res)[names[i]] = values[i];
            }
            return res;
        }

        public static object IncorrectBoxType(Type expected, object received) {
            throw new ArgumentTypeException(String.Format("Expected type {0}, got {1}", expected, CompilerHelpers.GetType(received)));
        }

        public static void UpdateBox<T>(StrongBox<T> box, T value) {
            box.Value = value;
        }

        public static T GetBox<T>(StrongBox<T> box) {
            return box.Value;
        }

        public static bool CheckDictionaryMembers(IDictionary dict, string[] names) {
            if (dict.Count != names.Length) return false;

            foreach (string name in names) {
                if(!dict.Contains(name)) {
                    return false;
                }
            }
            return true;
        }

        #region Event support

        public static void SetEvent(EventTracker eventTracker, object value) {
            EventTracker et = value as EventTracker;
            if(et != null){
                if (et != eventTracker) {
                    throw new ArgumentException(String.Format("expected event from {0}.{1}, got event from {2}.{3}",
                                                eventTracker.DeclaringType.Name,
                                                eventTracker.Name,
                                                et.DeclaringType.Name,
                                                et.Name));
                }
                return;
            } 

            BoundMemberTracker bmt = value as BoundMemberTracker;
            if (bmt == null) throw new ArgumentTypeException("expected bound event, got " + CompilerHelpers.GetType(value).Name);
            if (bmt.BoundTo.MemberType != TrackerTypes.Event) throw new ArgumentTypeException("expected bound event, got " + bmt.BoundTo.MemberType.ToString());

            if (bmt.BoundTo != eventTracker) throw new ArgumentException(String.Format("expected event from {0}.{1}, got event from {2}.{3}", 
                eventTracker.DeclaringType.Name, 
                eventTracker.Name, 
                bmt.BoundTo.DeclaringType.Name, 
                bmt.BoundTo.Name));
        }

        public static EventTracker EventTrackerInPlaceAdd<T>(EventTracker self, T target) {
            MethodInfo add = self.Event.GetAddMethod(ScriptDomainManager.Options.PrivateBinding);
            add.Invoke(null, new object[] { target });
            return self;
        }

        public static EventTracker EventTrackerInPlaceRemove<T>(EventTracker self, T target) {
            MethodInfo remove = self.Event.GetRemoveMethod(ScriptDomainManager.Options.PrivateBinding);
            remove.Invoke(null, new object[] { target });
            return self;
        }

        public static BoundMemberTracker BoundEventTrackerInPlaceAdd<T>(BoundMemberTracker self, T target) {
            if (self.BoundTo.MemberType == TrackerTypes.Event) {
                EventTracker et = (EventTracker)self.BoundTo;

                MethodInfo add = et.Event.GetAddMethod(ScriptDomainManager.Options.PrivateBinding);
                add.Invoke(self.ObjectInstance, new object[] { target });
                return self;
            }
            throw new InvalidOperationException();
        }

        public static BoundMemberTracker BoundEventTrackerInPlaceRemove<T>(BoundMemberTracker self, T target) {
            if (self.BoundTo.MemberType == TrackerTypes.Event) {
                EventTracker et = (EventTracker)self.BoundTo;

                MethodInfo remove = et.Event.GetRemoveMethod(ScriptDomainManager.Options.PrivateBinding);
                remove.Invoke(self.ObjectInstance, new object[] { target });
                return self;
            }
            throw new InvalidOperationException();
        }
        
        #endregion
    }
}
