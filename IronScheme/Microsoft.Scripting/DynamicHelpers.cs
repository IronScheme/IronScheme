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
using System.Reflection;
using System.Threading;

using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Types;
using Microsoft.Scripting.Actions;

namespace Microsoft.Scripting {
    public static class DynamicHelpers {
        private static TopNamespaceTracker _topNamespace;

        /// <summary> Table of dynamicly generated delegates which are shared based upon method signature. </summary>
        private static Publisher<DelegateSignatureInfo, DelegateInfo> _dynamicDelegateCache = new Publisher<DelegateSignatureInfo, DelegateInfo>();
        private static Dictionary<Type, List<Type>> _extensionTypes = new Dictionary<Type, List<Type>>();

        public static DynamicType GetDynamicTypeFromType(Type type) {
            Contract.RequiresNotNull(type, "type");

            PerfTrack.NoteEvent(PerfTrack.Categories.DictInvoke, "TypeLookup " + type.FullName);

            DynamicType ret = DynamicType.GetDynamicType(type);
            if (ret != null) return ret;

            ret = ReflectedTypeBuilder.Build(type);

            return DynamicType.SetDynamicType(type, ret);
        }

        public static DynamicStackFrame[] GetDynamicStackFrames(Exception e) {
            return GetDynamicStackFrames(e, true);
        }

        public static DynamicStackFrame[] GetDynamicStackFrames(Exception e, bool filter) {
            List<DynamicStackFrame> frames = Utils.ExceptionUtils.GetDataDictionary(e)[typeof(DynamicStackFrame)] as List<DynamicStackFrame>;

            if (frames == null) {
                // we may have missed a dynamic catch, and our host is looking
                // for the exception...
                frames = ExceptionHelpers.AssociateDynamicStackFrames(e);
                ExceptionHelpers.ClearDynamicStackFrames();
            }

            if (frames == null) {
                return new DynamicStackFrame[0];
            }

            if (!filter) return frames.ToArray();
#if !SILVERLIGHT
            frames = new List<DynamicStackFrame>(frames);            
            List<DynamicStackFrame> res = new List<DynamicStackFrame>();

            // the list of _stackFrames we build up in RuntimeHelpers can have
            // too many frames if exceptions are thrown from script code and
            // caught outside w/o calling GetDynamicStackFrames.  Therefore we
            // filter down to only script frames which we know are associated
            // w/ the exception here.
            try {
                StackTrace outermostTrace = new StackTrace(e);
                IList<StackTrace> otherTraces = ExceptionHelpers.GetExceptionStackTraces(e) ?? new List<StackTrace>();
                List<StackFrame> clrFrames = new List<StackFrame>();
                foreach (StackTrace trace in otherTraces) {
                    clrFrames.AddRange(trace.GetFrames());
                }
                clrFrames.AddRange(outermostTrace.GetFrames());
                
                int lastFound = 0;
                foreach (StackFrame clrFrame in clrFrames) {
                    MethodBase method = clrFrame.GetMethod();

                    for (int j = lastFound; j < frames.Count; j++) {
                        MethodBase other = frames[j].GetMethod();
                        // method info's don't always compare equal, check based
                        // upon name/module/declaring type which will always be a correct
                        // check for dynamic methods.
                        if (method.Module == other.Module &&
                            method.DeclaringType == other.DeclaringType &&
                            method.Name == other.Name) {
                            res.Add(frames[j]);
                            frames.RemoveAt(j);
                            lastFound = j;
                            break;
                        }
                    }
                }
            } catch (MemberAccessException) {
                // can't access new StackTrace(e) due to security
            }
            return res.ToArray();
#else 
            return frames.ToArray();
#endif
        }
        public static DynamicType GetDynamicType(object o) {
            ISuperDynamicObject dt = o as ISuperDynamicObject;
            if (dt != null) return dt.DynamicType;
            
            if (o == null) return DynamicType.NullType;

            return GetDynamicTypeFromType(o.GetType());
        }

        public static TopNamespaceTracker TopNamespace {
            get {
                if (_topNamespace == null)
                    Interlocked.CompareExchange<TopNamespaceTracker>(ref _topNamespace, new TopNamespaceTracker(), null);

                return _topNamespace;
            }
        }

        // TODO: remove exceptionHandler param (Silverlight hack):
        /// <summary>
        /// Creates a delegate with a given signature that could be used to invoke this object from non-dynamic code (w/o code context).
        /// A stub is created that makes appropriate conversions/boxing and calls the object.
        /// The stub should be executed within a context of this object's language.
        /// </summary>
        /// <returns>The delegate or a <c>null</c> reference if the object is not callable.</returns>
        public static Delegate GetDelegate(object callableObject, Type delegateType, Action<Exception> exceptionHandler) {
            Contract.RequiresNotNull(delegateType, "delegateType");

            Delegate result = callableObject as Delegate;
            if (result != null) {
                if (!delegateType.IsAssignableFrom(result.GetType())) {
                    throw RuntimeHelpers.SimpleTypeError(String.Format("Cannot cast {0} to {1}.", result.GetType(), delegateType));
                }

                return result;
            }

            IDynamicObject dynamicObject = callableObject as IDynamicObject;
            if (dynamicObject != null) {

                MethodInfo invoke;
                
                if (!typeof(Delegate).IsAssignableFrom(delegateType) || (invoke = delegateType.GetMethod("Invoke")) == null) {
                    throw RuntimeHelpers.SimpleTypeError("A specific delegate type is required.");
                }

// using IDynamicObject.LanguageContext for now, we need todo better
                Debug.Assert(dynamicObject.LanguageContext != null, "Invalid implementation");

                ParameterInfo[] parameters = invoke.GetParameters();

                dynamicObject.LanguageContext.CheckCallable(dynamicObject, parameters.Length);
                
                DelegateSignatureInfo signatureInfo = new DelegateSignatureInfo(
                    dynamicObject.LanguageContext.Binder,
                    invoke.ReturnType,
                    parameters,
                    exceptionHandler
                );

                DelegateInfo delegateInfo = _dynamicDelegateCache.GetOrCreateValue(signatureInfo,
                    delegate() {
                        // creation code
                        return signatureInfo.GenerateDelegateStub();
                    });


                result = delegateInfo.CreateDelegate(delegateType, dynamicObject);
                if (result != null) {
                    return result;
                }
            }

            throw RuntimeHelpers.SimpleTypeError("Object is not callable.");
        }

        /// <summary>
        /// Registers a set of extension methods from the provided assemly.
        /// </summary>
        public static void RegisterAssembly(Assembly assembly) {
            object[] attrs = assembly.GetCustomAttributes(typeof(ExtensionTypeAttribute), false);
            foreach (ExtensionTypeAttribute et in attrs) {
                RegisterOneExtension(et.Extends, et.Type);
            }
        }

        private static void RegisterOneExtension(Type extending, Type extension) {
            lock (_extensionTypes) {
                List<Type> extensions;
                if (!_extensionTypes.TryGetValue(extending, out extensions)) {
                    _extensionTypes[extending] = extensions = new List<Type>();
                }
                extensions.Add(extension);
            }

            ExtensionTypeAttribute.RegisterType(extending, extension);

            FireExtensionEvent(extending, extension);
        }

        private static void FireExtensionEvent(Type extending, Type extension) {
            EventHandler<TypeExtendedEventArgs> ev = _extended;
            if (ev != null) {
                ev(null, new TypeExtendedEventArgs(extending, extension));
            }
        }

        public class TypeExtendedEventArgs : EventArgs {
            public TypeExtendedEventArgs(Type extending, Type extension) {
                Extending = extending;
                Extension = extension;
            }

            public Type Extending;
            public Type Extension;
        }

        /// <summary>
        /// Provides a notification when a language agnostic extension event has been registered.
        /// 
        /// Maybe just a work around until Python can pull out the extension types on-demand or 
        /// if we require extension to be registered w/ an engine.
        /// </summary>
        public static event EventHandler<TypeExtendedEventArgs> TypeExtended {
            add {
                List<KeyValuePair<Type, Type>> existing = new List<KeyValuePair<Type,Type>>();
                lock (_extensionTypes) {
                    _extended += value;

                    foreach (KeyValuePair<Type, List<Type>> kvp in _extensionTypes) {
                        foreach(Type t in kvp.Value) {
                            existing.Add(new KeyValuePair<Type, Type>(kvp.Key, t));
                        }
                    }
                }
                foreach (KeyValuePair<Type, Type> extended in existing) {
                    FireExtensionEvent(extended.Key, extended.Value);
                }
            }
            remove {
                _extended -= value;
            }
        }

        private static EventHandler<TypeExtendedEventArgs> _extended;

        internal static Type[] GetExtensionTypes(Type t) {
            lock (_extensionTypes) {
                List<Type> res;
                if (_extensionTypes.TryGetValue(t, out res)) {
                    return res.ToArray();
                }
            }

            return ArrayUtils.EmptyTypes;
        }
    }
}
