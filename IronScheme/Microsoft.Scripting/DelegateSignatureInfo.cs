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
using System.Text;
using System.Reflection;
using System.Reflection.Emit;
using System.Diagnostics;

using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting {
    class DelegateSignatureInfo {
        private readonly ActionBinder _binder;
        private readonly Type _returnType;
        private readonly ParameterInfo[] _parameters;

#if DEBUG
        internal static readonly object TargetPlaceHolder = new object();
#endif

        public DelegateSignatureInfo(ActionBinder binder, Type returnType, ParameterInfo[] parameters) {
            _binder = binder;
            _parameters = parameters;
            _returnType = returnType;
        }

        public override bool Equals(object obj) {
            DelegateSignatureInfo dsi = obj as DelegateSignatureInfo;
            if (dsi == null) {
                return false;
            }

            if (dsi._binder != _binder) {
                return false;
            }

            if (dsi._parameters.Length != _parameters.Length) {
                return false;
            }

            if (_returnType != dsi._returnType) {
                return false;
            }

            for (int i = 0; i < _parameters.Length; i++) {
                if (dsi._parameters[i] != _parameters[i]) {
                    return false;
                }
            }

            return true;
        }

        public override int GetHashCode() {
            int hashCode = 5331;

            hashCode ^= _binder.GetHashCode();
            for (int i = 0; i < _parameters.Length; i++) {
                hashCode ^= _parameters[i].GetHashCode();
            }
            hashCode ^= _returnType.GetHashCode();
            return hashCode;
        }

        public override string ToString() {
            StringBuilder text = new StringBuilder();
            text.Append(_returnType.ToString());
            text.Append("(");
            for (int i = 0; i < _parameters.Length; i++) {
                if (i != 0) text.Append(", ");
                text.Append(_parameters[i].ParameterType.Name);
            }
            text.Append("), using ");
            text.Append(_binder.GetType().Name);
            return text.ToString();
        }

        public DelegateInfo GenerateDelegateStub() {
            PerfTrack.NoteEvent(PerfTrack.Categories.DelegateCreate, ToString());

            Type[] delegateParams = new Type[_parameters.Length];
            for (int i = 0; i < _parameters.Length; i++) {
                delegateParams[i] = _parameters[i].ParameterType;
            }

            AssemblyGen snippets = ScriptDomainManager.CurrentManager.Snippets.Assembly;

            // Create new constant pool
            ConstantPool constants = new ConstantPool();

            // Create the method
            CodeGen cg = snippets.DefineMethod(ToString(), _returnType, delegateParams, constants);
            cg.Binder = _binder;

            // Add the space for the delegate target and save the index at which it was placed,
            // most likely zero.
            int targetIndex = constants.Count;
#if DEBUG
            Slot target = constants.AddData(TargetPlaceHolder);
#else
            Slot target = constants.AddData(null);
#endif

            // Add the CodeContext into the constant pool
            Slot context = cg.ConstantPool.AddData(_binder.Context);
            Debug.Assert(typeof(CodeContext).IsAssignableFrom(context.Type));
            cg.ContextSlot = context;

            // Emit the stub
            StubGenerator.EmitClrCallStub(cg, target, 0, StubGenerator.CallType.None);

            // Finish the method
            MethodInfo method = cg.CreateDelegateMethodInfo();

            // Save the constants in the delegate info class
            return new DelegateInfo(method, constants.Data, targetIndex);
        }
    }

    public class DelegateInfo {
        private readonly MethodInfo _method;
        private readonly object[] _constants;
        private readonly int _target;

        internal DelegateInfo(MethodInfo method, object[] constants, int target) {
            Assert.NotNull(method, constants);

            _method = method;
            _constants = constants;
            _target = target;
        }

        public Delegate CreateDelegate(Type delegateType, object target) {
            Assert.NotNull(delegateType, target);

            object[] clone = (object[])_constants.Clone();
#if DEBUG
            Debug.Assert(clone[_target] == DelegateSignatureInfo.TargetPlaceHolder);
#endif
            clone[_target] = target;
            return ReflectionUtils.CreateDelegate(_method, delegateType, clone);
        }
    }
}
