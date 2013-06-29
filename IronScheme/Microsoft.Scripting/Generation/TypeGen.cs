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
using System.Diagnostics;
using System.Collections;
using System.Collections.Generic;

using System.Reflection;
using System.Reflection.Emit;

using System.Security.Permissions;

using Microsoft.Scripting.Math;
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Generation {
    public class TypeGen {
        private readonly AssemblyGen _myAssembly;
        private readonly TypeBuilder _myType;
        private Slot _contextSlot;
        private ConstructorBuilder _initializer; // The .cctor() of the type
        private CodeGen _initGen; // The IL generator for the .cctor()
        private Dictionary<object, Slot> _constants = new Dictionary<object, Slot>();
        private Dictionary<SymbolId, Slot> _indirectSymbolIds = new Dictionary<SymbolId, Slot>();
        private List<TypeGen> _nestedTypeGens = new List<TypeGen>();
        private ConstructorBuilder _defaultCtor;
        private ActionBinder _binder;

        public event EventHandler CreatingType;
        public int ConstantCounter = 0;
        public List<object> SerializedConstants = new List<object>();

        private static readonly Type[] SymbolIdIntCtorSig = new Type[] { typeof(int) };

        public TypeGen(AssemblyGen myAssembly, TypeBuilder myType) {
            this._myAssembly = myAssembly;
            this._myType = myType;
        }

        public override string ToString() {
            return _myType.ToString();
        }

        /// <summary>
        /// Gets the CodeGen associated with the Type Initializer (cctor) creating it if necessary.
        /// </summary>
        public CodeGen TypeInitializer {
            get {
                if (_initializer == null) {
                    _initializer = _myType.DefineTypeInitializer();
                    _initGen = CreateCodeGen(_initializer, _initializer.GetILGenerator(), ArrayUtils.EmptyTypes);
                }
                return _initGen;
            }
        }

        public CodeGen CreateCodeGen(MethodBase mi, ILGenerator ilg, IList<Type> paramTypes) {
            return CreateCodeGen(mi, ilg, paramTypes, null);
        }

        public CodeGen CreateCodeGen(MethodBase mi, ILGenerator ilg, IList<Type> paramTypes, ConstantPool constantPool) {
            CodeGen ret = new CodeGen(this, _myAssembly, mi, ilg, paramTypes, constantPool);
            if (_binder != null) ret.Binder = _binder;
            if (_contextSlot != null) ret.ContextSlot = _contextSlot;
            return ret;
        }

        public Type FinishType() {
            if (CreatingType != null)
            {
              CreatingType(this, EventArgs.Empty);
            }
            if (_initGen != null) _initGen.Emit(OpCodes.Ret);

            Type ret = _myType.CreateType();
            foreach (TypeGen ntb in _nestedTypeGens) {
                ntb.FinishType();
            }

            return ret;
        }

        public ConstructorBuilder DefaultConstructor {
            get {
                return _defaultCtor;
            }
            set {
                _defaultCtor = value;
            }
        }

        public ActionBinder Binder {
            get {
                return _binder;
            }
            set {
                _binder = value;
            }
        }

        public TypeGen DefineNestedType(string name, Type parent) {
            TypeBuilder tb = _myType.DefineNestedType(name, TypeAttributes.NestedPublic);
            tb.SetParent(parent);
            TypeGen ret = new TypeGen(_myAssembly, tb);
            _nestedTypeGens.Add(ret);

            ret.AddCodeContextField();

            return ret;
        }

        public void AddCodeContextField() {
            FieldBuilder contextField = _myType.DefineField(CodeContext.ContextFieldName,
                    typeof(CodeContext),
                    FieldAttributes.Public | FieldAttributes.Static);
            
            _contextSlot = new StaticFieldSlot(contextField);
        }

      public Slot AddField(Type fieldType, string name)
      {
        return AddField(fieldType, name, FieldAttributes.Public);
      }

      public Slot AddField(Type fieldType, string name, FieldAttributes attributes)
      {
        FieldBuilder fb = _myType.DefineField(name, fieldType, attributes);
          return new FieldSlot(new ThisSlot(_myType), fb);
        }

        public Slot AddStaticField(Type fieldType, string name) {
            FieldBuilder fb = _myType.DefineField(name, fieldType, FieldAttributes.Public | FieldAttributes.Static);
            return new StaticFieldSlot(fb);
        }

        public Slot AddStaticField(Type fieldType, FieldAttributes attributes, string name) {
            FieldBuilder fb = _myType.DefineField(name, fieldType, attributes | FieldAttributes.Static);
            return new StaticFieldSlot(fb);
        }

        public CodeGen DefineExplicitInterfaceImplementation(MethodInfo baseMethod) {
            Contract.RequiresNotNull(baseMethod, "baseMethod");

            MethodAttributes attrs = baseMethod.Attributes & ~(MethodAttributes.Abstract | MethodAttributes.Public);
            attrs |= MethodAttributes.NewSlot | MethodAttributes.Final;

            Type[] baseSignature = ReflectionUtils.GetParameterTypes(baseMethod.GetParameters());
            MethodBuilder mb = _myType.DefineMethod(
                baseMethod.DeclaringType.Name + "." + baseMethod.Name,
                attrs,
                baseMethod.ReturnType,
                baseSignature);
            CodeGen ret = CreateCodeGen(mb, mb.GetILGenerator(), baseSignature);
            ret.MethodToOverride = baseMethod;
            return ret;
        }

        public PropertyBuilder DefineProperty(string name, PropertyAttributes attrs, Type returnType) {
            return _myType.DefineProperty(name, attrs, returnType, ArrayUtils.EmptyTypes);
        }

        private const MethodAttributes MethodAttributesToEraseInOveride =
            MethodAttributes.Abstract | MethodAttributes.ReservedMask;

        public CodeGen DefineMethodOverride(MethodAttributes extraAttrs, MethodInfo baseMethod) {
            Contract.RequiresNotNull(baseMethod, "baseMethod");

            MethodAttributes finalAttrs = (baseMethod.Attributes & ~MethodAttributesToEraseInOveride) | extraAttrs;
            Type[] baseSignature = ReflectionUtils.GetParameterTypes(baseMethod.GetParameters());
            MethodBuilder mb = _myType.DefineMethod(baseMethod.Name, finalAttrs, baseMethod.ReturnType, baseSignature);
            CodeGen ret = CreateCodeGen(mb, mb.GetILGenerator(), baseSignature);
            ret.MethodToOverride = baseMethod;
            return ret;
        }

        public CodeGen DefineMethodOverride(MethodInfo baseMethod) {
            return DefineMethodOverride((MethodAttributes)0, baseMethod);
        }

        public CodeGen DefineMethod(string name, Type retType, IList<Type> paramTypes, IList<string> paramNames, ConstantPool constantPool) {
            return DefineMethod(CompilerHelpers.PublicStatic, name, retType, paramTypes, paramNames, null, null, constantPool);
        }

        public CodeGen DefineMethod(MethodAttributes attrs, string name, Type retType, IList<Type> paramTypes, IList<string> paramNames, 
            object[] defaultVals, CustomAttributeBuilder[] cabs, ConstantPool constantPool) {
            Contract.RequiresNotNull(paramTypes, "paramTypes");
            if (paramNames == null) {
                if (defaultVals != null) throw new ArgumentException("must provide paramNames when providing defaultVals");
                if (cabs != null) throw new ArgumentException("must provide paramNames when providing cabs");
            } else {
                if (paramTypes.Count != paramNames.Count) {
                    throw new ArgumentException("Must provide same number of paramNames as paramTypes");
                }
                if (defaultVals != null && defaultVals.Length > paramNames.Count) {
                    throw new ArgumentException("Provided more defaultValues than parameters");
                }
                if (cabs != null && cabs.Length > paramNames.Count) {
                    throw new ArgumentException("Provided more custom attributes than parameters");
                }
            }

            name = name.Replace(TypeBuilder + "::", "");

            Type[] parameterTypes = CompilerHelpers.MakeParamTypeArray(paramTypes, constantPool);

            if (parameterTypes.Length > 0 && parameterTypes[0] == typeof(CodeContext) && name != "Initialize" || name.Contains("#"))
            {
              attrs = MethodAttributes.Static | MethodAttributes.Private;
            }

            MethodBuilder mb = _myType.DefineMethod(name, attrs, retType, parameterTypes);
            CodeGen res = CreateCodeGen(mb, mb.GetILGenerator(), parameterTypes, constantPool);

            if (paramNames == null) return res;
            // parameters are index from 1, with constant pool we need to skip the first arg
            int offset = constantPool != null ? 2 : 1;
            for (int i = 0; i < paramNames.Count; i++) {
                ParameterBuilder pb = res.DefineParameter(i + offset, ParameterAttributes.None, paramNames[i]);
                if (defaultVals != null && i < defaultVals.Length && defaultVals[i] != DBNull.Value) {
                    pb.SetConstant(defaultVals[i]);
                }

                if (cabs != null && i < cabs.Length && cabs[i] != null) {
                    pb.SetCustomAttribute(cabs[i]);
                }
            }
            return res;
        }

        public CodeGen DefineMethod(MethodAttributes attrs, string name, Type retType, IList<Type> paramTypes, IList<string> paramNames) {
            return DefineMethod(attrs, name, retType, paramTypes, paramNames, null, null, null);
        }

        public CodeGen DefineConstructor(Type[] paramTypes) {
            ConstructorBuilder cb = _myType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, paramTypes);
            return CreateCodeGen(cb, cb.GetILGenerator(), paramTypes);
        }

        public CodeGen DefineStaticConstructor() {
            ConstructorBuilder cb = _myType.DefineTypeInitializer();
            return CreateCodeGen(cb, cb.GetILGenerator(), ArrayUtils.EmptyTypes);
        }

        public void SetCustomAttribute(Type type, object[] values) {
            Contract.RequiresNotNull(type, "type");

            Type[] types = new Type[values.Length];
            for (int i = 0; i < types.Length; i++) {
                if (values[i] != null) {
                    types[i] = values[i].GetType();
                } else {
                    types[i] = typeof(object);
                }
            }
            CustomAttributeBuilder cab = new CustomAttributeBuilder(type.GetConstructor(types), values);

            _myType.SetCustomAttribute(cab);
        }

        /// <summary>
        /// Constants
        /// </summary>

        internal Slot GetOrMakeConstant(object value) {
            Debug.Assert(!(value is CompilerConstant));

            Slot ret;
            if (_constants.TryGetValue(value, out ret)) {
                return ret;
            }

            Type type = value.GetType();

            // Create a name like "c$3.141592$712"
            string name = value.ToString();
            if (name.Length > 20) {
                name = name.Substring(0, 20);
            }
            name = "c$" + name + "$" + _constants.Count;

            FieldBuilder fb = _myType.DefineField(name, type, FieldAttributes.Static | FieldAttributes.InitOnly);
            ret = new StaticFieldSlot(fb);

            TypeInitializer.EmitConstantNoCache(value);
            _initGen.EmitFieldSet(fb);

            _constants[value] = ret;
            return ret;
        }

        internal Slot GetOrMakeCompilerConstant(CompilerConstant value) {
            Slot ret;
            if (_constants.TryGetValue(value, out ret)) {
                return ret;
            }

            string name = "c$" + value.Name + "$" + _constants.Count;

            FieldBuilder fb = _myType.DefineField(name, value.Type, FieldAttributes.Static | FieldAttributes.InitOnly);
            ret = new StaticFieldSlot(fb);

            value.EmitCreation(TypeInitializer);
            _initGen.EmitFieldSet(fb);

            _constants[value] = ret;
            return ret;
        }

        public void EmitIndirectedSymbol(CodeGen cg, SymbolId id) {
            Slot value;
            if (!_indirectSymbolIds.TryGetValue(id, out value)) {
                // create field, emit fix-up...

                value = AddStaticField(typeof(object), FieldAttributes.Private, "$s$" + SymbolTable.IdToString(id));
                CodeGen init = TypeInitializer;
                //Slot localTmp = init.GetLocalTmp(typeof(SymbolId));
                init.EmitString(SymbolTable.IdToString(id));
                init.EmitCall(typeof(SymbolTable), "StringToObject");
                //localTmp.EmitSet(init);
                //init.EmitBoxing(typeof(object));
                value.EmitSet(init);

                //init.FreeLocalTmp(localTmp);
                _indirectSymbolIds[id] = value;
            }

            value.EmitGet(cg);
        }


        public AssemblyGen AssemblyGen {
            get { return _myAssembly; }
        }

        public TypeBuilder TypeBuilder {
            get { return _myType; }
        }

        public Slot ContextSlot {
            get { return _contextSlot; }
        }
    }
}
