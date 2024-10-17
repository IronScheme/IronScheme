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
using System.Reflection;
using System.Reflection.Emit;
using System.Diagnostics;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Generation.Slots;
using Microsoft.Scripting.Generation.Allocators;

namespace Microsoft.Scripting.Generation.Factories
{
    class ClassEnvironmentReference : Storage
    {
        private SymbolId _name;
        private Type _storageType;
        private Type _type;

        public ClassEnvironmentReference(Type storageType, SymbolId name, Type type)
        {
            Debug.Assert(storageType != null);

            _storageType = storageType;
            _name = name;
            _type = type;
        }


        public override bool RequireAccessSlot
        {
            get { return true; }
        }

        public override Slot CreateSlot(Slot instance)
        {
            var sym = SymbolTable.IdToString(_name);
            Slot s = new FieldSlot(instance, _storageType.GetField(sym));
            if (_type != s.Type)
            {
                s = new CastSlot(s, _type);
            }
            return s;
        }
    }

    class ClassEnvironmentFactory : EnvironmentFactory
    {
        private Type _type;
        private Type _envType;

        public ClassEnvironmentFactory(Type type, Type envType)
        {
            _type = type;
            _envType = envType;
        }

        public override Type EnvironmentType
        {
            get { return _envType; }
        }

        public override Type StorageType
        {
            get { return _type; }
        }

        public override Storage MakeEnvironmentReference(SymbolId name, Type type)
        {
            return new ClassEnvironmentReference(_type, name, type);
        }

        public override void EmitNewEnvironment(CodeGen cg)
        {
            ConstructorInfo ctor = null;

            if (StorageType is TypeBuilder)
            {
                var baseci = EnvironmentType.GetGenericTypeDefinition().GetConstructors()[0];
                ctor = TypeBuilder.GetConstructor(EnvironmentType, baseci);
            }
            else
            {
                ctor = EnvironmentType.GetConstructor(new Type[] { StorageType });
            }

            //ConstructorInfo ctor = EnvironmentType.GetConstructor(new Type[] {StorageType});

            // emit: dict.Tuple[.Item000...].Item000 = dict, and then leave dict on the stack

            cg.EmitNew(ctor);
            cg.Emit(OpCodes.Dup);

            Slot tmp = cg.GetLocalTmp(EnvironmentType);
            tmp.EmitSet(cg);

            cg.EmitPropertyGet(EnvironmentType, "Data");

            var fld = StorageType.GetField("$parent$");

            //cg.EmitFieldGet(fld);

            tmp.EmitGet(cg);
            cg.EmitFieldSet(fld);

            cg.FreeLocalTmp(tmp);
        }

        public override void EmitStorage(CodeGen cg)
        {
            cg.EmitNew(StorageType.GetConstructor(ArrayUtils.EmptyTypes));
        }

        public override void EmitGetStorageFromContext(CodeGen cg)
        {
            cg.EmitCodeContext();
            cg.EmitPropertyGet(typeof(CodeContext), "Scope");
            cg.EmitCall(typeof(RuntimeHelpers).GetMethod(nameof(RuntimeHelpers.GetStorageData)).MakeGenericMethod(StorageType));
        }

        public override EnvironmentSlot CreateEnvironmentSlot(CodeGen cg)
        {
            return new ClassEnvironmentSlot(cg.GetNamedLocal(StorageType, "$environment"), StorageType);
        }
    }
}
