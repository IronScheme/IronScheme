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
using System.Collections.Generic;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Generation {
    /// <summary>
    /// Environment factory which constructs storage into a tuple-backed FunctionEnvironment.
    /// 
    /// Used for environments with less than 128 members.
    /// </summary>
    class PropertyEnvironmentFactory : EnvironmentFactory {
        private Type _type;
        private Type _envType;
        private int _index;

        /// <summary>
        /// Creates a new PropertyEnvironmentFactory backed by the specified type of tuple and
        /// FunctionEnvironment.
        /// </summary>
        public PropertyEnvironmentFactory(Type tupleType, Type envType) {
            ValidateTupleType(tupleType);

            _type = tupleType;
            _envType = envType;

            // 1st entry always points back to our dictionary            
            _index = 1;
        }

        public override Type EnvironmentType {
            get { return _envType; }
        }

        public override Type StorageType {
            get {
                return _type;
            }
        }

        public override Storage MakeEnvironmentReference(SymbolId name, Type type) {
            return new PropertyEnvironmentReference(_type, _index++, type);
        }

        protected int Index {
            get { return _index; }
            set { _index = value; }
        }

        public override void EmitStorage(CodeGen cg) {
            cg.EmitNew(StorageType.GetConstructor(ArrayUtils.EmptyTypes));
            //cg.Emit(OpCodes.Dup);
            //EmitNestedTupleInit(cg, StorageType);
        }

        private static void EmitNestedTupleInit(CodeGen cg, Type storageType)
        {
          if (Tuple.GetSize(storageType) > Tuple.MaxSize)
          {
            
            Slot tmp = cg.GetLocalTmp(storageType);
            tmp.EmitSet(cg);

            Type[] nestedTuples = storageType.GetGenericArguments();
            for (int i = 0; i < nestedTuples.Length; i++)
            {
              Type t = nestedTuples[i];
              if (t.IsSubclassOf(typeof(Tuple)))
              {
                tmp.EmitGet(cg);

                cg.EmitNew(t.GetConstructor(ArrayUtils.EmptyTypes));
                cg.EmitPropertySet(storageType, String.Format("Item{0:D3}", i));

                tmp.EmitGet(cg);
                cg.EmitPropertyGet(storageType, String.Format("Item{0:D3}", i));

                EmitNestedTupleInit(cg, t);
              }
            }

            cg.FreeLocalTmp(tmp);
          }
          else
          {
            int capacity = 0;
            foreach (Type t in storageType.GetGenericArguments())
            {
              if (t == typeof(None))
              {
                break;
              }
              capacity++;
            }
            cg.EmitInt(capacity);
            cg.EmitCall(typeof(RuntimeHelpers), "UninitializeEnvironmentTuple");
          }
        }

        public override void EmitNewEnvironment(CodeGen cg) {
            ConstructorInfo ctor = EnvironmentType.GetConstructor(
              ScriptDomainManager.Options.DebugMode ?
                new Type[] {
                    StorageType,
                    typeof(SymbolId[]),
                    } :
                new Type[] {
                    StorageType,
                });

            // emit: dict.Tuple[.Item000...].Item000 = dict, and then leave dict on the stack

            cg.EmitNew(ctor);
            cg.Emit(OpCodes.Dup);

            Slot tmp = cg.GetLocalTmp(EnvironmentType);
            tmp.EmitSet(cg);

            cg.EmitPropertyGet(EnvironmentType, "TupleData");

            PropertyInfo last = null;
            foreach (PropertyInfo pi in Tuple.GetAccessPath(StorageType, 0)) {
                if (last != null) {
                    cg.EmitPropertyGet(last);
                }
                last = pi;
            }

            tmp.EmitGet(cg);
            cg.EmitPropertySet(last);

            cg.FreeLocalTmp(tmp);
        }

        public override void EmitGetStorageFromContext(CodeGen cg) {
            cg.EmitCodeContext();
            cg.EmitPropertyGet(typeof(CodeContext), "Scope");
            cg.EmitCall(typeof(RuntimeHelpers).GetMethod("GetTupleDictionaryData").MakeGenericMethod(StorageType));
        }

        public override EnvironmentSlot CreateEnvironmentSlot(CodeGen cg) {
            return new FunctionEnvironmentSlot(cg.GetNamedLocal(StorageType, "$environment"), StorageType);
        }

        [Conditional("DEBUG")]
        private static void ValidateTupleType(Type type) {
            Type curType = type.BaseType;
            while (curType != typeof(Tuple)) {
                Debug.Assert(curType != typeof(object));
                curType = curType.BaseType;
            }
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
      cg.EmitCall(typeof(RuntimeHelpers).GetMethod("GetStorageData").MakeGenericMethod(StorageType));
    }

    public override EnvironmentSlot CreateEnvironmentSlot(CodeGen cg)
    {
      return new ClassEnvironmentSlot(cg.GetNamedLocal(StorageType, "$environment"), StorageType);
    }
  }
}
