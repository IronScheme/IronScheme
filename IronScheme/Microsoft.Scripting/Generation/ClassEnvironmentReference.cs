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
using System.Diagnostics;
using Microsoft.Scripting.Generation.Slots;
using Microsoft.Scripting.Generation.Allocators;

namespace Microsoft.Scripting.Generation
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
}
