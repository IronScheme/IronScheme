#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/
#endregion

using System;
using System.Collections.Generic;
using System.Text;

namespace IronScheme.Runtime
{
  [AttributeUsage(AttributeTargets.Assembly, AllowMultiple=true)]
  public sealed class ExtensionAttribute : Attribute
  {
    Type generatortype, builtinstype;
    string scriptresource;

    public string ScriptResource
    {
      get { return scriptresource; }
      set { scriptresource = value; }
    }

    public Type BuiltinsType
    {
      get { return builtinstype; }
      set { builtinstype = value; }
    }

    public Type GeneratorType
    {
      get { return generatortype; }
      set { generatortype = value; }
    }
  }
}
