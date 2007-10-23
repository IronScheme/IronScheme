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
