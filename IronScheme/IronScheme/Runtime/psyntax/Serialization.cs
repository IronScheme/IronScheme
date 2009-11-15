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
using Microsoft.Scripting;
using System.Runtime.Serialization.Formatters.Binary;
using System.IO;
using System.Runtime.Serialization.Formatters;
using System.Runtime.Serialization;
using System.Reflection;
using Microsoft.Scripting.Generation;
using System.Text.RegularExpressions;

namespace IronScheme.Runtime.psyntax
{
  public class Serialization : Builtins
  {
    static Serialization()
    {
      SERIALIZER.AssemblyFormat = FormatterAssemblyStyle.Simple;
      SERIALIZER.Binder = new TypeCorrector();
      SERIALIZER.SurrogateSelector = new Selector();
      AppDomain.CurrentDomain.AssemblyResolve += new ResolveEventHandler(CurrentDomain_AssemblyResolve);
    }

    static Assembly CurrentDomain_AssemblyResolve(object sender, ResolveEventArgs args)
    {
      foreach (Assembly ass in AppDomain.CurrentDomain.GetAssemblies())
      {
        if (ass.FullName == args.Name)
        {
          return ass;
        }
      }
      return null;
    }

    sealed class Selector : SurrogateSelector
    {
      public override ISerializationSurrogate GetSurrogate(Type type, StreamingContext context, out ISurrogateSelector selector)
      {
        if (type == typeof(SymbolId))
        {
          selector = this;
          return surrogate;
        }
        if (type == typeof(bool))
        {
          selector = this;
          return surrogate2;
        }
        return base.GetSurrogate(type, context, out selector);
      }

      static ISerializationSurrogate surrogate = new SymbolSurrogate();
      static ISerializationSurrogate surrogate2 = new BooleanSurrogate();

    }

    sealed class SymbolSurrogate : ISerializationSurrogate
    {
      public void GetObjectData(object obj, SerializationInfo info, StreamingContext context)
      {
        SymbolId s = (SymbolId)obj;
        info.AddValue("symbolName", SymbolTable.IdToString(s));
      }

      public object SetObjectData(object obj, SerializationInfo info, StreamingContext context, ISurrogateSelector selector)
      {
        string value = info.GetString("symbolName");
        int id = SymbolTable.StringToId(value).Id;
        return SymbolTable.GetSymbol(id);
      }
    }

    sealed class BooleanSurrogate : ISerializationSurrogate
    {
      public void GetObjectData(object obj, SerializationInfo info, StreamingContext context)
      {
        bool s = (bool)obj;
        info.AddValue("value", s);
      }

      public object SetObjectData(object obj, SerializationInfo info, StreamingContext context, ISurrogateSelector selector)
      {
        bool value = info.GetBoolean("value");
        return GetBool(value);
      }
    }


    static Assembly FindAssembly(string assname)
    {
      foreach (Assembly ass in AppDomain.CurrentDomain.GetAssemblies())
      {
        if (ass.FullName == assname)
        {
          return ass;
        }
      }
      // this might accidentally load the debug file..., and screw up stuff, dunno why...
      return Assembly.Load(assname);
    }

    sealed class TypeCorrector : SerializationBinder
    {
      readonly static Regex typematch = new Regex(@"record\.[^\.]+\.", RegexOptions.Compiled);

      public override Type BindToType(string assemblyName, string typeName)
      {
        Assembly a = FindAssembly(assemblyName);
        Type tt = a.GetType(typeName, false);

        if (tt == null)
        {
          //fall back for dynamic records
          typeName = typematch.Replace(typeName, string.Empty);
          foreach (var kvp in R6RS.Records.typedescriptors)
          {
            var t = kvp.Key;
            var v = kvp.Value;
            if (typeName == v.Name) // watch for exceptions?
            {
              return t;
            }
          }
          return AssertionViolation(FALSE, "could not find type to deserialize", assemblyName, typeName) as Type;
        }
        else
        {
          return tt;
        }
      }
    }

    internal static readonly BinaryFormatter SERIALIZER = new BinaryFormatter();
  }
}
