#region License
/* Copyright (c) 2007-2013 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Reflection;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters;
using System.Runtime.Serialization.Formatters.Binary;
using System.Text.RegularExpressions;
using Microsoft.Scripting;
using System.IO;

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

      // last chance
      var fn = Path.Combine(Environment.CurrentDirectory, args.Name + ".dll");

      try
      {
        var a = Assembly.LoadFrom(fn);
        return a;
      }
      catch (FileNotFoundException)
      {
        return null;
      }
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

    static bool IsILMerged
    {
      get { return typeof(Builtins).Assembly.GetType("#", false) != null; }
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
      if (IsILMerged)
      {
        //return BootfileAssembly;
      }
      // this might accidentally load the debug file..., and screw up stuff, dunno why...
      try
      {
        return Assembly.Load(assname);
      }
        // deal with public key shit, starting to regret this...
      catch (FileLoadException)
      {
        return Assembly.Load(assname.Replace("PublicKeyToken=null", "PublicKeyToken=78f2e9d9541a0dee"));
      }
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
