#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Text.RegularExpressions;
using Microsoft.Scripting;

namespace IronScheme.Runtime.psyntax
{
  public class Serialization : Builtins
  {
    readonly static Regex typematch = new Regex(@"record\.[^\.]+\.", RegexOptions.Compiled);

    private static Type RecordBinder(string assemblyName, string typeName)
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
    
    internal static readonly ISerializer SERIALIZER = PAL.GetSerializer(RecordBinder);
  }
}
