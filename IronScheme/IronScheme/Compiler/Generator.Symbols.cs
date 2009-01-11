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

namespace IronScheme.Compiler
{
  partial class Generator
  {
    internal static readonly object quote = SymbolTable.StringToObject("quote");
    static readonly object unquote_splicing = SymbolTable.StringToObject("unquote-splicing");
    internal static readonly object quasiquote = SymbolTable.StringToObject("quasiquote");
    internal static readonly object unquote = SymbolTable.StringToObject("unquote");

    readonly static SymbolId Anonymous = SymbolTable.StringToId("anon");

    internal static object define = SymbolTable.StringToObject("define");
    internal static object lambda = SymbolTable.StringToObject("lambda");
    internal static object set = SymbolTable.StringToObject("set!");
  }
}
