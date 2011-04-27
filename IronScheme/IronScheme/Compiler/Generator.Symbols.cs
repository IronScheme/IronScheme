#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

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
