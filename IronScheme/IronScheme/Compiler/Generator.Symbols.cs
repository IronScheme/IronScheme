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
    internal static readonly SymbolId quote = SymbolTable.StringToId("quote");
    static readonly SymbolId unquote_splicing = SymbolTable.StringToId("unquote-splicing");
    internal static readonly SymbolId quasiquote = SymbolTable.StringToId("quasiquote");
    internal static readonly SymbolId unquote = SymbolTable.StringToId("unquote");

    readonly static SymbolId Anonymous = SymbolTable.StringToId("anon");

    internal static SymbolId define = SymbolTable.StringToId("define");
    internal static SymbolId lambda = SymbolTable.StringToId("lambda");
    internal static SymbolId set = SymbolTable.StringToId("set!");
  }
}
