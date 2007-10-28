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
