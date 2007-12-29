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

%namespace IronScheme.Compiler
%using IronScheme.Runtime
%using Microsoft.Scripting
%{

public Cons parsed;

static Cons Last(Cons c)
{
  while (c.cdr != null)
  {
    c = c.cdr as Cons;
  }
  return c;
}

static Cons Append(Cons c, Cons t)
{
  if (c == null || c.car == Ignore)
  {
    return t;
  }
  if (t == null || t.car == Ignore)
  {
    return c;
  }
  Last(c).cdr = t;
  return c;
}

public static Dictionary<object,SourceSpan> sourcemap = new Dictionary<object,SourceSpan>();

static SourceSpan GetLocation(gppg.LexLocation start, gppg.LexLocation end)
{
  return new SourceSpan(
    new SourceLocation(1, start.sLin, start.sCol + 1),
    new SourceLocation(1, end.eLin, end.eCol + 1));
}

protected override SourceSpan GetLocation(gppg.LexLocation loc)
{
  return new SourceSpan(
    new SourceLocation(1, loc.sLin, loc.sCol + 1),
    new SourceLocation(1, loc.eLin, loc.eCol + 1));
}

static object SetLocation(object o, gppg.LexLocation start, gppg.LexLocation end)
{
  if (o == null)
  {
    return null;
  }
  sourcemap[o] = GetLocation(start, end);
  return o;
}

static Cons SetLocation(Cons o, gppg.LexLocation start, gppg.LexLocation end)
{
  if (o == null)
  {
    return null;
  }
  sourcemap[o] = GetLocation(start, end);
  return o;
}


static string CleanString(string input)
{
  input = input.Substring(1, input.Length - 2);
  input = input.Replace("\\\\", "\\");
  input = input.Replace("\\\"", "\"");
  input = input.Replace("\\r", "\r");
  input = input.Replace("\\n", "\n");
  input = input.Replace("\\t", "\t");
  return input;
}

static readonly object Ignore = new object();
static readonly SymbolId quote = SymbolTable.StringToId("quote");
static readonly SymbolId unquote_splicing = SymbolTable.StringToId("unquote-splicing");
static readonly SymbolId quasiquote = SymbolTable.StringToId("quasiquote");
static readonly SymbolId unquote = SymbolTable.StringToId("unquote");
static readonly SymbolId syntax = SymbolTable.StringToId("syntax");
static readonly SymbolId unsyntax_splicing = SymbolTable.StringToId("unsyntax-splicing");
static readonly SymbolId quasisyntax = SymbolTable.StringToId("quasisyntax");
static readonly SymbolId unsyntax = SymbolTable.StringToId("unsyntax");

%} 

%union
{
  public Cons list;
  public object elem;
  public string text;
}

%token LBRACE RBRACE LBRACK RBRACK QUOTE QUASIQUOTE UNQUOTE UNQUOTESPLICING VECTORLBRACE DOT BYTEVECTORLBRACE
%token UNSYNTAX SYNTAX UNSYNTAXSPLICING QUASISYNTAX IGNOREDATUM
%token <text> SYMBOL LITERAL STRING NUMBER CHARACTER 

%type <list> exprlist list file
%type <elem> expr specexpr

%start file

%%

file 
    : exprlist                                    { parsed = $1; }
    ;
    
list
    : LBRACE exprlist RBRACE                      { $$ = SetLocation($2,@1,@3); }
    | LBRACK exprlist RBRACK                      { $$ = SetLocation($2,@1,@3); }
    | LBRACE exprlist expr DOT expr RBRACE        { $$ = SetLocation(Append($2, new Cons($3,$5)),@1,@6); } 
    | specexpr expr                               { $$ = SetLocation(new Cons($1, new Cons($2)), @1, @1); }
    ;

exprlist
    :                                             { $$ = null; }
    |  exprlist expr                              { $$ = Append($1,new Cons($2)); }
    ;       
    
expr
    : list                                        { $$ = $1;}
    | SYMBOL                                      { $$ = SymbolTable.StringToId($1); }
    | STRING                                      { $$ = CleanString($1); }
    | NUMBER                                      { $$ = Builtins.StringToNumber($1);}
    | LITERAL                                     { $$ = $1 == "#t" ? (object)true : ($1 == "#f" ? (object)false : null);}
    | CHARACTER                                   { $$ = $1[0];}
    | VECTORLBRACE exprlist RBRACE                { $$ = SetLocation(Builtins.ListToVector($2),@1,@3);}
    | BYTEVECTORLBRACE exprlist RBRACE            { $$ = SetLocation(Builtins.ListToByteVector($2),@1,@3); }
    | IGNOREDATUM expr                            { $$ = Ignore; }
    ; 

specexpr
    : QUOTE                                       { $$ = quote;}
    | UNQUOTESPLICING                             { $$ = unquote_splicing; }
    | QUASIQUOTE                                  { $$ = quasiquote; }
    | UNQUOTE                                     { $$ = unquote; }
    | SYNTAX                                      { $$ = syntax;}
    | UNSYNTAXSPLICING                            { $$ = unsyntax_splicing; }
    | QUASISYNTAX                                 { $$ = quasisyntax; }
    | UNSYNTAX                                    { $$ = unsyntax; }
    
    ;    

%%









    