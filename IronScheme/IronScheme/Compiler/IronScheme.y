/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Permissive License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Permissive License.
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
  while (c.Cdr != null)
  {
    c = c.Cdr as Cons;
  }
  return c;
}

static Cons Append(Cons c, Cons t)
{
  if (c == null)
  {
    return t;
  }
  if (t == null)
  {
    return c;
  }
  Last(c).Cdr = t;
  return c;
}

public static Dictionary<Cons,SourceSpan> sourcemap = new Dictionary<Cons,SourceSpan>();

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

static Cons SetLocation(Cons o, gppg.LexLocation start, gppg.LexLocation end)
{
  if (o == null)
  {
    return null;
  }
  sourcemap[o] = GetLocation(start, end);
  return o;
}

static readonly SymbolId quote = SymbolTable.StringToId("quote");
static readonly SymbolId unquote_splicing = SymbolTable.StringToId("unquote-splicing");
static readonly SymbolId quasiquote = SymbolTable.StringToId("quasiquote");
static readonly SymbolId unquote = SymbolTable.StringToId("unquote");

%} 

%union
{
  public Cons list;
  public object elem;
  public string text;
}

%token LBRACE RBRACE LBRACK RBRACK QUOTE QUASIQUOTE UNQUOTE UNQUOTESPLICING VECTORLBRACE DOT
%token <text> SYMBOL LITERAL STRING INTEGER REAL CHARACTER 

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
    | specexpr expr                               { $$ = new Cons($1, new Cons($2)); }
    ;

exprlist
    :                                             { $$ = null; }
    |  exprlist expr                              { $$ = Append($1,new Cons($2)); }
    ;       
    
expr
    : list                                        { $$ = $1;}
    | SYMBOL                                      { $$ = SymbolTable.StringToId($1); }
    | STRING                                      { $$ = $1.Trim('"'); }
    | INTEGER                                     { $$ = Convert.ToInt32($1);}
    | LITERAL                                     { $$ = $1 == "#t" ? (object)true : ($1 == "#f" ? (object)false : null);}
    | REAL                                        { $$ = Convert.ToDouble($1);}
    | CHARACTER                                   { $$ = $1[0];}
    | VECTORLBRACE exprlist RBRACE                { $$ = Builtins.ListToVector($2);}
    ; 

specexpr
    : QUOTE                                       { $$ = quote;}
    | UNQUOTESPLICING                             { $$ = unquote_splicing; }
    | QUASIQUOTE                                  { $$ = quasiquote; }
    | UNQUOTE                                     { $$ = unquote; }
    ;    

%%









    