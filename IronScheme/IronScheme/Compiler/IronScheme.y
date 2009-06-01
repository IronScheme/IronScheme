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
%using IronScheme.Runtime.psyntax
%using Microsoft.Scripting
%{

public bool skipnumbers = false;

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
  int ecol = end.eCol + 1;
  if (ecol <= 0)
  {
    ecol = 1;
  }
  return new SourceSpan(
    new SourceLocation(1, start.sLin, start.sCol + 1),
    new SourceLocation(1, end.eLin, ecol));
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

static object MakeNumber(string input)
{
  object n = Builtins.StringToNumber(input);
  if (!Builtins.IsTrue(n))
  {
    Builtins.LexicalError("number could not be parsed", input);
  }
  return n;
}

static Annotation Annotate(object obj, gppg.LexLocation start, gppg.LexLocation end)
{
  return Annotate(obj, GetLocation(start,end));
}

static Annotation Annotate(object obj, gppg.LexLocation start)
{
  return Annotate(obj, GetLocation(start, start));
}

static Annotation Annotate(object obj, SourceSpan loc)
{
  return AnnotationHelper.Annotate(obj, loc);
}


static ConsAnnotation AnnotateList(Cons obj, gppg.LexLocation start, gppg.LexLocation end)
{
  return AnnotationHelper.AnnotateList(obj, GetLocation(start,end));
}

static Cons Strip(Cons c)
{
  return AnnotationHelper.Strip(c);
}

static readonly Annotation Ignore = new Annotation(null,null,null);
static readonly object quote = SymbolTable.StringToObject("quote");
static readonly object unquote_splicing = SymbolTable.StringToObject("unquote-splicing");
static readonly object quasiquote = SymbolTable.StringToObject("quasiquote");
static readonly object unquote = SymbolTable.StringToObject("unquote");
static readonly object syntax = SymbolTable.StringToObject("syntax");
static readonly object unsyntax_splicing = SymbolTable.StringToObject("unsyntax-splicing");
static readonly object quasisyntax = SymbolTable.StringToObject("quasisyntax");
static readonly object unsyntax = SymbolTable.StringToObject("unsyntax");

%} 

%union
{
  public Cons lst;
  internal ConsAnnotation list;
  internal Annotation elem;
  public string text;
}

%token LBRACE RBRACE LBRACK RBRACK QUOTE QUASIQUOTE UNQUOTE UNQUOTESPLICING VECTORLBRACE DOT BYTEVECTORLBRACE
%token UNSYNTAX SYNTAX UNSYNTAXSPLICING QUASISYNTAX IGNOREDATUM
%token <text> SYMBOL LITERAL STRING NUMBER CHARACTER 

%type <lst> exprlist file
%type <list> list 
%type <elem> expr specexpr

%start file

%%

file 
    : exprlist                                    { parsed = $1; }
    ;
    
list
    : LBRACE exprlist RBRACE                      { $$ = AnnotateList($2,@1,@3); }
    | LBRACK exprlist RBRACK                      { $$ = AnnotateList($2,@1,@3); }
    | LBRACE exprlist expr DOT expr RBRACE        { $$ = AnnotateList(Append($2, new Cons($3,$5)),@1, @6); } 
    | LBRACK exprlist expr DOT expr RBRACK        { $$ = AnnotateList(Append($2, new Cons($3,$5)),@1, @6); } 
    | specexpr expr                               { $$ = AnnotateList(new Cons($1, new Cons($2)), @1, @2); }
    ;

exprlist
    :                                             { $$ = null; }
    |  exprlist expr                              { $$ = Append($1,new Cons($2)); }
    ;       
    
expr
    : list                                        { $$ = $1;}
    | SYMBOL                                      { $$ = Annotate( SymbolTable.StringToObject($1), @1); }
    | STRING                                      { $$ = Annotate(Helper.CleanString($1), @1); }
    | NUMBER                                      { $$ = Annotate( skipnumbers ? null : MakeNumber($1), @1);}
    | LITERAL                                     { $$ = Annotate( $1 == "#t" ? Builtins.TRUE : ($1 == "#f" ? Builtins.FALSE : null), @1);}
    | CHARACTER                                   { $$ = Annotate($1[0], @1);}
    | VECTORLBRACE exprlist RBRACE                { $$ = Annotate(Builtins.ListToVector($2),@1,@3);}
    | BYTEVECTORLBRACE exprlist RBRACE            { $$ = Annotate(Builtins.ListToByteVector(Strip($2)),@1,@3); }
    | IGNOREDATUM expr                            { $$ = Ignore; }
    ; 

specexpr
    : QUOTE                                       { $$ = Annotate(quote, @1);}
    | UNQUOTESPLICING                             { $$ = Annotate(unquote_splicing, @1); }
    | QUASIQUOTE                                  { $$ = Annotate(quasiquote, @1); }
    | UNQUOTE                                     { $$ = Annotate(unquote, @1); }
    | SYNTAX                                      { $$ = Annotate(syntax, @1);}
    | UNSYNTAXSPLICING                            { $$ = Annotate(unsyntax_splicing, @1); }
    | QUASISYNTAX                                 { $$ = Annotate(quasisyntax, @1); }
    | UNSYNTAX                                    { $$ = Annotate(unsyntax, @1); }
    
    ;    

%%









    