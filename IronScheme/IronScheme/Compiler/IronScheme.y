/* Copyright (c) 2007-2014 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */

%namespace IronScheme.Compiler
%using IronScheme.Runtime
%using IronScheme.Runtime.psyntax
%using Microsoft.Scripting
%{

public bool skipnumbers = false;
bool FoldCase = false;

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
  if (c == null)
  {
    return t;
  }
  if (t == null)
  {
    return c;
  }
  Last(c).cdr = t;
  return c;
}

static Cons Append(Cons c, Cons t, object end)
{
  if (c == null)
  {
    Last(t).cdr = end;
    return t;
  }
  else if (t == null)
  {
    Last(c).cdr = end;
  }
  else
  {
    Last(t).cdr = end;
    Last(c).cdr = t;
  }
  return c;
}

static SourceSpan GetLocation(gppg.LexLocation start, gppg.LexLocation end)
{
  int ecol = end.eCol + 1;
  if (ecol <= 0)
  {
    ecol = 1;
  }
  return new SourceSpan(
    new SourceLocation(1, start.sLin, start.sCol + 1),
    new SourceLocation(1, Math.Max(end.eLin, start.sLin), Math.Max(ecol, start.eCol + 1)));
}

protected override SourceSpan GetLocation(gppg.LexLocation loc)
{
  return new SourceSpan(
    new SourceLocation(1, loc.sLin, loc.sCol + 1),
    new SourceLocation(1, loc.eLin, loc.eCol + 1));
}

static object MakeNumber(string input)
{
  object n = Builtins.StringToNumber(input);
  if (n == Builtins.FALSE)
  {
    throw new SyntaxErrorException(string.Format("number could not be parsed: {0}", input));
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

static object ParseBoolean(string s)
{
  switch (s)
  {
    case "#t":
    case "#true":
      return Builtins.TRUE;
    case "#f":
    case "#false":
      return Builtins.FALSE;
    default:
      return null;      
  }
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

%token LBRACE RBRACE LBRACK RBRACK QUOTE QUASIQUOTE UNQUOTE UNQUOTESPLICING VECTORLBRACE DOT VALUEVECTORLBRACE
%token UNSYNTAX SYNTAX UNSYNTAXSPLICING QUASISYNTAX IGNOREDATUM FOLDCASE NOFOLDCASE DIRECTIVE
%token <text> SYMBOL LITERAL STRING NUMBER CHARACTER MLSTRING
%token maxParseToken COMMENT

%type <text> mlstring
%type <lst> exprlist file listcont
%type <list> list 
%type <elem> expr specexpr ignore

%start file

%%

file 
    : exprlist                                    { parsed = $1; }
    ;
    
list
    : LBRACE listcont RBRACE                      { $$ = AnnotateList($2,@1,@3); }
    | LBRACK listcont RBRACK                      { $$ = AnnotateList($2,@1,@3); }
    | specexpr expr                               { $$ = AnnotateList(new Cons($1, new Cons($2)), @1, @2); }
    ;
    
listcont
    :                                             { $$ = null; }
    | ignore                                      { $$ = null; }
    | expr                                        { $$ = $1 == Ignore ? null : new Cons($1); }
    | expr DOT expr                               { $$ = new Cons($1, $3); }
    | expr DOT expr DOT expr exprlist             { $$ = new Cons($3, new Cons($1, new Cons($5, $6))); }
    | expr expr exprlist                          { $$ = new Cons($1, new Cons($2, $3)); }
    | expr expr exprlist DOT expr                 { $$ = Append(new Cons($1, new Cons($2)), $3, $5); }
    ;

exprlist
    :                                             { $$ = null; }
    |  exprlist expr                              { $$ = Append($1,new Cons($2)); }
    ;       
    

mlstring
    :  MLSTRING                                   { $$ = $1; }
    |  mlstring MLSTRING                          { $$ = $1 + $2; }
    ;     
    
expr
    : list                                        { $$ = $1;}
    | mlstring                                    { $$ = Annotate(Helper.CleanString($1), @1); }
    | SYMBOL                                      { $$ = Annotate( SymbolTable.StringToObjectWithCase($1, FoldCase), @1); }
    | STRING                                      { $$ = Annotate(Helper.CleanString($1), @1); }
    | NUMBER                                      { $$ = Annotate( skipnumbers ? null : MakeNumber($1), @1);}
    | LITERAL                                     { $$ = Annotate( ParseBoolean($1), @1);}
    | CHARACTER                                   { $$ = Annotate($1[0], @1);}
    | VECTORLBRACE exprlist RBRACE                { $$ = Annotate(Builtins.ListToVector($2),@1,@3);}
    | VALUEVECTORLBRACE exprlist RBRACE           { $$ = Annotate(Helper.ListToVector(this, $1.text, Strip($2)),@1,@3); }
    | expr ignore                                 { $$ = $1; }
    | ignore expr                                 { $$ = $2; } // this causes a lot of S/R conflicts, but seemingly ok
    ; 
    
ignore
    : IGNOREDATUM expr                            { $$ = Ignore; }
    | DIRECTIVE                                   { $$ = Ignore; }
    | FOLDCASE                                    { FoldCase = true; $$ = Ignore; }
    | NOFOLDCASE                                  { FoldCase = false; $$ = Ignore; }
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









    