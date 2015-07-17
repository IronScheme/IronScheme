/* Copyright (c) 2007-2015 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */

%namespace IronScheme.Compiler.Numbers
%using IronScheme.Runtime
%using Microsoft.Scripting


%{

public object result;

protected override SourceSpan GetLocation(gppg.LexLocation loc)
{
  return new SourceSpan(
    new SourceLocation(1, loc.sLin, loc.sCol + 1),
    new SourceLocation(1, loc.eLin, loc.eCol + 1));
}

static readonly Dictionary<char, int> charmap = GetCharMap();

static Dictionary<char, int> GetCharMap()
{
  Dictionary<char, int> map = new Dictionary<char,int>();
  map['0'] = 0;
  map['1'] = 1;
  map['2'] = 2;
  map['3'] = 3;
  map['4'] = 4;
  map['5'] = 5;
  map['6'] = 6;
  map['7'] = 7;
  map['8'] = 8;
  map['9'] = 9;
  map['a'] = 10;
  map['b'] = 11;
  map['c'] = 12;
  map['d'] = 13;
  map['e'] = 14;
  map['f'] = 15;

  return map;
}

static int GetNum(string s)
{
  char c = s[0];
  int i;
  if (charmap.TryGetValue(char.ToLowerInvariant(c), out i))
  {
    return i;
  }
  return int.MaxValue;
}

object ApplyExactness(bool? exact, object num)
{
  if (exact == null)
  {
    return (exactr != null) ? Builtins.Inexact(num) : num;
  }
  return exact.Value ? Builtins.Exact(num) : Builtins.Inexact(num);
}

static object Fraction(object num, object den)
{
  try
  {
    Fraction f = new Fraction(Builtins.ConvertToBigInteger(num),Builtins.ConvertToBigInteger(den));
    if (f.Denominator == 1)
    {
      return Builtins.ToIntegerIfPossible(f.Numerator);
    }
    return f;
  }
  catch (DivideByZeroException)
  {
    throw;
  }
}

internal object exactr;

object ConvertToDouble(string s)
{
  exactr = null;
  object r = Helper.ParseReal(s);
  if (r != null)
  {
    exactr = r;
    return r;
  }
  try
  {
    return Convert.ToDouble(s, System.Globalization.CultureInfo.InvariantCulture);
  }
  catch (OverflowException)
  {
    return double.PositiveInfinity;
  }
}

%} 

%union
{
  public bool? exact;
  public object value;
  public string text;
}


%token DIGIT2 DIGIT8 DIGIT10 DIGIT16 RADIX2 RADIX8 RADIX10 RADIX16
%token EXACT INEXACT EXPMARKER NANINF
%token DOT SLASH IMAG AT PLUS MINUS PIPE

%type <value> number uinteger2 uinteger8 uinteger10 uinteger16 decimal10 naninf
%type <value> ureal2 ureal8 ureal10 ureal16 real2 real8 real10 real16 sreal2 sreal8 sreal10 sreal16
%type <value> complex2 complex8 complex10 complex16 num2 num8 num10 num16
%type <text> digit2 digit8 digit10 digit16 digit10x suffix
%type <exact> exactness prefix2 prefix8 prefix10 prefix16

%start number

%%

digit2    : DIGIT2          { $$ = $1.text; }
          ;

digit8    : digit2
          | DIGIT8          { $$ = $1.text; }
          ;

digit10   : digit8
          | DIGIT10         { $$ = $1.text; }
          ;
        
digit16   : digit10
          | DIGIT16         { $$ = $1.text; }
          ;
          
naninf    : NANINF          { $$ = $1.text == "nan.0" ? double.NaN : double.PositiveInfinity; }
          ;          

exactness : /* empty */     { $$ = null; }
          | EXACT           { $$ = true; }
          | INEXACT         { $$ = false; }
          ;

suffix    : /* empty */                 { $$ = string.Empty; }
          | EXPMARKER                   { $$ = "e" + $1.text.Substring(1); /* always e */ }
          ;

prefix2   : RADIX2 exactness            { $$ = $2;}
          | exactness RADIX2            { $$ = $1;}
          ;

prefix8   : RADIX8 exactness            { $$ = $2;}
          | exactness RADIX8            { $$ = $1;}
          ;

prefix10  : RADIX10 exactness           { $$ = $2;}
          | exactness RADIX10           { $$ = $1;}
          | exactness   
          ;

prefix16  : RADIX16 exactness           { $$ = $2;}
          | exactness RADIX16           { $$ = $1;}
          ;

uinteger2 : digit2                        { $$ = GetNum($1); }
          | uinteger2 digit2              { $$ = Builtins.Add(Builtins.Multiply(2, $1),  GetNum($2)); }
          ;

uinteger8 : digit8                        { $$ = GetNum($1); }
          | uinteger8 digit8              { $$ = Builtins.Add(Builtins.Multiply(8, $1),  GetNum($2)); }
          ;

uinteger10  : digit10                     { $$ = GetNum($1); }
            | uinteger10 digit10          { $$ = Builtins.Add(Builtins.Multiply(10, $1),  GetNum($2)); }
            ;

uinteger16  : digit16                     { $$ = GetNum($1); }
            | uinteger16 digit16          { $$ = Builtins.Add(Builtins.Multiply(16, $1),  GetNum($2)); }
            ;
            
manttisa  :
          | PIPE uinteger10
          ;          

digit10x  : /* empty */                   { $$ = string.Empty; }
          | digit10x digit10              { $$ = $1 + $2; }
          ;

decimal10 : uinteger10 suffix             { $$ = ($2.Length == 0) ? $1 : ConvertToDouble($1 + $2); }
          | DOT digit10 digit10x suffix   { $$ = ConvertToDouble("." + $2 + $3 + $4); }
          | uinteger10 DOT digit10x suffix { $$ = ConvertToDouble($1 + "." + $3 + $4); }
          ;

ureal2    : uinteger2
          | uinteger2 SLASH uinteger2     { $$ = Fraction($1,$3); }
          ;

ureal8    : uinteger8
          | uinteger8 SLASH uinteger8     { $$ = Fraction($1,$3); }
          ;

ureal10   : decimal10 manttisa           { $$ = $1; }
          | uinteger10 SLASH uinteger10   { $$ = Fraction($1,$3); }
          ;


ureal16   : uinteger16
          | uinteger16 SLASH uinteger16   { $$ = Fraction($1,$3); }
          ;

real2     : sreal2                   
          | ureal2
          | PLUS naninf                   { $$ = Builtins.Multiply(1, $2); }
          | MINUS naninf                  { $$ = Builtins.Multiply(-1, $2); }          
          ;

real8     : ureal8
          | sreal8
          | PLUS naninf                   { $$ = Builtins.Multiply(1, $2); }
          | MINUS naninf                  { $$ = Builtins.Multiply(-1, $2); }          
          ;

real10    : ureal10                 
          | sreal10
          | PLUS naninf                   { $$ = Builtins.Multiply(1, $2); }
          | MINUS naninf                  { $$ = Builtins.Multiply(-1, $2); }
          ;

real16    : ureal16
          | sreal16
          | PLUS naninf                   { $$ = Builtins.Multiply(1, $2); }
          | MINUS naninf                  { $$ = Builtins.Multiply(-1, $2); }          
          ;

sreal2    : PLUS ureal2                   { $$ = $2; }
          | MINUS ureal2                  { $$ = Builtins.Multiply(-1, $2); }
          ;   

sreal8    : PLUS ureal8                   { $$ = $2; }
          | MINUS ureal8                  { $$ = Builtins.Multiply(-1, $2); }
          ;   

sreal10   : PLUS ureal10                  { $$ = $2; }
          | MINUS ureal10                 { $$ = Builtins.Multiply(-1, $2); }
          ;   

sreal16   : PLUS ureal16                  { $$ = $2; }
          | MINUS ureal16                 { $$ = Builtins.Multiply(-1, $2); }
          ;   


complex2  : real2
          | real2 AT real2                { $$ = Helper.MakePolar($1,$3); }
          | real2 PLUS  ureal2 IMAG       { $$ = Helper.MakeRectangular($1,$3); }
          | real2 MINUS ureal2 IMAG       { $$ = Helper.MakeRectangular($1, Builtins.Multiply(-1, $3)); }
          | real2 PLUS IMAG               { $$ = Helper.MakeRectangular($1,1); }
          | real2 MINUS IMAG              { $$ = Helper.MakeRectangular($1,-1); }
          | sreal2 IMAG                   { $$ = Helper.MakeRectangular(0,$1); }
          | PLUS IMAG                     { $$ = Helper.MakeRectangular(0,1); }
          | MINUS IMAG                    { $$ = Helper.MakeRectangular(0,-1); }
          | real2 PLUS naninf IMAG        { $$ = Helper.MakeRectangular($1, $3); }
          | real2 MINUS naninf IMAG       { $$ = Helper.MakeRectangular($1, Builtins.Multiply(-1, $3)); }
          | PLUS naninf IMAG              { $$ = Helper.MakeRectangular(0, $2); }
          | MINUS naninf IMAG             { $$ = Helper.MakeRectangular(0, Builtins.Multiply(-1, $2)); }          
          ;

complex8  : real8
          | real8 AT real8                { $$ = Helper.MakePolar($1,$3); }
          | real8 PLUS  ureal8 IMAG       { $$ = Helper.MakeRectangular($1,$3); }
          | real8 MINUS ureal8 IMAG       { $$ = Helper.MakeRectangular($1, Builtins.Multiply(-1, $3)); }
          | real8 PLUS IMAG               { $$ = Helper.MakeRectangular($1,1); }
          | real8 MINUS IMAG              { $$ = Helper.MakeRectangular($1,-1); }
          | PLUS ureal8 IMAG              { $$ = Helper.MakeRectangular(0,$2); }
          | MINUS ureal8 IMAG             { $$ = Helper.MakeRectangular(0, Builtins.Multiply(-1, $2)); }
          | PLUS IMAG                     { $$ = Helper.MakeRectangular(0,1); }
          | MINUS IMAG                    { $$ = Helper.MakeRectangular(0,-1); }
          | real8 PLUS naninf IMAG        { $$ = Helper.MakeRectangular($1, $3); }
          | real8 MINUS naninf IMAG       { $$ = Helper.MakeRectangular($1, Builtins.Multiply(-1, $3)); }
          | PLUS naninf IMAG              { $$ = Helper.MakeRectangular(0, $2); }
          | MINUS naninf IMAG             { $$ = Helper.MakeRectangular(0, Builtins.Multiply(-1, $2)); }          
          ;

complex10 : real10
          | real10 AT real10              { $$ = Helper.MakePolar($1,$3); }
          | real10 PLUS  ureal10 IMAG     { $$ = Helper.MakeRectangular($1,$3); }
          | real10 MINUS ureal10 IMAG     { $$ = Helper.MakeRectangular($1, Builtins.Multiply(-1, $3)); }
          | real10 PLUS IMAG              { $$ = Helper.MakeRectangular($1,1); }
          | real10 MINUS IMAG             { $$ = Helper.MakeRectangular($1,-1); }
          | PLUS ureal10 IMAG             { $$ = Helper.MakeRectangular(0,$2); }
          | MINUS ureal10 IMAG            { $$ = Helper.MakeRectangular(0, Builtins.Multiply(-1, $2)); }
          | PLUS IMAG                     { $$ = Helper.MakeRectangular(0,1); }
          | MINUS IMAG                    { $$ = Helper.MakeRectangular(0,-1); }
          | real10 PLUS naninf IMAG       { $$ = Helper.MakeRectangular($1, $3); }
          | real10 MINUS naninf IMAG      { $$ = Helper.MakeRectangular($1, Builtins.Multiply(-1, $3)); }
          | PLUS naninf IMAG              { $$ = Helper.MakeRectangular(0, $2); }
          | MINUS naninf IMAG             { $$ = Helper.MakeRectangular(0, Builtins.Multiply(-1, $2)); }
          ;

complex16 : real16
          | real16 AT real16              { $$ = Helper.MakePolar($1,$3); }
          | real16 PLUS  ureal16 IMAG     { $$ = Helper.MakeRectangular($1,$3); }
          | real16 MINUS ureal16 IMAG     { $$ = Helper.MakeRectangular($1, Builtins.Multiply(-1, $3)); }
          | real16 PLUS IMAG              { $$ = Helper.MakeRectangular($1,1); }
          | real16 MINUS IMAG             { $$ = Helper.MakeRectangular($1,-1); }
          | PLUS ureal16 IMAG             { $$ = Helper.MakeRectangular(0,$2); }
          | MINUS ureal16 IMAG            { $$ = Helper.MakeRectangular(0, Builtins.Multiply(-1, $2)); }
          | PLUS IMAG                     { $$ = Helper.MakeRectangular(0,1); }
          | MINUS IMAG                    { $$ = Helper.MakeRectangular(0,-1); }
          | real16 PLUS naninf IMAG       { $$ = Helper.MakeRectangular($1, $3); }
          | real16 MINUS naninf IMAG      { $$ = Helper.MakeRectangular($1, Builtins.Multiply(-1, $3)); }
          | PLUS naninf IMAG              { $$ = Helper.MakeRectangular(0, $2); }
          | MINUS naninf IMAG             { $$ = Helper.MakeRectangular(0, Builtins.Multiply(-1, $2)); }          
          ;

num2      : prefix2 complex2    { $$ = ApplyExactness($1, $2); }
          ;
          
num8      : prefix8 complex8    { $$ = ApplyExactness($1, $2); }
          ;
          
num10     : prefix10 complex10  { $$ = ApplyExactness($1, $2); }
          ;
          
num16     : prefix16 complex16  { $$ = ApplyExactness($1, $2); }
          ;

number    : num2                  { result = $1; }
          | num8                  { result = $1; }
          | num10                 { result = $1; }
          | num16                 { result = $1; }
          ;

%%









    