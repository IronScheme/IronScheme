
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


%namespace IronScheme.Compiler.Numbers

%{

public int Make(Tokens token)
{
  yylval.text = yytext;
  yylloc = new LexLocation(yyline,yycol,yyline,yycol + yyleng);
  return (int)token;
}

%}

digit2                 [01]
digit8                 [2-7]
digit10                [89]
digit16                [a-fA-F]

digits                 {digit2}|{digit8}|{digit10}

radix2                 #[bB]
radix8                 #[oO]
radix10                #[dD]
radix16                #[xX]

exact                  #[eE]
inexact                #[iI]

exponentmarker         [eEsSfFdDlL]

dot                    "."
plus                   "+"
minus                  "-"
slash                  "/"
imag                   "i"
at                     "@"

naninf                 ("nan.0"|"inf.0")

%%



{digit2}               { return Make(Tokens.DIGIT2); }
{digit8}               { return Make(Tokens.DIGIT8); }
{digit10}              { return Make(Tokens.DIGIT10); }
{digit16}              { return Make(Tokens.DIGIT16); }

{exponentmarker}({plus}|{minus})?({digits})+   { return Make(Tokens.EXPMARKER); }

{radix2}               { return Make(Tokens.RADIX2); }
{radix8}               { return Make(Tokens.RADIX8); }
{radix10}              { return Make(Tokens.RADIX10); }
{radix16}              { return Make(Tokens.RADIX16); }

{exact}                { return Make(Tokens.EXACT); }
{inexact}              { return Make(Tokens.INEXACT); }

{dot}                  { return Make(Tokens.DOT); }
{plus}                 { return Make(Tokens.PLUS); }
{minus}                { return Make(Tokens.MINUS); }
{slash}                { return Make(Tokens.SLASH); }
{imag}                 { return Make(Tokens.IMAG); }
{at}                   { return Make(Tokens.AT); }

{naninf}               { return Make(Tokens.NANINF); }

<<EOF>>               { }
%%



