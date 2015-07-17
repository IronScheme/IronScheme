/* Copyright (c) 2007-2015 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */

%namespace IronScheme.Compiler.Numbers

%option minimize

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
pipe                   "|"

naninf                 ("nan.0"|"inf.0")

%s RAD2
%s RAD8
%s RAD10
%s RAD16

%%


<RAD2>{digit2}         { return Make(Tokens.DIGIT2); }

<RAD8>{digit2}         { return Make(Tokens.DIGIT2); }
<RAD8>{digit8}         { return Make(Tokens.DIGIT8); }

<RAD10>{digit2}        { return Make(Tokens.DIGIT2); }
<RAD10>{digit8}        { return Make(Tokens.DIGIT8); }
<RAD10>{digit10}       { return Make(Tokens.DIGIT10); }

<RAD16>{digit2}        { return Make(Tokens.DIGIT2); }
<RAD16>{digit8}        { return Make(Tokens.DIGIT8); }
<RAD16>{digit10}       { return Make(Tokens.DIGIT10); }
<RAD16>{digit16}       { return Make(Tokens.DIGIT16); }

<RAD10>{exponentmarker}({plus}|{minus})?({digits})+   { return Make(Tokens.EXPMARKER); }

{radix2}               { BEGIN(RAD2); return Make(Tokens.RADIX2); }
{radix8}               { BEGIN(RAD8); return Make(Tokens.RADIX8); }
{radix10}              { BEGIN(RAD10); return Make(Tokens.RADIX10); }
{radix16}              { BEGIN(RAD16); return Make(Tokens.RADIX16); }

{exact}                { return Make(Tokens.EXACT); }
{inexact}              { return Make(Tokens.INEXACT); }

<RAD10>{dot}           { return Make(Tokens.DOT); }
{plus}                 { return Make(Tokens.PLUS); }
{minus}                { return Make(Tokens.MINUS); }
{slash}                { return Make(Tokens.SLASH); }
{imag}                 { return Make(Tokens.IMAG); }
{at}                   { return Make(Tokens.AT); }
{pipe}                 { return Make(Tokens.PIPE); }

{naninf}               { return Make(Tokens.NANINF); }

.                      { if (yytext.Length > 0) throw new  SyntaxErrorException("Unexpected character: " + yytext); }

<<EOF>>               { }
%%



