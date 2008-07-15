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



%} 

%union
{
  public string text;
}


%token DIGIT2 DIGIT8 DIGIT10 DIGIT16 RADIX2 RADIX8 RADIX10 RADIX16
%token EXACTNESS EXPMARKER NANINF
%token DOT PLUS MINUS SLASH IMAG AT

%start number

%%

digit2  : DIGIT2;

digit8  : digit2
        | DIGIT8
        ;

digit10 : digit8
        | DIGIT10
        ;
        
digit16 : digit10
        | DIGIT16
        ;

exactness : 
          | EXACTNESS
          ;

sign  : 
      | PLUS
      | MINUS
      ;


suffix  : 
        | EXPMARKER sign uinteger10
        ;

prefix2  : RADIX2 exactness
          | exactness RADIX2
          ;

prefix8  : RADIX8 exactness
          | exactness RADIX8
          ;

prefix10  : RADIX10 exactness
          | exactness RADIX10
          | exactness 
          ;

prefix16  : RADIX16 exactness
          | exactness RADIX16
          ;

uinteger2  : digit2
            | uinteger2 digit2
            ;

uinteger8  : digit8
            | uinteger8 digit8
            ;

uinteger10  : digit10
            | uinteger10 digit10
            ;

uinteger16  : digit16
            | uinteger16 digit16
            ;

digit10x  : /* empty */
          | digit10x digit10
          ;

decimal10 : uinteger10 suffix
          | DOT uinteger10 suffix
          | uinteger10 DOT digit10x suffix
          ;

ureal2 : uinteger2
        | uinteger2 SLASH uinteger2
        ;

ureal8 : uinteger8
        | uinteger8 SLASH uinteger8
        ;

ureal10 : decimal10
        | uinteger10 SLASH uinteger10
        ;


ureal16 : uinteger16
        | uinteger16 SLASH uinteger16
        ;

real2  : sign ureal2
        | PLUS NANINF
        | MINUS NANINF
        ;

real8  : sign ureal8
        | PLUS NANINF
        | MINUS NANINF
        ;

real10  : sign ureal10
        | PLUS NANINF
        | MINUS NANINF
        ;


real16  : sign ureal16
        | PLUS NANINF
        | MINUS NANINF
        ;


complex2 : real2
          | real2 AT real2
          | real2 PLUS  ureal2 IMAG
          | real2 MINUS ureal2 IMAG
          | real2 PLUS IMAG
          | real2 MINUS IMAG
          | PLUS ureal2 IMAG
          | MINUS ureal2 IMAG
          | PLUS IMAG
          | MINUS IMAG
          | real2 PLUS NANINF IMAG
          | real2 MINUS NANINF IMAG
          | PLUS NANINF IMAG
          | MINUS NANINF IMAG
          ;

complex8 : real8
          | real8 AT real8
          | real8 PLUS  ureal8 IMAG
          | real8 MINUS ureal8 IMAG
          | real8 PLUS IMAG
          | real8 MINUS IMAG
          | PLUS ureal8 IMAG
          | MINUS ureal8 IMAG
          | PLUS IMAG
          | MINUS IMAG
          | real8 PLUS NANINF IMAG
          | real8 MINUS NANINF IMAG
          | PLUS NANINF IMAG
          | MINUS NANINF IMAG
          ;

complex10 : real10
          | real10 AT real10
          | real10 PLUS  ureal10 IMAG
          | real10 MINUS ureal10 IMAG
          | real10 PLUS IMAG
          | real10 MINUS IMAG
          | PLUS ureal10 IMAG
          | MINUS ureal10 IMAG
          | PLUS IMAG
          | MINUS IMAG
          | real10 PLUS NANINF IMAG
          | real10 MINUS NANINF IMAG
          | PLUS NANINF IMAG
          | MINUS NANINF IMAG
          ;

complex16 : real16
          | real16 AT real16
          | real16 PLUS  ureal16 IMAG
          | real16 MINUS ureal16 IMAG
          | real16 PLUS IMAG
          | real16 MINUS IMAG
          | PLUS ureal16 IMAG
          | MINUS ureal16 IMAG
          | PLUS IMAG
          | MINUS IMAG
          | real16 PLUS NANINF IMAG
          | real16 MINUS NANINF IMAG
          | PLUS NANINF IMAG
          | MINUS NANINF IMAG
          ;

num2  : prefix2 complex2;
num8  : prefix8 complex8;
num10 : prefix10 complex10;
num16 : prefix16 complex16;

number: num2
      | num8
      | num10
      | num16 
      ;

%%









    