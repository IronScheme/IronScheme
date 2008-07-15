
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

%{

%token DIGIT2 DIGIT8 DIGIT10 DIGIT16 RADIX2 RADIX8 RADIX10 RADIX16
%token EXACTNESS EXPMARKER NANINF
%token DOT PLUS MINUS SLASH IMAG AT


%}

digit2                 [01]
digit8                 [2-7]
digit10                [89]
digit16                [a-fA-F]

radix2                 #[bB]
radix8                 #[oO]
radix10                #[dD]
radix16                #[xX]

exactness              #[iIeE]

exponentmarker         [eEsSfFdDlL]

dot                    "."
plus                   "+"
minus                  "-"
slash                  "/"
imag                   "i"
at                     "@"

naninf                 ("nan.0"|"inf.0")

%%

<<EOF>>               { }
%%



