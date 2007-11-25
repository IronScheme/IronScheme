
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

public override void yyerror(string format, params object[] args)
{
  if (!format.Contains("EOF"))
  {
    Console.Error.WriteLine(format, args);
  }
}

public int MakeChar()
{
  switch (yytext.ToLower())
  {
    case "#\\":
    case "#\\space":
      yylval.text = " ";
      break;
    case "#\\newline":
      yylval.text = "\n";
      break;
    default:
      if (yytext[2] == 'x' && yytext.Length > 3)
      {
        //hex escape
      }
      else
      {
        yylval.text = yytext[2].ToString();
      }
      break;
  }
  yylloc = new LexLocation(tokLin,tokCol,tokELin,tokECol);
  return (int)Tokens.CHARACTER;
}


public int Make(Tokens token)
{
  yylval.text = yytext;
  yylloc = new LexLocation(tokLin,tokCol,tokELin,tokECol);
  return (int)token;
}

%}


line_comment           ";"[^\n]*

comment_start          "#|"
comment_end            "|#"

white_space            [ \t]
new_line               \n|\r\n

digit                  [0-9]
letter                 [a-zA-Z]
idinitial              {letter}|[!$%*/:<=>?~_^&]
subsequent             {idinitial}|{digit}|[\.\+@]|"-"
identifier             (({idinitial})({subsequent})*)|"+"|"..."|"-"



digit2                 [01]
digit8                 [0-8]
digit10                {digit}
digit16                {digit10}|[a-fA-F]

radix2                 #[bB]
radix8                 #[oO]
radix10                (#[dD])?
radix16                #[xX]

exactness              (#[iIeE])?

sign                   ("-"|"+")?

exponentmarker         [eEsSfFdDlL]

suffix                 ({exponentmarker}{sign}({digit10})+)?

prefix2                ({radix2}{exactness})|({exactness}{radix2})
prefix8                ({radix8}{exactness})|({exactness}{radix8})
prefix10               ({radix10}{exactness})|({exactness}{radix10})
prefix16               ({radix16}{exactness})|({exactness}{radix16})

uinteger2              ({digit2})+
uinteger8              ({digit8})+
uinteger10             ({digit10})+
uinteger16             ({digit16})+

decimal10              ({uinteger10}{suffix})|("."({digit10})+{suffix})|(({digit10})+"."({digit10})*{suffix})|(({digit10})+"."{suffix})

ureal2                 ({uinteger2})|({uinteger2}"/"{uinteger2})
ureal8                 ({uinteger8})|({uinteger8}"/"{uinteger8})
ureal10                ({uinteger10})|({uinteger10}"/"{uinteger10})|({decimal10})
ureal16                ({uinteger16})|({uinteger16}"/"{uinteger16})

real2                  ({sign}{ureal2})
real8                  ({sign}{ureal8})
real10                 ({sign}{ureal10})
real16                 ({sign}{ureal16})

complex2               ({real2}|({real2}"@"{real2})|({real2}"+"{real2}"i")|({real2}"-"{real2}"i")|({real2}"+i")|({real2}"-i")|("+"{real2}"i")|("-"{real2}"i")|("+i")|("-i"))
complex8               ({real8}|({real8}"@"{real8})|({real8}"+"{real8}"i")|({real8}"-"{real8}"i")|({real8}"+i")|({real8}"-i")|("+"{real8}"i")|("-"{real8}"i")|("+i")|("-i"))
complex10              ({real10}|({real10}"@"{real10})|({real10}"+"{real10}"i")|({real10}"-"{real10}"i")|({real10}"+i")|({real10}"-i")|("+"{real10}"i")|("-"{real10}"i")|("+i")|("-i"))
complex16              ({real16}|({real16}"@"{real16})|({real16}"+"{real16}"i")|({real16}"-"{real16}"i")|({real16}"+i")|({real16}"-i")|("+"{real16}"i")|("-"{real16}"i")|("+i")|("-i"))

num2                   ({prefix2}{complex2})
num8                   ({prefix8}{complex8})
num10                  ({prefix10}{complex10})
num16                  ({prefix16}{complex16})

number                 ({num2}|{num8}|{num10}|{num16})



single_char            [^\n ]
character              {single_char}|([Nn][Ee][Ww][Ll][Ii][Nn][Ee])|([Ss][Pp][Aa][Cc][Ee])
character_literal      #\\({character})?

single_string_char     [^\\\"]
string_esc_seq         \\[\"\\abfnrtv]
hex_esc_seq            \\x({digit16})+
reg_string_char        {single_string_char}|{string_esc_seq}
string_literal         \"({reg_string_char})*\"

atoms                  (#[TtFf])

%x ML_COMMENT

%%

{white_space}+        { ; }
{new_line}            { ;}
                     
{comment_start}       { yy_push_state(ML_COMMENT);  }                      
{line_comment}        {  }


<ML_COMMENT>[^\n\|]+          { ; }
<ML_COMMENT>{comment_end}     { yy_pop_state();  }
<ML_COMMENT>"|"               { ; }
 
{atoms}               { return Make(Tokens.LITERAL); } 

{character_literal}   { return MakeChar(); }                      
{string_literal}      { return Make(Tokens.STRING); }
{number}              { return Make(Tokens.NUMBER); }

"["                   { return Make(Tokens.LBRACK); }                     
"]"                   { return Make(Tokens.RBRACK); } 

"("                   { return Make(Tokens.LBRACE); }                     
"#("                  { return Make(Tokens.VECTORLBRACE); }                     
"#vu8("               { return Make(Tokens.BYTEVECTORLBRACE); }                     
")"                   { return Make(Tokens.RBRACE); } 
"`"                   { return Make(Tokens.QUASIQUOTE); }
"'"                   { return Make(Tokens.QUOTE); }
",@"                  { return Make(Tokens.UNQUOTESPLICING); }
","                   { return Make(Tokens.UNQUOTE);}


"#'"                  { return Make(Tokens.SYNTAX);}
"#`"                  { return Make(Tokens.QUASISYNTAX);}
"#,@"                 { return Make(Tokens.UNSYNTAXSPLICING);}
"#,"                  { return Make(Tokens.UNSYNTAX);}


{identifier}          { return Make(Tokens.SYMBOL); }

"."                   { return Make(Tokens.DOT); }

.                     { Errors.Add(SourceUnit, string.Format("Bad input: '{0}'", yytext), 
                          new SourceSpan( new SourceLocation(1,tokLin,tokCol + 1) , new SourceLocation(1,tokLin,tokCol + yytext.Length + 1)), 2, Microsoft.Scripting.Hosting.Severity.Error); }
<<EOF>>               { }
%%



