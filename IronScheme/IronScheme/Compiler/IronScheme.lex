
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
  switch (yytext)
  {
    case "#\\":
    case "#\\space":
      yylval.text = " ";
      break;
    case "#\\newline":
      yylval.text = "\n";
      break;
    default:
      yylval.text = yytext[2].ToString();
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


/*
Identifiers.  Identifiers may denote variables, keywords, or symbols, depending upon context. They are formed from sequences of letters, 
digits, and special characters. With three exceptions, identifiers cannot begin with a character that can also begin a number, i.e., 
they cannot begin with ., +, -, or a digit. The three exceptions are the identifiers ..., +, and -. Case is insignificant in identifiers 
so that, for example, newspaper, NewsPaper, and NEWSPAPER all represent the same identifier. 

<identifier>  <initial> <subsequent>* | + | - | ... 
<initial>  <letter> | ! | $ | % | & | * | / | : | < | = | > | ? | ~ | _ | ^ 
<subsequent>  <initial> | <digit> | . | + | - | @ 
<letter>  a | b | ... | z 
<digit>  0 | 1 | ... | 9  


*/

digit                  [0-9]
letter                 [a-zA-Z]
idinitial              {letter}|[!$%*/:<=>?~_^&]
subsequent             {idinitial}|{digit}|[\.\+@]|"-"
identifier             ({idinitial}({subsequent})*)|"+"|"..."|"-"







sign                   "-"|"+"

dec_digit              [0-9]
hex_digit              [0-9A-Fa-f]
int_suffix             [UuLl]|[Uu][Ll]|[Ll][Uu]
dec_literal            ({dec_digit})+({int_suffix})?
hex_literal            0[xX]({hex_digit})+({int_suffix})?
integer_literal        ({sign})?({dec_literal}|{hex_literal})

real_suffix            [FfDdMm]
exponent_part          [eE]({sign})?({dec_digit})+
whole_real1            ({dec_digit})+{exponent_part}({real_suffix})?
whole_real2            ({dec_digit})+{real_suffix}
part_real              ({dec_digit})*\.({dec_digit})+({exponent_part})?({real_suffix})?
real_literal           {whole_real1}|{whole_real2}|{part_real}









single_char            [^\n ]
character              {single_char}|"newline"|"space"
character_literal      #\\({character})?

single_string_char     [^\\\"\n]
string_esc_seq         \\[\"\\abfnrtv]
reg_string_char        {single_string_char}|{string_esc_seq}
string_literal         \"({reg_string_char})*\"

atoms                  (#t|#f)

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
{integer_literal}     { return Make(Tokens.INTEGER); }
{real_literal}        { return Make(Tokens.REAL); }
{string_literal}      { return Make(Tokens.STRING); }

"["                   { return Make(Tokens.LBRACK); }                     
"]"                   { return Make(Tokens.RBRACK); } 

"("                   { return Make(Tokens.LBRACE); }                     
"#("                  { return Make(Tokens.VECTORLBRACE); }                     
")"                   { return Make(Tokens.RBRACE); } 
"`"                   { return Make(Tokens.QUASIQUOTE); }
"'"                   { return Make(Tokens.QUOTE); }
",@"                  { return Make(Tokens.UNQUOTESPLICING); }
","                   { return Make(Tokens.UNQUOTE);}

/*
"#'"                  { return Make(Tokens.SYNTAX);}
"#`"                  { return Make(Tokens.QUASISYNTAX);}
"#,@"                 { return Make(Tokens.UNSYNTAXSPLICING);}
"#,"                  { return Make(Tokens.UNSYNTAX);}
*/

{identifier}          { return Make(Tokens.SYMBOL); }

"."                   { return Make(Tokens.DOT); }

.                     { Console.Error.WriteLine("Bad input: '{0}' @ {1}:{2}", yytext, tokLin, tokCol); }


<<EOF>>               { }
%%



