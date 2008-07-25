
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

%option minimize, unicode


%{

void FixLineNum(string text)
{
  if (text.EndsWith("\n") || text.EndsWith("\r"))
  {
    lNum--;
  }
}

public override void yyerror(string format, params object[] args)
{
  if (!format.Contains("EOF"))
  {
    Console.Error.WriteLine(format, args);
  }
}

public int MakeSymbol()
{
  string t = yytext;
  FixLineNum(t);
  t = t.Substring(0, t.Length - 1);
  yylval.text = t;
  yyless(t.Length);
  yylloc = new LexLocation(yyline,yycol,yyline,yycol + yyleng);
  return (int)Tokens.SYMBOL;
}

public int MakeBoolean()
{
  string t = yytext;
  FixLineNum(t);
  t = t.Substring(0, t.Length - 1);
  yylval.text = t.ToLower();
  yyless(t.Length);
  yylloc = new LexLocation(yyline,yycol,yyline,yycol + yyleng);
  return (int)Tokens.LITERAL;
}

public int MakeNumber()
{
  string t = yytext;
  FixLineNum(t);
  t = t.Substring(0, t.Length - 1);
  yylval.text = t;
  yyless(t.Length);
  yylloc = new LexLocation(yyline,yycol,yyline,yycol + yyleng);
  return (int)Tokens.NUMBER;
}

public int MakeChar()
{
  string t = yytext;
  FixLineNum(t);
  t = t.Substring(0, t.Length - 1);
  yylval.text = Helper.ParseChar(t);
  yyless(t.Length);
  yylloc = new LexLocation(yyline,yycol,yyline,yycol + yyleng);
  return (int)Tokens.CHARACTER;
}


public int Make(Tokens token)
{
  yylval.text = yytext;
  yylloc = new LexLocation(yyline,yycol,yyline,yycol + yyleng);
  return (int)token;
}

%}


line_comment           (";"[^\n]*)|("#!"[^\n]*)

ignore_datum           "#;"

comment_start          "#|"
comment_end            "|#"

white_space            [ \t]
new_line               "\n\r"|\r|\n

delimiter              "\n\r"|[\[\]\(\)\";#\r\n\t ]
but_delimiter          [^\[\]\(\)\";#\r\n\t ]
numbut_delimiter       [^\[\]\(\)\";#\r\n\t i]

unichar                \x03bb

digit                  [0-9]
letter                 [a-zA-Z]
idinitial              ("->"|({letter})|({unichar})|[!$%*/:<=>?~_^&])
subsequent             ({idinitial})|{digit}|[\.\+@]|"-"|"[]"
identifier             (({idinitial})({subsequent})*)|"+"|"..."|"-"

good_id                {identifier}{delimiter}
bad_id                 {identifier}{but_delimiter}


digit2                 [01]
digit8                 [0-7]
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

decimal10              (({uinteger10}{suffix})|("."({digit10})+{suffix})|(({digit10})+"."({digit10})*{suffix}))

ureal2                 (({uinteger2})|({uinteger2}"/"{uinteger2}))
ureal8                 (({uinteger8})|({uinteger8}"/"{uinteger8}))
ureal10                (({decimal10})|({uinteger10}"/"{uinteger10}))
ureal16                (({uinteger16})|({uinteger16}"/"{uinteger16}))

naninf                 ("nan.0"|"inf.0")

real2                  ({sign}{ureal2}|"+"{naninf}|"-"{naninf})
real8                  ({sign}{ureal8}|"+"{naninf}|"-"{naninf})
real10                 ({sign}{ureal10}|"+"{naninf}|"-"{naninf})
real16                 ({sign}{ureal16}|"+"{naninf}|"-"{naninf})

complex2               ({real2}|({real2}"@"{real2})|({real2}"+"{ureal2}"i")|({real2}"-"{ureal2}"i")|({real2}"+i")|({real2}"-i")|("+"{ureal2}"i")|("-"{ureal2}"i")|("+i")|("-i")|({real2}"+"{naninf}"i")|({real2}"-"{naninf}"i")|("+"{naninf}"i")|("-"{naninf}"i"))
complex8               ({real8}|({real8}"@"{real8})|({real8}"+"{ureal8}"i")|({real8}"-"{ureal8}"i")|({real8}"+i")|({real8}"-i")|("+"{ureal8}"i")|("-"{ureal8}"i")|("+i")|("-i")|({real8}"+"{naninf}"i")|({real8}"-"{naninf}"i")|("+"{naninf}"i")|("-"{naninf}"i"))
complex10              ({real10}|({real10}"@"{real10})|({real10}"+"{ureal10}"i")|({real10}"-"{ureal10}"i")|({real10}"+i")|({real10}"-i")|("+"{ureal10}"i")|("-"{ureal10}"i")|("+i")|("-i")|({real10}"+"{naninf}"i")|({real10}"-"{naninf}"i")|("+"{naninf}"i")|("-"{naninf}"i"))
complex16              ({real16}|({real16}"@"{real16})|({real16}"+"{ureal16}"i")|({real16}"-"{ureal16}"i")|({real16}"+i")|({real16}"-i")|("+"{ureal16}"i")|("-"{ureal16}"i")|("+i")|("-i")|({real16}"+"{naninf}"i")|({real16}"-"{naninf}"i")|("+"{naninf}"i")|("-"{naninf}"i"))

num2                   ({prefix2}{complex2})
num8                   ({prefix8}{complex8})
num10                  ({prefix10}{complex10})
num16                  ({prefix16}{complex16})

number                 ({num2}|{num8}|{num10}|{num16})

good_number            {number}{delimiter}
bad_number             {number}{numbut_delimiter}+

good_dot               "."{delimiter}?
bad_dot                "."{but_delimiter}+


single_char            [^\n ]
character              {single_char}
char_hex_esc_seq       (#\\x({digit16})+)
char_esc_seq           (#\\("nul"|alarm|backspace|tab|linefeed|newline|vtab|page|return|esc|space|delete))
character_literal      ((#\\({character})?)|{char_hex_esc_seq}|{char_esc_seq})

good_char              {character_literal}{delimiter}
bad_char               {character_literal}{but_delimiter}+

single_string_char     [^\\\"]
string_esc_seq         (\\[\"\\abfnrtv])
hex_esc_seq            (\\x({digit16})+";")
string_continuation    (\\{new_line})
reg_string_char        {string_continuation}|{single_string_char}|{string_esc_seq}|{hex_esc_seq}
string_literal         \"({reg_string_char})*\"

atoms                  (#[TtFf])
good_atoms             {atoms}{delimiter}
bad_atoms              {atoms}{but_delimiter}+



%x ML_COMMENT

%%

{white_space}+        { ; }
{new_line}            { ;}
                     
{comment_start}       { yy_push_state(ML_COMMENT);  }                      
{line_comment}        {  }


<ML_COMMENT>[^\n\|]+          { ; }
<ML_COMMENT>{comment_end}     { yy_pop_state();  }
<ML_COMMENT>"|"               { ; }

{ignore_datum}        { return Make(Tokens.IGNOREDATUM); }
 
{good_atoms}          { return MakeBoolean(); } 

{good_char}           { return MakeChar(); }                      
{string_literal}      { return Make(Tokens.STRING); }
{good_number}         { return MakeNumber(); }

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


{good_id}          { return MakeSymbol(); }

{good_dot}           { yyless(1); return Make(Tokens.DOT); }


{bad_dot}             { Errors.Add(SourceUnit, string.Format("bad dot:{0}", yytext), 
                          new SourceSpan( new SourceLocation(1,tokLin,tokCol + 1) , new SourceLocation(1,tokLin,tokCol + yytext.Length + 1)), 2, Microsoft.Scripting.Hosting.Severity.Error); }
{bad_id}             { Errors.Add(SourceUnit, string.Format("bad identifier:{0}", yytext), 
                          new SourceSpan( new SourceLocation(1,tokLin,tokCol + 1) , new SourceLocation(1,tokLin,tokCol + yytext.Length + 1)), 2, Microsoft.Scripting.Hosting.Severity.Error); }
{bad_atoms}          { Errors.Add(SourceUnit, string.Format("bad boolean:{0}", yytext), 
                          new SourceSpan( new SourceLocation(1,tokLin,tokCol + 1) , new SourceLocation(1,tokLin,tokCol + yytext.Length + 1)), 2, Microsoft.Scripting.Hosting.Severity.Error); }
{bad_number}          { Errors.Add(SourceUnit, string.Format("bad number:{0}", yytext), 
                          new SourceSpan( new SourceLocation(1,tokLin,tokCol + 1) , new SourceLocation(1,tokLin,tokCol + yytext.Length + 1)), 2, Microsoft.Scripting.Hosting.Severity.Error); }
{bad_char}            { Errors.Add(SourceUnit, string.Format("bad char:{0}", yytext), 
                          new SourceSpan( new SourceLocation(1,tokLin,tokCol + 1) , new SourceLocation(1,tokLin,tokCol + yytext.Length + 1)), 2, Microsoft.Scripting.Hosting.Severity.Error); }

.                     { Errors.Add(SourceUnit, string.Format("bad input:{0}", yytext), 
                          new SourceSpan( new SourceLocation(1,tokLin,tokCol + 1) , new SourceLocation(1,tokLin,tokCol + yytext.Length + 1)), 2, Microsoft.Scripting.Hosting.Severity.Error); }
<<EOF>>               { }
%%



