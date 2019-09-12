/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */

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

int diff()
{
  //return code == -1 ? 0 : (code > ushort.MaxValue ? -2 : -1);
  //return (code == -1 || (eofseen && code == ' ')) ? 0 : -1;
  return (code == -1) ? 0 : -1;
}

public int MakeSymbol()
{
  string t = yytext;
  int len = yyleng;
  FixLineNum(t);
  t = t.Substring(0, t.Length + diff());
  yylval.text = t;
  yyless(len + diff());
  yylloc = new LexLocation(yyline,yycol,yyline,yycol + len);
  return (int)Tokens.SYMBOL;
}

public int MakeBoolean()
{
  string t = yytext;
  int len = yyleng;
  FixLineNum(t);
  t = t.Substring(0, t.Length + diff());
  yylval.text = t.ToLower();
  yyless(len);
  yylloc = new LexLocation(yyline,yycol,yyline,yycol + len);
  return (int)Tokens.LITERAL;
}

public int MakeNumber()
{
  string t = yytext;
  int len = yyleng;
  FixLineNum(t);
  t = t.Substring(0, t.Length + diff());
  yylval.text = t;
  yyless(len);
  yylloc = new LexLocation(yyline,yycol,yyline,yycol + len);
  return (int)Tokens.NUMBER;
}

public int MakeChar()
{
  string t = yytext;
  int len = yyleng;
  FixLineNum(t);
  t = t.Substring(0, t.Length + diff());
  yylval.text = Helper.ParseChar(t);
  yyless(len + diff());
  yylloc = new LexLocation(yyline,yycol,yyline,yycol + len);
  return (int)Tokens.CHARACTER;
}

public int MakeError(string message)
{
  if (maxParseToken == parserMax)
  {
    throw new SyntaxErrorException(string.Format("{1}|{0}", yytext, message) , SourceUnit,
                new SourceSpan( new SourceLocation(1,tokLin,tokCol + 1) , new SourceLocation(1,tokLin,tokCol + yytext.Length + 1)), 2, Microsoft.Scripting.Hosting.Severity.Error);  
  }
  ErrorMessage = message;
  yylval.text = yytext;
  yylloc = new LexLocation(yyline,yycol,yyline,yycol + yyleng);
  return (int)Tokens.error;
}


public int Make(Tokens token)
{
  string t = yytext;
  int len = yyleng;
  yylval.text = t;
  yyless(len);
  yylloc = new LexLocation(yyline,yycol,yyline,yycol + len);
  return (int)token;
}

%}

delimiter              "\n\r"|[\[\]\(\)\";\r\n\t\u000c ]
but_delimiter          [^\[\]\(\)\";\r\n\t\u000c ]
numbut_delimiter       [^\[\]\(\)\";\r\n\t\u000c i]

fold_case              "#!fold-case"
no_fold_case           "#!no-fold-case" 

line_comment           (";"[^\n]*)

ignore_datum           "#;"

comment_start          "#|"
comment_end            "|#"

white_space            [ \t\u000c]
new_line               "\n\r"|\r|\n|"\r\n"

digit                  [0-9]
digit2                 [01]
digit8                 [0-7]
digit10                {digit}
digit16                [0-9a-fA-F]

letter                 [[:IsLetterOrUnicodeSchemeIdentifier:]]
idescape               ("\\x"({digit16})+";")
idinitial              ("->"|({letter})|{idescape}|[!$%*/:<=>?~_^&])
subsequent             ({idinitial})|{digit}|[\.\+@]|"-"|"[]"
identifier             (({idinitial})({subsequent})*)|"+o"|"..."|"-o"|"@"|"+"|"-"

good_id                {identifier}{delimiter}
bad_id                 {identifier}{but_delimiter}

directive              "#!"(({identifier})|([ \t]*"/"[^\n]*))


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

mantissa_width         ("|"{uinteger10})?

decimal10              (({uinteger10}{suffix})|("."({digit10})+{suffix})|(({digit10})+"."({digit10})*{suffix}))

ureal2                 (({uinteger2})|({uinteger2}"/"{uinteger2}))
ureal8                 (({uinteger8})|({uinteger8}"/"{uinteger8}))
ureal10                (({decimal10}{mantissa_width})|({uinteger10}"/"{uinteger10}))
ureal16                (({uinteger16})|({uinteger16}"/"{uinteger16}))

naninf                 ("nan.0"|"inf.0")

real2                  (({sign}{ureal2})|("+"{naninf})|("-"{naninf}))
real8                  (({sign}{ureal8})|("+"{naninf})|("-"{naninf}))
real10                 (({sign}{ureal10})|("+"{naninf})|("-"{naninf}))
real16                 (({sign}{ureal16})|("+"{naninf})|("-"{naninf}))

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

single_string_char     [^\\\"\n\r]
string_esc_seq         (\\[\"\\abfnrtv])
hex_esc_seq            (\\x({digit16})+";")
string_continuation    (\\)
reg_string_char        {single_string_char}|{string_esc_seq}|{hex_esc_seq}
string_literal         \"({reg_string_char})*\"

ml_string_start        \"({reg_string_char})*({string_continuation})?{new_line}
ml_string_body         ({reg_string_char})*({string_continuation})?{new_line}
ml_string_end          ({reg_string_char})*\"

atoms                  ((#[TtFf])|"#true"|"#false")
good_atoms             {atoms}{delimiter}
bad_atoms              {atoms}{but_delimiter}+

vvstart                "#"({identifier})"("


%x ML_COMMENT
%x ML_STRING
%x IGNORE

%%



<IGNORE>.+            { ; }
"#!eof"               { yy_push_state(IGNORE); }

{white_space}+        { ; }
{new_line}            { ; }

{fold_case}           { return Make(Tokens.FOLDCASE); }
{no_fold_case}        { return Make(Tokens.NOFOLDCASE); }

{directive}           { return Make(Tokens.DIRECTIVE); }
                     
{comment_start}       { yy_push_state(ML_COMMENT); return Make(Tokens.COMMENT); }                      
{line_comment}        { return Make(Tokens.COMMENT); }


<ML_COMMENT>[^\r\n\|#]+       { if (code != -1) return Make(Tokens.COMMENT); }
<ML_COMMENT>{comment_start}   { yy_push_state(ML_COMMENT); return Make(Tokens.COMMENT); }    
<ML_COMMENT>{comment_end}     { yy_pop_state(); return Make(Tokens.COMMENT); }
<ML_COMMENT>"|"               { return Make(Tokens.COMMENT); }
<ML_COMMENT>"#"               { return Make(Tokens.COMMENT); }

{ml_string_start}     { yy_push_state(ML_STRING); return Make(Tokens.MLSTRING); }

<ML_STRING>{ml_string_body} { return Make(Tokens.MLSTRING); }
<ML_STRING>{ml_string_end}  { yy_pop_state(); return Make(Tokens.MLSTRING); }

{ignore_datum}        { return Make(Tokens.IGNOREDATUM); }
 
{good_atoms}          { return MakeBoolean(); } 

{good_char}           { return MakeChar(); }                      
{string_literal}      { return Make(Tokens.STRING); }
{good_number}         { return MakeNumber(); }

"["                   { return Make(Tokens.LBRACK); }                     
"]"                   { return Make(Tokens.RBRACK); } 

"("                   { return Make(Tokens.LBRACE); }
"#("                  { return Make(Tokens.VECTORLBRACE); }   
{vvstart}             { return Make(Tokens.VALUEVECTORLBRACE); }
")"                   { return Make(Tokens.RBRACE); }

"`"                   { return Make(Tokens.QUASIQUOTE); }
"'"                   { return Make(Tokens.QUOTE); }
",@"                  { return Make(Tokens.UNQUOTESPLICING); }
","                   { return Make(Tokens.UNQUOTE);}


"#'"                  { return Make(Tokens.SYNTAX);}
"#`"                  { return Make(Tokens.QUASISYNTAX);}
"#,@"                 { return Make(Tokens.UNSYNTAXSPLICING);}
"#,"                  { return Make(Tokens.UNSYNTAX);}

{good_id}             { return MakeSymbol(); }

{good_dot}            { yyless(1); return Make(Tokens.DOT); }

{bad_dot}             { return MakeError("bad dot"); }
{bad_id}              { return MakeError("bad identifier"); }
{bad_atoms}           { return MakeError("bad boolean"); }
{bad_number}          { return MakeError("bad number"); }
{bad_char}            { return MakeError("bad character"); }



.                     { return MakeError("bad input"); }

<<EOF>>               { ; }

%%



