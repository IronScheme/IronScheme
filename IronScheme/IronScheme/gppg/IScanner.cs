// Gardens Point Parser Generator
// Copyright (c) Wayne Kelly, QUT 2005-2006
// (see accompanying GPPGcopyright.rtf)


using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;


namespace gppg
{
  /// <summary>
  /// Classes implementing this interface must supply a
  /// method that merges two location objects to return
  /// a new object of the same type.
  /// GPPG-generated parsers have the default location
  /// action equivalent to "@$ = @1.Merge(@N);" where N
  /// is the right-hand-side length of the production.
  /// </summary>
  /// <typeparam name="YYLTYPE"></typeparam>
  public interface IMerge<YYLTYPE>
  {
    YYLTYPE Merge(YYLTYPE last);
  }

  /// <summary>
  /// This is the default class that carries location
  /// information from the scanner to the parser.
  /// If you don't declare "%YYLTYPE Foo" the parser
  /// will expect to deal with this type.
  /// </summary>
  public class LexLocation : IMerge<LexLocation>
  {
    public int sLin; // start line
    public int sCol; // start column
    public int eLin; // end line
    public int eCol; // end column

    public LexLocation()
    { }

    public LexLocation(int sl, int sc, int el, int ec)
    {
      sLin = sl; sCol = sc; eLin = el; eCol = ec;
      if (ec < sc && sl == el)
      {
        Debugger.Break();
      }
    }

    public LexLocation Merge(LexLocation last)
    {
      if (last == null)
      {
        return this;
      }
      return new LexLocation(this.sLin, this.sCol, last.eLin, last.eCol);
    }

    public override string ToString()
    {
      return string.Format("({0}:{1}-{2}:{3})", sLin, sCol, eLin, eCol);
    }
  }


  /// <summary>
  /// Abstract scanner class that GPPG expects its scanners to extend.
  /// </summary>
  /// <typeparam name="YYSTYPE"></typeparam>
  /// <typeparam name="YYLTYPE"></typeparam>
  public abstract class IScanner<YYSTYPE, YYLTYPE>
    where YYSTYPE : struct
    where YYLTYPE : IMerge<YYLTYPE>
  {
    public YYSTYPE yylval;                      // lexical value: set by scanner
    public abstract YYLTYPE yylloc { get; set;} // location value: set by scanner

    public abstract int yylex();
    public virtual void yyerror(string format, params object[] args) { }

    public string ErrorMessage { get; protected set; }
    public Microsoft.Scripting.Hosting.ErrorSink Errors;
    public Microsoft.Scripting.Hosting.SourceUnit SourceUnit;

  }
}
