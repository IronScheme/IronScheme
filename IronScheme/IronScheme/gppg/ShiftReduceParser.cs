// Gardens Point Parser Generator
// Copyright (c) Wayne Kelly, QUT 2005-2006
// (see accompanying GPPGcopyright.rtf)

using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;
using Microsoft.Scripting;


namespace gppg
{
  public abstract class ShiftReduceParser<YYSTYPE, YYLTYPE>
    where YYSTYPE : struct
    where YYLTYPE : IMerge<YYLTYPE>
  {
    public bool Trace = false;
    public IScanner<YYSTYPE, YYLTYPE> scanner;

    protected YYSTYPE yyval;
    protected YYLTYPE yyloc;
    // Experimental : last yylloc prior to call of yylex()
    protected YYLTYPE lastL;

    private int next;
    private State current_state;

    private bool recovering;
    private int tokensSinceLastError;

    private ParserStack<State> state_stack = new ParserStack<State>();
    protected ParserStack<YYSTYPE> value_stack = new ParserStack<YYSTYPE>();
    protected ParserStack<YYLTYPE> location_stack = new ParserStack<YYLTYPE>();

    protected string[] nonTerminals;
    protected State[] states;
    protected Rule[] rules;

    protected int errToken;
    protected int eofToken;



    protected abstract void Initialize();


    public bool Parse()
    {
      Initialize();	// allow derived classes to instantiate rules, states and nonTerminals

      next = 0;
      current_state = states[0];

      state_stack.Push(current_state);
      value_stack.Push(yyval);
      location_stack.Push(yyloc);

      while (true)
      {

        int action = current_state.defaultAction;

        if (current_state.parser_table != null)
        {
          if (next == 0)
          {
            // We save the last token span, so that the location span
            // of production right hand sides that begin or end with a
            // nullable production will be correct.
            lastL = scanner.yylloc;
            next = scanner.yylex();
          }

          int vnext;
          if (current_state.parser_table.TryGetValue(next, out vnext))
            action = vnext;
        }

        if (action > 0)         // shift
        {
          Shift(action);
        }
        else if (action < 0)   // reduce
        {
          Reduce(-action);

          if (action == -1)	// accept
            return true;
        }
        else if (action == 0)   // error
          if (!ErrorRecovery())
            return false;
      }
    }


    protected void Shift(int state_nr)
    {
      current_state = states[state_nr];

      value_stack.Push(scanner.yylval);
      state_stack.Push(current_state);
      location_stack.Push(scanner.yylloc);

      if (recovering)
      {
        if (next != errToken)
          tokensSinceLastError++;

        if (tokensSinceLastError > 5)
          recovering = false;
      }

      if (next != eofToken)
        next = 0;
    }


    protected void Reduce(int rule_nr)
    {
      Rule rule = rules[rule_nr];
      //
      //  Default action "$$ = $1" for unit productions.
      //
      if (rule.rhs.Length == 1)
        yyval = value_stack.Top(); // default action: $$ = $1;
      else
        yyval = new YYSTYPE();
      //
      //  Default action "@$ = @1.Merge(@N)" for location info.
      //
      if (rule.rhs.Length == 1)
        yyloc = location_stack.Top();
      else if (rule.rhs.Length == 0)
        // The location span for an empty production will start with the
        // beginning of the next lexeme, and end with the finish of the
        // previous lexeme.  This gives the correct behaviour when this
        // nonsense value is used in later Merge operations.
        yyloc = (scanner.yylloc != null ? scanner.yylloc.Merge(lastL) : default(YYLTYPE));
      else
      {
        YYLTYPE at1 = location_stack.array[location_stack.top - rule.rhs.Length];
        YYLTYPE atN = location_stack.array[location_stack.top - 1];
        if (at1 != null && atN != null) yyloc = at1.Merge(atN);
      }

      DoAction(rule_nr);

      for (int i = 0; i < rule.rhs.Length; i++)
      {
        state_stack.Pop();
        value_stack.Pop();
        location_stack.Pop();
      }

      current_state = state_stack.Top();

      int vlhs;
      if (current_state.Goto.TryGetValue(rule.lhs, out vlhs))
        current_state = states[vlhs];

      state_stack.Push(current_state);
      value_stack.Push(yyval);
      location_stack.Push(yyloc);
    }


    protected abstract void DoAction(int action_nr);

    public bool ErrorRecovery()
    {
      bool discard;

      if (!recovering) // if not recovering from previous error
        ReportError();

      if (!FindErrorRecoveryState())
        return false;
      //
      //  The interim fix for the "looping in error recovery"
      //  artifact involved moving the setting of the recovering 
      //  bool until after invalid tokens have been discarded.
      //
      ShiftErrorToken();
      discard = DiscardInvalidTokens();
      recovering = true;
      tokensSinceLastError = 0;
      return discard;
    }


    public void ReportError()
    {
      StringBuilder errorMsg = new StringBuilder();
      errorMsg.AppendFormat("unexpected {0}", TerminalToString(next));

      if (current_state.parser_table.Count < 20)
      {
        bool first = true;
        foreach (int terminal in current_state.parser_table.Keys)
        {
          if (first)
            errorMsg.Append(", expecting ");
          else
            errorMsg.Append(", or ");

          errorMsg.Append(TerminalToString(terminal));
          first = false;
        }
      }

      if (scanner.Errors != null)
      {
        scanner.Errors.Add(scanner.SourceUnit, errorMsg.ToString(), GetLocation(lastL), 1, Microsoft.Scripting.Hosting.Severity.Error);
      }
      System.Diagnostics.Trace.WriteLine(errorMsg.ToString());
    }

    protected abstract SourceSpan GetLocation(YYLTYPE lastL);

    public void ShiftErrorToken()
    {
      int old_next = next;
      next = errToken;

      Shift(current_state.parser_table[next]);

      next = old_next;
    }


    public bool FindErrorRecoveryState()
    {
      while (true)    // pop states until one found that accepts error token
      {
        if (current_state.parser_table != null)
        {
          int errt;
          if (current_state.parser_table.TryGetValue(errToken, out errt) &&
            errt > 0) // shift
            return true;
        }

        state_stack.Pop();
        value_stack.Pop();
        location_stack.Pop();

        if (state_stack.IsEmpty())
        {
          return false;
        }
        else
          current_state = state_stack.Top();
      }
    }


    public bool DiscardInvalidTokens()
    {

      int action = current_state.defaultAction;

      if (current_state.parser_table != null)
      {
        // Discard tokens until find one that works ...
        while (true)
        {
          if (next == 0)
          {
            next = scanner.yylex();
          }

          if (next == eofToken)
            return false;

          int taction;
          if (current_state.parser_table.TryGetValue(next, out taction))
            action = taction;

          if (action != 0)
            return true;
          else
          {
            next = 0;
          }
        }
      }
      else if (recovering && tokensSinceLastError == 0)
      {
        // 
        //  Boolean recovering is not set until after the first
        //  error token has been shifted.  Thus if we get back 
        //  here with recovering set and no tokens read we are
        //  looping on the same error recovery action.  This 
        //  happens if current_state.parser_table is null because
        //  the state has an LR(0) reduction, but not all
        //  lookahead tokens are valid.  This only occurs for
        //  error productions that *end* on "error".
        //
        //  This action discards tokens one at a time until
        //  the looping stops.  Another attack would be to always
        //  use the LALR(1) table if a production ends on "error"
        //
        next = 0;
        return true;
      }
      else
        return true;

    }


    protected void yyclearin()  // experimental in this version.
    {
      next = 0;
    }

    protected void yyerrok()
    {
      recovering = false;
    }


    protected void AddState(int statenr, State state)
    {
      states[statenr] = state;
      state.num = statenr;
    }

    protected abstract string TerminalToString(int terminal);

    private string SymbolToString(int symbol)
    {
      if (symbol < 0)
        return nonTerminals[-symbol];
      else
        return TerminalToString(symbol);
    }


    protected string CharToString(char ch)
    {
      switch (ch)
      {
        case '\a': return @"'\a'";
        case '\b': return @"'\b'";
        case '\f': return @"'\f'";
        case '\n': return @"'\n'";
        case '\r': return @"'\r'";
        case '\t': return @"'\t'";
        case '\v': return @"'\v'";
        case '\0': return @"'\0'";
        default: return string.Format("'{0}'", ch);
      }
    }
  }
}