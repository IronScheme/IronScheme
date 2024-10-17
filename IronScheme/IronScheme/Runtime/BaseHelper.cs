#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using IronScheme.Compiler;
using IronScheme.Hosting;
using Microsoft.Scripting;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Hosting;

namespace IronScheme.Runtime
{
  public abstract class BaseHelper : Glue
  {
    static IronSchemeScriptEngine se;
    internal static CodeContext cc;
    internal static ScriptModule scriptmodule;
    internal static ActionBinder binder;
    static LanguageProvider lp;

    protected static LanguageProvider LanguageProvider
    {
      get { return lp; }
    }

    protected static ScriptEngine ScriptEngine
    {
      get { return se; }
    }

    protected static CodeContext Context
    {
      get { return cc; }
    }

    protected static ActionBinder Binder
    {
      get { return binder; }
    }

    internal static void Initialize(IronSchemeLanguageProvider ironSchemeLanguageProvider)
    {
      lp = ironSchemeLanguageProvider;
      se = lp.GetEngine() as IronSchemeScriptEngine;

      scriptmodule = ScriptDomainManager.CurrentManager.Host.DefaultModule as ScriptModule;

      var mc = new ModuleContext(scriptmodule);

      mc.CompilerContext = new CompilerContext(SourceUnit.CreateSnippet(se, ""));

      cc = new CodeContext(scriptmodule.Scope, se.LanguageContext, mc);

      binder = new Actions.IronSchemeActionBinder(cc);

      MethodCallExpression.BuiltinsIsTrue = typeof(Builtins).GetMethod("IsTrue");

      Generator.initme = true;
    }
  }
}
