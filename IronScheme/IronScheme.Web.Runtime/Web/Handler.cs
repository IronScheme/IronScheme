#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.IO;
using System.Web;
using System.Web.SessionState;
using IronScheme.Hosting;
using IronScheme.Runtime;
using Microsoft.Scripting;
using Microsoft.Scripting.Hosting;


namespace IronScheme.Web
{
  public sealed class Handler : IHttpHandler, IRequiresSessionState
  {
    IronSchemeLanguageProvider lp;
    IScriptEngine se;
    Callable process_routes;
    Dictionary<string, Compiled> compiled = new Dictionary<string, Compiled>();

    static object GLOBALLOCK = new object();

    class Compiled
    {
      public DateTime Time;
      public Callable Closure;
    }

    public bool IsReusable
    {
      get { return true; }
    }

    public void ProcessRequest(HttpContext context)
    {
      if (lp == null)
      {
        lp = Helpers.Provider as IronSchemeLanguageProvider;
        se = lp.GetEngine();
        RuntimeExtensions.Eval("(library-path (cons {0} (library-path)))", context.Server.MapPath("~/lib"));
        compiled.Clear();
      }

      if (!File.Exists(context.Request.PhysicalPath))
      {
        if (context.Request.AppRelativeCurrentExecutionFilePath == "~/process-routes.ss")
        {
          if (process_routes == null)
          {
            Callable eval = Builtins.SymbolValue(SymbolTable.StringToObject("eval-r6rs")) as Callable;
            StringReader r = new StringReader("(eval 'process-request (environment '(ironscheme web routing)))");

            process_routes = eval.Call(Builtins.Read(r)) as Callable;
          }
          process_routes.Call();
        }
        else
        {
          context.Response.StatusCode = 404;
        }
        return;
      }


      Compiled cc;

      lock (GLOBALLOCK)
      {
        if (!compiled.TryGetValue(context.Request.PhysicalPath, out cc) || cc.Time < File.GetLastWriteTime(context.Request.PhysicalPath) || cc.Closure == null)
        {
          Callable ccc = se.Evaluate(string.Format("(compile->closure \"{0}\")", context.Request.PhysicalPath.Replace('\\', '/'))) as Callable;
          cc = new Compiled();
          cc.Time = DateTime.Now;
          cc.Closure = ccc;

          compiled[context.Request.PhysicalPath] = cc;
        }
      }

      cc.Closure.Call();
    }
  }
}
