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
    ICallable process_routes;
    Dictionary<string, Compiled> compiled = new Dictionary<string, Compiled>();

    class Compiled
    {
      public DateTime Time;
      public ICallable Closure;
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
        compiled.Clear();
      }
      
      if (!File.Exists(context.Request.PhysicalPath))
      {
        if (context.Request.AppRelativeCurrentExecutionFilePath == "~/process-routes.ss")
        {
          if (process_routes == null)
          {
            ICallable eval = Builtins.SymbolValue(SymbolTable.StringToObject("eval-r6rs")) as ICallable;
            StringReader r = new StringReader("(eval 'process-request (environment '(ironscheme web routing)))");

            process_routes = eval.Call(Builtins.Read(r)) as ICallable;
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

      lock (this)
      {
        if (!compiled.TryGetValue(context.Request.PhysicalPath, out cc) || cc.Time < File.GetLastWriteTime(context.Request.PhysicalPath))
        {
          ICallable ccc = se.Evaluate(string.Format("(compile->closure \"{0}\")", context.Request.PhysicalPath.Replace('\\', '/'))) as ICallable;
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
