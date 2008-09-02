using System;
using System.Collections.Generic;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using Microsoft.Scripting;
using Microsoft.Scripting.Hosting;
using IronScheme.Hosting;
using IronScheme.Runtime;
using System.IO;
using System.Web.SessionState;
using System.Security.Principal;

namespace IronScheme.Web
{
  public sealed class Handler : IHttpHandler, IRequiresSessionState
  {
    IronSchemeLanguageProvider lp;
    IScriptEngine se;
    static readonly object outlock = new object();
    ICallable process_routes;

    class Compiled
    {
      public DateTime Time;
      public ICallable Closure;
    }

    static Dictionary<string, Compiled> compiled = new Dictionary<string, Compiled>();
    
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

      if (context.User == null)
      {
        context.User = new GenericPrincipal(new GenericIdentity(""), new string[0]);
      }

      if (!File.Exists(context.Request.PhysicalPath))
      {
        if (context.Request.AppRelativeCurrentExecutionFilePath == "~/process-routes.ss")
        {
          if (process_routes == null)
          {
            ICallable eval = Builtins.SymbolValue(SymbolTable.StringToId("eval-r6rs")) as ICallable;
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
