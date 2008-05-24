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

namespace IronScheme.Web
{
  public sealed class Handler : IHttpHandler, IRequiresSessionState
  {
    IronSchemeLanguageProvider lp;
    IScriptEngine se;

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
      if (!File.Exists(context.Request.PhysicalPath))
      {
        context.Response.StatusCode = 404;
        return;
      }
      lock (this)
      {
        if (lp == null)
        {
          lp = context.Application["LanguageProvider"] as IronSchemeLanguageProvider;
          if (lp == null)
          {
            try
            {
              ScriptDomainManager sdm = ScriptDomainManager.CurrentManager;
              sdm.GlobalOptions.AssemblyGenAttributes = Microsoft.Scripting.Generation.AssemblyGenAttributes.None;
              lp = new IronSchemeLanguageProvider(sdm);
              se = lp.GetEngine();
              se.Execute("(load \"~/ironscheme.boot.pp\")");
              context.Application["LanguageProvider"] = lp;
              compiled.Clear();
            }
            catch
            {
              lp = null;
              se = null;

              throw;
            }
          }
          else
          {
            se = lp.GetEngine();
          }
        }
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
      
      object oldo = Builtins.CurrentOutputPort();
      try
      {
        Builtins.CurrentOutputPort(context.Response.Output);
        cc.Closure.Call();
      }
      finally
      {
        Builtins.CurrentOutputPort(oldo);
      }
    }

    
  }
}
