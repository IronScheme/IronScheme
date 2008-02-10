using System;
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

  public class Handler : IHttpHandler, IRequiresSessionState
  {
    IronSchemeLanguageProvider lp;


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
          if (context.Application["lp"] == null)
          {
            ScriptDomainManager sdm = ScriptDomainManager.CurrentManager;
            Environment.CurrentDirectory = Builtins.ApplicationDirectory;
            lp = new IronSchemeLanguageProvider(sdm);
            lp.GetEngine().Execute("(load \"init.scm\")");
            context.Application["lp"] = lp;
          }
          else
          {
            lp = context.Application["lp"] as IronSchemeLanguageProvider;
          }

        }
      }
      
      object oldo = Builtins.CurrentOutputPort();
      try
      {
        Builtins.CurrentOutputPort(context.Response.Output);
        lp.GetEngine().Execute(string.Format("(eval-r6rs '(load \"{0}\"))", context.Request.PhysicalPath.Replace('\\', '/')));
      }
      finally
      {
        Builtins.CurrentOutputPort(oldo);
      }
    }

    
  }
}
