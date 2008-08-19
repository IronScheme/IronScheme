using System;
using System.Web;
using System.IO;
using System.Configuration;
using System.Web.Configuration;
using System.Reflection;
using System.Security.Principal;

namespace IronScheme.Web
{
  public class RoutingModule : IHttpModule
  {
    public void Dispose()
    {
    }

    public void Init(HttpApplication app)
    {
      app.PostResolveRequestCache += new EventHandler(app_PostResolveRequestCache);
      app.AuthorizeRequest += new EventHandler(app_AuthorizeRequest);
    }
    
    #region Fix authorization that does not appear to work on non-existing directories

    static PropertyInfo everyoneallowed = typeof(AuthorizationSection).GetProperty("EveryoneAllowed", 
      BindingFlags.Instance | BindingFlags.NonPublic);

    static MethodInfo checkuser = typeof(AuthorizationSection).GetMethod("IsUserAllowed",
      BindingFlags.Instance | BindingFlags.NonPublic);

    void app_AuthorizeRequest(object sender, EventArgs e)
    {
      HttpApplication app = sender as HttpApplication;

      Configuration c = WebConfigurationManager.OpenWebConfiguration("/");

      var s = app.Request.AppRelativeCurrentExecutionFilePath;

      foreach (ConfigurationLocation loc in c.Locations)
      {
        if (s.StartsWith(loc.Path))
        {
          Configuration sc = loc.OpenConfiguration();
          AuthorizationSection ac = sc.GetSection("system.web/authorization") as AuthorizationSection;

          if (ac != null)
          {
            if (!(bool)everyoneallowed.GetValue(ac, null))
            {
              bool ok = (bool)checkuser.Invoke(ac, new object[] { app.User, app.Request.HttpMethod });
              if (!ok)
              {
                app.Context.Response.StatusCode = 0x191;
                app.CompleteRequest();
              }
            }
          }
        }
      }
    }

    #endregion

    void app_PostResolveRequestCache(object sender, EventArgs e)
    {
      HttpApplication app = sender as HttpApplication;
      if (!File.Exists(app.Request.PhysicalPath))
      {
        app.Context.RewritePath("~/router.ss");
      }
    }
  }
}
