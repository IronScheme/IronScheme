using System;
using System.Web;
using System.IO;

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
    }

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
