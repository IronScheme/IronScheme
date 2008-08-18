using System;
using System.Data;
using System.Configuration;
using System.Linq;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.HtmlControls;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Xml.Linq;
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
