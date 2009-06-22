﻿#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/
#endregion

using System;
using System.Web;
using System.IO;
using System.Configuration;
using System.Web.Configuration;
using System.Reflection;
using System.Security.Principal;
using System.Web.Security;

namespace IronScheme.Web
{
  public sealed class RoutingModule : IHttpModule
  {
    public void Dispose()
    {
    }

    class TraceWriter : TextWriter
    {
      public override void Write(string value)
      {
        System.Diagnostics.Trace.Write(value);
      }

      public override void Write(char value)
      {
        System.Diagnostics.Trace.Write(value);
      }


      public override System.Text.Encoding Encoding
      {
        get { return System.Text.Encoding.Default; }
      }
    }

    public void Init(HttpApplication app)
    {
      System.Diagnostics.Trace.AutoFlush = true;
      Console.SetOut(new TraceWriter());
      app.PostResolveRequestCache += new EventHandler(app_PostResolveRequestCache);
      app.AuthorizeRequest += new EventHandler(app_AuthorizeRequest);
      app.AuthenticateRequest += new EventHandler(app_AuthenticateRequest);
    }


    #region Fix FormsAuthentication that does not appear to be working in IIS 7.0 Integrated Pipepline mode

    void app_AuthenticateRequest(object sender, EventArgs e)
    {
      HttpApplication app = sender as HttpApplication;

      HttpContext context = app.Context;

      if (context.User == null)
      {
        HttpCookie authc = context.Request.Cookies[FormsAuthentication.FormsCookieName];
        if (authc != null)
        {
          FormsAuthenticationTicket fat = FormsAuthentication.Decrypt(authc.Value);

          if (fat.Expired)
          {
            context.User = new GenericPrincipal(new GenericIdentity(""), new string[0]);
          }
          else
          {
            context.User = new GenericPrincipal(new FormsIdentity(FormsAuthentication.RenewTicketIfOld(fat)), new string[0]);
          }
        }
        else
        {
          context.User = new GenericPrincipal(new GenericIdentity(""), new string[0]);
        }
      }
    }

    #endregion

    #region Fix authorization that does not appear to work on non-existing directories

    static PropertyInfo everyoneallowed = typeof(AuthorizationSection).GetProperty("EveryoneAllowed", 
      BindingFlags.Instance | BindingFlags.NonPublic);

    static MethodInfo checkuser = typeof(AuthorizationSection).GetMethod("IsUserAllowed",
      BindingFlags.Instance | BindingFlags.NonPublic);

    readonly static string[] DISALLOWED = 
    { 
      "~/web.routes", 
      "~/controllers", 
      "~/views", 
      "~/models", 
      "~/data" 
    };

    void app_AuthorizeRequest(object sender, EventArgs e)
    {
      HttpApplication app = sender as HttpApplication;

      string s = app.Request.AppRelativeCurrentExecutionFilePath;

      foreach (string disp in DISALLOWED)
      {
        if (s.StartsWith(disp))
        {
          app.Context.Response.StatusCode = 403;
          app.CompleteRequest();
          return;
        }
      }

      Configuration c = WebConfigurationManager.OpenWebConfiguration("~/");

      if (!CheckAuth(app, c))
      {
        return;
      }

      foreach (ConfigurationLocation loc in c.Locations)
      {
        if (s.StartsWith(loc.Path))
        {
          Configuration sc = loc.OpenConfiguration();
          if (!CheckAuth(app, sc))
          {
            return;
          }
        }
      }
    }

    static bool CheckAuth(HttpApplication app, Configuration sc)
    {
      AuthorizationSection ac = sc.GetSection("system.web/authorization") as AuthorizationSection;

      if (ac != null)
      {
        if (!(bool)everyoneallowed.GetValue(ac, null))
        {
          bool ok = (bool)checkuser.Invoke(ac, new object[] { app.User, app.Request.HttpMethod });
          if (!ok)
          {
            app.Context.Response.StatusCode = 401;
            app.CompleteRequest();
            return false;
          }
        }
      }

      return true;
    }

    #endregion


    void app_PostResolveRequestCache(object sender, EventArgs e)
    {
      HttpApplication app = sender as HttpApplication;
      if (!File.Exists(app.Request.PhysicalPath) && Path.GetExtension(app.Request.PhysicalPath).Length <= 1)
      {
        app.Context.RewritePath("~/process-routes.ss");
      }
    }
  }
}
