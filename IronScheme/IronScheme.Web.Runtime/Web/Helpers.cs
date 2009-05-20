using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using IronScheme.Hosting;
using Microsoft.Scripting.Hosting;
using System.Web;
using System.Threading;

namespace IronScheme.Web
{
  static class Helpers
  {
    readonly static object initlock = new object();

    public static LanguageProvider Provider
    {
      get
      {
        lock (initlock)
        {
          LanguageProvider lp = HttpContext.Current.Application["LanguageProvider"] as LanguageProvider;

          if (lp == null)
          {
            HttpContext.Current.Application["LanguageProvider"] = lp = GetLanguageProvider();
          }

          return lp;
        }
      }
    }

    static LanguageProvider GetLanguageProvider()
    {
      ScriptDomainManager sdm = ScriptDomainManager.CurrentManager;
      return sdm.GetLanguageProvider(typeof(IronSchemeLanguageProvider));
    }
  }
}
