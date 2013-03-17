#region License
/* Copyright (c) 2007-2013 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System.Web;
using IronScheme.Hosting;
using Microsoft.Scripting;
using Microsoft.Scripting.Hosting;

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
