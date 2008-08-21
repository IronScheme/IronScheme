﻿using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using IronScheme.Hosting;
using Microsoft.Scripting.Hosting;
using System.Web;

namespace IronScheme.Web
{
  static class Helpers
  {
    readonly static object initlock = new object();

    static Helpers()
    {
    }

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
      sdm.GlobalOptions.AssemblyGenAttributes = Microsoft.Scripting.Generation.AssemblyGenAttributes.None;
      IronSchemeLanguageProvider lp = new IronSchemeLanguageProvider(sdm);
      ScriptEngine se = lp.GetEngine();
      se.Execute("(load \"~/ironscheme.boot.pp\")");
      return lp;
    }
  }
}
