#region License
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
