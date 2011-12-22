using System;
using System.Configuration;
using System.Reflection;
using System.IO;

namespace IronScheme
{
  partial class ExecutableTemplate
  {
    static void Main(string[] args)
    {
      Assembly ass = typeof(ExecutableTemplate).Assembly;

      string path = ConfigurationManager.AppSettings["IronScheme.Directory"] as string;

      if (path == null)
      {
        path = PATH;
      }

      AppDomainSetup ads = new AppDomainSetup();
      ads.PrivateBinPath = path;
      ads.ApplicationBase = path;

      AppDomain ad = AppDomain.CreateDomain("IronScheme", null, ads);

      Stream s = ass.GetManifestResourceStream(RESOURCE);
      ad.CreateInstance("IronScheme", "IronScheme.ExecutableLoader", false, 0, null, new object[] { s, args}, null, null, null);
    }
  }
}