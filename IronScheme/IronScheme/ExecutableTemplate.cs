using System;
using System.Configuration;
using System.Reflection;
using System.IO;

namespace IronScheme
{
  class ExecutableTemplate
  {
    static void Main(string[] args)
    {
      Assembly ass = typeof(ExecutableTemplate).Assembly;

      string path = ConfigurationManager.AppSettings["IronScheme.Directory"] as string;

      AppDomainSetup ads = new AppDomainSetup();
      ads.PrivateBinPath = path;
      ads.ApplicationBase = path;

      AppDomain ad = AppDomain.CreateDomain("IronScheme", null, ads);

      Stream s = ass.GetManifestResourceStream("AA853EBC-97FA-4e82-86FD-749009FDDE5D.sps");
      ad.CreateInstance("IronScheme", "IronScheme.ExecutableLoader", false, 0, null, new object[] { s, args}, null, null, null);
    }
  }
}