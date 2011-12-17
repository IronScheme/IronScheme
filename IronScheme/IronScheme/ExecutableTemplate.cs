using System.IO;
using IronScheme.Runtime;

namespace IronScheme
{
  class ExecutableTemplate
  {
    static void Main(string[] args)
    {
      using (TextReader r = new StreamReader(typeof(ExecutableTemplate).Assembly.GetManifestResourceStream("AA853EBC-97FA-4e82-86FD-749009FDDE5D.sps")))
      {
        Cons cmdline = Cons.FromList(args);
        RuntimeExtensions.Eval("(apply load-port {0} {1})", r, cmdline);
      }
    }
  }
}
