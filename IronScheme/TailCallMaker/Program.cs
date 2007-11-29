using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace TailCallMaker
{
  class Program
  {
    static void Main(string[] args)
    {
      string fn = args[0];

      List<string> lines = new List<string>();

      using (TextReader r = File.OpenText(fn))
      {
        string line = null;
        while ((line = r.ReadLine()) != null)
        {
          lines.Add(line);
        }
      }

      for (int i = 0; i < lines.Count; i++)
      {
        string line = lines[i];

        int ci = line.IndexOf(':');

        if (ci >= 0)
        {

          if (line.Substring(ci + 1).Trim().StartsWith("ret"))
          {
            string prevline = lines[i - 1];

            ci = prevline.IndexOf(':');

            if (ci >= 0)
            {

              if (prevline.Substring(ci + 1).Trim().StartsWith("call"))
              {
                lines[i - 1] = "tail. " + prevline;
              }
            }
          }
        }
      }

      using (TextWriter w = File.CreateText(fn))
      {
        foreach (string line in lines)
        {
          w.WriteLine(line);
        }
      }
    }
  }
}
