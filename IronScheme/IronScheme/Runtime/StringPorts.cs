using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace IronScheme.Runtime
{
  // i wonder if these are still being used... ?
  //partial class Builtins
  //{
  //  [Builtin("open-input-string")]
  //  public static object OpenInputString(object str)
  //  {
  //    string s = RequiresNotNull<string>(str);
  //    return new StringReader(s);
  //  }
    
  //  [Builtin("open-output-string")]
  //  public static object OpenOutputString()
  //  {
  //    return new StringWriter();
  //  }

  //  [Builtin("get-output-string")]
  //  public static object GetOutputString(object outputport)
  //  {
  //    StringWriter w = RequiresNotNull<StringWriter>(outputport);
  //    return w.ToString();
  //  }
  //}


}
