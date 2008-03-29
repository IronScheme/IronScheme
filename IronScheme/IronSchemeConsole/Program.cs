#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Permissive License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Permissive License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using IronScheme.Hosting;
using Microsoft.Scripting;
using IronScheme.Runtime;
using System.Runtime;

namespace IronScheme.Runtime
{
  class Program
  {
    static int Main(string[] args)
    {
      // dont use this and pass -notabcompletion at the cl to use with mono
//#if !MONO
//      System.Runtime.GCSettings.LatencyMode = GCLatencyMode.Interactive;
//#endif
      return new IronSchemeConsoleHost().Run(args);
    }
  }
}
