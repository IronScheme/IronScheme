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
using IronScheme.Runtime;

namespace IronScheme.Console
{
  public class ConsoleBuiltins : Builtins
  {
    [Builtin("all-defined")]
    public static Cons Environment(CodeContext cc)
    {
      List<SymbolId> s = new List<SymbolId>(cc.Scope.Keys);
      s.Sort(delegate(SymbolId a, SymbolId b)
      {
        return SymbolTable.IdToString(a).CompareTo(SymbolTable.IdToString(b));
      });
      return Runtime.Cons.FromList(s);
    }

    [Builtin]
    public static object Describe(object obj)
    {

      return Unspecified;
    }

    [Builtin]
    public static object Help()
    {

      return Unspecified;
    }

    [Builtin("enable-macro-trace")]
    public static object EnableMacroTrace()
    {
      Compiler.Generator.MacroTrace = true;
      return Unspecified;
    }

    [Builtin("disable-macro-trace")]
    public static object DisableMacroTrace()
    {
      Compiler.Generator.MacroTrace = false;
      return Unspecified;
    }

    [Builtin]
    public static object Help(object obj)
    {

      return Unspecified;
    }

    [Builtin]
    public static object License()
    {
      return Display(@"Microsoft Public License (Ms-PL)
================================

This license governs use of the accompanying software. If you use the software, you accept this license. If you do not accept the license, do not use the software.

1. Definitions
The terms ""reproduce,"" ""reproduction,"" ""derivative works,"" and ""distribution"" have the same meaning here as under U.S. copyright law.
A ""contribution"" is the original software, or any additions or changes to the software.
A ""contributor"" is any person that distributes its contribution under this license.
""Licensed patents"" are a contributor's patent claims that read directly on its contribution.

2. Grant of Rights
(A) Copyright Grant- Subject to the terms of this license, including the license conditions and limitations in section 3, each contributor grants you a non-exclusive, worldwide, royalty-free copyright license to reproduce its contribution, prepare derivative works of its contribution, and distribute its contribution or any derivative works that you create.
(B) Patent Grant- Subject to the terms of this license, including the license conditions and limitations in section 3, each contributor grants you a non-exclusive, worldwide, royalty-free license under its licensed patents to make, have made, use, sell, offer for sale, import, and/or otherwise dispose of its contribution in the software or derivative works of the contribution in the software.

3. Conditions and Limitations
(A) No Trademark License- This license does not grant you rights to use any contributors' name, logo, or trademarks.
(B) If you bring a patent claim against any contributor over patents that you claim are infringed by the software, your patent license from such contributor to the software ends automatically.
(C) If you distribute any portion of the software, you must retain all copyright, patent, trademark, and attribution notices that are present in the software.
(D) If you distribute any portion of the software in source code form, you may do so only under this license by including a complete copy of this license with your distribution. If you distribute any portion of the software in compiled or object code form, you may only do so under a license that complies with this license.
(E) The software is licensed ""as-is."" You bear the risk of using it. The contributors give no express warranties, guarantees or conditions. You may have additional consumer rights under your local laws which this license cannot change. To the extent permitted under your local laws, the contributors exclude the implied warranties of merchantability, fitness for a particular purpose and non-infringement.
");
    }


  }
}
