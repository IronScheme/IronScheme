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
using Microsoft.Scripting.Ast;
using Microsoft.Scripting;

namespace IronScheme.Compiler
{
  [AttributeUsage(AttributeTargets.Method, AllowMultiple=true)]
  public class InlineEmitterAttribute : Attribute
  {
    readonly string name;

    public string Name
    {
      get { return name; }
    }

    public InlineEmitterAttribute()
    {
    }

    public InlineEmitterAttribute(string name)
    {
      this.name = name;
    }
  }

  public delegate Expression InlineEmitter(CodeContext cc, params Expression[] args);
}
