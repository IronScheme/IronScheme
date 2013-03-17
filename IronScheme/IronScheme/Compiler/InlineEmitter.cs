#region License
/* Copyright (c) 2007-2013 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using Microsoft.Scripting.Ast;

namespace IronScheme.Compiler
{
  // there arent many places this is viable, except where the types are known and IL can be generated more efficiently
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

  public delegate Expression InlineEmitter(params Expression[] args);
}
