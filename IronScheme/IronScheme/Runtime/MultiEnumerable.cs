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
using System.Collections;

namespace IronScheme.Runtime
{
  sealed class MultiEnumerable : IEnumerable
  {
    Cons[] lists;
    public MultiEnumerable(params object[] lists)
    {
      this.lists = lists as Cons[];
    }

    #region IEnumerable Members

    public IEnumerator GetEnumerator()
    {
      return new MultiEnumerator(this);
    }

    #endregion

    sealed class MultiEnumerator : IEnumerator, IDisposable
    {
      Cons[] iters;

      public MultiEnumerator(MultiEnumerable container)
      {
        iters = container.lists.Clone() as Cons[];
      }
      #region IEnumerator Members

      public object Current
      {
        get 
        {
          object[] v = new object[iters.Length];

          for (int i = 0; i < iters.Length; i++)
          {
            v[i] = iters[i].Car;
          }

          return v;
        }
      }

      public bool MoveNext()
      {
        for (int i = 0; i < iters.Length; i++)
        {
          iters[i] = iters[i].Cdr as Cons;
          if (iters[i] == null)
          {
            return false;
          }
        }
        return true;
      }

      public void Reset()
      {

      }

      #endregion

      #region IDisposable Members

      public void Dispose()
      {

      }

      #endregion
    }
  }
}
