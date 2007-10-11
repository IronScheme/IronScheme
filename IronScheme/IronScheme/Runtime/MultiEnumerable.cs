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
using System.Collections;

namespace IronScheme.Runtime
{
  sealed class MultiEnumerable : IEnumerable
  {
    IEnumerable[] lists;
    public MultiEnumerable(params object[] lists)
    {
      this.lists = Array.ConvertAll<object,IEnumerable>(lists , delegate(object o) { return o as IEnumerable ; });
    }

    #region IEnumerable Members

    public IEnumerator GetEnumerator()
    {
      return new MultiEnumerator(this);
    }

    #endregion

    sealed class MultiEnumerator : IEnumerator, IDisposable
    {
      MultiEnumerable container;
      List<IEnumerator> iters = new List<IEnumerator>();

      public MultiEnumerator(MultiEnumerable container)
      {
        this.container = container;

        foreach (IEnumerable e in container.lists)
        {
          iters.Add(e.GetEnumerator());
        }
      }
      #region IEnumerator Members

      public object Current
      {
        get 
        {
          List<object> currents = new List<object>();
          foreach (IEnumerator e in iters)
          {
            currents.Add(e.Current);
          }
          return currents;
        }
      }

      public bool MoveNext()
      {
        foreach (IEnumerator e in iters)
        {
          if (!e.MoveNext())
          {
            return false;
          }
        }
        return true;
      }

      public void Reset()
      {
        foreach (IEnumerator e in iters)
        {
          e.Reset();
        }
      }

      #endregion

      #region IDisposable Members

      public void Dispose()
      {
        foreach (IEnumerator e in iters)
        {
          IDisposable id = e as IDisposable;
          if (id != null)
          {
            id.Dispose();
          }
        }
      }

      #endregion
    }
  }
}
