using System;
using System.Collections.Generic;
using System.Collections;

namespace Microsoft.Scripting
{
    public class Storage<T> : IAttributesCollection
  {
    #region IAttributesCollection Members

    public bool TryGetValue(SymbolId name, out object value)
    {
      var s = SymbolTable.IdToString(name);
      var fi = Data.GetType().GetField(s);
      if (fi == null)
      {
        value = null;
        return false;
      }
      value = fi.GetValue(Data);
      return true;
    }

    public bool Remove(SymbolId name)
    {
      throw new NotImplementedException();
    }

    public object this[SymbolId name]
    {
      get
      {
        throw new NotImplementedException();
      }
      set
      {
        throw new NotImplementedException();
      }
    }

    public IEnumerable<SymbolId> Keys
    {
      get
      {
        foreach (var fi in Data.GetType().GetFields())
        {
          if (fi.Name != "$parent$")
          {
            yield return SymbolTable.StringToId(fi.Name);
          }
        }
      }
    }

    #endregion

    #region IEnumerable<KeyValuePair<object,object>> Members

    public IEnumerator<KeyValuePair<object, object>> GetEnumerator()
    {
      throw new NotImplementedException();
    }

    #endregion

    #region IEnumerable Members

    IEnumerator IEnumerable.GetEnumerator()
    {
      throw new NotImplementedException();
    }

    #endregion

    public T Data { get; private set; }

    public Storage(T data)
    {
      Data = data;
    }
  }
}
