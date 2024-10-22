/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. A 
 * copy of the license can be found in the License.html file at the root of this distribution. If 
 * you cannot locate the  Microsoft Public License, please send an email to 
 * dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 *
 *
 * ***************************************************************************/

using System;
using System.Collections.Generic;
using System.Threading;
using System.Text.RegularExpressions;

namespace Microsoft.Scripting
{
    public static class SymbolTable {
        readonly static Dictionary<string, int> _idDict = new Dictionary<string, int>(InitialTableSize);

        private const int InitialTableSize = 8192;
        readonly static Dictionary<int, string> _fieldDict = new Dictionary<int, string>(InitialTableSize);
        readonly static Dictionary<SymbolId, object> _boxDict = new Dictionary<SymbolId, object>(InitialTableSize);

        private static int _nextCaseInsensitiveId = 1;

        static SymbolTable() {
            _fieldDict[0] = null;   // initialize the null string
        }

        readonly static Regex unichar = new Regex(@"\\x[\da-f]+;", RegexOptions.Compiled | RegexOptions.IgnoreCase);

        public static object GetSymbol(int id)
        {
          object value;
          SymbolId sid = new SymbolId(id);
          if (_boxDict.TryGetValue(sid, out value))
          {
            return value;
          }
          else
          {
            return _boxDict[sid] = sid;
          }
        }

        public static object StringToObjectWithCase(string name, bool foldcase)
        {
          if (foldcase)
          {
            name = name.ToUpperInvariant().ToLowerInvariant();
          }
          SymbolId id = StringToId(name);
          return GetSymbol(id.Id);
        }

        public static object Intern(SymbolId s)
        {
          return GetSymbol(s.Id);
        }

        public static object StringToObject(string name)
        {
          SymbolId id = StringToId(name);
          return GetSymbol(id.Id);
        }

        public static SymbolId StringToId(string field) {
            if (field == null) {
                throw new ArgumentNullException(Resources.NameMustBeString);
            }

            // convert unicode escapes
            field = unichar.Replace(field, delegate(Match m)
            {
              string s = m.Value;
              s = s.Substring(2, s.Length - 3);
              int iv = int.Parse(s, System.Globalization.NumberStyles.HexNumber);
              return char.ConvertFromUtf32(iv);
            });

            int res;
            // First, look up the identifier case-sensitively.
            if (!_idDict.TryGetValue(field, out res))
            {
                res = Interlocked.Increment(ref _nextCaseInsensitiveId);

                _idDict[field] = res;
                _fieldDict[res] = field;
            }
            return new SymbolId(res);
        }

        public static string IdToString(SymbolId id) {
            string s = _fieldDict[id.Id];
            return s;
        }

        public static string[] IdsToStrings(IList<SymbolId> ids) {
            string[] ret = new string[ids.Count];
            for (int i = 0; i < ids.Count; i++) {
                if (ids[i] == SymbolId.Empty) ret[i] = null;
                else ret[i] = IdToString(ids[i]);
            }
            return ret;
        }
    }
}
