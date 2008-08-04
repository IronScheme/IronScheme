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

using System.Diagnostics;
using System.Threading;
using System.Text.RegularExpressions;

namespace Microsoft.Scripting {
    public static class SymbolTable {
        private static object _lockObj = new object();

        private static Dictionary<string, int> _idDict = new Dictionary<string, int>(InitialTableSize);

        private const int InitialTableSize = 2048;
        private static Dictionary<int, string> _fieldDict = new Dictionary<int, string>(InitialTableSize);

        private static int _nextCaseInsensitiveId = 1;

        static SymbolTable() {
            _fieldDict[0] = null;   // initialize the null string
        }

        static Regex unichar = new Regex(@"\\x[\da-f]+;", RegexOptions.Compiled | RegexOptions.IgnoreCase);

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
            lock (_lockObj) {
                // First, look up the identifier case-sensitively.
                if (!_idDict.TryGetValue(field, out res)) {
                    //string invariantField = field.ToUpper(System.Globalization.CultureInfo.InvariantCulture);

                    // OK, didn't find it, so let's look up the case-insensitive
                    // identifier.
                    //if (_idDict.TryGetValue(invariantField, out res)) {
                    //    // OK, this is a new casing of an existing identifier.
                    //    Debug.Assert(res < 0, "Must have invariant bit set!");

                    //    // Throw if we've exhausted the number of casings.
                    //    if (unchecked(((uint)res & 0x00FFFFFF) == 0x00FFFFFF)) {
                    //        throw new InvalidOperationException(String.Format(Resources.CantAddCasing, field));
                    //    }

                    //    int invariantRes = res + 0x01000000;

                    //    // Mask off the high bit.
                    //    res = unchecked((int)((uint)res & 0x7FFFFFFF));

                    //    _idDict[field] = res;
                    //    _idDict[invariantField] = invariantRes;
                    //    _fieldDict[res] = field;
                    //} else {
                        // This is a whole new identifier.

                        if (_nextCaseInsensitiveId == int.MaxValue) {
                            throw new InvalidOperationException(String.Format(Resources.CantAddIdentifier, field));
                        }

                        // register new id...
                        res = _nextCaseInsensitiveId++;
                        // Console.WriteLine("Registering {0} as {1}", field, res);

                        //_fieldDict[res] = invariantField;

                        //if (field != invariantField) {
                            res |= 0x01000000;
                            _idDict[field] = res;
                            _fieldDict[res] = field;
                        //}

                        //_idDict[invariantField] = unchecked((int)(((uint)res | 0x80000000) + 0x01000000));
                    //}
                } else {
                    // If this happens to be the invariant field, then we need to
                    // mask off the top byte, since that's just used to pick the next
                    // id for this identifier.
                    if (res < 0) {
                        res &= 0x00FFFFFF;
                    }
                }
            }
            return new SymbolId(res);
        }

        public static SymbolId StringToCaseInsensitiveId(string field) {
            return StringToId(field.ToUpper(System.Globalization.CultureInfo.InvariantCulture));
        }

        public static SymbolId[] QualifiedStringToIds(string fields) {
            if (fields != null) {
                string[] strings = fields.Split('.');
                SymbolId[] identifiers = new SymbolId[strings.Length];

                for (int i = 0; i < strings.Length; i++) {
                    identifiers[i] = StringToId(strings[i]);
                }

                return identifiers;
            }

            return null;
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

        public static SymbolId[] StringsToIds(IList<string> strings) {
            SymbolId[] ret = new SymbolId[strings.Count];
            for (int i = 0; i < strings.Count; i++) {
                if (strings[i] == null) ret[i] = SymbolId.Empty;
                else ret[i] = StringToId(strings[i]);
            }
            return ret;
        }

        public static bool StringHasId(string symbol) {
            if (symbol == null) {
                throw new ArgumentNullException(Resources.NameMustBeString);
            }

            lock (_lockObj) {
                return _idDict.ContainsKey(symbol);
            }
        }
    }
}
