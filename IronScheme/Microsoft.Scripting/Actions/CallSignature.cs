/* **********************************************************************************
 *
 * Copyright (c) Microsoft Corporation. All rights reserved.
 *
 * This source code is subject to terms and conditions of the Shared Source License
 * for IronPython. A copy of the license can be found in the License.html file
 * at the root of this distribution. If you can not locate the Shared Source License
 * for IronPython, please send an email to dlr@microsoft.com.
 * By using this source code in any fashion, you are agreeing to be bound by
 * the terms of the Shared Source License for IronPython.
 *
 * You must not remove this notice, or any other, from this software.
 *
 * **********************************************************************************/

using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Utils;
using System.Diagnostics;
using Microsoft.Scripting.Generation;

namespace Microsoft.Scripting.Actions {
    public struct CallSignature : IEquatable<CallSignature> {
        //^ invariant _infos != null ==> _argumentCount == _infos.Length
        
        private readonly ArgumentInfo[] _infos;
        private readonly int _argumentCount;

        public bool IsSimple {
            get { return _infos == null; }
        }

        public int ArgumentCount {
            get {
                Debug.Assert(_infos == null || _infos.Length == _argumentCount);
                return _argumentCount; 
            }
        }

        #region Construction

        public CallSignature(CallSignature signature) {
            _infos = signature.GetArgumentInfos();
            _argumentCount = signature._argumentCount;
        }
        
        public CallSignature(int argumentCount) {
            Contract.Requires(argumentCount >= 0, "argumentCount");
            _argumentCount = argumentCount;
            _infos = null;
        }

        public CallSignature(params ArgumentInfo[] infos) {
            bool simple = true;

            if (infos != null) {
                _argumentCount = infos.Length;
                for (int i = 0; i < infos.Length; i++) {
                    if (infos[i].Kind != ArgumentKind.Simple) {
                        simple = false;
                        break;
                    }
                }
            } else {
                _argumentCount = 0;
            }

            _infos = (!simple) ? infos : null;
        }

        public CallSignature(params Arg[] args) {
            bool simple = true;
            if (args != null) {
                _argumentCount = args.Length;
                for (int i = 0; i < args.Length; i++) {
                    if (args[i].Kind != ArgumentKind.Simple) {
                        simple = false;
                        break;
                    }
                }
            } else {
                _argumentCount = 0;
            }

            if (!simple) {
                _infos = new ArgumentInfo[args.Length];
                for (int i = 0; i < args.Length; i++) {
                    _infos[i] = args[i].Info;
                }
            } else {
                _infos = null;
            }
        }

        public CallSignature(params ArgumentKind[] kinds) {
            bool simple = true;

            if (kinds != null) {
                _argumentCount = kinds.Length;
                for (int i = 0; i < kinds.Length; i++) {
                    if (kinds[i] != ArgumentKind.Simple) {
                        simple = false;
                        break;
                    }
                }
            } else {
                _argumentCount = 0;
            }

            if (!simple) {
                _infos = new ArgumentInfo[kinds.Length];
                for (int i = 0; i < kinds.Length; i++) {
                    _infos[i] = new ArgumentInfo(kinds[i]);
                }
            } else {
                _infos = null;
            }
        }

        #endregion

        #region IEquatable<CallSignature> Members

        public bool Equals(CallSignature other) {
            if (_infos == null) {
                return other._infos == null;
            } else if (other._infos == null) {
                return false;
            }

            if (_infos.Length != other._infos.Length) return false;

            for (int i = 0; i < _infos.Length; i++) {
                if (!_infos[i].Equals(other._infos[i])) return false;
            }

            return true;
        }

        #endregion

        #region Overrides

        public override bool Equals(object obj) {
            return obj is CallSignature && Equals((CallSignature)obj);
        }

        public override string ToString() {
            if (_infos == null) {
                return "Simple";
            }
            
            StringBuilder sb = new StringBuilder("(");
            for (int i = 0; i < _infos.Length; i++) {
                if (i > 0) {
                    sb.Append(", ");
                }
                sb.Append(_infos[i].ToString());
            }
            sb.Append(")");
            return sb.ToString();
        }

        public override int GetHashCode() {
            int h = 6551;
            if (_infos != null) {
                foreach (ArgumentInfo info in _infos) {
                    h ^= (h << 5) ^ info.GetHashCode();
                }
            }
            return h;
        }

        #endregion

        #region Helpers

        public ArgumentInfo[] GetArgumentInfos() {
            return (_infos != null) ? ArrayUtils.Copy(_infos) : CompilerHelpers.MakeRepeatedArray(ArgumentInfo.Simple, _argumentCount);
        }

        public CallSignature InsertArgument(ArgumentInfo info) {
            if (info.IsSimple && this.IsSimple) {
                return new CallSignature(_argumentCount + 1);
            }

            return new CallSignature(ArrayUtils.Insert(info, _infos));
        }

        public CallSignature RemoveFirstArgument() {
            if (_argumentCount == 0) {
                throw new InvalidOperationException();
            }
            
            if (IsSimple) {
                return new CallSignature(_argumentCount - 1);
            }

            return new CallSignature(ArrayUtils.RemoveFirst(_infos));
        }

        public int IndexOf(ArgumentKind kind) {
            if (_infos == null) {
                return (kind == ArgumentKind.Simple && _argumentCount > 0) ? 0 : -1;
            }

            for (int i = 0; i < _infos.Length; i++) {
                if (_infos[i].Kind == kind) {
                    return i;
                }
            }
            return -1;
        }

        public bool HasDictionaryArgument() {
            return IndexOf(ArgumentKind.Dictionary) > -1;
        }

        public bool HasInstanceArgument() {
            return IndexOf(ArgumentKind.Instance) > -1;
        }

        public bool HasListArgument() {
            return IndexOf(ArgumentKind.List) > -1;
        }

        internal bool HasNamedArgument() {
            return IndexOf(ArgumentKind.Named) > -1;
        }

        /// <summary>
        /// True if the CallAction includes an ArgumentInfo of ArgumentKind.Dictionary or ArgumentKind.Named.
        /// </summary>
        public bool HasKeywordArgument() {
            if (_infos != null) {
                foreach (ArgumentInfo info in _infos) {
                    if (info.Kind == ArgumentKind.Dictionary || info.Kind == ArgumentKind.Named) {
                        return true;
                    }
                }
            }
            return false;
        }
        
        public ArgumentKind GetArgumentKind(int index) {
            // TODO: Contract.Requires(index >= 0 && index < _argumentCount, "index");
            return _infos != null ? _infos[index].Kind : ArgumentKind.Simple;
        }

        public SymbolId GetArgumentName(int index) {
            Contract.Requires(index >= 0 && index < _argumentCount);
            return _infos != null ? _infos[index].Name : SymbolId.Empty;
        }

        /// <summary>
        /// Gets the number of positional arguments the user provided at the call site.
        /// </summary>
        public int GetProvidedPositionalArgumentCount() {
            int result = _argumentCount;

            if (_infos != null) {
                for (int i = 0; i < _infos.Length; i++) {
                    ArgumentKind kind = _infos[i].Kind;

                    if (kind == ArgumentKind.Dictionary || kind == ArgumentKind.List || kind == ArgumentKind.Named) {
                        result--;
                    }
                }
            }

            return result;
        }

        public SymbolId[] GetArgumentNames() {
            if (_infos == null) {
                return SymbolId.EmptySymbols;
            }

            List<SymbolId> result = new List<SymbolId>();
            foreach (ArgumentInfo info in _infos) {
                if (info.Name != SymbolId.Empty) {
                    result.Add(info.Name);
                }
            }

            return result.ToArray();
        }

        #endregion
    }
}
