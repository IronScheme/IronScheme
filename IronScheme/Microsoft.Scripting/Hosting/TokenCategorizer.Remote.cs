
#if FULL
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
#if !SILVERLIGHT

using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;

namespace Microsoft.Scripting.Hosting {
    internal class RemoteTokenCategorizer : RemoteWrapper, ITokenCategorizer {
        private TokenCategorizer _categorizer;

        public SourceLocation CurrentPosition {
            get {
                return _categorizer.CurrentPosition;
            }
        }

        public object CurrentState {
            get {
                return _categorizer.CurrentState;
            }
        }
        
        public RemoteTokenCategorizer(TokenCategorizer classifier) {
            Debug.Assert(classifier != null);
            _categorizer = classifier;
        }

        public override ILocalObject LocalObject {
            get { return _categorizer; }
        }

        public void Initialize(object state, SourceUnitReader sourceReader, SourceLocation initialLocation) {
            _categorizer.Initialize(state, sourceReader, initialLocation);
        }

        public TokenInfo ReadToken() {
            return _categorizer.ReadToken();
        }

        public bool SkipToken() {
            return _categorizer.SkipToken();
        }

        public IEnumerable<TokenInfo> ReadTokens(int countOfChars) {
            return _categorizer.ReadTokens(countOfChars);
        }

        public bool SkipTokens(int countOfChars) {
            return _categorizer.SkipTokens(countOfChars);
        }

        public ErrorSink ErrorSink {
            get { return _categorizer.ErrorSink; }
            set { _categorizer.ErrorSink = value; }
        }
    }
}

#endif 
#endif	
