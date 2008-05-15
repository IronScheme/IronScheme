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
using System.Text;

namespace Microsoft.Scripting.Hosting {
    public interface ITokenCategorizer : IRemotable, ILanguageService {
        void Initialize(object state, SourceUnitReader sourceReader, SourceLocation initialLocation);
        
        /// <summary>
        /// Move the tokenizer past the next token and return its category.
        /// </summary>
        /// <returns>The token information associated with the token just scanned.</returns>
        TokenInfo ReadToken();

        /// <summary>
        /// Move the tokenizer past the next token.
        /// </summary>
        /// <returns><c>False</c> if the end of stream has been reached, <c>true</c> otherwise.</returns>
        bool SkipToken();

        /// <summary>
        /// Get all tokens over a block of the stream.
        /// </summary>
        /// <remarks>
        /// <para>
        /// The scanner should return full tokens. If startLocation + length lands in the middle of a token, the full token
        /// should be returned.
        /// </para>
        /// </remarks>
        /// <param name="countOfChars">The mininum number of characters to process while getting tokens.</param>
        /// <returns>A enumeration of tokens.</returns>
        IEnumerable<TokenInfo> ReadTokens(int countOfChars);

        /// <summary>
        /// Scan from startLocation to at least startLocation + length.
        /// </summary>
        /// <param name="countOfChars">The mininum number of characters to process while getting tokens.</param>
        /// <remarks>
        /// This method is used to determine state at arbitrary startLocation.
        /// </remarks>
        /// <returns><c>False</c> if the end of stream has been reached, <c>true</c> otherwise.</returns>
        bool SkipTokens(int countOfChars);

        /// <summary>
        /// The current startLocation of the scanner.
        /// </summary>
        SourceLocation CurrentPosition { get; }

        /// <summary>
        /// The current internal state of the scanner.
        /// </summary>
        object CurrentState { get; }

        ErrorSink ErrorSink { get; set; }
    }

    public abstract class TokenCategorizer : ITokenCategorizer, ILocalObject {

        // static contract:
        protected TokenCategorizer() {
        }

        #region ILocalObject Members

#if !SILVERLIGHT
        RemoteWrapper ILocalObject.Wrap() {
            return new RemoteTokenCategorizer(this);
        }
#endif

        #endregion

        public abstract void Initialize(object state, SourceUnitReader sourceReader, SourceLocation initialLocation);

        public abstract object CurrentState { get; }
        public abstract SourceLocation CurrentPosition { get; }
        public abstract TokenInfo ReadToken();
        public abstract bool IsRestartable { get; }
        public abstract ErrorSink ErrorSink { get; set; }
        
        public virtual bool SkipToken() {
            return ReadToken().Category != TokenCategory.EndOfStream;
        }

        // TODO: shouldn't be virutal (JS tokenizer needs to be fixed)
        public virtual IEnumerable<TokenInfo> ReadTokens(int countOfChars) {
            List<TokenInfo> tokens = new List<TokenInfo>();

            int start_index = CurrentPosition.Index;

            while (CurrentPosition.Index - start_index < countOfChars) {
                TokenInfo token = ReadToken();
                if (token.Category == TokenCategory.EndOfStream) break;
                tokens.Add(token);
            }

            return tokens;
        }

        public bool SkipTokens(int countOfChars) {
            bool eos = false;
            int start_index = CurrentPosition.Index;

            while (CurrentPosition.Index - start_index < countOfChars && (eos = SkipToken()));

            return eos;
        }
    }
}
