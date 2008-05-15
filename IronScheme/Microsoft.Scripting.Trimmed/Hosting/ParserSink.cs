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

namespace Microsoft.Scripting.Hosting {
    public class ParserSink {

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly ParserSink Null = new ParserSink();
        
        public virtual void MatchPair(SourceSpan start, SourceSpan end, int priority) {
        }

        public virtual void MatchTriple(SourceSpan start, SourceSpan middle, SourceSpan end, int priority) {
        }

        public virtual void EndParameters(SourceSpan span) {
        }

        public virtual void NextParameter(SourceSpan span) {
        }

        public virtual void QualifyName(SourceSpan selector, SourceSpan span, string name) {
        }

        public virtual void StartName(SourceSpan span, string name) {
        }

        public virtual void StartParameters(SourceSpan context) {
        }
    }
}
