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

namespace Microsoft.Scripting.Ast
{
    public abstract class Node {
        private readonly AstNodeType _nodeType;

        protected Node(AstNodeType nodeType) {
            _nodeType = nodeType;
        }

        public AstNodeType NodeType {
            get { return _nodeType; }
        }

        protected internal static long SpanToLong(SourceSpan span)
        {
          if (!span.IsValid)
          {
            return 0;
          }

          var start = span.Start;
          var end = span.End;
          var st = (uint)((start.Line << 10) | (start.Column));
          var en = (uint)((end.Line << 10) | (end.Column));
          return (long) (((ulong)en) << 32 | st); 
        }
    }

    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Maintainability", "CA1506:AvoidExcessiveClassCoupling")]
    public static partial class Ast {
    }
}
