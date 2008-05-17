
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

using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace Microsoft.Scripting.Utils {
    public enum ConsoleStreamType {
        Input,
        Output,
        ErrorOutput,
    }

    public sealed class ConsoleStream : TextStreamBase {

        private ConsoleStreamType _type;
#if SILVERLIGHT
        // We need to return a version of the UTF8Encoding that doesn't emit the UT8
        // identifier. Otherwise we get garbage on the first line of the console output
        private static readonly Encoding _encoding = new UTF8Encoding(false);
#endif
        public override TextReader Reader {
            get { return (_type == ConsoleStreamType.Input) ? Console.In : null; }
        }

        public override TextWriter Writer {
            get {
                switch (_type) {
                    case ConsoleStreamType.Output: return Console.Out;
                    case ConsoleStreamType.ErrorOutput: return Console.Error;
                }
                return null;
            }
        }

        public override Encoding Encoding {
            get {
#if SILVERLIGHT
                return _encoding;
#else
                return CanRead ? Console.InputEncoding : Console.OutputEncoding;
#endif
            }
        }

        public ConsoleStream(ConsoleStreamType type)
            : this(type, true) {
        }

        public ConsoleStream(ConsoleStreamType type, bool buffered)
            : base(buffered) {
            _type = type;
        }
    }
}

#endif	
