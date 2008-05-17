
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
    public abstract class TextStreamBase : Stream {

        private bool _buffered;

        protected TextStreamBase(bool buffered) {
            _buffered = buffered;
        }

        public abstract Encoding Encoding { get; }
        public abstract TextReader Reader { get; }
        public abstract TextWriter Writer { get; }

        public sealed override bool CanSeek {
            get { return false; }
        }

        public sealed override bool CanWrite {
            get { return Writer != null; }
        }

        public sealed override bool CanRead {
            get { return Reader != null; }
        }

        public sealed override void Flush() {
            if (!CanWrite) throw new InvalidOperationException();
            Writer.Flush();
        }

        public sealed override int Read(byte[] buffer, int offset, int count) {
            if (!CanRead) throw new InvalidOperationException();
            Contract.RequiresArrayRange(buffer, offset, count, "offset", "count");

            char[] char_buffer = new char[count];
            int real_count = Reader.Read(char_buffer, 0, count);
            return Encoding.GetBytes(char_buffer, 0, real_count, buffer, offset);
        }

        public sealed override void Write(byte[] buffer, int offset, int count) {
            Contract.RequiresArrayRange(buffer, offset, count, "offset", "count");
            char[] char_buffer = Encoding.GetChars(buffer, offset, count);
            Writer.Write(char_buffer, 0, char_buffer.Length);
            if (!_buffered) Writer.Flush();
        }

        #region Invalid Operations

        public sealed override long Length {
            get {
                throw new InvalidOperationException();
            }
        }

        public sealed override long Position {
            get {
                throw new InvalidOperationException();
            }
            set {
                throw new InvalidOperationException();
            }
        }

        public sealed override long Seek(long offset, SeekOrigin origin) {
            throw new InvalidOperationException();
        }

        public sealed override void SetLength(long value) {
            throw new InvalidOperationException();
        }

        #endregion
    }

    public sealed class TextStream : TextStreamBase {

        private TextReader _reader;
        private TextWriter _writer;
        private Encoding _encoding;

        public override Encoding Encoding {
            get { return _encoding; }
        }

        public override TextReader Reader {
            get { return _reader; }
        }

        public override TextWriter Writer {
            get { return _writer; }
        }

        public TextStream(TextReader reader, Encoding encoding)
            : base(true) {
            Contract.RequiresNotNull(reader, "reader");
            Contract.RequiresNotNull(encoding, "encoding");

            this._reader = reader;
            this._encoding = encoding;
        }

        public TextStream(TextWriter writer, Encoding encoding)
            : this(writer, encoding, true) {
        }

        public TextStream(TextWriter writer, Encoding encoding, bool buffered)
            : base(buffered) {
            Contract.RequiresNotNull(writer, "writer");
            Contract.RequiresNotNull(encoding, "encoding");

            this._writer = writer;
            this._encoding = encoding;
        }
    }


}

#endif	
