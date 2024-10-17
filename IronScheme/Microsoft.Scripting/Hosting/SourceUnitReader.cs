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
using System.Diagnostics;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Hosting
{

    /// <summary>
    /// Couples a source unit with an open text reader. Remotable (TextReader is a MBRO).
    /// </summary>
    [Serializable]
    public sealed class SourceUnitReader : TextReader
    {

        private readonly TextReader _textReader;
        private readonly SourceUnit _sourceUnit;

        public SourceUnit SourceUnit
        {
            get { return _sourceUnit; }
        }

        internal SourceUnitReader(SourceUnit sourceUnit, TextReader textReader)
        {
            Assert.NotNull(sourceUnit, textReader);

            _textReader = textReader;
            _sourceUnit = sourceUnit;
        }

        public override string ReadLine()
        {
            if (_sourceUnit.DisableLineFeedLineSeparator)
            {
                return IOUtils.ReadTo(_textReader, '\n');
            }
            else
            {
                return _textReader.ReadLine();
            }
        }

        public bool SeekLine(int line)
        {
            if (_sourceUnit.DisableLineFeedLineSeparator)
            {
                var current_line = 1;

                for (; ; )
                {
                    if (!IOUtils.SeekTo(_textReader, '\n')) return false;
                    current_line++;
                    if (current_line == line) return true;
                }
            }
            else
            {
                return IOUtils.SeekLine(_textReader, line);
            }
        }

        public override string ReadToEnd()
        {
            return _textReader.ReadToEnd();
        }

        public override int Read(char[] buffer, int index, int count)
        {
            return _textReader.Read(buffer, index, count);
        }

        public override int Peek()
        {
            return _textReader.Peek();
        }

        public override int Read()
        {
            return _textReader.Read();
        }

        protected override void Dispose(bool disposing)
        {
            _textReader.Dispose();
        }
    }
}
