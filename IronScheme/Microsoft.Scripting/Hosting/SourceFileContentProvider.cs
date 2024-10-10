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
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Hosting {

    [Serializable]
    public class SourceFileContentProvider : SourceContentProvider {
        private readonly string _path;
        private readonly Encoding _defaultEncoding; // optional
        private readonly IScriptEngine _engine;     // optional

        public string Path {
            get { return _path; }
        }

        #region Construction

        /// <summary>
        /// Binary file with an explicit encoding.
        /// </summary>
        public SourceFileContentProvider(string path, Encoding defaultEncoding)
            : this(path, defaultEncoding, null) {
        }

        /// <summary>
        /// Binary file with a default encoding. The actual encoding of the file is determined by first 
        /// bytes of the file in a language specific way.
        /// </summary>
        public SourceFileContentProvider(string path, Encoding defaultEncoding, IScriptEngine engine) {
            Contract.RequiresNotNull(defaultEncoding, "defaultEncoding");

            _path = path;
            _defaultEncoding = defaultEncoding;
            _engine = engine;
        }

        #endregion

        public override TextReader GetReader() {
            Stream stream = File.OpenRead(Path);

            if (stream == null) {
                throw new InvalidImplementationException();
            }

            TextReader reader;
            Encoding encoding = _defaultEncoding;

            if (_engine != null) {
                reader = LanguageContext.FromEngine(_engine).GetSourceReader(stream, encoding);
            } else if (encoding != null) {
                reader = new StreamReader(stream, encoding, true);
            } else {
                reader = new StreamReader(stream, true);
            }

            return reader;
        }
    }
}
