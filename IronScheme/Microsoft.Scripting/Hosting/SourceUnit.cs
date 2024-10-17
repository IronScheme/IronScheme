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
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Hosting
{

    [Serializable]
    public sealed class SourceUnit {

        private readonly SourceCodeKind _kind;
        private string _id;
        private readonly IScriptEngine _engine;

        private SourceContentProvider _contentProvider;
        private Action<SourceUnit> _contentReloader; // TODO: better: ReloadableStringProvider?

        private bool _isDebuggable = true;
        private bool _disableLineFeedLineSeparator;

        // content dependent properties:
        // SourceUnit is serializable => updated properties are not transmitted back to the host unless the unit is passed by-ref
        private SourceCodeProperties? _codeProperties;
        private KeyValuePair<int, int>[] _lineMap;
        private KeyValuePair<int, string>[] _fileMap;

        /// <summary>
        /// Identification of the source unit. Assigned by the host. 
        /// The format and semantics is host dependent (could be a path on file system or URL).
        /// Empty string for anonymous source units.
        /// </summary>
        public string Id {
            get { return _id; }
          set { _id = value; }
        }

        public SourceCodeKind Kind {
            get { return _kind; }
        }

        /// <summary>
        /// Script engine of the language of the unit.
        /// </summary>
        public IScriptEngine Engine {
            get { return _engine; }
        }

        public SourceCodeProperties? CodeProperties {
            get { return _codeProperties; }
            set { _codeProperties = value; } 
        }

        public bool IsVisibleToDebugger {
            get { return _isDebuggable; }
            set { _isDebuggable = value; }
        }

        public bool DisableLineFeedLineSeparator {
            get { return _disableLineFeedLineSeparator; }
            set { _disableLineFeedLineSeparator = value; }
        }

        public Action<SourceUnit> ContentReloader {
            get { return _contentReloader; }
            set { _contentReloader = value; }
        }

        public bool IsReloadable {
            get { return _contentReloader != null; }
        }

        #region Construction

        private SourceUnit(IScriptEngine engine, SourceContentProvider contentProvider, string id, SourceCodeKind kind) {
            Assert.NotNull(engine, contentProvider);

            _engine = engine;
            _contentProvider = contentProvider;
            _kind = kind;
            _id = id;
        }

        // move factories to ScriptEngine/ScriptHost?

        public static SourceUnit Create(IScriptEngine engine, SourceContentProvider contentProvider, string id, SourceCodeKind kind) {
            Contract.RequiresNotNull(engine, "engine");
            Contract.RequiresNotNull(contentProvider, "contentProvider");

            return new SourceUnit(engine, contentProvider, id, kind);
        }

        public static SourceUnit CreateSnippet(IScriptEngine engine, string code) {
            return CreateSnippet(engine, code, null, SourceCodeKind.Default);
        }

        public static SourceUnit CreateSnippet(IScriptEngine engine, string code, SourceCodeKind kind) {
            return CreateSnippet(engine, code, null, kind);
        }

        public static SourceUnit CreateSnippet(IScriptEngine engine, string code, string id) {
            return CreateSnippet(engine, code, id, SourceCodeKind.Default);
        }

        public static SourceUnit CreateSnippet(IScriptEngine engine, string code, string id, SourceCodeKind kind) {
            Contract.RequiresNotNull(engine, "engine");
            Contract.RequiresNotNull(code, "code");

            return new SourceUnit(engine, new SourceStringContentProvider(code), id, kind);
        }

        /// <summary>
        /// Should be called by host only. TODO: move to the ScriptHost?
        /// </summary>
        public static SourceUnit CreateFileUnit(IScriptEngine engine, string path) {
            return CreateFileUnit(engine, path, (Encoding)null);
        }

        public static SourceUnit CreateFileUnit(IScriptEngine engine, string path, Encoding encoding) {
            Contract.RequiresNotNull(engine, "engine");
            Contract.RequiresNotNull(path, "path");

            SourceContentProvider provider = new SourceFileContentProvider(path, encoding ?? Encoding.Default, engine);
            SourceUnit result = new SourceUnit(engine, provider, path, SourceCodeKind.File);
            result.IsVisibleToDebugger = true;
            return result;
        }

        public static SourceUnit CreateFileUnit(IScriptEngine engine, string path, string content) {
            return CreateFileUnit(engine, path, content, null);
        }

        public static SourceUnit CreateFileUnit(IScriptEngine engine, string path, string content, Action<SourceUnit> contentReloader) {
            Contract.RequiresNotNull(engine, "engine");
            Contract.RequiresNotNull(path, "path");
            Contract.RequiresNotNull(content, "content");

            SourceContentProvider provider = new SourceStringContentProvider(content);
            SourceUnit result = new SourceUnit(engine, provider, path, SourceCodeKind.File);
            result.ContentReloader = contentReloader;
            result.IsVisibleToDebugger = true;
            return result;
        }

        #endregion

        public SourceUnitReader GetReader() {
            return new SourceUnitReader(this, _contentProvider.GetReader());
        }

        public void Reload() {
            if (_contentReloader != null) {
                _contentReloader(this);
            }
        }

        public void SetContent(string content) {
            Contract.RequiresNotNull(content, "content");
            _contentProvider = new SourceStringContentProvider(content);
            ContentChanged();
        }

        private void ContentChanged() {
            _codeProperties = null;
            _lineMap = null;
            _fileMap = null;
        }
        
        /// <summary>
        /// Reads specified range of lines (or less) from the source unit. 
        /// Line numbers starts with 1.
        /// </summary>
        public string[] GetCodeLines(int start, int count) {
            Contract.Requires(start > 0, "start");
            Contract.Requires(count > 0, "count");

            List<string> result = new List<string>(count);

            using (SourceUnitReader reader = GetReader()) {
                reader.SeekLine(start);
                while (count > 0) {
                    string line = reader.ReadLine();
                    if (line == null) break;
                    result.Add(line);
                    count--;
                }
            }

            return result.ToArray();
        }

        public string GetCodeLine(int line) {
            string[] lines = GetCodeLines(line, 1);
            return (lines.Length > 0) ? lines[0] : null;
        }

        public string GetCode() {
            using (SourceUnitReader reader = GetReader()) {
                return reader.ReadToEnd();
            }
        }

        public override string ToString() {
            return _id ?? "*interactive*";
        }

        #region Line/File mapping

        public SourceSpan MapLine(SourceSpan span) {
            return new SourceSpan(MapLine(span.Start), MapLine(span.End));
        }

        public SourceLocation MapLine(SourceLocation loc) {
            return new SourceLocation(loc.Index, MapLine(loc.Line), loc.Column);
        }

        public int MapLine(int line) {
            if (_lineMap != null) {
                int match = BinarySearch(_lineMap, line);
                int delta = line - _lineMap[match].Key;
                return _lineMap[match].Value + delta;
            }

            return line;
        }

        /// <summary>
        /// Returns null if unknown/undefined.
        /// </summary>
        public string GetSymbolDocument(int line) {
            if (_fileMap != null) {
                int match = BinarySearch(_fileMap, line);
                return _fileMap[match].Value;
            }

            return _id;
        }

        private int BinarySearch<T>(KeyValuePair<int, T>[] array, int line) {
            int match = Array.BinarySearch(array, new KeyValuePair<int, T>(line, default(T)), new KeyComparer<T>());
            if (match < 0) {
                // If we couldn't find an exact match for this line number, get the nearest
                // matching line number less than this one
                match = ~match - 1;

                // If our index = -1, it means that this line is before any line numbers that
                // we know about. If that's the case, use the first entry in the list
                if (match == -1) {
                    match = 0;
                }
            }
            return match;
        }

        public void SetLineMapping(KeyValuePair<int, int>[] lineMap) {
            // implementation detail: so we don't always have to check for null and empty
            _lineMap = (lineMap.Length == 0) ? null : lineMap;
        }

        public void SetDocumentMapping(KeyValuePair<int, string>[] fileMap) {
            _fileMap = (fileMap.Length == 0) ? null : fileMap;
        }

        class KeyComparer<T1> : IComparer<KeyValuePair<int, T1>> {
            public int Compare(KeyValuePair<int, T1> x, KeyValuePair<int, T1> y) {
                return x.Key - y.Key;
            }
        }

        #endregion
    }
}
