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

using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Shell {

    [Serializable]
    public class ConsoleOptions {
        private string _command;
        private string _filename;
        private bool _printVersionAndExit;
        private int _autoIndentSize = 4;
        private string[] _remainingArgs;
        private bool _introspection;
        private bool _autoIndent;
        private bool _handleExceptions = true;
        private bool _tabCompletion;
        private bool _colorfulConsole;
        private bool _printUsageAndExit;

        public bool AutoIndent {
            get { return _autoIndent; }
            set { _autoIndent = value; }
        }

        public bool HandleExceptions {
            get { return _handleExceptions; }
            set { _handleExceptions = value; }
        }

        public bool TabCompletion {
            get { return _tabCompletion; }
            set { _tabCompletion = value; }
        }

        public bool ColorfulConsole {
            get { return _colorfulConsole; }
            set { _colorfulConsole = value; }
        }

        public bool PrintUsageAndExit {
            get { return _printUsageAndExit; }
            set { _printUsageAndExit = value; }
        }

        /// <summary>
        /// Literal script command given using -c option
        /// </summary>
        public string Command {
            get { return _command; }
            set { _command = value; }
        }

        /// <summary>
        /// Filename to execute passed on the command line options.
        /// </summary>
        public string FileName {
            get { return _filename; }
            set { _filename = value; }
        }

        /// <summary>
        /// Only print the version of the script interpreter and exit
        /// </summary>
        public bool PrintVersionAndExit {
            get { return _printVersionAndExit; }
            set { _printVersionAndExit = value; }
        }

        public int AutoIndentSize {
            get { return _autoIndentSize; }
            set { _autoIndentSize = value; }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1819:PropertiesShouldNotReturnArrays")] // TODO: fix
        public string[] RemainingArgs {
            get { return _remainingArgs; }
            set { _remainingArgs = value; }
        }

        public bool Introspection {
            get { return _introspection; }
            set { _introspection = value; }
        }

        internal protected ConsoleOptions() {
	    }

        protected ConsoleOptions(ConsoleOptions options) {
            Contract.RequiresNotNull(options, "options");

            _command = options._command;
            _filename = options._filename;
            _printVersionAndExit = options._printVersionAndExit;
            _autoIndentSize = options._autoIndentSize;
            _remainingArgs = ArrayUtils.Copy(options._remainingArgs);
            _introspection = options._introspection;
            _autoIndent = options._autoIndent;
            _handleExceptions = options._handleExceptions;
            _tabCompletion = options._tabCompletion;
            _colorfulConsole = options._colorfulConsole;
            _printUsageAndExit = options._printUsageAndExit;
        }
    }
}
