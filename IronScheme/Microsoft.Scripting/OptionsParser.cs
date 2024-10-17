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
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Shell;
using System.Globalization;
using System.Runtime.Serialization;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting
{

    [Serializable]
    public class InvalidOptionException : Exception {
        public InvalidOptionException(string message) : base(message) {
        }

#if !SILVERLIGHT // SerializationInfo
        public InvalidOptionException(SerializationInfo info, StreamingContext context) : base(info, context) { }
#endif
    }

    public abstract class OptionsParser {
       
        private List<string> _ignoredArgs = new List<string>();
        private ScriptDomainOptions _globalOptions;
        private string[] _args;
        private int _current = -1;

        protected OptionsParser() {
        }

        public ScriptDomainOptions GlobalOptions { 
            get { 
                return _globalOptions; 
            } set { 
                _globalOptions = value; 
            } 
        }
        
        public virtual ConsoleOptions ConsoleOptions { 
            get {
                throw new NotSupportedException(); 
            } 
            set {
                throw new NotSupportedException(); 
            } 
        }

        public virtual ConsoleOptions GetDefaultConsoleOptions() {
            return new ConsoleOptions();
        }

        public IList<string> IgnoredArgs { get { return _ignoredArgs; } }
        
        /// <exception cref="InvalidOptionException">On error.</exception>
        public virtual void Parse(string[] args) {
            Contract.RequiresNotNull(args, "args");

            if (_globalOptions == null) _globalOptions = new ScriptDomainOptions();

            _args = args;

            try {
                _current = 0;
                while (_current < args.Length) {
                    ParseArgument(args[_current++]);
                }
            } finally {
                _args = null;
                _current = -1;
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Maintainability", "CA1502:AvoidExcessiveComplexity")]
        protected virtual void ParseArgument(string arg) {
            Contract.RequiresNotNull(arg, "arg");

            // the following extension switches are in alphabetic order
            switch (arg) {
                //case "-c":
                //    ConsoleOptions.Command = PeekNextArg();
                //    break;

                case "-h":
                case "-help":
                case "-?":
                    ConsoleOptions.PrintUsageAndExit = true;
                    IgnoreRemainingArgs();
                    break;

                //case "-i": ConsoleOptions.Introspection = true; break;

                case "-V":
                    ConsoleOptions.PrintVersionAndExit = true;
                    IgnoreRemainingArgs();
                    break;

                case "-debug": GlobalOptions.DebugMode = true; break;
//                case "-O": GlobalOptions.DebugMode = false; break;
//                case "-D": GlobalOptions.EngineDebug = true; break;

//                case "-X:AssembliesDir":

//                    string dir = PopNextArg();

//                    if (!ScriptDomainManager.CurrentManager.PAL.DirectoryExists(dir))
//                        throw new System.IO.DirectoryNotFoundException(String.Format("Directory '{0}' doesn't exist.", dir));

//                    GlobalOptions.BinariesDirectory = dir;
//                    break;

//                case "-OO":
//                    GlobalOptions.DebugMode = false;
//                    GlobalOptions.StripDocStrings = true;
//                    break;

//                case "-X:Interpret": EngineOptions.InterpretedMode = true; break;
//                case "-X:Frames": GlobalOptions.Frames = true; break;
//                case "-X:GenerateAsSnippets": GlobalOptions.GenerateModulesAsSnippets = true; break;
//                case "-X:GenerateReleaseAssemblies": GlobalOptions.AssemblyGenAttributes &= ~AssemblyGenAttributes.GenerateDebugAssemblies; break;
//                case "-X:ILDebug": GlobalOptions.AssemblyGenAttributes |= AssemblyGenAttributes.ILDebug; break;

//                case "-X:PassExceptions": ConsoleOptions.HandleExceptions = false; break;
//                // TODO: #if !IRONPYTHON_WINDOW
//                case "-X:ColorfulConsole": ConsoleOptions.ColorfulConsole = true; break;
//                case "-X:ExceptionDetail": EngineOptions.ExceptionDetail = true; break;
//                case "-X:TabCompletion": ConsoleOptions.TabCompletion = true; break;
//                case "-X:AutoIndent": ConsoleOptions.AutoIndent = true; break;
//                //#endif
//                case "-X:NoOptimize": GlobalOptions.DebugCodeGeneration = true; break;
//                case "-X:Optimize": GlobalOptions.DebugCodeGeneration = false; break;
//                case "-X:NoTraceback": GlobalOptions.DynamicStackTraceSupport = false; break;

//                case "-X:ShowRules": GlobalOptions.ShowRules = true; break;
//                case "-X:DumpASTs": GlobalOptions.DumpASTs = true; break;
//                case "-X:ShowASTs": GlobalOptions.ShowASTs = true; break;


//                case "-X:PrivateBinding": GlobalOptions.PrivateBinding = true; break;
//                case "-X:SaveAssemblies": GlobalOptions.AssemblyGenAttributes |= AssemblyGenAttributes.SaveAndReloadAssemblies; break;
//                case "-X:ShowClrExceptions": EngineOptions.ShowClrExceptions = true; break;
//                case "-X:StaticMethods": GlobalOptions.AssemblyGenAttributes |= AssemblyGenAttributes.GenerateStaticMethods; break;
//                case "-X:TrackPerformance": // accepted but ignored on retail builds
//#if DEBUG
//                    GlobalOptions.TrackPerformance = true;
//#endif
//                    break;

                default:
                    ConsoleOptions.FileName = arg;
                    // The language-specific parsers may want to do something like this to pass arguments to the script
                    IgnoreRemainingArgs();
                    break;
            }
        }

        protected void IgnoreRemainingArgs() {
            while (_current < _args.Length) {
                _ignoredArgs.Add(_args[_current++]);
            }
        }

        protected string[] PopRemainingArgs() {
            string[] result = ArrayUtils.ShiftLeft(_args, _current);
            _current = _args.Length;
            return result;
        }

        protected string PeekNextArg() {
            if (_current < _args.Length)
                return _args[_current];
            else
                throw new InvalidOptionException(String.Format(CultureInfo.CurrentCulture, Resources.MissingOptionValue, _current > 0 ? _args[_current - 1] : ""));
        }

        protected string PopNextArg() {
            string result = PeekNextArg();
            _current++;
            return result;
        }

        protected void PushArgBack() {
            _current--;
        }

        protected static Exception InvalidOptionValue(string option, string value) {
            return new InvalidOptionException(String.Format(Resources.InvalidOptionValue, value, option));
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Performance", "CA1814:PreferJaggedArraysOverMultidimensional", MessageId = "Body")] // TODO: fix
        public virtual void GetHelp(out string commandLine, out string[,] options, out string[,] environmentVariables, out string comments) {

            commandLine = "[options] [file|- [arguments]]";

            options = new string[,] {
//                { "-c cmd",                 "Program passed in as string (terminates option list)" },
                { "-h",                     "Display usage" },
//#if !IRONPYTHON_WINDOW
//                { "-i",                     "Inspect interactively after running script" },
//#endif
                { "-V",                     "Print the version number and exit" },
//                { "-O",                     "Enable optimizations" },
//#if DEBUG
//                { "-D",                     "EngineDebug mode" },
//#endif
//                { "-OO",                    "Remove doc-strings in addition to the -O optimizations" },
    
               
//                { "-X:AutoIndent",          "" },
//                { "-X:AssembliesDir",       "Set the directory for saving generated assemblies" },
//#if !SILVERLIGHT
//                { "-X:ColorfulConsole",     "Enable ColorfulConsole" },
//#endif
//                { "-X:ExceptionDetail",     "Enable ExceptionDetail mode" },
//                { "-X:Interpret",           "Enable interpreted mode" },
//                { "-X:Frames",              "Generate custom frames" },
//                { "-X:GenerateAsSnippets",  "Generate code to run in snippet mode" },
//                { "-X:ILDebug",             "Output generated IL code to a text file for debugging" },
//                { "-X:MaxRecursion",        "Set the maximum recursion level" },
//                { "-X:NoOptimize",          "Disable JIT optimization in generated code" },
//                { "-X:NoTraceback",         "Do not emit traceback code" },
//                { "-X:PassExceptions",      "Do not catch exceptions that are unhandled by script code" },
//                { "-X:PrivateBinding",      "Enable binding to private members" },
//                { "-X:SaveAssemblies",      "Save generated assemblies" },
//                { "-X:ShowClrExceptions",   "Display CLS Exception information" },
//                { "-X:SlowOps",             "Enable fast ops" },
//                { "-X:StaticMethods",       "Generate static methods only" },
//#if !SILVERLIGHT
//                { "-X:TabCompletion",       "Enable TabCompletion mode" },
//#endif
//#if DEBUG
//                { "-X:TrackPerformance",    "Track performance sensitive areas" },
//#endif
           };

            environmentVariables = new string[0, 0];

            comments = null;
        }
    }
}
