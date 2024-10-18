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
using System.Diagnostics;
using System.Runtime.CompilerServices;
using Microsoft.Scripting.Shell;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Hosting
{

    public interface ILanguageService {
        // TODO:
    }

    public interface ILanguageProvider  {
        IScriptEnvironment Environment { get; }
        string LanguageDisplayName { get; }
        string[] GetRegisteredIdentifiers();
        string[] GetRegisteredExtensions();

        // well known services:
        IScriptEngine GetEngine();

        // TODO:
        OptionsParser GetOptionsParser(); // TODO
        CommandLine GetCommandLine(); // TODO
        IConsole GetConsole(CommandLine commandLine, IScriptEngine engine, ConsoleOptions options); // TODO
       
        // generic interface:
        ServiceType/*?*/ GetService<ServiceType>(params object[] args) where ServiceType : class;
    }

    /// <summary>
    /// Should be stateless - multiple instances for each language can exist.
    /// </summary>
    public abstract class LanguageProvider : ILanguageProvider {

        private ScriptDomainManager _manager;

        internal ScriptDomainManager Manager {
            get { return _manager; }
        }

        public IScriptEnvironment Environment {
            get { return _manager.Environment; }
        }

        protected LanguageProvider(ScriptDomainManager manager) {
            Contract.RequiresNotNull(manager, "manager");
            _manager = manager;
        }

        public string[] GetRegisteredIdentifiers() {
            return _manager.GetLanguageIdentifiers(GetType(), false);
        }

        public string[] GetRegisteredExtensions() {
            return _manager.GetLanguageIdentifiers(GetType(), true);
        }


        #region Remotable Services
        public abstract string LanguageDisplayName { get; }

        IScriptEngine ILanguageProvider.GetEngine() {
            return this.GetEngine();
        }

        #endregion

        #region Local Service Factories

        public virtual ScriptEngine GetEngine() {
            throw new NotSupportedException(String.Format(Resources.MissingService_OptionsParser, LanguageDisplayName));
        }

        public virtual CommandLine GetCommandLine() {
            throw new NotSupportedException(String.Format(Resources.MissingService_CommandLine, LanguageDisplayName));
        }

        public virtual OptionsParser GetOptionsParser() {
            throw new NotSupportedException(String.Format(Resources.MissingService_OptionsParser, LanguageDisplayName));
        }

        #endregion


        // TODO:

        static bool IsInputRedirected()
        {
          try
          {
            var f = System.Console.KeyAvailable;
            return false;
          }
          catch
          {
            return true;
          }
        }

        public static bool InputRedirected
        {
          get { return IsInputRedirected(); }
        } 

        public virtual IConsole GetConsole(CommandLine commandLine, IScriptEngine engine, ConsoleOptions options) {
            Contract.RequiresNotNull(engine, "engine");
            Contract.RequiresNotNull(options, "options");

            if (options.TabCompletion && !InputRedirected)
            {
                return CreateSuperConsole(commandLine, engine, options.ColorfulConsole);
            } else {
                return new BasicConsole(engine, options.ColorfulConsole);
            }
        }

        // The advanced console functions are in a special non-inlined function so that 
        // dependencies are pulled in only if necessary.
        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        private static IConsole CreateSuperConsole(CommandLine commandLine, IScriptEngine engine, bool isColorful) {
            Debug.Assert(engine != null);
            return new SuperConsole(commandLine, engine, isColorful);
        }

        // generic interface:
        public virtual ServiceType/*?*/ GetService<ServiceType>(params object[] args) 
            where ServiceType : class {

            Type service_type = typeof(ServiceType);
            
            if (service_type == typeof(ScriptEngine)) {
                return (ServiceType)(object)GetEngine();
            }

            if (service_type == typeof(IConsole)) {
                return (ServiceType)GetConsole(GetArg<CommandLine>(args, 0, false), GetArg<ScriptEngine>(args, 1, false), GetArg<ConsoleOptions>(args, 2, false));
            }

            if (service_type == typeof(CommandLine)) {
                return (ServiceType)(object)GetCommandLine();
            }

            if (service_type == typeof(OptionsParser)) {
                return (ServiceType)(object)GetOptionsParser();
            }

            return null;
        }

        private T GetArg<T>(object[] arg, int index, bool optional) {
            if (!optional && index >= arg.Length) {
                throw new ArgumentException("Invalid number of parameters for the service");
            }
            
            if (!(arg is T)) {
                throw new ArgumentException(
                    String.Format("Invalid argument type; expecting {0}", typeof(T)),
                    String.Format("arg[{0}]", index));
            }

            return (T)arg[index];
        }

    }
}
