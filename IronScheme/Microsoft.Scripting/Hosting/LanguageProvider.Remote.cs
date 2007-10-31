/* ****************************************************************************
 *
 * Copyright (c) Microsoft Corporation. 
 *
 * This source code is subject to terms and conditions of the Microsoft Permissive License. A 
 * copy of the license can be found in the License.html file at the root of this distribution. If 
 * you cannot locate the  Microsoft Permissive License, please send an email to 
 * dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
 * by the terms of the Microsoft Permissive License.
 *
 * You must not remove this notice, or any other, from this software.
 *
 *
 * ***************************************************************************/
#if !SILVERLIGHT

using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using Microsoft.Scripting.Shell;

namespace Microsoft.Scripting.Hosting {

    internal sealed class RemoteLanguageProvider : RemoteWrapper, ILanguageProvider {
        private readonly LanguageProvider _provider;

        public override ILocalObject LocalObject {
            get { return _provider; }
        }

        public string LanguageDisplayName {
            get { return _provider.LanguageDisplayName; }
        }

        public IScriptEnvironment Environment {
            get {
                return new RemoteScriptEnvironment(_provider.Manager);
            }
        }

        internal RemoteLanguageProvider(LanguageProvider provider) {
            Debug.Assert(provider != null);
            _provider = provider;
        }

        public override object InitializeLifetimeService() {
            return null;
        }

        public string[] GetRegisteredIdentifiers() {
            return _provider.GetRegisteredIdentifiers();
        }

        public string[] GetRegisteredExtensions() {
            return _provider.GetRegisteredExtensions();
        }

        public IScriptEngine GetEngine() {
            return new RemoteScriptEngine(_provider.GetEngine());
        }

        public IScriptEngine GetEngine(EngineOptions options) {
            return new RemoteScriptEngine(_provider.GetEngine(options));
        }

        public ITokenCategorizer GetTokenCategorizer() {
            return new RemoteTokenCategorizer(_provider.GetTokenCategorizer());
        }

        public OptionsParser GetOptionsParser() {
            throw new NotImplementedException("TODO");
        }

        public CommandLine GetCommandLine() {
            throw new NotImplementedException("TODO");
        }

        public IConsole GetConsole(CommandLine commandLine, IScriptEngine engine, ConsoleOptions options) {
           throw new NotImplementedException("TODO");
        }

        public ServiceType/*?*/ GetService<ServiceType>(params object[] args) 
            where ServiceType : class {

            // TODO: Remote wrapper
            return _provider.GetService<ServiceType>(args);
        }

    }
}

#endif