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

namespace Microsoft.Scripting.Hosting
{
    public interface IScriptEnvironment {
        string[] GetRegisteredFileExtensions();
        ILanguageProvider GetLanguageProvider(Type languageProviderType);
    }

    public sealed class ScriptEnvironment : IScriptEnvironment {

        private readonly ScriptDomainManager _manager;
        
        internal ScriptEnvironment(ScriptDomainManager manager) {
            Debug.Assert(manager != null);
            _manager = manager;
        }

        public static IScriptEnvironment GetEnvironment() {
            return ScriptDomainManager.CurrentManager.Environment;
        }

        public string[] GetRegisteredFileExtensions() {
            return _manager.GetRegisteredFileExtensions();
        }

        public ILanguageProvider GetLanguageProvider(Type languageProviderType) {
            return _manager.GetLanguageProvider(languageProviderType);
        }
    }
}
