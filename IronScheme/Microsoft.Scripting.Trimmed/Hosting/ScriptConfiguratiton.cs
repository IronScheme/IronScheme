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

#if !SILVERLIGHT

using System;
using System.Collections.Generic;
using System.Text;
using System.Configuration;

namespace Microsoft.Scripting.Hosting {

    // <microsoft.scripting>
    //   <languages>
    //     <add name="IronPython" extensions=".py" provider="..." />
    //   </language>
    // </microsoft.scripting>

    public class ScriptConfiguration : ConfigurationSection {

        public const string Section = "microsoft.scripting";

        //[ConfigurationProperty("languages")]
        //public ScriptLanguages Languages {
        //    get {
        //        return (ScriptLanguages)this["languages"];
        //    }
        //    set {
        //        this["languages"] = value;
        //    }
        //}
    }

    //public class ScriptLanguages : ConfigurationElementCollection {
    //    public override ConfigurationElementCollectionType CollectionType {
    //        get {
    //            return ConfigurationElementCollectionType.BasicMapAlternate;
    //        }
    //    }
    //    protected override ConfigurationElement CreateNewElement() {
    //        return new ScriptLanguage();
    //    }

    //    protected override object GetElementKey(ConfigurationElement element) {
    //        return ((ScriptLanguage)element).Name;
    //    }
    //}

    //public class ScriptLanguage : ConfigurationElement {
    //    [ConfigurationProperty("name", IsRequired = true)]
    //    public string Name {
    //        get {
    //            return (string)this["name"];
    //        }
    //        set {
    //            this["name"] = value;
    //        }
    //    }

    //    [ConfigurationProperty("extensions", IsRequired = true)]
    //    public string Extensions {
    //        get {
    //            return (string)this["extensions"];
    //        }
    //        set {
    //            this["extensions"] = value;
    //        }
    //    }

    //    [ConfigurationProperty("type", IsRequired = true)]
    //    public string EngineType {
    //        get {
    //            return (string)this["engineType"];
    //        }
    //        set {
    //            this["engineType"] = value;
    //        }
    //    }

    //    public override string ToString() {
    //        return String.Format("ScriptingLanguage: {0}", Name);
    //    }
    //}
}

#endif