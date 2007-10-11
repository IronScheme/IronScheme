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

using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;
using System.Reflection;
using System.Reflection.Emit;

namespace Microsoft.Scripting {
    /// <summary>
    /// Helper for storing information about stack frames.
    /// </summary>
    public class DynamicStackFrame {
        private CodeContext _context;
        private string _funcName;
        private string _filename;
        private int _lineNo;
        private MethodBase _method;

        public DynamicStackFrame(CodeContext context, MethodBase method, string funcName, string filename, int line) {
            _context = context;
            _funcName = funcName;
            _filename = filename;
            _lineNo = line;
            _method = method;
        }

        public CodeContext CodeContext {
            get {
                return _context;
            }
        }

        public MethodBase GetMethod() {
            return _method;
        }

        public string GetMethodName() {
            return _funcName;
        }

        public string GetFileName() {
            return _filename;
        }

        public int GetFileLineNumber() {
            return _lineNo;
        }

    }
}
