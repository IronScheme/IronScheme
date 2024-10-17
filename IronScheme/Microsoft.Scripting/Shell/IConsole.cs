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

namespace Microsoft.Scripting.Shell
{
    public interface IConsole {
        // Read a single line of interactive input
        // AutoIndentSize is the indentation level to be used for the current suite of a compound statement.
        // The console can ignore this argument if it does not want to support auto-indentation
        string ReadLine(int autoIndentSize);

        void Write(string text, Style style);
        void WriteLine(string text, Style style);
        void WriteLine();
    }
}