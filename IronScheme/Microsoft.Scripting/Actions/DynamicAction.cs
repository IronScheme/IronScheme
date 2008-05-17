
#if FULL
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

namespace Microsoft.Scripting.Actions {
    public enum DynamicActionKind {
        DoOperation,
        ConvertTo,

        GetMember,
        SetMember,
        DeleteMember,
        InvokeMember,

        Call,
        CreateInstance
    }

    public abstract class DynamicAction {
        public abstract DynamicActionKind Kind { get; }
        
        public override string ToString() {
            return Kind.ToString();
        }

        public static GetMemberAction GetMember(string name) {
            return GetMemberAction.Make(name);
        }

        public static SetMemberAction SetMember(string name) {
            return SetMemberAction.Make(name);
        }

        public static DeleteMemberAction DeleteMember(string name) {
            return DeleteMemberAction.Make(SymbolTable.StringToId(name));
        }

        public static DoOperationAction DoOperation(Operators op) {
            return DoOperationAction.Make(op);
        }

        public static CallAction Call(int argumentCount) {
            return CallAction.Make(argumentCount);
        }

        public static CreateInstanceAction CreateInstance(int argumentCount) {
            return CreateInstanceAction.Make(argumentCount);
        }

        public static InvokeMemberAction InvokeMember(string name, int argumentCount) {
            return InvokeMemberAction.Make(SymbolTable.StringToId(name),
                InvokeMemberActionFlags.None, new CallSignature(argumentCount));
        }
    }
}

#endif	
