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

using System.Diagnostics.CodeAnalysis;

[assembly: SuppressMessage("Microsoft.Performance", "CA1810:InitializeReferenceTypeStaticFieldsInline", Scope = "member", Target = "Microsoft.Scripting.SymbolTable..cctor()")]
[assembly: SuppressMessage("Microsoft.Design", "CA1051:DoNotDeclareVisibleInstanceFields", Scope="member", Target="Microsoft.Scripting.SymbolId.Id")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1025:ReplaceRepetitiveArgumentsWithParamsArray", Scope = "member", Target = "Microsoft.Scripting.CallTarget5.Invoke(System.Object,System.Object,System.Object,System.Object,System.Object):System.Object")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1025:ReplaceRepetitiveArgumentsWithParamsArray", Scope = "member", Target = "Microsoft.Scripting.CallTarget4.Invoke(System.Object,System.Object,System.Object,System.Object):System.Object")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1043:UseIntegralOrStringArgumentForIndexers", Scope = "member", Target = "Microsoft.Scripting.IAttributesCollection.Item[Microsoft.Scripting.SymbolId]")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1062:ValidateArgumentsOfPublicMethods", Scope = "member", Target = "Microsoft.Scripting.DynamicTypeBuilder..ctor(System.Type)")]
[assembly: SuppressMessage("Microsoft.Performance", "CA1819:PropertiesShouldNotReturnArrays", Scope = "member", Target = "Microsoft.Scripting.ConsoleOptions.RemainingArgs")]
[assembly: SuppressMessage("Microsoft.Performance", "CA1805:DoNotInitializeUnnecessarily", Scope = "member", Target = "Microsoft.Scripting.SuperConsole..ctor(Microsoft.Scripting.Hosting.ScriptEngine,System.Boolean)")]
[assembly: SuppressMessage("Microsoft.Performance", "CA1819:PropertiesShouldNotReturnArrays", Scope = "member", Target = "Microsoft.Scripting.CommandLine.Arguments")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1004:GenericMethodsShouldProvideTypeParameter", Scope = "member", Target = "Microsoft.Scripting.Generation.CodeGen.EmitArray(System.Int32,Microsoft.Scripting.Generation.EmitArrayHelper):System.Void")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1004:GenericMethodsShouldProvideTypeParameter", Scope = "member", Target = "Microsoft.Scripting.Generation.CodeGen.EmitArray(System.Collections.Generic.IList`1<T>):System.Void")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1062:ValidateArgumentsOfPublicMethods", Scope = "member", Target = "Microsoft.Scripting.Generation.CodeGen..ctor(Microsoft.Scripting.Generation.TypeGen,System.Reflection.MethodBase,System.Reflection.Emit.ILGenerator,System.Type[])")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1062:ValidateArgumentsOfPublicMethods", Scope = "member", Target = "Microsoft.Scripting.Generation.AssemblyGen.CreateCodeGen(Microsoft.Scripting.Generation.TypeGen,System.Reflection.MethodBase,System.Reflection.Emit.ILGenerator,System.Type[]):Microsoft.Scripting.Generation.CodeGen")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1020:AvoidNamespacesWithFewTypes", Scope = "namespace", Target = "Microsoft.Scripting.Ast")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1020:AvoidNamespacesWithFewTypes", Scope = "namespace", Target = "Microsoft.Scripting.Hosting")]
[assembly: System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1043:UseIntegralOrStringArgumentForIndexers", Scope = "member", Target = "Microsoft.Scripting.Generation.LexicalScope.Item[Microsoft.Scripting.SymbolId]")]
