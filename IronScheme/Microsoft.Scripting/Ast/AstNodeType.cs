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

namespace Microsoft.Scripting.Ast {
    public enum AstNodeType {
        Add,
        //    AddChecked,
        And,
        AndAlso,
        //    ArrayLength,
        //    ArrayIndex,
        Call,
        //    Coalesce,
        Conditional,
        Constant,
        Convert,
        //    ConvertChecked,
        Divide,
        Equal,
        ExclusiveOr,
        GreaterThan,
        GreaterThanOrEqual,
        //    Invoke,
        //    Lambda,
        LeftShift,
        LessThan,
        LessThanOrEqual,
        //    ListInit,
        //    MemberAccess,
        //    MemberInit,
        Modulo,
        Multiply,
        //    MultiplyChecked,
        Negate,
        //    UnaryPlus,
        //    NegateChecked,
        New,
        //    NewArrayInit,
        //    NewArrayBounds,
        Not,
        NotEqual,
        Or,
        OrElse,
        //    Parameter,
        //    Power,
        //    Quote,
        RightShift,
        Subtract,
        //    SubtractChecked,
        //    TypeAs,
        TypeIs,

        // DLR Added values
        ActionExpression,
        ArrayIndexAssignment,
        ArrayIndexExpression,
        BlockStatement,
        BoundAssignment,
        BoundExpression,
        BreakStatement,
        CatchBlock,
        CodeBlock,
        CodeBlockExpression,
        CodeContextExpression,
        CommaExpression,
        ContinueStatement,
        DebugStatement,
        DeleteStatement,
        DeleteUnboundExpression,
        DoStatement,
        DynamicConversionExpression,
        EmptyStatement,
        EnvironmentExpression,
        ExpressionStatement,
        GeneratorCodeBlock,
        IfStatement,
        IfStatementTest,
        LabeledStatement,
        LoopStatement,
        MemberAssignment,
        MemberExpression,
        NewArrayExpression,

        OnesComplement,

        ParamsExpression,
        ParenthesizedExpression,
        ReturnStatement,
        ScopeStatement,
        SwitchCase,
        SwitchStatement,
        ThrowStatement,
        TryStatement,

        UnboundAssignment,
        UnboundExpression,
        VoidExpression,
        YieldStatement
    }
}