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

namespace Microsoft.Scripting {
    /// <summary>
    /// Enum representing different types of operators.
    /// 
    /// Operators can be Unary, Binary, or Ternary.  An individual operator can have one or 
    /// more arity.  
    /// 
    /// Each operator is associated with a standard name.  If a method is named using the standard
    /// name and is marked with OperatorMethodAttribute then the method will automatically be
    /// detected as an operator.
    /// </summary>
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2217:DoNotMarkEnumsWithFlags")]
    public enum Operators {
        None,

        /// <summary>
        /// Unary conversion operator. 
        /// 
        /// Returns the string representation of the instance.
        /// 
        /// The standard name for this operator is "ToString".
        /// </summary>
        ConvertToString,                     
        /// <summary>
        /// Unary operator.
        /// 
        /// Get the hash code of the instance.  May throw an exception if the instance is unhashable.
        /// </summary>
        ValueHash,                  
        /// <summary>
        /// Binary operator.
        /// 
        /// Attempt to call the object.  Arguments are the object and the arguments for the call.  The
        /// arguments for the call can either be an object array (normal call) or a KwCallInfo class for
        /// performing a keyword based call.
        /// 
        /// The standard name for this operator is "Call".
        /// </summary>
        Call,                       
        /// <summary>
        /// Unary operator.
        /// 
        /// Returns a string which defines the object in code or a language specific format for
        /// objects which cannot be represented in code.  This operator generally is not used in
        /// a non-language specific scenario.
        /// </summary>
        CodeRepresentation,         

        #region Generated Table of Operators

        // *** BEGIN GENERATED CODE ***

        ///<summary>Operator for performing add</summary>
        Add,
        ///<summary>Operator for performing sub</summary>
        Subtract,
        ///<summary>Operator for performing pow</summary>
        Power,
        ///<summary>Operator for performing mul</summary>
        Multiply,
        ///<summary>Operator for performing floordiv</summary>
        FloorDivide,
        ///<summary>Operator for performing div</summary>
        Divide,
        ///<summary>Operator for performing truediv</summary>
        TrueDivide,
        ///<summary>Operator for performing mod</summary>
        Mod,
        ///<summary>Operator for performing lshift</summary>
        LeftShift,
        ///<summary>Operator for performing rshift</summary>
        RightShift,
        ///<summary>Operator for performing and</summary>
        BitwiseAnd,
        ///<summary>Operator for performing or</summary>
        BitwiseOr,
        ///<summary>Operator for performing xor</summary>
        Xor,
        ///<summary>Operator for performing lt</summary>
        LessThan,
        ///<summary>Operator for performing gt</summary>
        GreaterThan,
        ///<summary>Operator for performing le</summary>
        LessThanOrEqual,
        ///<summary>Operator for performing ge</summary>
        GreaterThanOrEqual,
        ///<summary>Operator for performing eq</summary>
        Equals,
        ///<summary>Operator for performing ne</summary>
        NotEquals,
        ///<summary>Operator for performing lg</summary>
        LessThanGreaterThan,
        ///<summary>Operator for performing in-place add</summary>
        InPlaceAdd,
        ///<summary>Operator for performing in-place sub</summary>
        InPlaceSubtract,
        ///<summary>Operator for performing in-place pow</summary>
        InPlacePower,
        ///<summary>Operator for performing in-place mul</summary>
        InPlaceMultiply,
        ///<summary>Operator for performing in-place floordiv</summary>
        InPlaceFloorDivide,
        ///<summary>Operator for performing in-place div</summary>
        InPlaceDivide,
        ///<summary>Operator for performing in-place truediv</summary>
        InPlaceTrueDivide,
        ///<summary>Operator for performing in-place mod</summary>
        InPlaceMod,
        ///<summary>Operator for performing in-place lshift</summary>
        InPlaceLeftShift,
        ///<summary>Operator for performing in-place rshift</summary>
        InPlaceRightShift,
        ///<summary>Operator for performing in-place and</summary>
        InPlaceBitwiseAnd,
        ///<summary>Operator for performing in-place or</summary>
        InPlaceBitwiseOr,
        ///<summary>Operator for performing in-place xor</summary>
        InPlaceXor,
        ///<summary>Operator for performing reverse add</summary>
        ReverseAdd,
        ///<summary>Operator for performing reverse sub</summary>
        ReverseSubtract,
        ///<summary>Operator for performing reverse pow</summary>
        ReversePower,
        ///<summary>Operator for performing reverse mul</summary>
        ReverseMultiply,
        ///<summary>Operator for performing reverse floordiv</summary>
        ReverseFloorDivide,
        ///<summary>Operator for performing reverse div</summary>
        ReverseDivide,
        ///<summary>Operator for performing reverse truediv</summary>
        ReverseTrueDivide,
        ///<summary>Operator for performing reverse mod</summary>
        ReverseMod,
        ///<summary>Operator for performing reverse lshift</summary>
        ReverseLeftShift,
        ///<summary>Operator for performing reverse rshift</summary>
        ReverseRightShift,
        ///<summary>Operator for performing reverse and</summary>
        ReverseBitwiseAnd,
        ///<summary>Operator for performing reverse or</summary>
        ReverseBitwiseOr,
        ///<summary>Operator for performing reverse xor</summary>
        ReverseXor,

        // *** END GENERATED CODE ***

        #endregion

        /// <summary>
        /// Binary operator.
        /// 
        /// Checks to see if the instance contains another object.  Returns true or false.
        /// 
        /// The standard name for this operator is "Contains".
        /// </summary>
        Contains,                   
        /// <summary>
        /// n-ary operator.
        /// 
        /// Gets the value at the specified index from the instance.
        /// 
        /// One or more indexes can be provided as individual arguments.
        /// </summary>
        GetItem,                    
        /// <summary>
        /// n-ary operator.
        /// 
        /// Sets the value at the specified index in the instance.
        /// 
        /// One or more indexes can be provided as individual arguments.  The last value provided is the value to be set.
        /// </summary>
        SetItem,                    
        /// <summary>
        /// n-ary operator.
        /// 
        /// Removes the item from the specified index in the instance.
        /// 
        /// One or more indexes can be provided as individual arguments.
        /// </summary>
        DeleteItem,
        /// <summary>
        /// Binary or Ternary operator.
        /// 
        /// Gets the specified range of elements (slice) from the instance.
        /// 
        /// The slice parameters may include the start index, the end index, and the step value.  The step value is optional.
        /// 
        /// A value of Type.Missing may be provided if no parameter was explicitly provided for a start, stop or step parameter.
        /// </summary>
        GetSlice,
        /// <summary>
        /// n-ary operator.
        /// 
        /// Sets the specified range of elements in the instance.
        /// 
        /// The slice parameters may include the start index, the end index, and the step value.  The step
        /// value is optional.  The last parameter is the value to be assigned.
        /// 
        /// A value of Type.Missing may be provided if no parameter was explicitly provided for a start, stop or step parameter.
        /// </summary>
        SetSlice,
        /// <summary>
        /// n-ary operator.
        /// 
        /// Removes the specified range of elements from the instance.
        /// 
        /// The slice parameters may include the start index, the end index, and the step value.  The step value is
        /// optional.
        /// 
        /// A value of Type.Missing may be provided if no parameter was explicitly provided for a start, stop or step parameter.
        /// </summary>
        DeleteSlice,
        /// <summary>
        /// Unary operator.
        /// 
        /// Returns the number of items stored in the object.
        /// </summary>      
        Length,                     
        /// <summary>
        /// Binary operator.
        /// 
        /// Compares two instances returning an integer indicating the relationship between them.  May
        /// throw if the object types are uncomparable.
        /// 
        /// The standard name for this operator is "Compare".
        /// </summary>
        Compare,      
        /// <summary>
        /// Binary operator.
        /// 
        /// Returns both the dividend and quotioent of x / y.
        /// </summary>
        DivMod,                     
        /// <summary>
        /// Binary operator.
        /// 
        /// Returns both the dividend and quotient of y / x.
        /// </summary>
        ReverseDivMod,              
        /// <summary>
        /// Member lookup customization (called after type lookup).
        /// 
        /// Arguments are the instance to get the member from and a SymbolId which represents the member.
        /// 
        /// The return value is the member.
        /// 
        /// The standard name for this operator is "GetMember".
        /// </summary>
        GetMember,                  
        /// <summary>
        /// Member lookup customization for bound attributes
        /// 
        /// Arguments are the instance to get the member from and a SymbolId which represents the bound member.
        /// 
        /// The return value is the bound member.
        /// 
        /// /// The standard name for this operator is "GetBoundMember".
        /// </summary>
        GetBoundMember,            
        /// <summary>
        /// Member set customization.
        /// 
        /// Arguments are the instance, the SymbolId to get, and the new value for the member.
        /// 
        /// The return value is ignored.
        /// 
        /// The standard name for this operator is "SetMember".
        /// </summary>
        SetMember,   
        /// <summary>
        /// Member delete customization.
        /// 
        /// Arguments are the instance and the SymbolId for the member to delete.
        /// 
        /// The return value is ignored.
        /// 
        /// The standard name for this operator is "DeleteMember".
        /// </summary>
        DeleteMember,      
        /// <summary>
        /// Attribute customization operator.  Returns a list of names that should be displayed as
        /// being part of the object.
        /// 
        /// Arguments are the instance to get the list of member names from.
        /// 
        /// Return value is IList&lt;SymbolId&gt;.
        /// 
        /// /// The standard name for this operator is "GetMemberNames".
        /// </summary>
        GetMemberNames,             
        /// <summary>
        /// Unary operator.
        /// 
        /// Get the absolute value of the instance.
        /// </summary>
        AbsoluteValue,
        /// <summary>
        /// Unary operator.
        /// 
        /// Gets the positive value of the instance.
        /// </summary>
        Positive,                   
        /// <summary>
        /// Unary operator.
        /// 
        /// Negates the instance and return the new value.
        /// </summary>        
        Negate,                     
        /// <summary>
        /// Unary operator.
        /// 
        /// Returns the ones complement of the instance.
        /// </summary>
        OnesComplement,             
        /// <summary>
        /// Unary conversion operator.
        /// 
        /// Attempt to convert the value to a BigInteger.
        /// </summary>
        ConvertToBigInteger,        
        /// <summary>
        /// Unary conversion operator.
        /// 
        /// Attempt to convert the value to a Complex64 value.
        /// </summary>
        ConvertToComplex,           
        /// <summary>
        /// Unary conversion operator.
        /// 
        /// Attempt to convert the value to a Double.
        /// </summary>
        ConvertToDouble,            
        /// <summary>
        /// Unary conversion operator.
        /// 
        /// Attempt to convert the value to an signed 32-bit integer.
        /// </summary>
        ConvertToInt32,             
        /// <summary>
        /// Unary conversion operator.
        /// 
        /// Attempt to convert the value to a hexidecimal string.
        /// </summary>
        ConvertToHex,               
        /// <summary>
        /// Unary conversion operator.
        /// 
        /// Attempt to convert the value to an octal string.
        /// </summary>
        ConvertToOctal,             
        /// <summary>
        /// Unary conversion operator.
        /// 
        /// Attempt to convert the value to True or False.
        /// </summary>
        ConvertToBoolean,

        RightShiftUnsigned,         //Operator for performing rshiftu
        InPlaceRightShiftUnsigned,  //Operator for performing in-place rshiftu
        ReverseRightShiftUnsigned,  //Operator for performing reverse rshiftu
        RightShiftSigned,  
        Not,                        // boolean negation
        Increment,
        Decrement,
        Assign,
        IsFalse,
        IsTrue,
        Or,
        And,
        Comma,

        GetEnumerator,
        Dispose,

        // TODO: What about these operators?  Should they go away?
        GetState,                   // Python reduce protocol
        Missing,                    // Python style missing dictionary key
        GetDescriptor,              // user defined descriptor
        SetDescriptor,              // user defined data descriptor
        DeleteDescriptor,           // user defined data descriptor
        MoveNext,                   // iterator protocol
        Coerce,                     // coersion
        Unassign,                   // finalization

        IdMask = 0x7fffffff,
        UserDefinedFlag = unchecked((int)0x80000000),
    }
}
