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
using System.Collections;
using System.Collections.Generic;

using System.Reflection;
using System.Reflection.Emit;

using System.Resources;
using System.Diagnostics;
using System.Diagnostics.SymbolStore;
using System.IO;
using System.Globalization;

using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Math;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Actions;
using System.Text;
using Microsoft.Scripting.Utils;
using BigInteger = Oyster.Math.IntX;
using System.Runtime.InteropServices;

namespace Microsoft.Scripting.Generation {

    public delegate void EmitArrayHelper(int index);

    class CodeGenDescriptor
    {
      public bool varargs;
      public int arity;
      public CodeGen cg;
    }

    /// <summary>
    /// CodeGen is a helper class to make code generation a simple task.  Rather than interacting
    /// at the IL level CodeGen raises the abstraction level to enable emitting of values, expressions,
    /// handling the details of exception handling, loops, etc...
    /// </summary>
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Maintainability", "CA1506:AvoidExcessiveClassCoupling")] // TODO: fix
    public class CodeGen : IDisposable {
        private CodeGenOptions _options;
        private TypeGen _typeGen;
        private ISymbolDocumentWriter _debugSymbolWriter;
        private ScopeAllocator _allocator;

        private readonly MethodBase _methodInfo;
        private readonly ILGenerator _ilg;
        private MethodInfo _methodToOverride;
        private ListStack<Targets> _targets = new ListStack<Targets>();
        private List<Slot> _freeSlots = new List<Slot>();
        private IList<Label> _yieldLabels;
        private Nullable<ReturnBlock> _returnBlock;
        internal static Dictionary<CodeBlock, CodeGen> _codeBlockImplementations = new Dictionary<CodeBlock,CodeGen>();
        internal static Dictionary<CodeBlock, CodeGen> _codeBlockStubs = new Dictionary<CodeBlock, CodeGen>();
        internal static Dictionary<SymbolId, CodeGen> _codeBlockLookup = new Dictionary<SymbolId, CodeGen>();
        internal static Dictionary<CodeBlock, CodeGen> _codeBlockStubsX = new Dictionary<CodeBlock, CodeGen>();
        internal static Dictionary<SymbolId, CodeGen> _codeBlockLookupX = new Dictionary<SymbolId, CodeGen>();
        internal static Dictionary<CodeBlock, CodeGen> _codeBlockStubsN = new Dictionary<CodeBlock, CodeGen>();
        internal static Dictionary<SymbolId, CodeGenDescriptor[]> _codeBlockLookupN = new Dictionary<SymbolId, CodeGenDescriptor[]>();


        // Key slots
        private EnvironmentSlot _environmentSlot;   // reference to function's own environment
        private Slot _contextSlot;                  // code context
        private Slot _paramsSlot;                   // slot for the parameter array, if any

        // Runtime line # tracking
        private Slot _currentLineSlot;              // used to track the current line # at runtime
        private int _currentLine;                   // last line number emitted to avoid dupes

        private Slot[] _argumentSlots;
        private CompilerContext _context;
        private ActionBinder _binder;

        private readonly ConstantPool _constantPool;
        internal Label startpoint;

        private bool _generator;                    // true if emitting generator, false otherwise
        private Slot _gotoRouter;                   // Slot that stores the number of the label to go to.

        public const int FinallyExitsNormally = 0;
        public const int BranchForReturn = 1;
        public const int BranchForBreak = 2;
        public const int BranchForContinue = 3;

        // This is true if we are emitting code while in an interpreted context.
        // This flag should always be flowed through to other CodeGen objects created from this one.
        private bool _interpretedMode = false;

        public void DefineStartPoint()
        {
          startpoint = DefineLabel();
          MarkLabel(startpoint);
        }

        public CodeGen(TypeGen typeGen, AssemblyGen assemblyGen, MethodBase mi, ILGenerator ilg,
            IList<Type> paramTypes, ConstantPool constantPool) {
            Debug.Assert(typeGen == null || typeGen.AssemblyGen == assemblyGen);
            this._typeGen = typeGen;
            this._methodInfo = mi;
            this._ilg = ilg;
            this._constantPool = constantPool;

            if (_typeGen == null) this.DynamicMethod = true;

            Debug.Assert(constantPool == null || mi.IsStatic);

            int firstArg;
            if (constantPool != null) {
                Debug.Assert(paramTypes.Count > 0);
                constantPool.SetCodeGen(this, new ArgSlot(0, constantPool.SlotType, this));
                firstArg = 1;
            } else {
                firstArg = 0;
                _constantPool = new ConstantPool();
                _constantPool.SetCodeGen(this, null);
            }

            int thisOffset = !mi.IsStatic ? 1 : 0;
            
            _argumentSlots = new Slot[paramTypes.Count - firstArg];
            for (int i = 0; i < _argumentSlots.Length; i++) {
                _argumentSlots[i] = new ArgSlot(i + firstArg + thisOffset, paramTypes[i + firstArg], this);
            }


            ILDebug = assemblyGen.ILDebug;
            // this is a bit more tricky than i would think :|
            //CacheConstants = true;

#if !DEBUG
        }
#else

            }

        private string GetPerfTrackName(MethodBase mi) {
            for (int i = 0; i < mi.Name.Length; i++) {                
                if (!Char.IsLetter(mi.Name[i]) && mi.Name[i] != '.') {
                    return mi.Name.Substring(0, i);
                }
            }
            return mi.Name;
        }
#endif

        public override string ToString() {
            return _methodInfo.ToString();
        }

        public bool EmitDebugInfo {
            get 
            {
              return _typeGen != null && _typeGen.AssemblyGen.SymbolWriter != null;// _debugSymbolWriter != null;
            }
        }

        //[Obsolete("use Methodbase instead")]
        public MethodInfo MethodInfo {
            get { return (MethodInfo)_methodInfo; }
        }

        public MethodBase MethodBase {
            get {
                return _methodInfo;
            }
        }

        internal bool IsGenerator {
            get { return _generator; }
            set { _generator  = value; }
        }

        public CompilerContext Context {
            get {
                Debug.Assert(_context != null);
                return _context;
            }
            set {
                _context = value;
                this.Binder = _context.SourceUnit.Engine.DefaultBinder;
            }
        }

        public bool HasContext {
            get { return _context != null; }
        }

        public ActionBinder Binder {
            get {
                if (_binder == null) {
                    throw new InvalidOperationException("no Binder has been set");
                }
                return _binder;
            }
            set {
                _binder = value;
            }
        }

        public TargetBlockType BlockType {
            get {
                if (_targets.Count == 0) return TargetBlockType.Normal;
                Targets t = _targets.Peek();
                return t.BlockType;
            }
        }

        public Nullable<Label> BlockContinueLabel {
            get {
                if (_targets.Count == 0) return Targets.NoLabel;
                Targets t = _targets.Peek();
                return t.continueLabel;
            }
        }

        public const int GotoRouterNone = -1;
        public const int GotoRouterYielding = -2;

        public Slot GotoRouter {
            get { return _gotoRouter; }
            set { _gotoRouter = value; }
        }

        /// <summary>
        /// Returns true if we are attempting to generate code while in interpreted mode;
        /// this changes some variable scoping behavior.
        /// </summary>
        public bool InterpretedMode {
            get { return _interpretedMode; }
            set { _interpretedMode = value; }
        }

        public void PushExceptionBlock(TargetBlockType type, Slot returnFlag) {
            if (_targets.Count == 0) {
                _targets.Push(new Targets(Targets.NoLabel, Targets.NoLabel, type, returnFlag, null));
            } else {
                Targets t = _targets.Peek();
                _targets.Push(new Targets(t.breakLabel, t.continueLabel, type, returnFlag ?? t.finallyReturns, null));
            }
        }

        public void PushTryBlock() {
            PushExceptionBlock(TargetBlockType.Try, null);
        }

        public void PushTargets(Nullable<Label> breakTarget, Nullable<Label> continueTarget, Statement statement) {
            if (_targets.Count == 0) {
                _targets.Push(new Targets(breakTarget, continueTarget, BlockType, null, statement));
            } else {
                Targets t = _targets.Peek();
                TargetBlockType bt = t.BlockType;
                if (bt == TargetBlockType.Finally) {
                    bt = TargetBlockType.LoopInFinally;
                }
                _targets.Push(new Targets(breakTarget, continueTarget, bt, t.finallyReturns, statement));
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA1801:ReviewUnusedParameters", MessageId = "type")]
        public void PopTargets(TargetBlockType type) {
            Targets t = _targets.Pop();
            Debug.Assert(t.BlockType == type);
        }

        public void PopTargets() {
            _targets.Pop();
        }

        public void CheckAndPushTargets(Statement statement) {
            for (int i = _targets.Count - 1; i >= 0; i--) {
                if (_targets[i].statement == statement) {
                    PushTargets(_targets[i].breakLabel, _targets[i].continueLabel, null);
                    return;
                }
            }

            throw new InvalidOperationException("Statement not on the stack");
        }

        public void EmitBreak() {
            Targets t = _targets.Peek();
            int finallyIndex = -1;
            switch (t.BlockType) {
                default:
                case TargetBlockType.Normal:
                case TargetBlockType.LoopInFinally:
                    if (t.breakLabel.HasValue)
                        Emit(OpCodes.Br, t.breakLabel.Value);
                    else
                        throw new InvalidOperationException();
                    break;
                case TargetBlockType.Try:
                case TargetBlockType.Else:
                case TargetBlockType.Catch:
                    for (int i = _targets.Count - 1; i >= 0; i--) {
                        if (_targets[i].BlockType == TargetBlockType.Finally) {
                            finallyIndex = i;
                            break;
                        }

                        if (_targets[i].BlockType == TargetBlockType.LoopInFinally)
                            break;
                    }

                    if (finallyIndex == -1) {
                        if (t.breakLabel.HasValue)
                            Emit(OpCodes.Leave, t.breakLabel.Value);
                        else
                            throw new InvalidOperationException();
                    } else {
                        if(!_targets[finallyIndex].leaveLabel.HasValue)
                            _targets[finallyIndex].leaveLabel = DefineLabel();

                        EmitInt(CodeGen.BranchForBreak);
                        _targets[finallyIndex].finallyReturns.EmitSet(this);

                        Emit(OpCodes.Leave, _targets[finallyIndex].leaveLabel.Value);
                    }
                    break;
                case TargetBlockType.Finally:
                    EmitInt(CodeGen.BranchForBreak);
                    t.finallyReturns.EmitSet(this);
                    Emit(OpCodes.Endfinally);
                    break;
            }
        }

        public void EmitContinue() {
            Targets t = _targets.Peek();
            switch (t.BlockType) {
                default:
                case TargetBlockType.Normal:
                case TargetBlockType.LoopInFinally:
                    if (t.continueLabel.HasValue)
                        Emit(OpCodes.Br, t.continueLabel.Value);
                    else
                        throw new InvalidOperationException();
                    break;
                case TargetBlockType.Try:
                case TargetBlockType.Else:
                case TargetBlockType.Catch:
                    if (t.continueLabel.HasValue)
                        Emit(OpCodes.Leave, t.continueLabel.Value);
                    else
                        throw new InvalidOperationException();
                    break;
                case TargetBlockType.Finally:
                    EmitInt(CodeGen.BranchForContinue);
                    t.finallyReturns.EmitSet(this);
                    Emit(OpCodes.Endfinally);
                    break;
            }
        }

        public void EmitReturn() {
            int finallyIndex = -1;
            switch (BlockType) {
                default:
                case TargetBlockType.Normal:
                    Emit(OpCodes.Ret);
                    EmitSequencePointNone();
                    break;
                case TargetBlockType.Catch:
                case TargetBlockType.Try:
                case TargetBlockType.Else:
                    // with has it's own finally block, so no need to search...
                    for (int i = _targets.Count - 1; i >= 0; i--) {
                        if (_targets[i].BlockType == TargetBlockType.Finally) {
                            finallyIndex = i;
                            break;
                        }
                    }

                    EnsureReturnBlock();
                    Debug.Assert(_returnBlock.HasValue);
                    if (CompilerHelpers.GetReturnType(_methodInfo) != typeof(void)) {
                        _returnBlock.Value.returnValue.EmitSet(this);
                    }

                    if (finallyIndex == -1) {
                        // emit the real return
                        Emit(OpCodes.Leave, _returnBlock.Value.returnStart);
                    } else {
                        // need to leave into the inner most finally block,
                        // the finally block will fall through and check
                        // the return value.
                        if(!_targets[finallyIndex].leaveLabel.HasValue)
                            _targets[finallyIndex].leaveLabel = DefineLabel();

                        EmitInt(CodeGen.BranchForReturn);
                        _targets[finallyIndex].finallyReturns.EmitSet(this);

                        Emit(OpCodes.Leave, _targets[finallyIndex].leaveLabel.Value);
                    }
                    break;
                case TargetBlockType.LoopInFinally:
                case TargetBlockType.Finally: {
                        Targets t = _targets.Peek();
                        EnsureReturnBlock();
                        if (CompilerHelpers.GetReturnType(_methodInfo) != typeof(void)) {
                            _returnBlock.Value.returnValue.EmitSet(this);
                        }
                        // Assert check ensures that those who pushed the block with finallyReturns as null 
                        // should not yield in their blocks.
                        Debug.Assert(t.finallyReturns != null);
                        EmitInt(CodeGen.BranchForReturn);
                        t.finallyReturns.EmitSet(this);
                        Emit(OpCodes.Endfinally);
                        break;
                    }
            }
        }

        public void EmitConvert(Type fromType, Type toType) {
            if (TryEmitCast(fromType, toType, true)) {
                return;
            }

            //TODO this is clearly not the most efficient conversion pattern...
            this.EmitBoxing(fromType);
            this.EmitConvertFromObject(toType);
        }

        public void EmitConvertFromObject(Type toType) {
            if (TryEmitCast(typeof(object), toType, true)) {
                return;
            }

            Binder.EmitConvertFromObject(this, toType);
        }

        public void EmitCast(Type fromType, Type toType) {
          if (!TryEmitCast(fromType, toType, false))
          {
            var ex = new ArgumentException(String.Format("Cannot cast from '{0}' to '{1}'", fromType, toType));
            ex.Data["Who"] = MethodBase.Name;
            throw ex;
          }
        }

        /// <summary>
        /// Tries to emit a cast in a language independent way. If not successful, returns false.
        /// </summary>
        public bool TryEmitCast(Type fromType, Type toType, bool implicitOnly) {
            Contract.RequiresNotNull(fromType, "fromType");
            Contract.RequiresNotNull(toType, "toType");

            if (fromType == toType) {
              if (fromType == typeof(SymbolId))
              {
                EmitUnbox(typeof(SymbolId));
              }
                return true;
            }

            if (toType.IsAssignableFrom(fromType)) {
                // T -> Nullable<T>
                if (toType.IsGenericType && (toType.GetGenericTypeDefinition() == typeof(Nullable<>))) {
                    Type genericArgument = toType.GetGenericArguments()[0];
                    // Cast the type on the top of the stack to the type of the generic argument
                    EmitCast(fromType, genericArgument);
                    EmitNew(toType.GetConstructor(new Type[] { genericArgument }));
                    return true;
                }

                Debug.Assert(!toType.IsValueType, "no type is assignable to a different value type, except for Nullable<T>");
                
                // Value -> object/interface (boxing)
                if (fromType.IsValueType) {
                    if (toType == typeof(object)) {
                      if (fromType != typeof(SymbolId))
                      {
                        EmitBoxing(fromType);
                      }
                        return true;
                    }

                    if (toType.IsInterface) {
                        // If toType is an interface, emit a box, which the JIT will optimize away.
                        // (also EmitBoxing won't work here because it will leave a System.Object on
                        // the stack for Int32 and Boolean, and that can't be converted to an interface)
                        Emit(OpCodes.Box, fromType);
                        return true;
                    }

                    // Box enum values when converting to Enum
                    if (fromType.IsEnum && toType == typeof(Enum)) {
                        Emit(OpCodes.Box, fromType);
                        return true;
                    }


                    
                    
                    // TODO: any other cases where we need to box?
                }

                // Sub -> Superclass, reference -> interface, covariant arrays
                return true;
            }

            if (toType == typeof(void)) {
                Emit(OpCodes.Pop);
                return true;
            }

            // object -> Value (unboxing)
            if (toType.IsValueType && fromType == typeof(object)) {
                if (implicitOnly) return false;
                Emit(OpCodes.Unbox_Any, toType);
                return true;
            }

            if (fromType == typeof(char) && toType == typeof(int))
            {
              return true;
            }

            if (toType.IsValueType != fromType.IsValueType) {
                return false;
            }

            // downcast:
            if (!toType.IsValueType) {
                if (implicitOnly) return false;
                if (!toType.IsVisible) throw new ArgumentException(String.Format(Resources.TypeMustBeVisible, toType.FullName));
                Emit(OpCodes.Castclass, toType);
                return true;
            }

            // integer/enum <-> enum (not assignable)
            toType = (toType.IsEnum) ? Enum.GetUnderlyingType(toType) : toType;
            fromType = (fromType.IsEnum) ? Enum.GetUnderlyingType(fromType) : fromType;

            if (toType == fromType) {
                return true;
            }

            if (TryEmitNumericCast(fromType, toType, implicitOnly)) { 
                return true;
            }

            return false;
        }

        public bool TryEmitNumericCast(Type fromType, Type toType, bool implicitOnly) {
            TypeCode fc = Type.GetTypeCode(fromType);
            TypeCode tc = Type.GetTypeCode(toType);
            int fx, fy, tx, ty;

            if (!TypeUtils.GetNumericConversionOrder(fc, out fx, out fy) || !TypeUtils.GetNumericConversionOrder(tc, out tx, out ty)) {
                // numeric <-> non-numeric
                return false;
            }

            bool isImplicit = TypeUtils.IsImplicitlyConvertible(fx, fy, tx, ty);

            if (implicitOnly && !isImplicit) {
                return false;
            }
            
            // IL conversion instruction also needed for floating point -> integer:
            if (!isImplicit || ty == 2 || tx == 2) {
                switch (tc) {
                    case TypeCode.SByte: Emit(OpCodes.Conv_I1); break;
                    case TypeCode.Int16: Emit(OpCodes.Conv_I2); break;
                    case TypeCode.Int32: Emit(OpCodes.Conv_I4); break;
                    case TypeCode.Int64: Emit(OpCodes.Conv_I8); break;
                    case TypeCode.Byte: Emit(OpCodes.Conv_U1); break;
                    case TypeCode.UInt16: Emit(OpCodes.Conv_U1); break;
                    case TypeCode.UInt32: Emit(OpCodes.Conv_U2); break;
                    case TypeCode.UInt64: Emit(OpCodes.Conv_U4); break;
                    case TypeCode.Single: Emit(OpCodes.Conv_R4); break;
                    case TypeCode.Double: Emit(OpCodes.Conv_R8); break;
                    default: throw Assert.Unreachable;
                }
            }

            return true;
        }

        /// <summary>
        /// Boxes the value of the stack. No-op for reference types. Void is converted to a null reference. For almost all value types this method
        /// will box them in the standard way.  Int32 and Boolean are handled with optimized conversions
        /// that reuse the same object for small values.  For Int32 this is purely a performance
        /// optimization.  For Boolean this is use to ensure that True and False are always the same
        /// objects.
        /// </summary>
        /// <param name="type"></param>
        public void EmitBoxing(Type type) {
            Contract.RequiresNotNull(type, "type");

            if (type.IsValueType) {
                if (type == typeof(void)) {
                    Emit(OpCodes.Ldnull);
                } else if (type == typeof(int)) {
                    EmitCall(typeof(RuntimeHelpers), "Int32ToObject");
                } else if (type == typeof(bool)) {
                    EmitCall(typeof(RuntimeHelpers), "BooleanToObject");
                } else {
                    Emit(OpCodes.Box, type);
                }
            }
        }

        public void EmitReturnValue() {
            EnsureReturnBlock();
            if (CompilerHelpers.GetReturnType(_methodInfo) != typeof(void)) {
                _returnBlock.Value.returnValue.EmitGet(this);
            }
        }

        static SourceSpan GetSpan(Expression expr)
        {
          if (expr != null)
          {
            if (expr.Span.IsValid)
            {
              return expr.Span;
            }

            if (expr is BoundExpression || expr is ConstantExpression)
            {
              return expr.Span;
            }

            var ue = expr as UnaryExpression;

            if (ue != null && ue.NodeType == AstNodeType.Convert)
            {
              return GetSpan(ue.Operand);
            }
          }

          return SourceSpan.Invalid;
        }

        public void EmitReturn(Expression expr) {
            if (_yieldLabels != null) {
                EmitReturnInGenerator(expr);
            } else {
                if (expr == null) {
                    EmitNull();
                    if (ScriptDomainManager.Options.LightweightDebugging)
                    {
                      EmitConstant(Node.SpanToLong(GetSpan(expr)));
                      EmitCall(Debugging.DebugMethods.ProcedureExit);
                    }
                    EmitReturnFromObject();
                } else {
                    if (ScriptDomainManager.Options.DebugMode)
                    {
                      if (!skipreturn)
                      {
                        var mce = expr as MethodCallExpression;
                        if (mce == null || !mce.TailCall)
                        {
                          var s = GetSpan(expr);
                          if (s.IsValid)
                          {
                            EmitPosition(s.Start, s.End);
                          }
                        }
                      }
                    }
                    expr.EmitAs(this, CompilerHelpers.GetReturnType(_methodInfo));
                    if (!skipreturn)
                    {
                      if (ScriptDomainManager.Options.LightweightDebugging)
                      {
                        var mce = expr as MethodCallExpression;
                        if (mce == null || !mce.TailCall)
                        {

                          EmitConstant(Node.SpanToLong(GetSpan(expr)));
                          EmitCall(Debugging.DebugMethods.ProcedureExit);
                        }
                      }
                      EmitReturn();
                    }
                    skipreturn = false;
                }
            }
        }

        internal bool skipreturn = false;

        public void EmitReturnFromObject() {
            EmitConvertFromObject(CompilerHelpers.GetReturnType(_methodInfo));
            EmitReturn();
        }

        public void EmitReturnInGenerator(Expression expr) {
            EmitSetGeneratorReturnValue(expr);

            EmitInt(0);
            EmitReturn();
        }

        internal void EmitYield(Expression expr, YieldTarget target) {
            Contract.RequiresNotNull(expr, "expr");

            EmitSetGeneratorReturnValue(expr);
            EmitUpdateGeneratorLocation(target.Index);

            // Mark that we are yielding, which will ensure we skip
            // all of the finally bodies that are on the way to exit

            EmitInt(GotoRouterYielding);
            GotoRouter.EmitSet(this);

            EmitInt(1);
            EmitReturn();

            MarkLabel(target.EnsureLabel(this));
            // Reached the routing destination, set router to GotoRouterNone
            EmitInt(GotoRouterNone);
            GotoRouter.EmitSet(this);
        }

        private void EmitSetGeneratorReturnValue(Expression expr) {
            ArgumentSlots[1].EmitGet(this);
            EmitExprAsObjectOrNull(expr);
            Emit(OpCodes.Stind_Ref);
        }

        public void EmitUpdateGeneratorLocation(int index) {
            ArgumentSlots[0].EmitGet(this);
            EmitInt(index);
            EmitFieldSet(typeof(Generator).GetField("location"));
        }

        public void EmitGetGeneratorLocation() {
            ArgumentSlots[0].EmitGet(this);
            EmitFieldGet(typeof(Generator), "location");
        }

        public void EmitUninitialized() {            
            EmitFieldGet(typeof(Uninitialized), "Instance");
        }

        internal SourceSpan lambdaspan;

        public void EmitPosition(SourceLocation start, SourceLocation end) {
            if (EmitDebugInfo) {

                Debug.Assert(start != SourceLocation.Invalid);
                Debug.Assert(end != SourceLocation.Invalid);

                if (start == SourceLocation.None || end == SourceLocation.None) {
                    return;
                }

                if (start == lambdaspan.Start && end == lambdaspan.End)
                {
                  return;
                }

                Debug.Assert(start.Line > 0 && end.Line > 0);

                MarkSequencePoint(
                    _debugSymbolWriter,
                    start.Line, start.Column,
                    end.Line, end.Column
                    );
            }
        }

        public void EmitSequencePointNone() {
          if (EmitDebugInfo)
          {
            MarkSequencePoint(
                _debugSymbolWriter,
                SourceLocation.None.Line, SourceLocation.None.Column,
                SourceLocation.None.Line, SourceLocation.None.Column
                );
            Emit(OpCodes.Nop);
          }
        }

        public Slot GetLocalTmp(Type type) {
            Contract.RequiresNotNull(type, "type");

            for (int i = 0; i < _freeSlots.Count; i++) {
                Slot slot = _freeSlots[i];
                if (slot.Type == type) {
                    _freeSlots.RemoveAt(i);
                    return slot;
                }
            }

            return new LocalSlot(DeclareLocal(type), this);
        }

        public Slot GetNamedLocal(Type type, string name) {
            Contract.RequiresNotNull(type, "type");
            Contract.RequiresNotNull(name, "name");

            LocalBuilder lb = DeclareLocal(type);
            if (EmitDebugInfo) lb.SetLocalSymInfo(name);
            return new LocalSlot(lb, this);
        }

        internal void FreeLocalTmp(Slot slot) {
            if (slot != null) {
                Debug.Assert(!_freeSlots.Contains(slot));
                _freeSlots.Add(slot);
            }
        }

        internal Slot DupAndStoreInTemp(Type type) {
            Debug.Assert(type != typeof(void));
            this.Emit(OpCodes.Dup);
            Slot ret = GetLocalTmp(type);
            ret.EmitSet(this);
            return ret;
        }

        internal ScopeAllocator Allocator {
            get {
                Debug.Assert(_allocator != null);
                return _allocator;
            }
            set { _allocator = value; }
        }

        public bool HasAllocator {
            get {
                return _allocator != null;
            }
        }

        /// <summary>
        /// The slot used for the current frames environment.  If this method defines has one or more closure functions
        /// defined within it then the environment will contain all of the variables that the closure variables lifted
        /// into the environment.
        /// </summary>
        public EnvironmentSlot EnvironmentSlot {
            get {
                Debug.Assert(_environmentSlot != null);
                return _environmentSlot;
            }
            set { _environmentSlot = value; }
        }

        public Slot ContextSlot {
            get { return _contextSlot; }
            set {
                //Debug.Assert(_contextSlot == null); // shouldn't change after creation

                Contract.RequiresNotNull(value, "value");
                if (!typeof(CodeContext).IsAssignableFrom(value.Type))
                    throw new ArgumentException("ContextSlot must be assignable from CodeContext", "value");

                _contextSlot = value; 
            }
        }

        public Slot ParamsSlot {
            get { return _paramsSlot; }
            set { _paramsSlot = value; }
        }

        [Conditional("DEBUG")]
        public void EmitDebugMarker(string marker) {
            EmitString(marker);
            EmitCall(typeof(Debug), "WriteLine", new Type[] { typeof(string) });
        }

        [Conditional("DEBUG")]
        public void EmitAssertNotNull() {
            EmitAssertNotNull("Accessing null reference.");
        }

        /// <summary>
        /// asserts the value at the top of the stack is not null
        /// </summary>
        [Conditional("DEBUG")]
        public void EmitAssertNotNull(string message) {
            Emit(OpCodes.Dup);
            Emit(OpCodes.Ldnull);
            Emit(OpCodes.Ceq);
            Emit(OpCodes.Ldc_I4_0);
            Emit(OpCodes.Ceq);

            if (message == null) {
                EmitCall(typeof(Debug), "Assert", new Type[] { typeof(bool) });
            } else {
                EmitString(message);
                EmitCall(typeof(Debug), "Assert", new Type[] { typeof(bool), typeof(string) });
            }
        }

        public void SetCustomAttribute(CustomAttributeBuilder cab) {
            MethodBuilder builder = _methodInfo as MethodBuilder;
            if (builder != null) {
                builder.SetCustomAttribute(cab);
            }
        }

        public ParameterBuilder DefineParameter(int position, ParameterAttributes attributes, string strParamName) {
            MethodBuilder builder = _methodInfo as MethodBuilder;
            if (builder != null) {
                return builder.DefineParameter(position, attributes, strParamName);
            }
            ConstructorBuilder cb = _methodInfo as ConstructorBuilder;
            if (cb != null)
            {
              return cb.DefineParameter(position, attributes, strParamName);
            }
            DynamicMethod dm = _methodInfo as DynamicMethod;
            if (dm != null) {
                return dm.DefineParameter(position, attributes, strParamName);
            }

            throw new InvalidOperationException(Resources.InvalidOperation_DefineParameterBakedMethod);
        }

        public void EmitGet(Slot slot, SymbolId name, bool check) {
            Contract.RequiresNotNull(slot, "slot");

            slot.EmitGet(this);
            if (check) {
                slot.EmitCheck(this, name);
            }
        }

        public virtual void EmitGetCurrentLine() {
            if (_currentLineSlot != null) {
                _currentLineSlot.EmitGet(this);
            } else {
                EmitInt(0);
            }
        }

        public virtual void EmitCurrentLine(int line) {
            if (!EmitLineInfo || !HasContext) return;

            line = _context.SourceUnit.MapLine(line);
            if (line != _currentLine && line != SourceLocation.None.Line) {
                if (_currentLineSlot == null) {
                    _currentLineSlot = GetNamedLocal(typeof(int), "$line");
                }

                EmitInt(_currentLine = line);
                _currentLineSlot.EmitSet(this);
            }
        }

        private void EnsureReturnBlock() {
            if (!_returnBlock.HasValue) {
                ReturnBlock val = new ReturnBlock();

                if (CompilerHelpers.GetReturnType(_methodInfo) != typeof(void)) {
                    val.returnValue = GetNamedLocal(CompilerHelpers.GetReturnType(_methodInfo), "retval");
                }
                val.returnStart = DefineLabel();

                _returnBlock = val;
            }
        }

        public void Finish() {
            // not sure if this check is needed
            //Debug.Assert(_targets.Count == 0);

            if (_returnBlock.HasValue) {
                MarkLabel(_returnBlock.Value.returnStart);
                if (CompilerHelpers.GetReturnType(_methodInfo) != typeof(void))
                    _returnBlock.Value.returnValue.EmitGet(this);
                Emit(OpCodes.Ret);
            }

            if (_methodToOverride != null) {
                _typeGen.TypeBuilder.DefineMethodOverride(this.MethodInfo, _methodToOverride);
            }

            if (DynamicMethod) {
                this.CreateDelegateMethodInfo();
            }
        }

        public void EmitCodeContext() {
            if (ContextSlot == null) throw new InvalidOperationException("ContextSlot not available.");

            ContextSlot.EmitGet(this);
        }

        public void EmitLanguageContext() {
            EmitCodeContext();
            EmitPropertyGet(typeof(CodeContext), "LanguageContext");
        }

        public void EmitEnvironmentOrNull() {
            if (_environmentSlot != null) {
                _environmentSlot.EmitGet(this);
            } else {
                EmitNull();
            }
        }

        public void EmitThis() {
            if (_methodInfo.IsStatic) throw new InvalidOperationException(Resources.InvalidOperation_ThisInStaticMethod);
            //!!! want to confirm this doesn't have a constant pool too
            Emit(OpCodes.Ldarg_0);
        }

        public void EmitExprAsObjectOrNull(Expression e) {
            if (e == null) {
                Emit(OpCodes.Ldnull);
            } else {
                e.EmitAsObject(this);
            }
        }

        /// <summary>
        /// Emits an array of constant values provided in the given list.  The array
        /// is strongly typed.
        /// </summary>
        public void EmitArray<T>(IList<T> items) {
            Contract.RequiresNotNull(items, "items");

            EmitInt(items.Count);
            Emit(OpCodes.Newarr, typeof(T));
            for (int i = 0; i < items.Count; i++) {
                Emit(OpCodes.Dup);
                EmitInt(i);
                EmitConstant(items[i]);
                EmitStoreElement(typeof(T));
            }
        }

        public void EmitTuple(Type tupleType, int count, EmitArrayHelper emit) {
            EmitTuple(tupleType, 0, count, emit);
        }

        private void EmitTuple(Type tupleType, int start, int end, EmitArrayHelper emit) {
            int size = end - start;

            if (size > Tuple.MaxSize) {
                int multiplier = 1;
                while (size > Tuple.MaxSize) {
                    size = (size + Tuple.MaxSize - 1) / Tuple.MaxSize;
                    multiplier *= Tuple.MaxSize;
                }
                for (int i = 0; i < size; i++) {
                    int newStart = start + (i * multiplier);
                    int newEnd = System.Math.Min(end, start + ((i + 1) * multiplier));

                    PropertyInfo pi = tupleType.GetProperty("Item" + String.Format("{0:D3}", i));
                    Debug.Assert(pi != null);
                    EmitTuple(pi.PropertyType, newStart, newEnd, emit);
                }
            } else {
                for (int i = start; i < end; i++) {
                    emit(i);
                }
            }

            // fill in emptys with null.
            Type[] genArgs = tupleType.GetGenericArguments();
            for (int i = size; i < genArgs.Length; i++) {
                EmitNull();
            }

            EmitTupleNew(tupleType);
        }

        private void EmitTupleNew(Type tupleType) {
            ConstructorInfo[] cis = tupleType.GetConstructors();
            foreach (ConstructorInfo ci in cis) {
                if (ci.GetParameters().Length != 0) {
                    EmitNew(ci);
                    break;
                }
            }
        }


        /// <summary>
        /// Emits an array of values of count size.  The items are emitted via the callback
        /// which is provided with the current item index to emit.
        /// </summary>
        public void EmitArray(Type elementType, int count, EmitArrayHelper emit) {
            Contract.RequiresNotNull(elementType, "elementType");
            Contract.RequiresNotNull(emit, "emit");
            Contract.Requires(count >= 0, "count", "Count must be non-negative.");
            
            EmitInt(count);
            Emit(OpCodes.Newarr, elementType);
            for (int i = 0; i < count; i++) {
                Emit(OpCodes.Dup);
                EmitInt(i);
                
                emit(i);

                EmitStoreElement(elementType);
            }
        }

        public void EmitTrueArgGet(int index) {
            Contract.Requires(index >= 0, "index");

            switch (index) {
                case 0: this.Emit(OpCodes.Ldarg_0); break;
                case 1: this.Emit(OpCodes.Ldarg_1); break;
                case 2: this.Emit(OpCodes.Ldarg_2); break;
                case 3: this.Emit(OpCodes.Ldarg_3); break;
                default:
                    if (index <= Byte.MaxValue) {
                        Emit(OpCodes.Ldarg_S, index);
                    } else {
                        this.Emit(OpCodes.Ldarg, index);
                    }
                    break;
            }
        }

        public void EmitArgGet(int index) {
            Contract.Requires(index >= 0 && index < Int32.MaxValue, "index");

            if (_methodInfo == null || !_methodInfo.IsStatic) {
                // making room for this
                index++;
            }

            EmitTrueArgGet(index);
        }

        public void EmitArgAddr(int index) {
            Contract.Requires(index >= 0, "index");

            if (index <= Byte.MaxValue) {
                Emit(OpCodes.Ldarga_S, index);
            } else {
                this.Emit(OpCodes.Ldarga, index);
            }
        }

        /// <summary>
        /// Emits a symbol id.  
        /// </summary>
        public virtual void EmitSymbolId(SymbolId id) {
            if (DynamicMethod) {
                EmitInt(id.Id);
                EmitNew(typeof(SymbolId), new Type[] { typeof(int) });
            } else {
                //TODO - This code is Python-centric, investigate perf issues
                //around removing it and consider re-adding a generic version
                //FieldInfo fi = Symbols.GetFieldInfo(id);
                //if (fi != null) {
                //    Emit(OpCodes.Ldsfld, fi);
                //} else {
                _typeGen.EmitIndirectedSymbol(this, id);
            }
        }

        public void EmitSymbolIdId(SymbolId id) {
            if (DynamicMethod) {
                EmitInt(id.Id);
            } else {
                EmitSymbolId(id);
                Slot slot = GetLocalTmp(typeof(SymbolId));
                slot.EmitSet(this);
                slot.EmitGetAddr(this);
                EmitPropertyGet(typeof(SymbolId), "Id");
                FreeLocalTmp(slot);
            }
        }

        public void EmitPropertyGet(Type type, string name) {
            Contract.RequiresNotNull(type, "type");
            Contract.RequiresNotNull(name, "name");

            EmitPropertyGet(type.GetProperty(name));
        }

        public void EmitPropertyGet(PropertyInfo pi) {
            Contract.RequiresNotNull(pi, "pi");

            if (!pi.CanRead) throw new InvalidOperationException(Resources.CantReadProperty);

            EmitCall(pi.GetGetMethod());
        }

        public void EmitPropertySet(Type type, string name) {
            Contract.RequiresNotNull(type, "type");
            Contract.RequiresNotNull(name, "name");

            EmitPropertySet(type.GetProperty(name));
        }

        public void EmitPropertySet(PropertyInfo pi) {
            Contract.RequiresNotNull(pi, "pi");

            if (!pi.CanRead) throw new InvalidOperationException(Resources.CantWriteProperty);

            EmitCall(pi.GetSetMethod());
        }

        public void EmitFieldAddress(FieldInfo fi) {
            Contract.RequiresNotNull(fi, "fi");

            if (fi.IsStatic) {
                Emit(OpCodes.Ldsflda, fi);
            } else {
                Emit(OpCodes.Ldflda, fi);
            }
        }

        public void EmitFieldGet(Type type, String name) {
            Contract.RequiresNotNull(type, "type");
            Contract.RequiresNotNull(name, "name");

            EmitFieldGet(type.GetField(name));
        }

        public void EmitFieldGet(FieldInfo fi) {
            Contract.RequiresNotNull(fi, "fi");

            if (fi.IsStatic) {
                Emit(OpCodes.Ldsfld, fi);
            } else {
                Emit(OpCodes.Ldfld, fi);
            }
        }

        public void EmitFieldSet(FieldInfo fi) {
            Contract.RequiresNotNull(fi, "fi");

            if (fi.IsStatic) {
                Emit(OpCodes.Stsfld, fi);
            } else {
                Emit(OpCodes.Stfld, fi);
            }
        }

        public void EmitNew(ConstructorInfo ci) {
            Contract.RequiresNotNull(ci, "ci");

            if (ci.DeclaringType.ContainsGenericParameters) {
                EmitIllegalNew(ci);
            }
            Emit(OpCodes.Newobj, ci);
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1011:ConsiderPassingBaseTypesAsParameters")]
        public void EmitIllegalNew(ConstructorInfo ci) {
            Contract.RequiresNotNull(ci, "ci");

            //TODO Python would like a 'TypeError' == 'ArgumentTypeException' here
            throw new ArgumentException(String.Format(CultureInfo.CurrentCulture, Resources.IllegalNew_GenericParams, ci.DeclaringType));
        }

        public void EmitNew(Type type, Type[] paramTypes) {
            Contract.RequiresNotNull(type, "type");
            Contract.RequiresNotNull(paramTypes, "paramTypes");

            EmitNew(type.GetConstructor(paramTypes));
        }

        public void EmitCall(MethodInfo mi) {
            Contract.RequiresNotNull(mi, "mi");

            if (mi.IsVirtual && !mi.DeclaringType.IsValueType /*&& !mi.DeclaringType.IsInterface*/) {
                Emit(OpCodes.Callvirt, mi);
            } else {
                Emit(OpCodes.Call, mi);
            }
        }

      public void EmitCall(MethodInfo mi, bool tailcall)
      {
        Contract.RequiresNotNull(mi, "mi");

        if (tailcall && ((mi is MethodBuilder) || !Attribute.IsDefined(mi, typeof(IronScheme.Runtime.NonRecursiveAttribute))))
        {
          Emit(OpCodes.Tailcall);
        }

        EmitCall(mi);
      }

      public void EmitCall(Type type, String name, bool tailcall)
      {
        Contract.RequiresNotNull(type, "type");
        Contract.RequiresNotNull(name, "name");
        if (!type.IsVisible) throw new ArgumentException(String.Format(Resources.TypeMustBeVisible, type.FullName));

        EmitCall(type.GetMethod(name), tailcall);
      }


        public void EmitCall(Type type, String name) {
            Contract.RequiresNotNull(type, "type");
            Contract.RequiresNotNull(name, "name");
            if (!type.IsVisible) throw new ArgumentException(String.Format(Resources.TypeMustBeVisible, type.FullName));

            EmitCall(type.GetMethod(name));
        }

        public void EmitCall(Type type, String name, Type[] paramTypes) {
            Contract.RequiresNotNull(type, "type");
            Contract.RequiresNotNull(name, "name");
            Contract.RequiresNotNull(paramTypes, "paramTypes");

            EmitCall(type.GetMethod(name, paramTypes));
        }

        public void EmitName(SymbolId name) {
            if (name == SymbolId.Empty) throw new ArgumentException(Resources.EmptySymbolId, "name");

            EmitString(SymbolTable.IdToString(name));
        }

        public void EmitType(Type type) {
            Contract.RequiresNotNull(type, "type");            
            if (!(type is TypeBuilder) && !type.IsGenericParameter && !type.IsVisible) {
                // can't ldtoken on a non-visible type, refer to it via a runtime constant...
                EmitConstant(new RuntimeConstant(type));
                return;
            }

            Emit(OpCodes.Ldtoken, type);
            EmitCall(typeof(Type), "GetTypeFromHandle");
        }

        // Not to be used with virtual methods
        public void EmitDelegateConstruction(CodeGen delegateFunction, Type delegateType, bool hasContext) {            
            Contract.RequiresNotNull(delegateFunction, "delegateFunction");
            Contract.RequiresNotNull(delegateType, "delegateType");

            if (delegateFunction.MethodInfo is DynamicMethod || delegateFunction.ConstantPool.IsBound) {
                Delegate d = delegateFunction.CreateDelegate(delegateType);
                this.ConstantPool.AddData(d).EmitGet(this);
            }
            else if (hasContext)
            {

              Emit(OpCodes.Ldftn, delegateFunction.MethodInfo);
              Emit(OpCodes.Newobj, (ConstructorInfo)(delegateType.GetMember(".ctor")[0]));
            } 
            else 
            {
              if (delegateFunction.MethodInfo.IsPublic)
              {
                EmitNull();
                Emit(OpCodes.Ldftn, delegateFunction.MethodInfo);
                Emit(OpCodes.Newobj, (ConstructorInfo)(delegateType.GetMember(".ctor")[0]));
              }
              else
              {
                var cache = TypeGen.AddStaticField(delegateType, FieldAttributes.Private, "$p$" + delegateFunction.MethodInfo.Name);
                TypeGen.TypeInitializer.EmitNull();
                TypeGen.TypeInitializer.Emit(OpCodes.Ldftn, delegateFunction.MethodInfo);
                TypeGen.TypeInitializer.Emit(OpCodes.Newobj, (ConstructorInfo)(delegateType.GetMember(".ctor")[0]));
                cache.EmitSet(TypeGen.TypeInitializer);
                cache.EmitGet(this);
              }
            }
        }

        /// <summary>
        /// Emits a Ldind* instruction for the appropriate type
        /// </summary>
        public void EmitLoadValueIndirect(Type type) {
            Contract.RequiresNotNull(type, "type");

            if (type.IsValueType) {
                if (type == typeof(int)) Emit(OpCodes.Ldind_I4);
                else if (type == typeof(uint)) Emit(OpCodes.Ldind_U4);
                else if (type == typeof(short)) Emit(OpCodes.Ldind_I2);
                else if (type == typeof(ushort)) Emit(OpCodes.Ldind_U2);
                else if (type == typeof(long) || type == typeof(ulong)) Emit(OpCodes.Ldind_I8);
                else if (type == typeof(char)) Emit(OpCodes.Ldind_I2);
                else if (type == typeof(bool)) Emit(OpCodes.Ldind_I1);
                else if (type == typeof(float)) Emit(OpCodes.Ldind_R4);
                else if (type == typeof(double)) Emit(OpCodes.Ldind_R8);
                else Emit(OpCodes.Ldobj, type);
            } else {
                Emit(OpCodes.Ldind_Ref);
            }

        }

        /// <summary>
        /// Emits a Stind* instruction for the appropriate type.
        /// </summary>
        public void EmitStoreValueIndirect(Type type) {
            Contract.RequiresNotNull(type, "type");

            if (type.IsValueType) {
                if (type == typeof(int)) Emit(OpCodes.Stind_I4);
                else if (type == typeof(short)) Emit(OpCodes.Stind_I2);
                else if (type == typeof(long) || type == typeof(ulong)) Emit(OpCodes.Stind_I8);
                else if (type == typeof(char)) Emit(OpCodes.Stind_I2);
                else if (type == typeof(bool)) Emit(OpCodes.Stind_I1);
                else if (type == typeof(float)) Emit(OpCodes.Stind_R4);
                else if (type == typeof(double)) Emit(OpCodes.Stind_R8);
                else Emit(OpCodes.Stobj, type);
            } else {
                Emit(OpCodes.Stind_Ref);
            }

        }

        /// <summary>
        /// Emits the Ldelem* instruction for the appropriate type
        /// </summary>
        /// <param name="type"></param>
        public void EmitLoadElement(Type type) {
            Contract.RequiresNotNull(type, "type");

            if (type.IsValueType) {
                if (type == typeof(System.SByte)) {
                    Emit(OpCodes.Ldelem_I1);
                } else if (type == typeof(System.Int16)) {
                    Emit(OpCodes.Ldelem_I2);
                } else if (type == typeof(System.Int32)) {
                    Emit(OpCodes.Ldelem_I4);
                } else if (type == typeof(System.Int64)) {
                    Emit(OpCodes.Ldelem_I8);
                } else if (type == typeof(System.Single)) {
                    Emit(OpCodes.Ldelem_R4);
                } else if (type == typeof(System.Double)) {
                    Emit(OpCodes.Ldelem_R8);
                } else if (type == typeof(System.Byte)) {
                    Emit(OpCodes.Ldelem_U1);
                } else if (type == typeof(System.UInt16)) {
                    Emit(OpCodes.Ldelem_U2);
                } else if (type == typeof(System.UInt32)) {
                    Emit(OpCodes.Ldelem_U4);
                } else {
                    Emit(OpCodes.Ldelem, type);
                }
            } else {
                Emit(OpCodes.Ldelem_Ref);
            }
        }

        /// <summary>
        /// Emits a Stelem* instruction for the appropriate type.
        /// </summary>
        public void EmitStoreElement(Type type) {
            Contract.RequiresNotNull(type, "type");

            if (type.IsValueType) {
                if (type == typeof(int) || type == typeof(uint)) {
                    Emit(OpCodes.Stelem_I4);
                } else if (type == typeof(short) || type == typeof(ushort)) {
                    Emit(OpCodes.Stelem_I2);
                } else if (type == typeof(long) || type == typeof(ulong)) {
                    Emit(OpCodes.Stelem_I8);
                } else if (type == typeof(char)) {
                    Emit(OpCodes.Stelem_I2);
                } else if (type == typeof(bool)) {
                    Emit(OpCodes.Stelem_I4);
                } else if (type == typeof(float)) {
                    Emit(OpCodes.Stelem_R4);
                } else if (type == typeof(double)) {
                    Emit(OpCodes.Stelem_R8);
                } else {
                    Emit(OpCodes.Stelem, type);
                }
            } else {
                Emit(OpCodes.Stelem_Ref);
            }
        }

        public void EmitNull() {
            Emit(OpCodes.Ldnull);
        }

        public void EmitString(string value) {
            Contract.RequiresNotNull(value, "value");

            Emit(OpCodes.Ldstr, (string)value);
        }

        #region Support for emitting constants

        public void EmitBoolean(bool value) {
            if (value) {
                Emit(OpCodes.Ldc_I4_1);
            } else {
                Emit(OpCodes.Ldc_I4_0);
            }
        }

        public void EmitChar(char value) {
            EmitInt(value);
            Emit(OpCodes.Conv_U2);
        }

        public void EmitByte(byte value) {
            EmitInt(value);
            Emit(OpCodes.Conv_U1);
        }

        private void EmitSByte(sbyte value) {
            EmitInt(value);
            Emit(OpCodes.Conv_I1);
        }

        private void EmitShort(short value) {
            EmitInt(value);
            Emit(OpCodes.Conv_I2);
        }

        private void EmitUShort(ushort value) {
            EmitInt(value);
            Emit(OpCodes.Conv_U2);
        }

        public void EmitInt(int value) {
            OpCode c;
            switch (value) {
                case -1: c = OpCodes.Ldc_I4_M1; break;
                case 0: c = OpCodes.Ldc_I4_0; break;
                case 1: c = OpCodes.Ldc_I4_1; break;
                case 2: c = OpCodes.Ldc_I4_2; break;
                case 3: c = OpCodes.Ldc_I4_3; break;
                case 4: c = OpCodes.Ldc_I4_4; break;
                case 5: c = OpCodes.Ldc_I4_5; break;
                case 6: c = OpCodes.Ldc_I4_6; break;
                case 7: c = OpCodes.Ldc_I4_7; break;
                case 8: c = OpCodes.Ldc_I4_8; break;
                default:
                    if (value >= -128 && value <= 127) {
                        Emit(OpCodes.Ldc_I4_S, (byte)value);
                    } else {
                        Emit(OpCodes.Ldc_I4, value);
                    }
                    return;
            }
            Emit(c);
        }

        [CLSCompliant(false)]
        public void EmitUInt(uint i) {
            EmitInt((int)i);
            Emit(OpCodes.Conv_U4);
        }

        public void EmitLong(long value) {
            Emit(OpCodes.Ldc_I8, value);
        }

        private void EmitULong(ulong value) {
            Emit(OpCodes.Ldc_I8, (long)value);
            Emit(OpCodes.Conv_U8);
        }

        private void EmitDouble(double value) {
            Emit(OpCodes.Ldc_R8, value);
        }

        private void EmitSingle(float value) {
            Emit(OpCodes.Ldc_R4, value);
        }

        private void EmitEnum(object value) {
            Debug.Assert(value != null);
            Debug.Assert(value.GetType().IsEnum);

            switch (((Enum)value).GetTypeCode()) {
                case TypeCode.Int32:
                    EmitInt((int)value);
                    break;
                case TypeCode.Int64:
                    EmitLong((long)value);
                    break;
                case TypeCode.Int16:
                    EmitShort((short)value);
                    break;
                case TypeCode.UInt32:
                    EmitUInt((uint)value);
                    break;
                case TypeCode.UInt64:
                    EmitULong((ulong)value);
                    break;
                case TypeCode.SByte:
                    EmitSByte((sbyte)value);
                    break;
                case TypeCode.UInt16:
                    EmitUShort((ushort)value);
                    break;
                case TypeCode.Byte:
                    EmitByte((byte)value);
                    break;
                default:
                    throw new NotImplementedException(String.Format(CultureInfo.CurrentCulture, Resources.NotImplemented_EnumEmit, value.GetType(), value));
            }
        }

        private void EmitComplex(Complex64 value) {
            if (value.Real != 0.0) {
                Emit(OpCodes.Ldc_R8, value.Real);
                if (value.Imag != 0.0) {
                    Emit(OpCodes.Ldc_R8, value.Imag);
                    EmitCall(typeof(Complex64), "Make");
                } else {
                    EmitCall(typeof(Complex64), "MakeReal");
                }
            } else {
                Emit(OpCodes.Ldc_R8, value.Imag);
                EmitCall(typeof(Complex64), "MakeImaginary");
            }
        }

        private void EmitBigInteger(BigInteger value) {
            int ival;
            if (value.AsInt32(out ival)) {
                EmitInt(ival);
                EmitNew(typeof(BigInteger), new Type[] { typeof(int) });
                return;
            }
            long lval;
            if (value.AsInt64(out lval)) {
                Emit(OpCodes.Ldc_I8, lval);
                EmitNew(typeof(BigInteger), new Type[] { typeof(long) });
                return;
            }
            EmitMetadataArray(value.GetBits());
            EmitBoolean(value.Negative);
            EmitNew(typeof(BigInteger), new Type[] { typeof(uint[]), typeof(bool) });
            return;
        }

        private void EmitMetadataArray(uint[] p)
        {
          var size = p.Length * 4;
          byte[] data = new byte[size];
          Buffer.BlockCopy(p, 0, data, 0, size);
          var fb = this._typeGen.TypeBuilder.DefineInitializedData(Guid.NewGuid().ToString(), data, FieldAttributes.Static);
          EmitInt(p.Length);
          Emit(OpCodes.Newarr, typeof(uint));
          Emit(OpCodes.Dup);
          Emit(OpCodes.Ldtoken, fb);
          EmitCall(typeof(System.Runtime.CompilerServices.RuntimeHelpers).GetMethod("InitializeArray"));
        }

        #endregion

        /// <summary>
        /// The main entry to the constant emitting.
        /// This will handle constant caching and compiler constants.
        /// Constants will be left on the execution stack as their direct type.
        /// </summary>
        /// <param name="value">Constant to be emitted</param>
        public void EmitConstant(object value) {
            CompilerConstant cc = value as CompilerConstant;

            if (cc != null) {
                if (CacheConstants) {
                    EmitCompilerConstantCache(cc);
                } else {
                    EmitCompilerConstantNoCache(cc);
                }
            } else {
                if (CacheConstants) {
                    EmitConstantCache(value);
                } else {
                    EmitConstantNoCache(value);
                }
            }
        }

        private void EmitConstantCache(object value) {
            Debug.Assert(!(value is CompilerConstant));

            string strVal;
            if (value == null) {
                EmitNull();
            } else if ((strVal = value as string) != null) {
                EmitString(strVal);
            } else {
                Slot s = _typeGen.GetOrMakeConstant(value);
                s.EmitGet(this);
            }
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Maintainability", "CA1502:AvoidExcessiveComplexity")]
        internal void EmitConstantNoCache(object value) {
            Debug.Assert(!(value is CompilerConstant));

            string strVal;
            BigInteger bi;
            string[] sa;
            MethodInfo methodInfo;
            Type type;

            if (value == null) {
                EmitNull();
            } else if (value is int) {
                EmitInt((int)value);
            } else if (value is double) {
                EmitDouble((double)value);
            } else if (value is float) {
                EmitSingle((float)value);
            } else if (value is long) {
                EmitLong((long)value);
            } else if (value is Complex64) {
                EmitComplex((Complex64)value);
            } else if (!Object.ReferenceEquals((bi = value as BigInteger), null)) {
                EmitBigInteger(bi);
            } else if ((strVal = value as string) != null) {
                EmitString(strVal);
            } else if (value is bool) {
                EmitBoolean((bool)value);
            } else if ((sa = value as string[]) != null) {
                EmitArray(sa);
            } else if (value is Missing) {
                Emit(OpCodes.Ldsfld, typeof(Missing).GetField("Value"));
            } else if (value.GetType().IsEnum) {
                EmitEnum(value);
            } else if (value is uint) {
                EmitUInt((uint)value);
            } else if (value is char) {
                EmitChar((char)value);
            } else if (value is byte) {
                EmitByte((byte)value);
            } else if (value is sbyte) {
                EmitSByte((sbyte)value);
            } else if (value is short) {
                EmitShort((short)value);
            } else if (value is ushort) {
                EmitUShort((ushort)value);
            } else if (value is ulong) {
                EmitULong((ulong)value);
            } else if (value is SymbolId) {
                EmitSymbolId((SymbolId)value);
            } else if ((type = value as Type) != null) {
                EmitType(type);
            } else if (value is RuntimeTypeHandle) {
                RuntimeTypeHandle rth = (RuntimeTypeHandle)value;
                if (!rth.Equals(default(RuntimeTypeHandle))) {
                    Emit(OpCodes.Ldtoken, Type.GetTypeFromHandle((RuntimeTypeHandle)value));
                } else {
                    EmitConstant(new RuntimeConstant(value));
                }
            } else if ((methodInfo = value as MethodInfo) != null) {
                Emit(OpCodes.Ldtoken, methodInfo);
                EmitCall(typeof(MethodBase).GetMethod("GetMethodFromHandle", new Type[] { typeof(RuntimeMethodHandle) }));
            } else if (value is RuntimeMethodHandle) {
                RuntimeMethodHandle rmh = (RuntimeMethodHandle)value;
                if (rmh != default(RuntimeMethodHandle)) {
                    Emit(OpCodes.Ldtoken, (MethodInfo)MethodBase.GetMethodFromHandle((RuntimeMethodHandle)value));
                } else {
                    EmitConstant(new RuntimeConstant(value));
                }
            } else {
                EmitConstant(new RuntimeConstant(value));
            }
        }

        private void EmitCompilerConstantCache(CompilerConstant value) {
            Debug.Assert(value != null);
            Debug.Assert(_typeGen != null);
            _typeGen.GetOrMakeCompilerConstant(value).EmitGet(this);
        }

        private void EmitCompilerConstantNoCache(CompilerConstant value) {
            Debug.Assert(value != null);
            if (ConstantPool.IsBound) {
                //TODO cache these so that we use the same slot for the same values
                _constantPool.AddData(value.Create(), value.Type).EmitGet(this);
            } else {
                value.EmitCreation(this);
            }
        }

        public void EmitUnbox(Type type) {
            Contract.RequiresNotNull(type, "type");

            Emit(OpCodes.Unbox_Any, type);
        }

        public void EmitMissingValue(Type type) {
            CodeGen cg = this;
            switch (Type.GetTypeCode(type)) {
                default:
                case TypeCode.Object:
                    // struct
                    if (type.IsSealed && type.IsValueType && !type.IsEnum) {
                        Slot s = cg.GetLocalTmp(type);
                        s.EmitGetAddr(cg);
                        cg.Emit(OpCodes.Initobj, type);
                        s.EmitGet(cg);
                    } else if (type == typeof(object)) {
                        // parameter of type object receives the actual Missing value
                        cg.Emit(OpCodes.Ldsfld, typeof(Missing).GetField("Value"));
                    } else if (!type.IsValueType) {
                        cg.Emit(OpCodes.Ldnull);
                    } else {
                        EmitTypeError("Cannot create default value for type {0}", type);
                    }
                    break;

                case TypeCode.Empty:
                case TypeCode.DBNull:
                    cg.Emit(OpCodes.Ldnull);
                    break;

                case TypeCode.Boolean:
                case TypeCode.Char:
                case TypeCode.SByte:
                case TypeCode.Byte:
                case TypeCode.Int16:
                case TypeCode.UInt16:
                case TypeCode.Int32:
                case TypeCode.UInt32:
                    cg.Emit(OpCodes.Ldc_I4_0); break;

                case TypeCode.Int64:
                case TypeCode.UInt64:
                    cg.Emit(OpCodes.Ldc_I4_0);
                    cg.Emit(OpCodes.Conv_I8);
                    break;

                case TypeCode.Single:
                    cg.Emit(OpCodes.Ldc_R4, default(Single));
                    break;
                case TypeCode.Double:
                    cg.Emit(OpCodes.Ldc_R8, default(Double));
                    break;
                case TypeCode.Decimal:
                    cg.Emit(OpCodes.Ldc_I4_0);
                    cg.EmitNew(typeof(Decimal).GetConstructor(new Type[] { typeof(int) }));
                    break;
                case TypeCode.DateTime:
                    Slot dt = cg.GetLocalTmp(typeof(DateTime));
                    dt.EmitGetAddr(cg);
                    cg.Emit(OpCodes.Initobj, typeof(DateTime));
                    dt.EmitGet(cg);
                    break;
                case TypeCode.String:
                    cg.Emit(OpCodes.Ldnull); break;
            }
        }

        public void EmitTypeError(string format, params object[] args) {
            EmitString(String.Format(format, args));
            EmitCall(typeof(RuntimeHelpers), "SimpleTypeError");
            Emit(OpCodes.Throw);
        }

        public Slot GetArgumentSlot(int index) {
            return _argumentSlots[index];
        }

        public MethodInfo CreateDelegateMethodInfo() {

            if (_methodInfo is DynamicMethod) {
                return (MethodInfo)_methodInfo;
            } else if (_methodInfo is MethodBuilder) {
                MethodBuilder mb = _methodInfo as MethodBuilder;
                Type methodType = _typeGen.FinishType();
                return methodType.GetMethod(mb.Name);
            } else {
                throw new InvalidOperationException();
            }
        }

        public bool IsDynamicMethod {
            get {
                return _methodInfo is DynamicMethod;
            }
        }
        public Delegate CreateDelegate(Type delegateType) {
            Contract.RequiresNotNull(delegateType, "delegateType");

            if (ConstantPool.IsBound) {
                return ReflectionUtils.CreateDelegate(CreateDelegateMethodInfo(), delegateType, _constantPool.Data);
            } else {
                return ReflectionUtils.CreateDelegate(CreateDelegateMethodInfo(), delegateType);
            }
        }

        public Delegate CreateDelegate(Type delegateType, object target) {
            Contract.RequiresNotNull(delegateType, "delegateType");
            Debug.Assert(!ConstantPool.IsBound);

            return ReflectionUtils.CreateDelegate(CreateDelegateMethodInfo(), delegateType, target);
        }

        public CodeGen DefineMethod(string name, Type retType, IList<Type> paramTypes, string[] paramNames, ConstantPool constantPool) {
            Contract.RequiresNotNullItems(paramTypes, "paramTypes");
            //Contract.RequiresNotNull(paramNames, "paramNames");

            CodeGen res;
            if (!DynamicMethod) {
                res = _typeGen.DefineMethod(name, retType, paramTypes, paramNames, constantPool);
            } else {
                if (CompilerHelpers.NeedDebuggableDynamicCodeGenerator(_context)) {
                    res = CompilerHelpers.CreateDebuggableDynamicCodeGenerator(_context, name, retType, paramTypes, paramNames, constantPool);
                } else {
                    res = CompilerHelpers.CreateDynamicCodeGenerator(name, retType, paramTypes, constantPool);
                }
            }

            if (_context != null) res.Context = _context;
            res.InterpretedMode = _interpretedMode;
            return res;
        }


        #region ILGenerator methods

        public void BeginCatchBlock(Type exceptionType) {
            _ilg.BeginCatchBlock(exceptionType);
        }

        public Label BeginExceptionBlock() {
            return _ilg.BeginExceptionBlock();
        }

        public void BeginFaultBlock() {
            _ilg.BeginFaultBlock();
        }

        public void BeginFinallyBlock() {
            _ilg.BeginFinallyBlock();
        }

        public LocalBuilder DeclareLocal(Type localType) {
            LocalBuilder lb = _ilg.DeclareLocal(localType);
            return lb;
        }

        public Label DefineLabel() {
            return _ilg.DefineLabel();
        }

        public void Emit(OpCode opcode) {
          if (!EmitDebugInfo || opcode != OpCodes.Nop)
          {
            _ilg.Emit(opcode);
          }
        }

        public void Emit(OpCode opcode, byte arg) {
             _ilg.Emit(opcode, arg);
        }

        public void Emit(OpCode opcode, ConstructorInfo con) {
            _ilg.Emit(opcode, con);
        }

        public void Emit(OpCode opcode, double arg) {
            _ilg.Emit(opcode, arg);
        }

        public void Emit(OpCode opcode, FieldInfo field) {
            _ilg.Emit(opcode, field);
        }
        
        public void Emit(OpCode opcode, float arg) {
            _ilg.Emit(opcode, arg);
        }
        
        public void Emit(OpCode opcode, int arg) {
            _ilg.Emit(opcode, arg);
        }
        
        public void Emit(OpCode opcode, Label label) {
            _ilg.Emit(opcode, label);
        }
        
        public void Emit(OpCode opcode, Label[] labels) {
            _ilg.Emit(opcode, labels);
        }
        
        public void Emit(OpCode opcode, LocalBuilder local) {
            _ilg.Emit(opcode, local);
        }
        
        public void Emit(OpCode opcode, long arg) {
            _ilg.Emit(opcode, arg);
        }
        
        public void Emit(OpCode opcode, MethodInfo meth) {
            _ilg.Emit(opcode, meth);
        }
        
        [CLSCompliant(false)]
        public void Emit(OpCode opcode, sbyte arg)
        {
            _ilg.Emit(opcode, arg);
        }
        
        public void Emit(OpCode opcode, short arg) {
            _ilg.Emit(opcode, arg);
        }
        
#if !SILVERLIGHT
        public void Emit(OpCode opcode, SignatureHelper signature) {
            _ilg.Emit(opcode, signature);
        }
#endif

        public void Emit(OpCode opcode, string str) {
            _ilg.Emit(opcode, str);
        }
        
        public void Emit(OpCode opcode, Type cls) {
            _ilg.Emit(opcode, cls);
        }
        
        public void EmitCall(OpCode opcode, MethodInfo methodInfo, Type[] optionalParameterTypes) {
            _ilg.EmitCall(opcode, methodInfo, optionalParameterTypes);
        }
        
        public void EndExceptionBlock() {
            if (_targets.Count > 0) {
                Targets t = _targets.Peek();
                Debug.Assert(t.BlockType != TargetBlockType.LoopInFinally);
                if (t.BlockType == TargetBlockType.Finally && t.leaveLabel.HasValue) {
                    MarkLabel(t.leaveLabel.Value);
                }
            }

            _ilg.EndExceptionBlock();
        }
        
        public void MarkLabel(Label loc) {
            _ilg.MarkLabel(loc);
        }
        
        void MarkSequencePoint(ISymbolDocumentWriter document, int startLine, int startColumn, int endLine, int endColumn) {
          if (document == null)
          {
            return;
          }
          if (_context != null) {
                startLine = _context.SourceUnit.MapLine(startLine);
                endLine = _context.SourceUnit.MapLine(endLine);
            }

            Debug.Assert(document != null);

            var fn = GetFilename(document);

            if (fn != null)
            {
              //Debug.WriteLine(string.Format("{4} : {5} ({0},{1}) - ({2},{3})", startLine, startColumn, endLine, endColumn, fn ?? "none", MethodBase.Name));
              _ilg.MarkSequencePoint(document, startLine, startColumn, endLine, endColumn);
            }
        }
        
        public void EmitWriteLine(string value) {
            _ilg.EmitWriteLine(value);
        }

        #endregion

        #region IDisposable Members

        public void Dispose() {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        protected virtual void Dispose(bool disposing) {
            if (disposing) {

            }
        }

        #endregion


        public MethodInfo MethodToOverride {
            get { return _methodToOverride; }
            set { _methodToOverride = value; }
        }
        
        /// <summary>
        /// Gets a list which can be used to inject references to objects from IL.  
        /// </summary>
        public ConstantPool ConstantPool {
            get {
                return _constantPool; 
            }
        }

        // TODO: fix
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2227:CollectionPropertiesShouldBeReadOnly")]
        public IList<Label> YieldLabels {
            get { return _yieldLabels; }
            set { _yieldLabels = value; }
        }
        
        public IList<Slot> ArgumentSlots {
            get { return _argumentSlots; }
        }

        public bool DynamicMethod {
            get {
                return (_options & CodeGenOptions.DynamicMethod) != 0;
            }
            set {
                if (value) _options |= CodeGenOptions.DynamicMethod;
                else _options &= ~CodeGenOptions.DynamicMethod;
            }
        }

        /// <summary>
        /// True if CodeGen should output a text file containing the generated IL, false otherwise.
        /// </summary>
        public bool ILDebug {
            get {
                return (_options & CodeGenOptions.ILDebug) != 0;
            }
            set {
                if (value) _options |= CodeGenOptions.ILDebug;
                else _options &= ~CodeGenOptions.ILDebug;
            }
        }

        /// <summary>
        /// True if CodeGen should store all constants in static fields and emit loads of those fields,
        /// false if constants should be emitted and boxed at runtime.
        /// </summary>
        public bool CacheConstants {
            get {
                return (_options & CodeGenOptions.CacheConstants) != 0;
            }
            set {
                if (value) _options |= CodeGenOptions.CacheConstants;
                else _options &= ~CodeGenOptions.CacheConstants;
            }
        }

        /// <summary>
        /// True if line information should be tracked during code execution to provide
        /// runtime line-information in non-debug builds, false otherwise.
        /// </summary>
        public bool EmitLineInfo {
            get {
                return (_options & CodeGenOptions.EmitLineInfo) != 0;
            }
            set {
                if (value) _options |= CodeGenOptions.EmitLineInfo;
                else _options &= ~CodeGenOptions.EmitLineInfo;
            }
        }

        /// <summary>
        /// Gets the TypeGen object which this CodeGen is emitting into.  TypeGen can be
        /// null if the method is a dynamic method.
        /// </summary>
        public TypeGen TypeGen {
            get {
                return _typeGen;
            }
        }

        struct ReturnBlock {
            public Slot returnValue;
            public Label returnStart;
        }

        private bool CanUseFastSite() {
            // TypeGen is required for fast sites.
            if (_typeGen == null) {
                return false;
            }

            // Fast sites are disabled for dynamic methods
            if (DynamicMethod) {
                return false;
            }

            // Fast sites only possible with global constext
            if (!(this.ContextSlot is StaticFieldSlot)) {
                return false;
            }

            return true;
        }

        internal Slot GetTemporarySlot(Type type) {
            Slot temp;

            if (IsGenerator) {
                temp = _allocator.GetGeneratorTemp();
                if (type != typeof(object)) {
                    temp = new CastSlot(temp, type);
                }
            } else {
                temp = GetLocalTmp(type);
            }
            return temp;
        }

        internal void FreeTemporarySlot(Slot temp) {
            if (!IsGenerator) {
                FreeLocalTmp(temp);
            }
        }

        readonly internal static Dictionary<string, ISymbolDocumentWriter> SymbolWriters = new Dictionary<string, ISymbolDocumentWriter>();

        static string GetFilename(ISymbolDocumentWriter w)
        {
          foreach (var kp in SymbolWriters)
          {
            if (kp.Value == w)
            {
              return kp.Key;
            }
          }
          return null;
        }

        internal CodeGen ProvideAbstractCodeBlock(CodeBlock block, bool hasContextParameter, bool hasThis)
        {
          Assert.NotNull(block);
          CodeGen impl = null;

          if (!hasContextParameter)
          {
            impl = block.CreateMethod(this, hasContextParameter, hasThis);
          }

          return impl;
        }

        static readonly ConstructorInfo NRC = typeof(IronScheme.Runtime.NonRecursiveAttribute).GetConstructor(Type.EmptyTypes);
        static readonly CustomAttributeBuilder CAB = new CustomAttributeBuilder(NRC, new object[0]);

        /// <summary>
        /// Returns the CodeGen implementing the code block.
        /// Emits the code block implementation if it hasn't been emitted yet.
        /// </summary>
        internal CodeGen ProvideCodeBlockImplementation(CodeBlock block, bool hasContextParameter, bool hasThis) {
            Assert.NotNull(block);
            CodeGen impl;

            Debug.Assert(!block.Inlined);

            // emit the code block method if it has:
            if (!_codeBlockImplementations.TryGetValue(block, out impl))
            {
              FlowChecker.Check(block);
              impl = block.CreateMethod(this, hasContextParameter, hasThis);
              impl.Binder = _binder;

              if (EmitDebugInfo)
              {
                if (block.Filename != null)
                {
                  var fn = block.Filename;
                  ISymbolDocumentWriter sw;
                  if (!SymbolWriters.TryGetValue(fn, out sw))
                  {
                    SymbolWriters[fn] = sw = _typeGen.AssemblyGen.ModuleBuilder.DefineDocument(
                      fn,
                      _typeGen.AssemblyGen.LanguageGuid,
                      _typeGen.AssemblyGen.VendorGuid,
                      SymbolGuids.DocumentType_Text);
                  }

                  impl._debugSymbolWriter = sw;
                }
                else
                {
                  impl._debugSymbolWriter = null;
                }
              }
              else
              {
                impl._debugSymbolWriter = null;
              }

              _codeBlockImplementations.Add(block, impl);

              if (_codeBlockStubs.ContainsKey(block))
              {
                _codeBlockStubs.Remove(block);
              }
              else if (_codeBlockStubsX.ContainsKey(block))
              {
                _codeBlockStubsX.Remove(block);
              }
              else if (_codeBlockStubsN.ContainsKey(block))
              {
                _codeBlockStubsN.Remove(block);
              }

              // do attributes, needs to be done before method body else there is a heavy performance penalty when calling CreateType.
              if (block.DecorateWithNonRecursive)
              {
                var mb = impl.MethodBase as MethodBuilder;
                mb.SetCustomAttribute(CAB);
              }

              // add custom attributes to method
              /*
              if (block.DecorateWithUnspecifiedReturn)
              {
                var mb = impl.MethodBase as MethodBuilder;
                mb.SetCustomAttribute(typeof(IronScheme.Runtime.UnspecifiedReturnAttribute).GetConstructor(Type.EmptyTypes), new byte[0]);
              }*/

              block.EmitFunctionImplementation(impl);

              impl.Finish();
            }
            else
            {
              ;
            }

            return impl;
        }
    }
}
