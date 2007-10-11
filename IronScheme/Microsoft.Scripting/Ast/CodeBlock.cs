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

using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;
using System.Diagnostics;
using System.Text;
using System.Threading;
using System.IO;

using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Ast {

    /// <summary>
    /// This captures a block of code that should correspond to a .NET method body.  It takes
    /// input through parameters and is expected to be fully bound.  This code can then be
    /// generated in a variety of ways.  The variables can be kept as .NET locals or in a
    /// 1st class environment object.  This is the primary unit used for passing around
    /// AST's in the DLR.
    /// 
    /// TODO - This should probably not be a Node but that will require some substantial walker changes.
    /// </summary>
    public partial class CodeBlock : Node {
        private static int _Counter = 0;

        private SourceLocation _start;
        private SourceLocation _end;

        private readonly Type _returnType;
        private string _name;
        private CodeBlock _parent;
        private Statement _body;

        private readonly List<Variable> _parameters = new List<Variable>();
        private readonly List<Variable> _variables = new List<Variable>();
        private IList<VariableReference> _references;

        private EnvironmentFactory _environmentFactory;

        private int _generatorTemps;

        private bool _isClosure;
        private bool _hasEnvironment;
        private bool _emitLocalDictionary;
        private bool _isGlobal;
        private bool _visibleScope = true;
        private bool _parameterArray;
        
        // Interpreted mode: Cache for emitted delegate so that we only generate code once.
        private Delegate _delegate;

        // Profile-driven compilation support
        private int _callCount = 0;
        private CompilerContext _declaringContext;
        private bool _forceWrapperMethod;
        private const int _maxInterpretedCalls = 2;
        
        /// <summary>
        /// True, if the block is referenced by a declarative reference (CodeBlockExpression).
        /// </summary>
        private bool _declarativeReferenceExists;

        private Expression _explicitCodeContextExpression;

        internal CodeBlock(SourceSpan span, string name, Type returnType) {
            Assert.NotNull(returnType);

            _name = name;
            _returnType = returnType;
            _start = span.Start;
            _end = span.End;
        }

        internal CodeBlock(SourceSpan span, string name)
            : this(span, name, typeof(object)) {
        }


        public SourceLocation Start {
            get { return _start; }
        }

        public SourceLocation End {
            get { return _end; }
        }

        public SourceSpan Span {
            get {
                return new SourceSpan(_start, _end);
            }
        }

        public Type ReturnType {
            get { return _returnType; }
        }

        public List<Variable> Parameters {
            get { return _parameters; }
        }

        public string Name {
            get { return _name; }
            set { _name = value; }
        }

        public Expression ExplicitCodeContextExpression {
            get { return _explicitCodeContextExpression; }
            set { _explicitCodeContextExpression = value; }
        }

        /// <summary>
        /// The method refers to a variable in one of its parents lexical context and will need an environment
        /// flown into it.  A function which is a closure does not necessarily contain an Environment unless
        /// it contains additional closures or uses language features which require lifting all locals to
        /// an environment.
        /// </summary>
        internal bool IsClosure {
            get { return _isClosure; }
            set { _isClosure = value; }
        }

        /// <summary>
        /// Scopes with environments will have some locals stored within a dictionary (FunctionEnvironment).  If
        /// we are also a closure an environment is flown into the method and our environment will point to the
        /// parent environment.  Ultimately this will enable our children to get at our or our parents envs.
        /// 
        /// Upon entering a function with an environment a new CodeContext will be allocated with a new
        /// FunctionEnviroment as its locals.  In the case of a generator this new CodeContext and environment
        /// is allocated in the function called to create the Generator, not the function that implements the
        /// Generator body.
        /// 
        /// The environment is provided as the Locals of a CodeContext or in the case of a Generator 
        /// as the parentEnvironment field.
        /// </summary>
        internal bool HasEnvironment {
            get { return _hasEnvironment; }
            set { _hasEnvironment = value; }
        }

        /// <summary>
        /// True to force a function to have an environment and have all of its locals lifted
        /// into this environment.  This provides access to local variables via a dictionary but
        /// comes with the performance penality of not using the real stack for locals.
        /// </summary>
        public bool EmitLocalDictionary {
            get {
                // When custom frames are turned on, we emit dictionaries everywhere
                return ScriptDomainManager.Options.Frames || _emitLocalDictionary;
            }
            set {
                _emitLocalDictionary = value;
            }
        }

        public bool IsGlobal {
            get { return _isGlobal; }
            set { _isGlobal = value; }
        }

        public bool ParameterArray {
            get { return _parameterArray; }
            set { _parameterArray = value; }
        }

        public CodeBlock Parent {
            get { return _parent; }
            set { _parent = value; }
        }

        public bool IsVisible {
            get { return _visibleScope; }
            set { _visibleScope = value; }
        }
        public Statement Body {
            get { return _body; }
            set { _body = value; }
        }

        internal IList<VariableReference> References {
            get { return _references; }
            set { _references = value; }
        }

        public List<Variable> Variables {
            get { return _variables; }
        }

        public Type EnvironmentType {
            get {
                Debug.Assert(_environmentFactory != null);
                return _environmentFactory.EnvironmentType;
            }
        }

        internal EnvironmentFactory EnvironmentFactory {
            get { return _environmentFactory; }
        }

        protected internal int GeneratorTemps {
            get { return _generatorTemps; }
        }

        internal void DeclarativeReferenceAdded() {
            if (_declarativeReferenceExists) throw new InvalidOperationException("Block cannot be declared twice");
            _declarativeReferenceExists = true;
        }

        public Variable CreateParameter(SymbolId name, Type type, Expression defaultValue) {
            Variable variable = Variable.Parameter(this, name, type, defaultValue);
            _parameters.Add(variable);
            return variable;
        }

        public Variable CreateParameter(SymbolId name, Type type, Expression defaultValue, bool inParameterArray) {
            Variable variable = Variable.Parameter(this, name, type, defaultValue, inParameterArray);
            _parameters.Add(variable);
            return variable;
        }

        public Variable CreateVariable(SymbolId name, Variable.VariableKind kind, Type type) {
            return CreateVariable(name, kind, type, null);
        }

        public Variable CreateVariable(SymbolId name, Variable.VariableKind kind, Type type, Expression defaultValue) {
            Contract.Requires(kind != Variable.VariableKind.Parameter, "kind");

            Variable variable = Variable.Create(name, kind, this, type, defaultValue);
            _variables.Add(variable);
            return variable;
        }

        public Variable CreateLocalVariable(SymbolId name, Type type) {
            Variable variable = Variable.Local(name, this, type);
            _variables.Add(variable);
            return variable;
        }

        public Variable CreateTemporaryVariable(SymbolId name, Type type) {
            Variable variable = Variable.Temporary(name, this, type);
            _variables.Add(variable);
            return variable;
        }

        public Variable CreateGeneratorTempVariable(SymbolId name, Type type) {
            Variable variable = Variable.GeneratorTemp(name, this, type);
            _variables.Add(variable);
            return variable;
        }

        private void EmitEnvironmentIDs(CodeGen cg) {
            int size = 0;
            foreach (Variable prm in _parameters) {
                if (prm.Lift) size++;
            }
            foreach (Variable var in _variables) {
                if (var.Lift) size++;
            }

            if (!cg.IsDynamicMethod) {
                Debug.Assert(cg.TypeGen != null);

                CodeGen cctor = cg.TypeGen.TypeInitializer;
                EmitEnvironmentIdArray(cctor, size);
                Slot fields = cg.TypeGen.AddStaticField(typeof(SymbolId[]), "__symbolIds$" + _name + "$" + Interlocked.Increment(ref _Counter));
                fields.EmitSet(cctor);
                fields.EmitGet(cg);
            } else {
                EmitEnvironmentIdArray(cg, size);
            }
        }

        private void EmitEnvironmentIdArray(CodeGen cg, int size) {
            // Create the array for the names
            cg.EmitInt(size);
            cg.Emit(OpCodes.Newarr, typeof(SymbolId));

            int index = 0;
            cg.EmitDebugMarker("--- Environment IDs ---");

            foreach (Variable prm in _parameters) {
                if (prm.Lift) {
                    EmitSetVariableName(cg, index++, prm.Name);
                }
            }

            foreach (Variable var in _variables) {
                if (var.Lift) {
                    EmitSetVariableName(cg, index++, var.Name);
                }
            }
            cg.EmitDebugMarker("--- End Environment IDs ---");
        }

        private static void EmitSetVariableName(CodeGen cg, int index, SymbolId name) {
            cg.Emit(OpCodes.Dup);
            cg.EmitInt(index);
            cg.Emit(OpCodes.Ldelema, typeof(SymbolId));
            cg.EmitSymbolId(name);
            cg.Emit(OpCodes.Call, typeof(SymbolId).GetConstructor(new Type[] { typeof(SymbolId) }));
        }

        internal void CreateEnvironmentFactory(bool generator) {
            if (HasEnvironment) {
                // Get the environment size
                int size = 0;

                if (generator) {
                    size += _generatorTemps;

                    foreach (Variable var in _variables) {
                        if (var.Kind == Variable.VariableKind.GeneratorTemporary) {
                            size++;
                        }
                    }
                }

                foreach (Variable parm in _parameters) {
                    if (parm.Lift) size++;
                }
                foreach (Variable var in _variables) {
                    if (var.Lift) size++;
                }
                // Find the right environment factory for the size of elements to store
                _environmentFactory = CreateEnvironmentFactory(size);
            }
        }

        internal EnvironmentSlot EmitEnvironmentAllocation(CodeGen cg) {
            Debug.Assert(_environmentFactory != null);

            cg.EmitDebugMarker("-- ENV ALLOC START --");

            _environmentFactory.EmitStorage(cg);
            cg.Emit(OpCodes.Dup);
            // Store the environment reference in the local
            EnvironmentSlot environmentSlot = _environmentFactory.CreateEnvironmentSlot(cg);
            environmentSlot.EmitSet(cg);

            // Emit the names array for the environment constructor
            EmitEnvironmentIDs(cg);
            // Emit code to generate the new instance of the environment

            _environmentFactory.EmitNewEnvironment(cg);

            cg.EmitDebugMarker("-- ENV ALLOC END --");

            return environmentSlot;
        }

        /// <summary>
        /// Creates a slot for context of type CodeContext from an environment slot.
        /// </summary>
        internal Slot CreateEnvironmentContext(CodeGen cg) {
            // update CodeContext so it contains the nested scope for the locals
            //  ctxSlot = new CodeContext(currentCodeContext, locals)
            Slot ctxSlot = cg.GetNamedLocal(typeof(CodeContext), "$frame");
            cg.EmitCodeContext();
            cg.EnvironmentSlot.EmitGetDictionary(cg);
            cg.EmitInt(_visibleScope ? 1 : 0);
            cg.EmitCall(typeof(RuntimeHelpers), "CreateNestedCodeContext");
            ctxSlot.EmitSet(cg);
            return ctxSlot;
        }

        internal void CreateSlots(CodeGen cg) {
            Contract.RequiresNotNull(cg, "cg");

            if (HasEnvironment) {
                // we're an environment slot, we need our own environment slot, and we're
                // going to update our Context slot to point to a CodeContext which has
                // its Locals pointing at our Environment.
                cg.EnvironmentSlot = EmitEnvironmentAllocation(cg);
                cg.ContextSlot = CreateEnvironmentContext(cg);
            }
            cg.Allocator.Block = this;
            
            CreateAccessSlots(cg);

            foreach (Variable prm in _parameters) {
                prm.Allocate(cg);
            }
            foreach (Variable var in _variables) {
                var.Allocate(cg);
            }
            foreach (VariableReference r in _references) {
                r.CreateSlot(cg);
                Debug.Assert(r.Slot != null);
            }

            cg.Allocator.LocalAllocator.PrepareForEmit(cg);
            cg.Allocator.GlobalAllocator.PrepareForEmit(cg);
        }

        internal void CreateAccessSlots(CodeGen cg) {
            CreateClosureAccessSlots(cg);
            CreateScopeAccessSlots(cg);
        }

        private void CreateClosureAccessSlots(CodeGen cg) {
            ScopeAllocator allocator = cg.Allocator;

            // Current context is accessed via environment slot, if any
            if (HasEnvironment) {
                allocator.AddClosureAccessSlot(this, cg.EnvironmentSlot);
            }

            if (IsClosure) {
                Slot scope = cg.GetLocalTmp(typeof(Scope));
                cg.EmitCodeContext();
                cg.EmitPropertyGet(typeof(CodeContext), "Scope");
                if (HasEnvironment) {
                    cg.EmitPropertyGet(typeof(Scope), "Parent");
                }
                scope.EmitSet(cg);

                CodeBlock current = this;
                do {
                    CodeBlock parent = current._parent;
                    if (parent._environmentFactory != null) {
                        scope.EmitGet(cg);

                        cg.EmitCall(typeof(RuntimeHelpers).GetMethod("GetTupleDictionaryData").MakeGenericMethod(parent._environmentFactory.StorageType));

                        Slot storage = new LocalSlot(cg.DeclareLocal(parent._environmentFactory.StorageType), cg);
                        storage.EmitSet(cg);
                        allocator.AddClosureAccessSlot(parent, storage);
                    }

                    scope.EmitGet(cg);
                    cg.EmitPropertyGet(typeof(Scope), "Parent");
                    scope.EmitSet(cg);

                    current = parent;
                } while (current != null && current.IsClosure);

                cg.FreeLocalTmp(scope);
            }
        }

        private void CreateScopeAccessSlots(CodeGen cg) {
            ScopeAllocator allocator = cg.Allocator;
            for (; ; ) {
                if (allocator == null) {
                    // TODO: interpreted mode anomaly
                    break;
                }
                if (allocator.Block != null && !allocator.Block.IsClosure) {
                    break;
                }
                allocator = allocator.Parent;
            }

            while (allocator != null) {
                if (allocator.Block != null) {
                    foreach (VariableReference reference in _references) {
                        if (!reference.Variable.Lift && reference.Variable.Block == allocator.Block) {
                            Slot accessSlot = allocator.LocalAllocator.GetAccessSlot(cg, allocator.Block);
                            if (accessSlot != null) {
                                cg.Allocator.AddScopeAccessSlot(allocator.Block, accessSlot);
                            }
                            break;
                        }
                    }
                }
                allocator = allocator.Parent;
            }
        }

        internal void AddGeneratorTemps(int count) {
            _generatorTemps += count;
        }

        public override void Walk(Walker walker) {
            if (walker.Walk(this)) {
                Body.Walk(walker);
            }
            walker.PostWalk(this);
        }

        private object DoExecute(CodeContext context) {
            object ret;

            // Make sure that locals owned by this block mask any identically-named variables in outer scopes
            if (!IsGlobal) {
                foreach (Variable v in _variables) {
                    if (v.Kind == Variable.VariableKind.Local && v.Uninitialized && v.Block == this) {
                        BoundAssignment.EvaluateAssign(context, v, Uninitialized.Instance);
                    }
                }
            }

            context.Scope.SourceLocation = Span.Start;
            ret = Body.Execute(context);

            if (ret == Statement.NextStatement) {
                return null;
            } else {
                Debug.Assert(!(ret is ControlFlow));
                return ret;
            }
        }

        public object Execute(CodeContext context) {
            try {
                return DoExecute(context);
            } catch (Exception e) {
#if !SILVERLIGHT
                MethodBase method = MethodBase.GetCurrentMethod();
#else
                MethodBase method = null;
#endif
                SourceUnit sourceUnit = context.ModuleContext.CompilerContext.SourceUnit;
                int line = context.Scope.SourceLocation.Line;

                ExceptionHelpers.UpdateStackTrace(context, method, _name, sourceUnit.GetSymbolDocument(line), sourceUnit.MapLine(line));
                ExceptionHelpers.AssociateDynamicStackFrames(e);
                throw ExceptionHelpers.UpdateForRethrow(e);
            }
        }

        internal object TopLevelExecute(CodeContext context) {
            FlowChecker.Check(this);

            try {
                return Execute(context);
            } finally {
                //RuntimeHelpers.ClearDynamicStackFrames();
                //throw; // ExceptionHelpers.UpdateForRethrow(e);
            }
        }

        protected bool NeedsWrapperMethod(bool stronglyTyped) {
            return _parameters.Count > (stronglyTyped ? ReflectionUtils.MaxSignatureSize - 1 : CallTargets.MaximumCallArgs);
        }

        private bool HasThis() {
            bool hasThis = false;
            for (int index = 0; index < _parameters.Count; index++) {
                if (!_parameters[index].InParameterArray) {
                    // Currently only one parameter can be out of parameter array
                    // TODO: Any number of parameters to be taken out of parameter array
                    Debug.Assert(hasThis == false);
                    Debug.Assert(index == 0);
                    hasThis = true;
                }
            }
            return hasThis;
        }

        protected ConstantPool GetStaticDataForBody(CodeGen cg) {
            if (cg.DynamicMethod) return new ConstantPool();
            else return null;
        }

        public void BindClosures() {
            ClosureBinder.Bind(this);
            FlowChecker.Check(this);
        }

        private bool ShouldCompile() {
            return _callCount++ > _maxInterpretedCalls;
        }

        private object ExecuteWithChildContext(CodeContext parent, params object[] args) {
            // Fast path for if we have emitted this code block as a delegate
            if (_delegate != null) {
                return ReflectionUtils.InvokeDelegate(_delegate, args);
            }

            if (parent.LanguageContext.Engine.Options.ProfileDrivenCompilation) {
                lock (this) {
                    // Check _delegate again -- maybe it appeared between our first check and taking the lock
                    if (_delegate == null && ShouldCompile()) {
                        _delegate = GetCompiledDelegate(_declaringContext, _forceWrapperMethod);
                    }
                }
                if (_delegate != null) {
                    return ReflectionUtils.InvokeDelegate(_delegate, args);
                }
            }
            
            CodeContext child = RuntimeHelpers.CreateNestedCodeContext(parent, new SymbolDictionary(), IsVisible);
            for (int i = 0; i < _parameters.Count; i++) {
                RuntimeHelpers.SetName(child, _parameters[i].Name, args[i]);
            }
            return Execute(child);
        }

        private object ExecuteWithChildContextAndThis(CodeContext parent, object @this, params object[] args) {
            if (_delegate != null) {
                return RuntimeHelpers.CallWithThis(parent, _delegate, @this, args);
            }

            if (parent.LanguageContext.Engine.Options.ProfileDrivenCompilation) {
                lock (this) {
                    if (_delegate == null && ShouldCompile()) {
                        _delegate = GetCompiledDelegate(_declaringContext, _forceWrapperMethod);
                    }
                }
                if (_delegate != null) {
                    return RuntimeHelpers.CallWithThis(parent, _delegate, @this, args);
                }
            }

            CodeContext child = RuntimeHelpers.CreateNestedCodeContext(parent, new SymbolDictionary(), IsVisible);
            RuntimeHelpers.SetName(child, _parameters[0].Name, @this);
            for (int i = 1; i < _parameters.Count; i++) {
                RuntimeHelpers.SetName(child, _parameters[i].Name, args[i-1]);
            }
            return Execute(child);
        }

        // Return a delegate to execute this block in interpreted mode.
        public virtual Delegate GetDelegateForInterpreter(CodeContext context, bool forceWrapperMethod) {
            FlowChecker.Check(this);

            bool delayedEmit = context.LanguageContext.Engine.Options.ProfileDrivenCompilation;
            // Walk the tree to determine whether to emit this CodeBlock or interpret it
            if (InterpretChecker.CanEvaluate(this, delayedEmit)) {
                // Hold onto our declaring context in case we decide to emit ourselves later
                _declaringContext = context.ModuleContext.CompilerContext;
                _forceWrapperMethod = forceWrapperMethod;
                
                if (HasThis()) {
                    return new CallTargetWithContextAndThisN(ExecuteWithChildContextAndThis);
                } else {
                    return new CallTargetWithContextN(ExecuteWithChildContext);
                }
            } else {
                lock (this) {
                    if (_delegate == null) {
                        _delegate = GetCompiledDelegate(context.ModuleContext.CompilerContext, forceWrapperMethod);
                    }
                    return _delegate;
                }
            }

        }

        protected Delegate GetCompiledDelegate(CompilerContext context, bool forceWrapperMethod) {

            bool createWrapperMethod = _parameterArray ? false : forceWrapperMethod || NeedsWrapperMethod(false);
            bool hasThis = HasThis();

            CodeGen cg = CreateMethod(context, hasThis);
            EmitFunctionImplementation(cg);
            cg.Finish();

            Type delegateType;
            if (createWrapperMethod) {
                CodeGen wrapper = MakeWrapperMethodN(null, cg, hasThis);
                wrapper.Finish();
                delegateType = hasThis ? typeof(CallTargetWithContextAndThisN) : typeof(CallTargetWithContextN);
                return wrapper.CreateDelegate(delegateType);
                //throw new NotImplementedException("Wrapper methods not implemented for code blocks in FastEval mode");
            } else if (_parameterArray) {
                delegateType = hasThis ? typeof(CallTargetWithContextAndThisN) : typeof(CallTargetWithContextN);
                return cg.CreateDelegate(delegateType);
                //throw new NotImplementedException("Parameter arrays not implemented for code blocks in FastEval mode");
            } else {
                delegateType = CallTargets.GetTargetType(true, _parameters.Count);
                return cg.CreateDelegate(delegateType);
            }
        }

        internal void EmitDelegateConstruction(CodeGen cg, bool forceWrapperMethod, bool stronglyTyped, Type delegateType) {
            FlowChecker.Check(this);

            // TODO: explicit delegate type may be wrapped...
            bool createWrapperMethod = _parameterArray ? false : (forceWrapperMethod || NeedsWrapperMethod(stronglyTyped));

            bool hasContextParameter = _explicitCodeContextExpression == null && 
                (createWrapperMethod ||
                IsClosure ||
                !(cg.ContextSlot is StaticFieldSlot) ||
                _parameterArray);

            bool hasThis = HasThis();

            cg.EmitSequencePointNone();

            // TODO: storing implementations on code gen doesn't allow blocks being referenced from different methods
            // the implementations should be stored on some kind of Module when available
            CodeGen impl = cg.ProvideCodeBlockImplementation(this, hasContextParameter, hasThis);
            
            // if the method has more than our maximum # of args wrap
            // it in a method that takes an object[] instead.
            if (createWrapperMethod) {
                CodeGen wrapper = MakeWrapperMethodN(cg, impl, hasThis);
                wrapper.Finish();
                
                if (delegateType == null) {
                    delegateType = hasThis ? typeof(CallTargetWithContextAndThisN) : typeof(CallTargetWithContextN);
                }

                cg.EmitDelegateConstruction(wrapper, delegateType);
            } else if (_parameterArray) {
                if (delegateType == null) {
                    delegateType = hasThis ? typeof(CallTargetWithContextAndThisN) : typeof(CallTargetWithContextN);
                }
                cg.EmitDelegateConstruction(impl, delegateType);
            } else {
                if (delegateType == null) {
                    if (stronglyTyped) {
                        delegateType = ReflectionUtils.GetDelegateType(GetParameterTypes(hasContextParameter), _returnType);
                    } else {
                        delegateType = CallTargets.GetTargetType(hasContextParameter, _parameters.Count);
                    }
                }
                cg.EmitDelegateConstruction(impl, delegateType);
            }
        }

        private Type[] GetParameterTypes(bool hasContextParameter) {
            Type[] result = new Type[_parameters.Count + (hasContextParameter ? 1 : 0)];
            int j = 0;
            if (hasContextParameter) {
                result[j++] = typeof(CodeContext);
            }

            for (int i = 0; i < _parameters.Count; i++) {
                result[j++] = _parameters[i].Type;
            }
            return result;
        }

        protected int ComputeSignature(bool hasContextParameter, bool hasThis,
            out List<Type> paramTypes, out List<SymbolId> paramNames, out string implName) {

            paramTypes = new List<Type>();
            paramNames = new List<SymbolId>();

            int parameterIndex = 0;

            if (hasContextParameter) {
                paramTypes.Add(typeof(CodeContext));
                paramNames.Add(SymbolTable.StringToId("$context"));
                parameterIndex = 1;
            }

            if (_parameterArray) {
                int startIndex = 0;
                if (hasThis) {
                    paramTypes.Add(typeof(Object));
                    paramNames.Add(SymbolTable.StringToId("$this"));
                    _parameters[0].ParameterIndex = parameterIndex++;
                    startIndex = 1;
                }

                paramTypes.Add(typeof(object[]));
                paramNames.Add(SymbolTable.StringToId("$params"));

                for (int index = startIndex; index < _parameters.Count; index++) {
                    _parameters[index].ParameterIndex = index - startIndex;
                }
            } else {
                foreach (Variable p in _parameters) {
                    paramTypes.Add(p.Type);
                    paramNames.Add(p.Name);
                    p.ParameterIndex = parameterIndex++;
                }
            }

            implName = _name + "$" + Interlocked.Increment(ref _Counter);

            return parameterIndex;
        }

        /// <summary>
        /// Defines the method with the correct signature and sets up the context slot appropriately.
        /// </summary>
        /// <returns></returns>
        internal CodeGen CreateMethod(CodeGen outer, bool hasContextParameter, bool hasThis) {
            List<Type> paramTypes = new List<Type>();
            List<SymbolId> paramNames = new List<SymbolId>();
            CodeGen impl;
            string implName;
            
            int lastParamIndex = ComputeSignature(hasContextParameter, hasThis, out paramTypes, out paramNames, out implName);

            // create the new method & setup its locals
            impl = outer.DefineMethod(implName, _returnType,
                paramTypes, SymbolTable.IdsToStrings(paramNames), GetStaticDataForBody(outer));

            if (_explicitCodeContextExpression != null) {
                Slot localContextSlot = impl.GetLocalTmp(typeof(CodeContext));
                
                // cannot access code context slot during emit:
                _explicitCodeContextExpression.Emit(impl);

                localContextSlot.EmitSet(impl);
                impl.ContextSlot = localContextSlot;

            } else {
                impl.ContextSlot = hasContextParameter ? impl.GetArgumentSlot(0) : outer.ContextSlot;
            }
            
            if (_parameterArray) {
                impl.ParamsSlot = impl.GetArgumentSlot(lastParamIndex);
            }

            impl.Allocator = CompilerHelpers.CreateLocalStorageAllocator(outer, impl);

            return impl;
        }

        private CodeGen CreateMethod(CompilerContext context, bool hasThis) {
            List<Type> paramTypes;
            List<SymbolId> paramNames;
            CodeGen impl;
            string implName;

            int lastParamIndex = ComputeSignature(true, hasThis, out paramTypes, out paramNames, out implName);

            impl = CompilerHelpers.CreateDynamicCodeGenerator(
                    implName,
                    typeof(object),
                    paramTypes.ToArray(),
                    new ConstantPool());
            impl.InterpretedMode = true;
            impl.ContextSlot = impl.ArgumentSlots[0];
            impl.Context = context;
            impl.EnvironmentSlot = new EnvironmentSlot(
                new PropertySlot(
                    new PropertySlot(impl.ContextSlot,
                        typeof(CodeContext).GetProperty("Scope")),
                    typeof(Scope).GetProperty("Dict"))
                );
            if (_parameterArray) {
                impl.ParamsSlot = impl.GetArgumentSlot(lastParamIndex);
            }

            impl.Allocator = CompilerHelpers.CreateLocalStorageAllocator(null, impl);

            return impl;
        }

        private CodeGen CreateWrapperCodeGen(CodeGen outer, string implName, List<Type> paramTypes, ConstantPool staticData) {
            if (outer == null) {
                return CompilerHelpers.CreateDynamicCodeGenerator(implName, typeof(object), paramTypes, staticData);
            } else {
                return outer.DefineMethod(implName, typeof(object), paramTypes.ToArray(), null, staticData);
            }
        }
        /// <summary>
        /// Creates a wrapper method for the user-defined function.  This allows us to use the CallTargetN
        /// delegate against the function when we don't have a CallTarget# which is large enough.
        /// </summary>
        private CodeGen MakeWrapperMethodN(CodeGen outer, CodeGen impl, bool hasThis) {
            CodeGen wrapper;
            Slot contextSlot = null;
            Slot argSlot;
            Slot thisSlot = null;
            ConstantPool staticData = null;

            bool hasContextParameter = impl.ArgumentSlots.Count > 0
                && impl.ArgumentSlots[0].Type == typeof(CodeContext);

            if (impl.ConstantPool.IsBound) {
                staticData = impl.ConstantPool.CopyData();
            }

            string implName = impl.MethodBase.Name;

            List<Type> paramTypes = new List<Type>();
            if (hasContextParameter) {
                paramTypes.Add(typeof(CodeContext));
                if (hasThis) {
                    paramTypes.Add(typeof(object));
                    paramTypes.Add(typeof(object[]));
                    wrapper = CreateWrapperCodeGen(outer, implName, paramTypes, staticData);
                    contextSlot = wrapper.GetArgumentSlot(0);
                    thisSlot = wrapper.GetArgumentSlot(1);
                    argSlot = wrapper.GetArgumentSlot(2);
                } else {
                    paramTypes.Add(typeof(object[]));
                    wrapper = CreateWrapperCodeGen(outer, implName, paramTypes, staticData);
                    contextSlot = wrapper.GetArgumentSlot(0);
                    argSlot = wrapper.GetArgumentSlot(1);
                }
            } else {
                // Context weirdness: DynamicMethods need to flow their context, and if we don't
                // have a TypeGen we'll create a DynamicMethod but we won't flow context w/ it.
                Debug.Assert(outer == null || outer.TypeGen != null);
                if (hasThis) {
                    paramTypes.Add(typeof(object));
                    paramTypes.Add(typeof(object[]));
                    wrapper = CreateWrapperCodeGen(outer, implName, paramTypes, staticData);
                    thisSlot = wrapper.GetArgumentSlot(0);
                    argSlot = wrapper.GetArgumentSlot(1);
                } else {
                    paramTypes.Add(typeof(object[]));
                    wrapper = CreateWrapperCodeGen(outer, implName, paramTypes, staticData);
                    argSlot = wrapper.GetArgumentSlot(0);
                }
            }

            if (wrapper.ConstantPool.IsBound) {
                wrapper.ConstantPool.Slot.EmitGet(wrapper);
            }

            if (contextSlot != null) {
                contextSlot.EmitGet(wrapper);
            }

            int startIndex = 0;
            if (thisSlot != null) {
                thisSlot.EmitGet(wrapper);
                startIndex = 1;
            }

            for (int pi = startIndex; pi < _parameters.Count; pi++) {
                argSlot.EmitGet(wrapper);
                wrapper.EmitInt(pi - startIndex);
                wrapper.Emit(OpCodes.Ldelem_Ref);
                wrapper.EmitCast(typeof(object), _parameters[pi].Type);
            }
            wrapper.EmitCall(impl.MethodInfo);
            wrapper.Emit(OpCodes.Ret);
            return wrapper;
        }

        internal void EmitFunctionImplementation(CodeGen impl) {
            CompilerHelpers.EmitStackTraceTryBlockStart(impl);

            // emit the actual body
            EmitBody(impl);

            string displayName;
            
            if (impl.HasContext) {
                displayName = impl.Context.SourceUnit.GetSymbolDocument(Span.Start.Line) ?? _name;
            } else {
                displayName = _name;
            }

            CompilerHelpers.EmitStackTraceFaultBlock(impl, _name, displayName);
        }

        internal protected virtual void EmitBody(CodeGen cg) {
            CreateEnvironmentFactory(false);
            CreateSlots(cg);
            if (cg.InterpretedMode) {
                foreach (VariableReference vr in _references) {
                    if (vr.Variable.Kind == Variable.VariableKind.Local && vr.Variable.Block == this) {
                        vr.Slot.EmitSetUninitialized(cg);
                    }
                }
            }

            EmitStartPosition(cg);

            Body.Emit(cg);

            EmitEndPosition(cg);

            cg.EmitReturn(null); //TODO skip if Body is guaranteed to return
        }

        private void EmitStartPosition(CodeGen cg) {
            // ensure a break point exists at the top
            // of the file if there isn't a statement
            // flush with the start of the file.
            if (!Start.IsValid) return;

            if (Body.Start.IsValid) {
                if (Body.Start != Start) {
                    cg.EmitPosition(Start, Start);
                }
            } else {
                BlockStatement block = Body as BlockStatement;
                if (block != null) {
                    for (int i = 0; i < block.Statements.Count; i++) {
                        if (block.Statements[i].Start.IsValid) {
                            if (block.Statements[i].Start != Start) {
                                cg.EmitPosition(Start, Start);
                            }
                            break;
                        }
                    }
                }
            }
        }

        private void EmitEndPosition(CodeGen cg) {
            // ensure we emit a sequence point at the end
            // so the user can inspect any info before exiting
            // the function.  Also make sure additional code
            // isn't associated with this function.
            cg.EmitPosition(End, End);
            cg.EmitSequencePointNone();
        }

        // This is used for compiling the toplevel CodeBlock object.
        internal T CreateDelegate<T>(CompilerContext context) 
            where T : class {
            CodeGen cg = CompilerHelpers.CreateDynamicCodeGenerator(context);
            cg.Allocator = CompilerHelpers.CreateFrameAllocator(cg.ContextSlot);
            
            cg.EnvironmentSlot = new EnvironmentSlot(                
                new PropertySlot(
                    new PropertySlot(cg.ContextSlot, 
                        typeof(CodeContext).GetProperty("Scope")),
                    typeof(Scope).GetProperty("Dict"))
                );

            EmitFunctionImplementation(cg);
            cg.Finish();

            return (T)(object)cg.CreateDelegate(typeof(T));
        }

        internal static EnvironmentFactory CreateEnvironmentFactory(int size) {
            size++; // +1 for the FunctionEnvironmentDictionary 

            Type[] argTypes = CompilerHelpers.MakeRepeatedArray(typeof(object), size);
            argTypes[0] = typeof(IAttributesCollection);

            Type tupleType = Tuple.MakeTupleType(argTypes);
            Type envType = typeof(FunctionEnvironmentDictionary<>).MakeGenericType(tupleType);

            return new PropertyEnvironmentFactory(tupleType, envType);
        }       
    }

    public static partial class Ast {
        public static CodeBlock CodeBlock(string name) {
            return CodeBlock(SourceSpan.None, name);
        }

        public static CodeBlock CodeBlock(SourceSpan span, string name) {
            return new CodeBlock(span, name, typeof(object));
        }

        public static CodeBlock CodeBlock(string name, Type returnType) {
            return new CodeBlock(SourceSpan.None, name, returnType);
        }

        public static CodeBlock CodeBlock(SymbolId name) {
            return CodeBlock(SourceSpan.None, SymbolTable.IdToString(name));
        }

        public static CodeBlock CodeBlock(SourceSpan span, SymbolId name) {
            return CodeBlock(span, SymbolTable.IdToString(name));
        }

        public static CodeBlock EventHandlerBlock(string name, EventInfo eventInfo) {
            Contract.RequiresNotNull(name, "name");
            Contract.RequiresNotNull(eventInfo, "eventInfo");

            ParameterInfo returnInfo;
            ParameterInfo[] parameterInfos;

            ReflectionUtils.GetDelegateSignature(eventInfo.EventHandlerType, out parameterInfos, out returnInfo);

            CodeBlock result = Ast.CodeBlock(name, returnInfo.ParameterType);
            for (int i = 0; i < parameterInfos.Length; i++) {
                result.Parameters.Add(Variable.Parameter(result, SymbolTable.StringToId("$" + i), parameterInfos[i].ParameterType));
            }

            return result;
        }
    }
}
