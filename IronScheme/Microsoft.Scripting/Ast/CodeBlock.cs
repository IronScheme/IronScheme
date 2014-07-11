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
        private readonly Dictionary<SymbolId, Variable> _parametersmap = new Dictionary<SymbolId, Variable>();
        private readonly Dictionary<SymbolId, Variable> _variablesmap = new Dictionary<SymbolId, Variable>();

        private IList<VariableReference> _references;

        private EnvironmentFactory _environmentFactory;

        private int _generatorTemps;

        public bool DecorateWithUnspecifiedReturn { get; set; }

        private bool _isClosure;
        private bool _hasEnvironment;
        private bool _emitLocalDictionary;
        private bool _isGlobal;
        private bool _visibleScope = true;
        private bool _parameterArray;
        internal bool Checked = false;
        bool inlined = false;

        public bool Inlined
        {
          get { return inlined; }
          set { inlined = value; }
        }

        public object Source { get; set; }
        public bool IsRest { get; set; }

        // Profile-driven compilation support
        private int _callCount = 0;
        private const int _maxInterpretedCalls = 2;
        
        /// <summary>
        /// True, if the block is referenced by a declarative reference (CodeBlockExpression).
        /// </summary>
        private bool _declarativeReferenceExists;

        private Expression _explicitCodeContextExpression;

        public string Filename { get; set; }

      public override string ToString()
      {
        return _name;
      }

        internal CodeBlock(AstNodeType nodeType, SourceSpan span, string name, Type returnType)
            : base(nodeType) {
            Assert.NotNull(returnType);

            _name = name;
            _returnType = returnType;
            _start = span.Start;
            _end = span.End;
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

        public IEnumerable<Variable> Parameters
        {
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
        public bool IsClosure {
            get { return _isClosure; }
            internal set { _isClosure = value; }
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
        public bool HasEnvironment {
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
                return ScriptDomainManager.Options.Frames || _emitLocalDictionary || ScriptDomainManager.Options.LightweightDebugging;
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
            get 
            {
              if (_parent != null && _parent.Inlined)
              {
                return _parent.Parent;
              }
              return _parent; 
            }
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

        public void Bind()
        {
          IsClosure = false;
          HasEnvironment = false;
          ClosureBinder.Bind(this);
        }

        internal IList<VariableReference> References {
            get { return _references; }
            set { _references = value; }
        }

        public IEnumerable<Variable> Variables {
            get { return _variables; }
        }

        public Variable Lookup(SymbolId id)
        {
          Variable v = null;

          if (_parametersmap.TryGetValue(id, out v))
          {
            return v;
          }

          if (_variablesmap.TryGetValue(id, out v))
          {
            return v;
          }

          if (Parent != null)
          {
            return Parent.Lookup(id);
          }

          return v;
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

        public Variable CreateParameter(SymbolId name, Type type) {
            Variable variable = Variable.Parameter(this, name, type);
            _parameters.Add(variable);
            _parametersmap.Add(variable.Name, variable);
            return variable;
        }

        public Variable CreateParameter(SymbolId name, Type type, bool inParameterArray) {
            Variable variable = Variable.Parameter(this, name, type, inParameterArray);
            _parameters.Add(variable);
            _parametersmap.Add(variable.Name, variable);
            return variable;
        }

        public Variable CreateVariable(SymbolId name, Variable.VariableKind kind, Type type) {
            return CreateVariable(name, kind, type, null);
        }

        public Variable CreateVariable(SymbolId name, Variable.VariableKind kind, Type type, Expression defaultValue) {
            Contract.Requires(kind != Variable.VariableKind.Parameter, "kind");

            Variable variable = Variable.Create(name, kind, this, type, defaultValue);
            _variables.Add(variable);
            Debug.Assert(!_variablesmap.ContainsKey(name));
            _variablesmap.Add(variable.Name, variable);
            return variable;
        }

        public Variable CreateLocalVariable(SymbolId name, Type type) {
            Variable variable = Variable.Local(name, this, type);
            _variables.Add(variable);
            Debug.Assert(!_variablesmap.ContainsKey(name));
            _variablesmap.Add(variable.Name, variable);
            return variable;
        }

        public Variable CreateTemporaryVariable(SymbolId name, Type type) {
            Variable variable = Variable.Temporary(name, this, type);
            _variables.Add(variable);
            Debug.Assert(!_variablesmap.ContainsKey(name));
            _variablesmap.Add(variable.Name, variable);
            return variable;
        }

        public Variable CreateGeneratorTempVariable(SymbolId name, Type type) {
            Variable variable = Variable.GeneratorTemp(name, this, type);
            _variables.Add(variable);
            Debug.Assert(!_variablesmap.ContainsKey(name));
            _variablesmap.Add(variable.Name, variable);
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
        }

        private static void EmitSetVariableName(CodeGen cg, int index, SymbolId name) {
            cg.Emit(OpCodes.Dup);
            cg.EmitInt(index);
            cg.Emit(OpCodes.Ldelema, typeof(SymbolId));
            cg.EmitSymbolId(name);
            cg.EmitUnbox(typeof(SymbolId));
            cg.Emit(OpCodes.Call, typeof(SymbolId).GetConstructor(new Type[] { typeof(SymbolId) }));
        }

        internal void CreateEnvironmentFactory(bool generator, CodeGen cg) {
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

                List<Variable> lifted = new List<Variable>();

                foreach (Variable parm in _parameters) {
                  if (parm.Lift)
                  {
                    if (!lifted.Contains(parm))
                    {
                      lifted.Add(parm);
                      size++;
                    }
                    else
                    {
                      Console.WriteLine(parm);
                    }
                  }
                }
                foreach (Variable var in _variables) {
                  if (var.Lift)
                  {
                    if (!lifted.Contains(var))
                    {
                      lifted.Add(var);
                      size++;
                    }
                    else
                    {
                      Console.WriteLine(var);
                    }
                  }
                }
                // Find the right environment factory for the size of elements to store
                if (useclass)
                {
                  _environmentFactory = CreateEnvironmentFactory(lifted, cg, GetParentEvironmentType());
                }
                else
                {
                  _environmentFactory = CreateEnvironmentFactory(size);
                }
            }
        }

        Type GetParentEvironmentType()
        {
          if (Parent == null || Parent.EnvironmentFactory == null)
          {
            return typeof(IAttributesCollection);
          }
          return Parent.EnvironmentType;
        }

        static bool useclass = true;

        internal EnvironmentSlot EmitEnvironmentAllocation(CodeGen cg) {
            Debug.Assert(_environmentFactory != null);

            _environmentFactory.EmitStorage(cg);
            cg.Emit(OpCodes.Dup);
            // Store the environment reference in the local
            EnvironmentSlot environmentSlot = _environmentFactory.CreateEnvironmentSlot(cg);
            environmentSlot.EmitSet(cg);

            // Emit code to generate the new instance of the environment
            _environmentFactory.EmitNewEnvironment(cg);

          return environmentSlot;
        }

        /// <summary>
        /// Creates a slot for context of type CodeContext from an environment slot.
        /// </summary>
        internal Slot CreateEnvironmentContext(CodeGen cg) {
            // update CodeContext so it contains the nested scope for the locals
            //  ctxSlot = new CodeContext(currentCodeContext, locals)
            Slot ctxSlot = cg.GetNamedLocal(typeof(CodeContext), "$frame");
            cg.EnvironmentSlot.EmitGetDictionary(cg);
            cg.EmitCodeContext();
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
              if (var.Block != null)
              {
                var.Allocate(cg);
              }
            }
            foreach (VariableReference r in References) {
              if (r.Variable.Block == null)
              {
                Debugger.Break();
              }
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


        int depth = -1;

        int GetDepth()
        {
          if (depth == -1)
          {
            depth = 0;
            CodeBlock parent = Parent;
            while (parent != null)
            {
              depth++;
              parent = parent.Parent;
            }
          }
          return depth;
        }

        private void CreateClosureAccessSlots(CodeGen cg)
        {
          ScopeAllocator allocator = cg.Allocator;

          // Current context is accessed via environment slot, if any
          if (HasEnvironment)
          {
            allocator.AddClosureAccessSlot(this, cg.EnvironmentSlot);
          }

          // this is the root of all evil...
          if (IsClosure)
          {
            int maxdepth = GetDepth();

            foreach (VariableReference r in References)
            {
              CodeBlock cb = r.Variable.Block;
              if (!cb.IsGlobal)
              {
                int d = cb.GetDepth();

                if (d < maxdepth)
                {
                  maxdepth = d;
                }
              }
            }

            int diff = depth - maxdepth;

            if (diff > 0)
            {
              Slot scope = cg.GetLocalTmp(typeof(Scope));
              cg.EmitCodeContext();
              cg.EmitPropertyGet(typeof(CodeContext), "Scope");
              if (HasEnvironment)
              {
                cg.EmitPropertyGet(typeof(Scope), "Parent");
              }
              scope.EmitSet(cg);

              int i = 0;
              CodeBlock current = this;
              do
              {
                CodeBlock parent = current.Parent;
                if (parent._environmentFactory != null)
                {
                  scope.EmitGet(cg);

                  if (useclass)
                  {
                    cg.EmitCall(typeof(RuntimeHelpers).GetMethod("GetStorageData").MakeGenericMethod(parent._environmentFactory.StorageType));
                  }
                  else
                  {
                    cg.EmitCall(typeof(RuntimeHelpers).GetMethod("GetTupleDictionaryData").MakeGenericMethod(parent._environmentFactory.StorageType));
                  }

                  Slot storage = new LocalSlot(cg.DeclareLocal(parent._environmentFactory.StorageType), cg);
                  storage.EmitSet(cg);
                  allocator.AddClosureAccessSlot(parent, storage);
                }

                i++;

                
                if (i < diff && parent.HasEnvironment)
                {
                  scope.EmitGet(cg);
                  cg.EmitPropertyGet(typeof(Scope), "Parent");
                  scope.EmitSet(cg);
                }

                current = parent;
                
              } while (i < diff && current != null && current.IsClosure);

              cg.FreeLocalTmp(scope);
            }


          }

          cg.DefineStartPoint();
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
                    foreach (VariableReference reference in References) {
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

        private bool ShouldCompile() {
            return _callCount++ > _maxInterpretedCalls;
        }

        public void Update()
      {
        FlowChecker.Check(this);
      }

        protected Delegate GetCompiledDelegate(CompilerContext context, Type delegateType, bool forceWrapperMethod) {

          if (_compiled != null)
          {
            return _compiled;
          }
            bool createWrapperMethod = !_parameterArray && (forceWrapperMethod || NeedsWrapperMethod(false));
            bool hasThis = HasThis();

            CodeGen cg = CreateInterprettedMethod(context, delegateType, hasThis);
            EmitFunctionImplementation(cg);

            cg.Finish();

            if (delegateType == null) {
                if (createWrapperMethod) {
                    CodeGen wrapper = MakeWrapperMethodN(null, cg, hasThis);
                    wrapper.Finish();
                    delegateType =
 typeof(CallTargetWithContextN);
                    return wrapper.CreateDelegate(delegateType);
                    //throw new NotImplementedException("Wrapper methods not implemented for code blocks in FastEval mode");
                } else if (_parameterArray) {
                  delegateType =
 typeof(CallTargetWithContextN);
                    return cg.CreateDelegate(delegateType);
                    //throw new NotImplementedException("Parameter arrays not implemented for code blocks in FastEval mode");
                } else {
                    delegateType = CallTargets.GetTargetType(true, _parameters.Count - (HasThis() ? 1 : 0), HasThis());
                    return _compiled = cg.CreateDelegate(delegateType);
                }
            } else {
              return _compiled = cg.CreateDelegate(delegateType);
            }
        }

      Delegate _compiled;
      MethodInfo _impl;

      internal void EmitDirectCall(CodeGen cg, bool forceWrapperMethod, bool stronglyTyped, Type delegateType, bool tailcall)
      {
        if (_impl != null)
        {
          cg.EmitCall(_impl, tailcall);
          return;
        }
        //FlowChecker.Check(this);

        // TODO: explicit delegate type may be wrapped...
        bool createWrapperMethod = !_parameterArray && (forceWrapperMethod || NeedsWrapperMethod(stronglyTyped));

        bool hasContextParameter = _explicitCodeContextExpression == null &&
            (createWrapperMethod ||
            IsClosure ||
            !(cg.ContextSlot is StaticFieldSlot) ||
            _parameterArray);

        //hasContextParameter = true;

        bool hasThis = HasThis();

        // TODO: storing implementations on code gen doesn't allow blocks being referenced from different methods
        // the implementations should be stored on some kind of Module when available
        CodeGen impl = cg.ProvideCodeBlockImplementation(this, hasContextParameter, hasThis);

        //// if the method has more than our maximum # of args wrap
        //// it in a method that takes an object[] instead.
        if (createWrapperMethod)
        {
          CodeGen wrapper = MakeWrapperMethodN(cg, impl, hasThis);
          wrapper.Finish();
          cg.EmitCall(_impl = wrapper.MethodInfo, tailcall);
        }
        else
        {
          impl.Finish();
          cg.EmitCall(_impl = impl.MethodInfo, tailcall);
        }
      }

        internal void EmitDelegateConstruction(CodeGen cg, bool forceWrapperMethod, bool stronglyTyped, Type delegateType) {
            //FlowChecker.Check(this);

            // TODO: explicit delegate type may be wrapped...
            bool createWrapperMethod = !_parameterArray && (forceWrapperMethod || NeedsWrapperMethod(stronglyTyped));

            bool hasContextParameter = _explicitCodeContextExpression == null && 
                (createWrapperMethod ||
                IsClosure ||
                !(cg.ContextSlot is StaticFieldSlot) ||
                _parameterArray);

            //hasContextParameter = true;

            bool hasThis = HasThis();

            

            // TODO: storing implementations on code gen doesn't allow blocks being referenced from different methods
            // the implementations should be stored on some kind of Module when available
            CodeGen impl = cg.ProvideCodeBlockImplementation(this, hasContextParameter, hasThis);
            
            // if the method has more than our maximum # of args wrap
            // it in a method that takes an object[] instead.
            if (createWrapperMethod) {
                CodeGen wrapper = MakeWrapperMethodN(cg, impl, hasThis);
                wrapper.Finish();
                
                if (delegateType == null) {
                  delegateType = typeof(CallTargetN);
                }
                _impl = wrapper.MethodInfo;
                //cg.EmitPosition(Start, Start);
                if (hasContextParameter)
                {
                  cg.EmitCodeContext();
                }
                cg.EmitDelegateConstruction(wrapper, delegateType, hasContextParameter);
            } else if (_parameterArray) {
                if (delegateType == null) {
                  delegateType = typeof(CallTargetN);
                }
                //cg.EmitPosition(Start, Start);
                if (hasContextParameter)
                {
                  cg.EmitCodeContext();
                }
                cg.EmitDelegateConstruction(impl, delegateType, hasContextParameter);
            } else {
                if (delegateType == null) {
                        delegateType = CallTargets.GetTargetType(false, _parameters.Count - (hasThis ? 1 : 0), hasThis);
                }
                _impl = impl.MethodInfo;
               // cg.EmitPosition(Start, Start);
                if (hasContextParameter)
                {
                  cg.EmitCodeContext();
                }
                cg.EmitDelegateConstruction(impl, delegateType, hasContextParameter);
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

        int ComputeSignature(bool hasContextParameter, bool hasThis, out List<Type> paramTypes, out List<SymbolId> paramNames, out string implName) {

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
                    paramNames.Add(Variable.UnGenSym(p.Name));
                    p.ParameterIndex = parameterIndex++;
                }
            }

            implName = GetGeneratedName();

            return parameterIndex;
        }

        private int ComputeDelegateSignature(Type delegateType, out List<Type> paramTypes, out List<SymbolId> paramNames, out string implName) {
            implName = GetGeneratedName();
            MethodInfo invoke = delegateType.GetMethod("Invoke");
            ParameterInfo[] pis = invoke.GetParameters();
            paramNames = new List<SymbolId>();
            paramTypes = new List<Type>();
            foreach (ParameterInfo pi in pis) {
                paramTypes.Add(pi.ParameterType);
                paramNames.Add(SymbolTable.StringToId(pi.Name));
            }
            return -1;
        }

        private string GetGeneratedName() {
          if (_name == "anon" || string.IsNullOrEmpty(_name) || _name.Contains("#") || _name.EndsWith("dummy"))
          {
            return _name + "$" + Interlocked.Increment(ref _Counter);
          }
          else
          {
            if (IsRest)
            {
              return _name + "+";
            }
            else
            {
              return _name;
            }
          }
        }

        CodeGen _stub;

        internal CodeGen CreateGlobalMethodStub(TypeGen tg)
        {
          if (_stub != null)
          {
            return _stub;
          }

          List<Type> paramTypes = new List<Type>();
          List<SymbolId> paramNames = new List<SymbolId>();
          string implName;

          int lastParamIndex = ComputeSignature(false, false, out paramTypes, out paramNames, out implName);

          if (paramTypes.Count <= 8)
          {
            var cg = tg.DefineMethod(implName, _returnType, paramTypes, SymbolTable.IdsToStrings(paramNames), null);

            if (_parameterArray)
            {
              cg.ParamsSlot = cg.GetArgumentSlot(lastParamIndex);
            }
            return _stub = cg;
          }
          else
          {
            return null;
          }
        }

        /// <summary>
        /// Defines the method with the correct signature and sets up the context slot appropriately.
        /// </summary>
        /// <returns></returns>
        internal CodeGen CreateMethod(CodeGen outer, bool hasContextParameter, bool hasThis) {
          CodeGen impl;

          if (!CodeGen._codeBlockStubs.TryGetValue(this, out impl) && 
              !CodeGen._codeBlockStubsX.TryGetValue(this, out impl) &&
              !CodeGen._codeBlockStubsN.TryGetValue(this, out impl))
          {
            List<Type> paramTypes = new List<Type>();
            List<SymbolId> paramNames = new List<SymbolId>();

            string implName;

            int lastParamIndex = ComputeSignature(hasContextParameter, hasThis, out paramTypes, out paramNames, out implName);

            // create the new method & setup its locals
            impl = outer.DefineMethod(implName, _returnType,
                paramTypes, SymbolTable.IdsToStrings(paramNames), GetStaticDataForBody(outer));

            if (_parameterArray)
            {
              impl.ParamsSlot = impl.GetArgumentSlot(lastParamIndex);
            }
          }

          if (_explicitCodeContextExpression != null && HasEnvironment)
            {
              Slot localContextSlot = impl.GetLocalTmp(typeof(CodeContext));
              
              //cannot access code context slot during emit:
              _explicitCodeContextExpression.Emit(impl);

              localContextSlot.EmitSet(impl);
              impl.ContextSlot = localContextSlot;

          } else {
            if (Parent == null || !Parent.IsGlobal)
            {
              impl.ContextSlot = hasContextParameter ? impl.GetArgumentSlot(0) : 
                  (Parent == null ? impl.ContextSlot : outer.ContextSlot);
            }
          }
          
          impl.Allocator = CompilerHelpers.CreateLocalStorageAllocator(outer, impl);

          return impl;
        }

        private CodeGen CreateInterprettedMethod(CompilerContext context, Type delegateType, bool hasThis) {
            List<Type> paramTypes;
            List<SymbolId> paramNames;
            CodeGen impl;
            string implName;

            int lastParamIndex;

            if (delegateType == null) {
                lastParamIndex = ComputeSignature(true, hasThis, out paramTypes, out paramNames, out implName);
            } else {
                Debug.Assert(!_parameterArray);
                lastParamIndex = ComputeDelegateSignature(delegateType, out paramTypes, out paramNames, out implName);
            }

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
/*
        private CodeGen MakeUntypedWrapperMethod(CodeGen outer, CodeGen impl)
        {
          string implName = impl.MethodBase.Name;
          List<Type> types = new List<Type>();

          foreach (var p in _parameters)
          {
            types.Add(typeof(object));
          }

          CodeGen wrapper = CreateWrapperCodeGen(outer, implName, types, null);

          for (int i = 0; i < _parameters.Count; i++)
          {
            wrapper.EmitArgAddr(i);
            wrapper.EmitCall(Unbox.MakeGenericMethod(_parameters[i].Type));
          }

          wrapper.EmitCall(impl.MethodInfo);
          wrapper.EmitCall(Box.MakeGenericMethod(_returnType), true);
          wrapper.EmitReturn();

          return wrapper;
        }

        public static MethodInfo Unbox;
        public static MethodInfo Box;
*/
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
            wrapper.EmitCall(impl.MethodInfo, true);
            wrapper.Emit(OpCodes.Ret);
            return wrapper;
        }

        internal void EmitFunctionImplementation(CodeGen impl)
        {
#if FULL
            CompilerHelpers.EmitStackTraceTryBlockStart(impl); 
#endif
          // emit the actual body
            Debug.Assert(!Inlined);
            EmitBody(impl);

#if FULL
            string displayName;
            
            if (impl.HasContext) {
                displayName = impl.Context.SourceUnit.GetSymbolDocument(Span.Start.Line) ?? _name;
            } else {
                displayName = _name;
            }

            CompilerHelpers.EmitStackTraceFaultBlock(impl, _name, displayName);
#endif
        }

        internal protected virtual void EmitBody(CodeGen cg) {

          var prevls = cg.lambdaspan;

          if (!ScriptDomainManager.Options.LightweightDebugging)
          {
            if (Start.IsValid)
            {
              var s = new SourceLocation(Start.Index, Start.Line, Start.Column + 1);

              cg.EmitPosition(Start, s);
              cg.Emit(OpCodes.Nop);
            }
            else
            {
              cg.EmitSequencePointNone();
            }

            cg.lambdaspan = Span;
          }

            CreateEnvironmentFactory(false, cg);
            CreateSlots(cg);

            if (ScriptDomainManager.Options.LightweightDebugging)
            {
              if (!cg.IsDynamicMethod)
              {
                cg.Emit(OpCodes.Ldtoken, cg.MethodInfo);
                cg.EmitConstant(Filename);
                cg.EmitConstant(SpanToLong(Span));
                cg.EmitCodeContext();
                cg.EmitCall(Debugging.DebugMethods.ProcedureEnter);
              }
            }

            Body.Emit(cg);

            if (!ScriptDomainManager.Options.LightweightDebugging)
            {
              cg.lambdaspan = prevls;
              cg.EmitSequencePointNone();
            }
        }


        // This is used for compiling the toplevel CodeBlock object.
        internal T CreateDelegate<T>(CompilerContext context) 
            where T : class {
            CodeGen cg = CompilerHelpers.CreateDynamicCodeGenerator(context);
            cg.Allocator = CompilerHelpers.CreateFrameAllocator();
            
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

        internal static EnvironmentFactory CreateEnvironmentFactory(List<Variable> vars, CodeGen cg, Type parentType)
        {
          Type storageType = GenerateStorageType(vars, cg, parentType);
          Type envType = typeof(Storage<>).MakeGenericType(storageType);
          return new ClassEnvironmentFactory(storageType, envType);
        }


        static int closure_counter = 0;

        static Type GenerateStorageType(List<Variable> vars, CodeGen cg, Type parentType)
        {
          int c = Interlocked.Increment(ref closure_counter);
          var tg = cg.TypeGen.AssemblyGen.DefinePublicType("closure.$env" + c, typeof(object), TypeAttributes.Sealed | TypeAttributes.NotPublic);

          tg.TypeBuilder.DefineField("$parent$", typeof(IAttributesCollection), FieldAttributes.Public);

          foreach (var v in vars)
          {
            tg.TypeBuilder.DefineField(SymbolTable.IdToString(v.Name), v.Type, FieldAttributes.Public);
          }

          var t = tg.FinishType();

          return t;
        }

        public void AddParameter(Variable par)
        {
          _parameters.Add(par);
          _parametersmap.Add(par.Name, par);
        }

      // for rewriting
        public void AddVariable(Variable par)
        {
          Debug.Assert(!_variables.Contains(par));
          Debug.Assert(!_variablesmap.ContainsKey(par.Name));
          _variables.Add(par);
          _variablesmap.Add(par.Name, par);
        }

        public int ParameterCount
        {
          get { return _parameters.Count; }
        }


        public void RemoveVariables(List<Variable> toremove)
        {
          foreach (var v in toremove)
          {
            _variables.Remove(v);
            _variablesmap.Remove(v.Name);
          }
        }


        internal void ResetBindings()
        {
          foreach (var v in Parameters)
          {
            v.Lift = false;
          }
          foreach (var v in Variables)
          {
            v.Lift = false;
          }
          HasEnvironment = false;
        }
    }

    public class CodeBlockDescriptor
    {
      public int arity;
      public CodeBlockExpression codeblock;
      public Expression callable;
      public bool varargs;
    }

    public static partial class Ast {
        public static CodeBlock CodeBlock(string name) {
            return CodeBlock(SourceSpan.None, name, typeof(object));
        }

        public static CodeBlock CodeBlock(string name, Type returnType) {
            return CodeBlock(SourceSpan.None, name, returnType);
        }

        public static CodeBlock CodeBlock(SourceSpan span, string name) {
            return CodeBlock(span, name, typeof(object));
        }

        public static CodeBlock CodeBlock(SymbolId name) {
            return CodeBlock(SourceSpan.None, SymbolTable.IdToString(name), typeof(object));
        }

        public static CodeBlock CodeBlock(SymbolId name, Type returnType) {
            return CodeBlock(SourceSpan.None, SymbolTable.IdToString(name), returnType);
        }

        public static CodeBlock CodeBlock(SourceSpan span, SymbolId name) {
            return CodeBlock(span, SymbolTable.IdToString(name), typeof(object));
        }

        public static CodeBlock CodeBlock(SourceSpan span, string name, Type returnType) {
            Contract.RequiresNotNull(name, "name");
            Contract.RequiresNotNull(returnType, "returnType");
            return new CodeBlock(AstNodeType.CodeBlock, span, name, returnType);
        }

        public static CodeBlock EventHandlerBlock(string name, EventInfo eventInfo) {
            Contract.RequiresNotNull(name, "name");
            Contract.RequiresNotNull(eventInfo, "eventInfo");

            ParameterInfo returnInfo;
            ParameterInfo[] parameterInfos;

            ReflectionUtils.GetDelegateSignature(eventInfo.EventHandlerType, out parameterInfos, out returnInfo);

            CodeBlock result = Ast.CodeBlock(name, returnInfo.ParameterType);
            for (int i = 0; i < parameterInfos.Length; i++) {
                result.AddParameter(Variable.Parameter(result, SymbolTable.StringToId("$" + i), parameterInfos[i].ParameterType));
            }

            return result;
        }
    }
}
