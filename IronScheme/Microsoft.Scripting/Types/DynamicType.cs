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
using System.Globalization;
using System.Threading;
using System.Reflection;

using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting.Ast;

namespace Microsoft.Scripting.Types {
    using Ast = Microsoft.Scripting.Ast.Ast;

    /// <summary>
    /// Represents a DynamicType.  Instances of DynamicType are created via DynamicTypeBuilder.  
    /// </summary>
#if !SILVERLIGHT
    [DebuggerDisplay("DynamicType: {Name}")]
#endif
    public class DynamicType : DynamicMixin, IConstructorWithCodeContext, IDynamicObject {
        private List<DynamicType> _bases;                   // the base classes of the type
        private ICallableWithCodeContext _ctor;             // fast implementation of ctor
        private List<WeakReference> _subtypes;              // all of the subtypes of the DynamicTypeect 
        private Type _underlyingSystemType;                 // the underlying CLI system type for this type
        private Type _extensionType;                        // a type that can be extended but acts like the underlying system type
        private Type _impersonationType;                    // the type we should pretend to be
        private List<ConversionInfo> _conversions;          // list of built-in conversions 
        private List<bool> _allowKeywordCtor;               // true if a context disallows keyword args constructing the type.
        private bool _extended;

        private static Dictionary<Type, DynamicType> _dynamicTypes = new Dictionary<Type, DynamicType>();
        internal static DynamicType _dynamicTypeType = DynamicHelpers.GetDynamicTypeFromType(typeof(DynamicType));
        private static WeakReference[] _emptyWeakRef = new WeakReference[0];

        public DynamicType(Type underlyingSystemType) {
            _bases = new List<DynamicType>(1);
            UnderlyingSystemType = underlyingSystemType;
        }

        /// <summary>
        /// Gets the dynamic type that corresponds with the provided static type. 
        /// 
        /// Returns null if no type is available.  TODO: In the future this will
        /// always return a DynamicType created by the DLR.
        /// </summary>
        /// <param name="type"></param>
        /// <returns></returns>
        public static DynamicType GetDynamicType(Type type) {
            lock (_dynamicTypes) {
                DynamicType res;
                if (_dynamicTypes.TryGetValue(type, out res)) return res;
            }

            return null;
        }

        /// <summary>
        /// Sets the dynamic type that corresponds with the given CLR type.
        /// 
        /// Deprecated on introduction and will be removed.
        /// </summary>
        /// <param name="type"></param>
        /// <param name="dynamicType"></param>
        public static DynamicType SetDynamicType(Type type, DynamicType dynamicType) {
            Contract.RequiresNotNull(dynamicType, "dynamicType");

            lock (_dynamicTypes) {
                // HACK: Work around until Ops doesn't have SaveDynamicType and this is entirely thread safe.
                DynamicType res;
                if (_dynamicTypes.TryGetValue(type, out res)) return res;

                _dynamicTypes[type] = dynamicType;
            }
            return dynamicType;
        }

        /// <summary>
        /// Creates an instance of the dynamic type and runs appropriate class initialization
        /// </summary>
        public object CreateInstance(CodeContext context, params object[] args) {
            Contract.RequiresNotNull(args, "args");

            Initialize();

            if (_ctor != null) {
                return _ctor.Call(context, args);
            }
#if SILVERLIGHT
            throw new MissingMethodException(Name + ".ctor");
#else
            throw new MissingMethodException(Name, ".ctor");
#endif
        }

        /// <summary>
        /// Creats an instance of the object using keyword parameters.
        /// </summary>
        public object CreateInstance(CodeContext context, object[] args, string[] names) {
            Contract.RequiresNotNull(names, "names");
            Contract.RequiresNotNull(names, "names");

            Initialize();

            IFancyCallable ifc = _ctor as IFancyCallable;
            if (ifc == null) throw new InvalidOperationException(
                String.Format(
                    CultureInfo.CurrentCulture,
                    Resources.KeywordCreateUnavailable,
                    _ctor.GetType()));

            return ifc.Call(context, args, names);
        }

        public bool CanConvertTo(Type to) {
            Initialize();

            if (_conversions != null) {
                for (int i = 0; i < _conversions.Count; i++) {
                    if (_conversions[i].To == to) {
                        return true;
                    }
                }
            }
            return false;
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1007:UseGenericsWhereAppropriate")]
        public bool TryConvertTo(object value, DynamicType to, out object result) {
            Initialize();

            if (_conversions != null) {
                for (int i = 0; i < _conversions.Count; i++) {
                    if (_conversions[i].To == to.UnderlyingSystemType) {
                        result = _conversions[i].Converter(value);
                        return true;
                    }
                }
            }
            result = null;
            return false;
        }

        public bool CanConvertFrom(Type type) {
            Initialize();

            if (_conversions != null) {
                for (int i = 0; i < _conversions.Count; i++) {
                    if (_conversions[i].To == this.UnderlyingSystemType &&
                        _conversions[i].From.IsAssignableFrom(type)) {
                        return true;
                    }
                }
            }
            return false;
        }

        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1007:UseGenericsWhereAppropriate")]
        public bool TryConvertFrom(object value, out object result) {
            Initialize();

            if (_conversions != null) {
                for (int i = 0; i < _conversions.Count; i++) {
                    if (_conversions[i].To == this.UnderlyingSystemType &&
                        _conversions[i].From.IsAssignableFrom(value.GetType())) {

                        result = _conversions[i].Converter(value);
                        return true;
                    }
                }
            }
            result = null;
            return false;
        }

        /// <summary>
        /// Gets the underlying system type that is backing this type.  All instances of this
        /// type are an instance of the underlying system type.
        /// </summary>
        public Type UnderlyingSystemType {
            get {
                // if we already have the underlying system type don't
                // do a full initialization.  This saves us several type inits
                // on startup.
                if (_underlyingSystemType == null) {
                    Initialize();
                }

                return _underlyingSystemType;
            }
            set {
                _underlyingSystemType = value;
            }
        }

        /// <summary>
        /// Gets the extension type for this type.  The extension type provides
        /// extra methods that logically appear on this type.
        /// </summary>
        public Type ExtensionType {
            get {
                if (_extensionType == null) {
                    return _underlyingSystemType;
                }
                return _extensionType;
            }
            internal set {
                _extensionType = value;
            }
        }

        /// <summary>
        /// Substitutes the provided types for the parameters of the generic type definition and
        /// returns a new DynamicType for the constructed type.
        /// </summary>
        public Type MakeGenericType(params DynamicType[] types) {
            Initialize();

            Contract.RequiresNotNull(types, "types");
            if (!UnderlyingSystemType.ContainsGenericParameters)
                throw new InvalidOperationException(Resources.InvalidOperation_MakeGenericOnNonGeneric);

            Type[] sysTypes = new Type[types.Length];
            for (int i = 0; i < sysTypes.Length; i++) sysTypes[i] = types[i].UnderlyingSystemType;

            //!!! propagate attributes?
            return _underlyingSystemType.MakeGenericType(sysTypes);
        }

        /// <summary>
        /// Returns true if the specified object is an instance of this type.
        /// </summary>
        public bool IsInstanceOfType(object instance) {
            Initialize();

            ISuperDynamicObject dyno = instance as ISuperDynamicObject;
            if (dyno != null) {
                return dyno.DynamicType.IsSubclassOf(this);
            }

            return UnderlyingSystemType.IsInstanceOfType(instance);
        }

        /// <summary>
        /// Gets a list of weak references to all the subtypes of this class.  May return null
        /// if there are no subtypes of the class.
        /// </summary>
        public IList<WeakReference> SubTypes {
            get {
                if (_subtypes == null) return _emptyWeakRef;

                lock (_subtypes) return _subtypes.ToArray();
            }
        }
        
        /// <summary>
        /// Gets a description of where this type from.  This may include if it's a system type,
        /// language specific information, or other information.  This should be used for displaying
        /// information about the type to the user.  For programmatic discovery other properties should
        /// be used.
        /// </summary>
        public string TypeCategory {
            get {
                if (IsSystemType) return "built-in type";

                return "user-type"; //!!! language info as well?
            }
        }

        /// <summary>
        /// Gets the base types from which this type inherits.
        /// </summary>
        public IList<DynamicType> BaseTypes {
            get {
                Initialize();

                lock (_bases) return _bases.ToArray();
            }
            internal set {
                // validate input...
                foreach (DynamicType dt in value) {
                    if (dt == null) throw new ArgumentNullException("value", "a DynamicType was null while assigning base classes");
                }

                // first update our sub-type list

                lock (_bases) {
                    foreach (DynamicType dt in _bases) {
                        dt.RemoveSubType(this);
                    }

                    // set the new bases
                    List<DynamicType> newBases = new List<DynamicType>(value);

                    // add us as subtypes of our new bases
                    foreach (DynamicType dt in newBases) {
                        dt.AddSubType(this);
                    }

                    UpdateVersion();
                    _bases = newBases;
                    
                }
            }
        }

        /// <summary>
        /// Returns true if this type is a subclass of other
        /// </summary>
        public bool IsSubclassOf(DynamicType other) {
            Initialize();

            // check for a type match
            if (other.CanonicalDynamicType == this.CanonicalDynamicType) {
                return true;
            }

            //Python doesn't have value types inheriting from ValueType, but we fake this for interop
            if (other.UnderlyingSystemType == typeof(ValueType) && UnderlyingSystemType.IsValueType) {
                return true;
            }

            // check for a match on UnderlyingSystemType if other is
            // a system type.
            if ((other._attrs & DynamicTypeAttributes.SystemType) != 0) {
                if (UnderlyingSystemType == other.UnderlyingSystemType) {
                    return true;
                }
            }

            // check the type hierarchy
            List<DynamicType> bases = _bases;
            for (int i = 0; i < bases.Count; i++) {
                DynamicType baseClass = bases[i];

                if (baseClass.IsSubclassOf(other)) return true;
            }

            // check for our impersonation type
            if (_impersonationType != null) {
                if (_impersonationType == other.UnderlyingSystemType ||
                    _impersonationType.IsSubclassOf(other.UnderlyingSystemType))
                    return true;
            }

            return false;
        }

        /// <summary>
        /// True if the type is a system type.  A system type is a type which represents an
        /// underlying .NET type and not a subtype of one of these types.
        /// </summary>
        public bool IsSystemType {
            get {
                return (_attrs & DynamicTypeAttributes.SystemType) != 0;
            }
            internal set {
                if (value) _attrs |= DynamicTypeAttributes.SystemType;
                else _attrs &= (~DynamicTypeAttributes.SystemType);
            }
        }

        /// <summary>
        /// Gets the type that this type impersonates.  This type will appear to
        /// be equivalent to the type it's impersonating.  Returns null if 
        /// the type doesn't impersonate a type.
        /// </summary>
        public Type ImpersonationType {
            get {
                Initialize();

                return _impersonationType;
            }
            internal set {
                _impersonationType = value;
            }
        }

        /// <summary>
        /// Always returns this type - unless this type is impersonating another one.  In
        /// that case it will return the type that is being impersonated.
        /// </summary>
        public DynamicType CanonicalDynamicType {
            get {
                if (ImpersonationType != null) {
                    return DynamicHelpers.GetDynamicTypeFromType(ImpersonationType);
                } else {
                    return this;
                }
            }
        }

        /// <summary>
        /// Internal helper function to add a subtype
        /// </summary>
        internal void AddSubType(DynamicType subtype) {
            if (_subtypes == null) {
                Interlocked.CompareExchange<List<WeakReference>>(ref _subtypes, new List<WeakReference>(), null);
            }

            lock (_subtypes) {
                _subtypes.Add(new WeakReference(subtype));
            }
        }

        internal void RemoveSubType(DynamicType subtype) {
            int i = 0;
            if (_subtypes != null) {
                lock (_subtypes) {
                    while (i < _subtypes.Count) {
                        if (!_subtypes[i].IsAlive || _subtypes[i].Target == subtype) {
                            _subtypes.RemoveAt(i);
                            continue;
                        }
                        i++;
                    }
                }
            }
        }

        internal void AddBaseType(DynamicType baseType) {
            if (_bases == null) {
                Interlocked.CompareExchange<List<DynamicType>>(ref _bases, new List<DynamicType>(), null);
            }

            lock (_bases) _bases.Add(baseType);
        }

        internal void AddConversion(Type from, Type to, CallTarget1 conversion) {
            if (_conversions == null) _conversions = new List<ConversionInfo>();

            ConversionInfo ci = new ConversionInfo();
            ci.From = from;
            ci.To = to;
            ci.Converter = conversion;
            _conversions.Add(ci);
        }


        internal void SetConstructor(ICallableWithCodeContext ctor) {
            _ctor = ctor;
        }

        private class ConversionInfo {
            public Type From;
            public Type To;
            public CallTarget1 Converter;

        }

        #region IConstructorWithCodeContext Members

        object IConstructorWithCodeContext.Construct(CodeContext context, params object[] args) {
            return CreateInstance(context, args);
        }

        #endregion

        internal new DynamicTypeBuilder Builder {
            get {
                return (DynamicTypeBuilder)_builder;
            }
            set {
                Debug.Assert(_builder == null || value == null);
                _builder = value;
            }
        }


        public bool IsExtended {
            get {
                return _extended;
            }
            set {
                _extended = value;
            }
        }

        internal void DisallowConstructorKeywordArguments(ContextId context) {
            if (_allowKeywordCtor == null) _allowKeywordCtor = new List<bool>();

            while (_allowKeywordCtor.Count <= context.Id)
                _allowKeywordCtor.Add(true);
            _allowKeywordCtor[context.Id] = false;
        }

        protected override void UpdateVersion() {
            foreach (WeakReference wr in SubTypes) {
                if (wr.IsAlive) {
                    ((DynamicType)wr.Target).UpdateVersion();
                }
            }

            base.UpdateVersion();
        }

        #region Object overrides

        // TODO Remove this method.  It was needed when Equals was overridden to depend on 
        // the impersonationType, but now that that's gone this can probably go as well.
        // Currently only pickling depends on this where identity of types is checked.
        public override int GetHashCode() {
            if (_impersonationType == null) return ~_underlyingSystemType.GetHashCode();

            return ~_impersonationType.GetHashCode();
        }

        #endregion

        #region IDynamicObject Members

        public LanguageContext LanguageContext {
            get { return InvariantContext.Instance; }
        }

        public StandardRule<T> GetRule<T>(DynamicAction action, CodeContext context, object[] args) {
            if (action.Kind == DynamicActionKind.CreateInstance) {
                if (IsSystemType) {
                    MethodBase[] ctors = CompilerHelpers.GetConstructors(UnderlyingSystemType);
                    StandardRule<T> rule;
                    if (ctors.Length > 0) {
                        rule = new CallBinderHelper<T, CallAction>(context, (CallAction)action, args, ctors).MakeRule();
                    } else {
                        rule = new StandardRule<T>();
                        rule.SetTarget(
                           rule.MakeError(context.LanguageContext.Binder,
                               Ast.New(
                                   typeof(ArgumentTypeException).GetConstructor(new Type[] { typeof(string) }),
                                   Ast.Constant("Cannot create instances of " + Name)
                               )
                           )
                       );
                    }
                    rule.AddTest(Ast.Equal(rule.Parameters[0], Ast.RuntimeConstant(args[0])));
                    return rule;
                } else {
                    // TODO: Pull in the Python create logic for this when DynamicType moves out of MS.Scripting, this provides
                    // a minimal level of interop until then.
                    StandardRule<T> rule = new StandardRule<T>();
                    Expression call = Ast.Call(
                        Ast.Convert(rule.Parameters[0], typeof(IConstructorWithCodeContext)), 
                        typeof(IConstructorWithCodeContext).GetMethod("Construct"), 
                        ArrayUtils.Insert((Expression)Ast.CodeContext(), ArrayUtils.RemoveFirst(rule.Parameters)));

                    rule.SetTarget(rule.MakeReturn(context.LanguageContext.Binder, call));
                    rule.SetTest(Ast.Equal(rule.Parameters[0], Ast.RuntimeConstant(args[0])));
                    return rule;
                }
            }
            return null;
        }

        #endregion

        public static implicit operator Type(DynamicType self) {
            return self.UnderlyingSystemType;
        }

        public static implicit operator TypeTracker(DynamicType self) {
            return ReflectionCache.GetTypeTracker(self.UnderlyingSystemType);
        }
    }
}
