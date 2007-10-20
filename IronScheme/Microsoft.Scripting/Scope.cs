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
using System.Threading;
using System.Diagnostics;
using System.Reflection;
using Microsoft.Scripting.Ast;

namespace Microsoft.Scripting {
    /// <summary>
    /// Represents a context of execution.  A context of execution has a set of variables
    /// associated with it (its dictionary) and a parent context.  
    /// 
    /// When looking up a name from a context first the local context is searched.  If the
    /// name is not found there the name lookup will be done against the parent context.
    /// 
    /// Scopes can have language-sensitive variables that are only exposed to a single
    /// language based upon its calling context.  When searching the
    /// language-sensitive dictionary is searched first.  If no matches are found the lookup
    /// is delegated back to the LanguageContext.  If the LanguageContext fails to lookup
    /// the name it delegates back to the (Host or ScriptEnvironment?)
    /// 
    /// Each member of the Scope can optionally have a certain set of attributes associated
    /// with it (ScopeMemberAttributes).  These permit members of the scope to be read-only,
    /// non-deletable, or hidden from enumeration.
    /// 
    /// Scopes, like IAttrbibuteCollections, support both being indexed by SymbolId for fast
    /// access as well as being indexed by object.  The preferred access is via SymbolId and
    /// object access is provided for languages which require additional semantics.  All
    /// features supported for feature IDs are also supported for objects (e.g. context-sentsitivity
    /// and attributes) but the object API does not contain all the same sets of overloads provided
    /// for convenience.
    /// 
    /// TODO: Thread safety
    /// </summary>
    public class Scope {
        private Scope _parent;
        private IAttributesCollection _dict;
        private ScopeAttributeDictionary _attrs;
        private ContextSensitiveScope _contextScopes;
        private IDictionary<Variable,object> _temps;
        private bool _isVisible;
        private SourceLocation _sourceLocation;

        /// <summary>
        /// Creates a new top-level scope with a new empty dictionary.  The scope
        /// is marked as being visible.
        /// </summary>
        public Scope()
            : this(null, null) {
        }

        /// <summary>
        /// Creates a new top-level Scope with the provided dictionary
        /// </summary>
        /// <param name="dictionary"></param>
        public Scope(IAttributesCollection dictionary)
            : this(null, dictionary) {
        }

        /// <summary>
        /// Creates a new Scope with the provided parent and dictionary.
        /// </summary>
        /// <param name="parent"></param>
        /// <param name="dictionary"></param>
        public Scope(Scope parent, IAttributesCollection dictionary)
            : this(parent, dictionary, true) {
        }

        /// <summary>
        /// Creates a new Scope with the provided parent, dictionary and visibility.
        /// </summary>
        public Scope(Scope parent, IAttributesCollection dictionary, bool isVisible) {
            _parent = parent;
            _dict = dictionary ?? new SymbolDictionary();
            _isVisible = isVisible;
            _temps = null;
        }

        /// <summary>
        /// Gets the parent of this Scope or null if the Scope has no parent.
        /// </summary>
        public Scope Parent {
            get {
                return _parent;
            }
        }

        /// <summary>
        /// Gets if the context is visible at this scope.  Visibility is a per-language feature that enables
        /// languages to include members in the Scope chain but hide them when directly exposed to the user.
        /// </summary>
        public bool IsVisible {
            get {
                return _isVisible;
            }
        }

        public SourceLocation SourceLocation {
            get {
                return _sourceLocation;
            }
            set {
                if (value != SourceLocation.Invalid && value != SourceLocation.None) {
                    _sourceLocation = value;
                }
            }
        }

        /// <summary>
        /// Gets the current container for temporary variables. These might need to be nested in a manner
        /// different than the Scope objects, so separate functions exist for pushing and popping them relative
        /// to the current scope.
        /// </summary>
        internal IDictionary<Ast.Variable, object> TemporaryStorage {
            get {
                if (_temps == null) {
                    _temps = new Dictionary<Variable,object>();
                }
                return _temps;
            }
        }

        /// <summary>
        /// Create a context for keeping track of allocated temporary variables inside of TemporaryStorage.
        /// When this function is called, the variables given by paramVars will be set to the values given by paramValues;
        /// when the returned object is disposed of, the supplied paramVars and tempVars variables will be removed from temporary storage.
        /// </summary>
        internal IDisposable TemporaryVariableContext(Variable[] tempVars, Variable[] paramVars, object[] paramValues) {
            return new TemporaryContextHelper(TemporaryStorage, tempVars, paramVars, paramValues);
        }

        // Keep track of temporary variables and parameters set in a given context (e.g. during the evaluation of a rule).
        // If we did not do so, we would break object lifetimes (and thus __del__ methods) by unnecessarily holding onto references inside
        // the scope's TemporaryStorage.
        private class TemporaryContextHelper : IDisposable {
            IDictionary<Variable, object> _temps;
            Variable[] _paramVars;
            Variable[] _tempVars;

            public TemporaryContextHelper(IDictionary<Variable, object> temps, Variable[] tempVars, Variable[] paramVars, object[] paramValues) {
                _temps = temps;
                _paramVars = paramVars;
                _tempVars = tempVars;
                for (int i = 0; i < paramValues.Length; i++) {
                    _temps[paramVars[i]] = paramValues[i];
                }
            }

            public void Dispose() {
                foreach (Variable v in _paramVars) {
                    _temps.Remove(v);
                }
                foreach (Variable v in _tempVars) {
                    _temps.Remove(v);
                }

                GC.SuppressFinalize(this);
            }
        }

        /// <summary>
        /// Returns the list of keys which are available to all languages.  Keys marked with the
        /// DontEnumerate flag will not be returned.
        /// </summary>
        public IEnumerable<SymbolId> Keys {
            get {
                foreach (SymbolId si in _dict.SymbolAttributes.Keys) {
                    if (_attrs == null || _attrs.CheckEnumerable(si)) yield return si;
                }
            }
        }

        /// <summary>
        /// Returns the list of Keys and Items which are available to all langauges.  Keys marked
        /// with the DontEnumerate flag will not be returned.
        /// </summary>
        public IEnumerable<KeyValuePair<SymbolId, object>> Items {
            get {
                foreach (KeyValuePair<SymbolId, object> kvp in _dict.SymbolAttributes) {
                    if (_attrs == null || _attrs.CheckEnumerable(kvp.Key)) {
                        yield return kvp;
                    }
                }
            }
        }

        /// <summary>
        /// Returns the list of Keys available to all languages in addition to those keys
        /// which are only available to the provided LanguageContext.
        /// 
        /// Keys marked with the DontEnumerate flag will not be returned.
        /// </summary>
        public IEnumerable<SymbolId> GetKeys(LanguageContext context) {
            foreach (SymbolId si in _dict.SymbolAttributes.Keys) {
                if (_attrs == null || _attrs.CheckEnumerable(si)) yield return si;
            }

            if (_contextScopes != null) {
                foreach (KeyValuePair<object, object> kvp in _contextScopes.GetItems(context)) {
                    if (kvp.Key is SymbolId) {
                        if (_dict.ContainsKey((SymbolId)kvp.Key)) continue;

                        yield return (SymbolId)kvp.Key;
                    }
                }
            }
        }

        /// <summary>
        /// Trys to lookup the provided name in the current scope.
        /// </summary>
        public bool TryGetName(SymbolId name, out object value) {
            return TryGetName(InvariantContext.Instance, name, out value);
        }

        /// <summary>
        /// Trys to lookup the provided name in the current scope.  Search includes
        /// names that are only visible to the provided LanguageContext.
        /// </summary>
        public bool TryGetName(LanguageContext context, SymbolId name, out object value) {
            if (_contextScopes != null) {
                if (_contextScopes.TryGetName(context, name, out value)) {
                    return true;
                }
            }

            if (_dict.TryGetValue(name, out value)) return true;

            value = null;
            return false;
        }

        /// <summary>
        /// Trys to lookup the provided name in the current scope's context specific dictionary.  
        /// Search includes names that are only visible to the provided LanguageContext.
        /// </summary>
        public bool TryGetNameForContext(LanguageContext context, SymbolId name, out object value) {
            if (_contextScopes != null) {
                if (_contextScopes.TryGetName(context, name, out value)) {
                    return true;
                }
            }

            value = null;
            return false;
        }

        /// <summary>
        /// Attempts to lookup the provided name in this scope or any outer scope.   
        /// </summary>
        public bool TryLookupName(SymbolId name, out object value) {
            return TryLookupName(InvariantContext.Instance, name, out value);
        }

        /// <summary>
        /// Attempts to lookup the provided name in this scope or any outer scope.   Lookup
        /// includes searching for names that are only visible to the provided LanguageContext.
        /// </summary>
        public bool TryLookupName(LanguageContext context, SymbolId name, out object value) {
            Scope curScope = this;
            do {
                if (curScope == this || curScope.IsVisible) {
                    if (curScope.TryGetName(context, name, out value)) {
                        return true;
                    }
                }

                curScope = curScope.Parent;
            } while (curScope != null);

            value = null;
            return false;
        }

        /// <summary>
        /// Attempts to lookup the provided name in this scope or any outer scope.   If the
        /// name is not defined MissingMemberException is thrown.
        /// </summary>
        public object LookupName(SymbolId name) {
            return LookupName(InvariantContext.Instance, name);
        }

        /// <summary>
        /// Attempts to lookup the provided name in this scope or any outer scope.  The
        /// search includes looking for names that are only visible to the provided LanguageContext.
        /// 
        /// If the name is not defined the language defined MissingName exception is thrown.
        /// </summary>
        public object LookupName(LanguageContext context, SymbolId name) {
            object res;
            if (!TryLookupName(context, name, out res)) {
                throw context.MissingName(name);
            }

            return res;
        }
        
        /// <summary>
        /// Sets the name to the specified value for the current context.
        /// </summary>
        /// <exception cref="MemberAccessException">The name has already been published and marked as ReadOnly</exception>
        public void SetName(SymbolId name, object value) {
            if (_attrs != null) _attrs.CheckWritable(name);

            _dict[name] = value;
        }

        /// <summary>
        /// Sets the name to the specified value for the current context.
        /// 
        /// Provides the ScopeMemberAttributes which should be set on the provided object.
        /// </summary>
        /// <exception cref="MemberAccessException">The name has already been published and marked as ReadOnly</exception>
        public void SetName(SymbolId name, object value, ScopeMemberAttributes attributes) {
            if (_attrs != null) _attrs.CheckWritable(name);

            if (_attrs == null) {
                _attrs = new ScopeAttributeDictionary();
            }
            _attrs.Set(name, attributes);
            _dict[name] = value;
        }

        /// <summary>
        /// Sets a name that is only available in the specified context.
        /// </summary>
        /// <exception cref="MemberAccessException">The name has already been published and marked as ReadOnly</exception>
        public void SetName(ContextId context, SymbolId name, object value) {
            if (_attrs != null) _attrs.CheckWritable(name);

            if (context == ContextId.Empty) {
                SetName(name, value);
            } else {
                if (_contextScopes == null) _contextScopes = new ContextSensitiveScope();
                _contextScopes.SetName(context, name, value);
            }
        }

        /// <summary>
        /// Sets a name that is only available in the specified context.
        /// 
        /// Provides the ScopeMemberAttributes which should be set on the provided object.
        /// </summary>
        /// <exception cref="MemberAccessException">The name has already been published and marked as ReadOnly</exception>
        public void SetName(ContextId context, SymbolId name, object value, ScopeMemberAttributes attributes) {
            if (_attrs != null) _attrs.CheckWritable(name);

            if (context == ContextId.Empty) {
                SetName(name, value);
            } else {
                if (_contextScopes == null) _contextScopes = new ContextSensitiveScope();
                _contextScopes.SetName(context, name, value, attributes);
            }
        }

        /// <summary>
        /// Removes all members from the dictionary and any context-sensitive dictionaries.
        /// </summary>
        public void Clear() {
            if (_contextScopes != null) _contextScopes.Clear();

            List<object> ids = new List<object>(_dict.Keys);
            foreach (object name in ids) {
                if (_attrs == null || _attrs.CheckDeletable(name)) {
                    _dict.RemoveObjectKey(name);
                }
            }
        }

        /// <summary>
        /// Determines if this context or any outer scope contains the defined name.
        /// </summary>
        public bool ContainsName(SymbolId name) {
            return ContainsName(InvariantContext.Instance, name);
        }

        /// <summary>
        /// Determines if this context or any outer scope contains the defined name that
        /// is available from the provided LanguageContext.
        /// </summary>
        public bool ContainsName(LanguageContext context, SymbolId name) {
            object tmp;
            return TryLookupName(context, name, out tmp);
        }

        /// <summary>
        /// Removes the provided name from this scope
        /// </summary>
        public void RemoveName(SymbolId name) {
            RemoveName(InvariantContext.Instance, name);
        }

        /// <summary>
        /// Removes the provided name from this scope removing names
        /// visible to both the current context and all contexts.
        /// </summary>
        public bool RemoveName(LanguageContext context, SymbolId name) {
            if (!TryRemoveName(context, name)) {
                throw context.MissingName(name);
            }
            return true;
        }

        /// <summary>
        /// Removes the provided name from this scope removing names
        /// visible to both the current context and all contexts.
        /// </summary>
        public void RemoveNameForContext(LanguageContext context, SymbolId name) {
            if (!TryRemoveForContext(context, name)) {
                throw context.MissingName(name);
            }
        }

        /// <summary>
        /// Attemps to remove the provided name from this scope
        /// </summary>
        public bool TryRemoveName(SymbolId name) {
            return TryRemoveName(InvariantContext.Instance, name);
        }

        /// <summary>
        /// Attemps to remove the provided name from this scope removing names visible
        /// to both the current context and all contexts.
        /// </summary>
        public bool TryRemoveName(LanguageContext context, SymbolId name) {
            bool fRemoved = false;

            if (_contextScopes != null) fRemoved = _contextScopes.TryRemoveName(context, name);
            
            // TODO: Ideally, we could do this without having to do two lookups.
            object removedObject;
            if ((_attrs == null || _attrs.CheckDeletable(name))
                && _dict.TryGetValue(name, out removedObject) && removedObject != Uninitialized.Instance) {
                fRemoved = _dict.Remove(name) || fRemoved;
            }

            return fRemoved;
        }

        /// <summary>
        /// Attemps to remove the provided name from this scope's context specific dictionary
        /// </summary>
        public bool TryRemoveForContext(LanguageContext context, SymbolId name) {
            if (_contextScopes != null) {
                return _contextScopes.TryRemoveName(context, name);
            }

            return false;
        }

        /// <summary>
        /// Gets the outer-most scope associated with this scope.  
        /// </summary>
        public Scope ModuleScope {
            get {
                Scope cur = this;
                while (cur.Parent != null) cur = cur.Parent;

                return cur;
            }
        }

        /// <summary>
        /// Default scope dictionary
        /// </summary>
        public IAttributesCollection Dict {
            get {
                return _dict;
            }
        }

        #region Object key access

        /// <summary>
        /// Attemps to remove the provided object name from this scope removing names visible
        /// to both the current context and all contexts.
        /// </summary>
        public bool TryRemoveObjectName(LanguageContext context, object name) {
            bool fRemoved = false;

            if (_contextScopes != null) fRemoved = _contextScopes.TryRemoveObjectName(context, name);

            if (_attrs == null || _attrs.CheckDeletable(name)) {
                fRemoved = _dict.RemoveObjectKey(name) || fRemoved;
            }

            return fRemoved;
        }

        /// <summary>
        /// Trys to lookup the provided name in the current scope.  Search includes
        /// names that are only visible to the provided LanguageContext.
        /// </summary>
        public bool TryGetObjectName(LanguageContext context, object name, out object value) {
            if (_contextScopes != null) {
                if (_contextScopes.TryGetObjectName(context, name, out value)) {
                    return true;
                }
            }

            if (_dict.TryGetObjectValue(name, out value)) return true;

            value = null;
            return false;
        }

        /// <summary>
        /// Attempts to lookup the provided object name in this scope or any outer scope.   Lookup
        /// includes searching for names that are visible to the provided LanguageContext as well
        /// as those available to all contexts.
        /// </summary>
        public bool TryLookupObjectName(LanguageContext context, object name, out object value) {
            Scope curScope = this;
            do {
                if (curScope == this || curScope.IsVisible) {
                    if (curScope.TryGetObjectName(context, name, out value)) {
                        return true;
                    }
                }

                curScope = curScope.Parent;
            } while (curScope != null);

            value = null;
            return false;
        }

        /// <summary>
        /// Sets the name to the specified value for the current context.
        /// 
        /// The name is an arbitrary object.
        /// </summary>
        public void SetObjectName(ContextId context, object name, object value, ScopeMemberAttributes attributes) {
            int id = context.Id;
            if (id == 0) {
                if (_attrs != null) _attrs.CheckWritable(name);

                _dict.AddObjectKey(name, value);
                if (attributes != ScopeMemberAttributes.None) {
                    if (_attrs == null) _attrs = new ScopeAttributeDictionary();
                    _attrs.Set(name, attributes);
                }
            } else {
                if (_contextScopes == null) _contextScopes = new ContextSensitiveScope();
                _contextScopes.SetObjectName(context, name, value, attributes);
            }
        }

        /// <summary>
        /// Returns the list of Keys available to all languages in addition to those keys
        /// which are only available to the provided LanguageContext.
        /// 
        /// Keys marked with the DontEnumerate flag will not be returned.
        /// </summary>
        public IEnumerable<object> GetAllKeys(LanguageContext context) {
            foreach (object key in _dict.Keys) {
                if (_attrs == null || _attrs.CheckEnumerable(key)) yield return key;
            }

            if (_contextScopes != null) {
                foreach (KeyValuePair<object, object> kvp in _contextScopes.GetItems(context)) {
                    if (_dict.ContainsObjectKey(kvp.Key)) continue;

                    if (_attrs == null || _attrs.CheckEnumerable(kvp.Key)) yield return kvp.Key;
                }
            }
        }

        /// <summary>
        /// Returns the list of Keys and Values available to all languages in addition to those
        /// keys which are only available to the provided LanguageContext.
        /// 
        /// Keys marked with DontEnumerate flag will not be returned.
        /// </summary>
        public IEnumerable<KeyValuePair<object, object>> GetAllItems(LanguageContext context) {
            foreach (KeyValuePair<object, object> kvp in _dict) {
                if (_attrs == null || _attrs.CheckEnumerable(kvp.Key)) {
                    yield return kvp;
                }
            }

            if (_contextScopes != null) {
                // TODO: Filter dups
                foreach (KeyValuePair<object, object> kvp in _contextScopes.GetItems(context)) {
                    if (_attrs == null || _attrs.CheckEnumerable(kvp.Key)) yield return kvp;
                }
            }
        }

        #endregion

        /// <summary>
        /// Helper class to hold onto all the context-sensitive information for a Scope.
        /// </summary>
        private class ContextSensitiveScope {
            private List<IAttributesCollection> _dicts;
            private List<ScopeAttributeDictionary> _attrs;

            public ContextSensitiveScope() {
                _dicts = new List<IAttributesCollection>();
            }

            public void SetName(ContextId context, SymbolId name, object value, ScopeMemberAttributes attrs) {
                int id = context.Id;

                EnsureDictionary(id);
                EnsureAttrDict(id);

                if (_attrs != null && id < _attrs.Count) _attrs[id].CheckWritable(name);

                _dicts[id][name] = value;
                _attrs[id].Set(name, attrs);
            }

            public void SetName(ContextId context, SymbolId name, object value) {
                int id = context.Id;
                EnsureDictionary(id);

                if (_attrs != null && id < _attrs.Count) _attrs[id].CheckWritable(name);

                _dicts[id][name] = value;
            }

            public void SetObjectName(ContextId context, object name, object value, ScopeMemberAttributes attrs) {
                int id = context.Id;

                EnsureDictionary(id);
                if (attrs != ScopeMemberAttributes.None) EnsureAttrDict(id);

                if (_attrs != null && id < _attrs.Count) _attrs[id].CheckWritable(name);

                _dicts[id].AddObjectKey(name, value);
                if (attrs != ScopeMemberAttributes.None) _attrs[id].Set(name, attrs);
            }

            public bool TryGetObjectName(LanguageContext context, object name, out object value) {
                int id = context.ContextId.Id;
                if (id < _dicts.Count && _dicts[id] != null) {
                    if (_dicts[id].TryGetObjectValue(name, out value)) {
                        return true;
                    }
                }

                value = null;
                return false;
            }

            public bool TryGetName(LanguageContext context, SymbolId name, out object value) {
                int id = context.ContextId.Id;
                if (id < _dicts.Count && _dicts[id] != null) {
                    if (_dicts[id].TryGetValue(name, out value)) {
                        return true;
                    }
                }

                value = null;
                return false;
            }

            public bool TryRemoveName(LanguageContext context, SymbolId name) {
                bool fRemoved = false;
                int id = context.ContextId.Id;

                if (id < _dicts.Count && _dicts[id] != null) {
                    if (_attrs == null || id > _attrs.Count || _attrs[id].CheckDeletable(name)) {
                        fRemoved = _dicts[id].Remove(name);
                    }
                }

                return fRemoved;
            }

            public bool TryRemoveObjectName(LanguageContext context, object name) {
                bool fRemoved = false;
                int id = context.ContextId.Id;

                if (id < _dicts.Count && _dicts[id] != null) {
                    if (_attrs == null || id > _attrs.Count || _attrs[id].CheckDeletable(name)) {
                        fRemoved = _dicts[id].RemoveObjectKey(name);
                    }
                }

                return fRemoved;
            }

            public IEnumerable<KeyValuePair<object, object>> GetItems(LanguageContext lc) {
                int id = lc.ContextId.Id;

                if (id < _dicts.Count && _dicts[id] != null) {
                    foreach (KeyValuePair<object, object> kvp in _dicts[id]) {
                        if (_attrs == null || id > _attrs.Count || _attrs[id].CheckEnumerable(kvp.Key)) {
                            yield return kvp;
                        }
                    }
                }
                yield break;
            }

            public void Clear() {
                if (_attrs == null) {
                    // fast clear, no attributes preventing us from clearing
                    _dicts = new List<IAttributesCollection>();
                    return;
                }

                for (int i = 0; i < _dicts.Count; i++) {
                    if (_attrs[i] == null) {
                        // no per-context attributes preventing us from clearing
                        _dicts[i] = null;
                        continue;
                    }

                    // need to check each key
                    if (_dicts[i] != null) {
                        List<SymbolId> keys = new List<SymbolId>(_dicts[i].SymbolAttributes.Keys);
                        foreach (SymbolId key in keys) {
                            if (_attrs == null || i > _attrs.Count || _attrs[i].CheckDeletable(key)) {
                                _dicts[i].Remove(key);
                            }
                        }
                    }
                }
            }

            private void EnsureDictionary(int id) {
                while (_dicts.Count <= id) {
                    _dicts.Add(null);
                }
                if (_dicts[id] == null) {
                    _dicts[id] = new SymbolDictionary();
                }
            }

            private void EnsureAttrDict(int id) {
                if (_attrs == null) _attrs = new List<ScopeAttributeDictionary>();

                while (_attrs.Count <= id) {
                    _attrs.Add(null);
                }
                if (_attrs[id] == null) {
                    _attrs[id] = new ScopeAttributeDictionary();
                }
            }
        }

        /// <summary>
        /// Helper class to hold the attributes for both SymbolId and object attributes.
        /// </summary>
        private class ScopeAttributeDictionary {
            private Dictionary<SymbolId, ScopeMemberAttributes> _attrs;
            private Dictionary<object, ScopeMemberAttributes> _objectAttrs;

            public ScopeAttributeDictionary() {
            }

            public void Set(SymbolId name, ScopeMemberAttributes value) {
                if (_attrs == null) _attrs = new Dictionary<SymbolId, ScopeMemberAttributes>();

                _attrs[name] = value;
            }

            public void Set(object name, ScopeMemberAttributes value) {
                if (_objectAttrs == null) _objectAttrs = new Dictionary<object, ScopeMemberAttributes>();

                _objectAttrs[name] = value;
            }

            public void CheckWritable(SymbolId name) {
                ScopeMemberAttributes scopeAttrs;
                if (_attrs != null && _attrs.TryGetValue(name, out scopeAttrs) && (scopeAttrs & ScopeMemberAttributes.ReadOnly) != 0) {
                    throw new MemberAccessException("can only write to member " + SymbolTable.IdToString(name));
                }
            }

            public bool CheckDeletable(SymbolId name) {
                ScopeMemberAttributes scopeAttrs;
                if (_attrs != null && _attrs.TryGetValue(name, out scopeAttrs) && (scopeAttrs & ScopeMemberAttributes.DontDelete) != 0) {
                    return false;
                }
                return true;
            }

            public bool CheckEnumerable(SymbolId name) {
                ScopeMemberAttributes scopeAttrs;
                return _attrs == null ||
                    !_attrs.TryGetValue(name, out scopeAttrs) ||
                    (scopeAttrs & ScopeMemberAttributes.DontEnumerate) == 0;
            }

            public void CheckWritable(object name) {
                if (name is SymbolId) {
                    CheckWritable((SymbolId)name);
                    return;
                }

                ScopeMemberAttributes scopeAttrs;
                if (_objectAttrs != null) {
                    if (_objectAttrs.TryGetValue(name, out scopeAttrs) && (scopeAttrs & ScopeMemberAttributes.ReadOnly) != 0) {
                        throw new MemberAccessException("can only write to member " + name.ToString());
                    }
                } else {
                    string stringName = name as string;
                    if (stringName != null) {
                        CheckWritable(SymbolTable.StringToId(stringName));
                    }
                }
            }

            public bool CheckDeletable(object name) {
                if (name is SymbolId) return CheckDeletable((SymbolId)name);

                ScopeMemberAttributes scopeAttrs;
                if (_objectAttrs != null){
                    return !_objectAttrs.TryGetValue(name, out scopeAttrs) ||
                            (scopeAttrs & ScopeMemberAttributes.DontDelete) == 0;
                } else {
                    string stringName = name as string;
                    if (stringName != null) {
                        return CheckDeletable(SymbolTable.StringToId(stringName));  
                    }
                }
                return true;
            }

            public bool CheckEnumerable(object name) {
                if (name is SymbolId) return CheckEnumerable((SymbolId)name);

                ScopeMemberAttributes scopeAttrs;
                if (_objectAttrs != null) {
                    return !_objectAttrs.TryGetValue(name, out scopeAttrs) ||
                        (scopeAttrs & ScopeMemberAttributes.DontEnumerate) == 0;
                } else {
                    string stringName = name as string;
                    if (stringName != null) {
                        return CheckEnumerable(SymbolTable.StringToId(stringName));  
                    }
                }
                return true;
            }
        }
    }

}
