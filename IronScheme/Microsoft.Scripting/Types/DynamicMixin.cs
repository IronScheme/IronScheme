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
using System.Threading;

using System.Reflection;
using System.Globalization;
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Types {
    public delegate bool TryGetMemberCustomizer(CodeContext context, object instance, SymbolId name, out object value);
    public delegate void SetMemberCustomizer(CodeContext context, object instance, SymbolId name, object value);
    public delegate void DeleteMemberCustomizer(CodeContext context, object instance, SymbolId name);
    
    public delegate DynamicTypeSlot CreateTypeSlot(object value);

    public delegate bool UnaryOperator(CodeContext context, object self, out object res);
    public delegate bool BinaryOperator(CodeContext context, object self, object other, out object res);
    public delegate bool TernaryOperator(CodeContext context, object self, object value1, object value2, out object res);

#if !SILVERLIGHT
    [DebuggerDisplay("DynamicMixin: {Name}")]
#endif
    public class DynamicMixin : ICustomMembers {
        private string _name;                               // the name of the type
        internal DynamicTypeAttributes _attrs;              // attributes of the type
        private List<DynamicMixin> _resolutionOrder;        // the search order for methods in the type
        private Dictionary<SymbolId, SlotInfo> _dict;       // type-level slots & attributes
        private VTable _operators;                          // table of operators for fast dispatch    
        private ContextId _context;                         // the context this type was created from
        internal DynamicMixinBuilder _builder;              // the builder who created this, or null if we're fully initialized        
        private TryGetMemberCustomizer _getboundmem;        // customized delegate for getting a member
        private SetMemberCustomizer _setmem;                // customized delegate for setting a member
        private DeleteMemberCustomizer _delmem;             // customized delegate fr deleting values.
        private CreateTypeSlot _slotCreator;                // used for creating default value slot (used for Python user types so we can implement user-defined descriptor protocol).
        private List<object> _contextTags;                  // tag info specific to the context
        private int _version = GetNextVersion();            // version of the type
        private int _altVersion;                            // the alternate version of  the type, when the version is DynamicVersion
        private bool _hasGetAttribute;                      // true if the type has __getattribute__, false otherwise.
        private static DynamicType _nullType = DynamicHelpers.GetDynamicTypeFromType(typeof(None));


        public const int DynamicVersion = Int32.MinValue;   // all lookups should be dynamic
        private static int MasterVersion = 1, MasterAlternateVersion;

        /// <summary>
        /// This will return a unique integer for every version of every type in the system.
        /// This means that DynamicSite code can generate a check to see if it has the correct
        /// DynamicType and version with a single integer compare.
        /// 
        /// TODO - This method and related code should fail gracefully on overflow.
        /// </summary>
        private static int GetNextVersion() {
            if (MasterVersion < 0) {
                throw new InvalidOperationException(Resources.TooManyVersions);
            }
            return Interlocked.Increment(ref MasterVersion);
        }

        private static int GetNextAlternateVersion() {
            if (MasterAlternateVersion  < 0) {
                throw new InvalidOperationException(Resources.TooManyVersions);
            }
            return Interlocked.Increment(ref MasterAlternateVersion);
        }

        public void Mutate() {
            UpdateVersion();
        }

        protected virtual void UpdateVersion() {
            if (_version != DynamicVersion) {
                _version = GetNextVersion();
                _altVersion = 0;
            } else {
                _altVersion = GetNextAlternateVersion();
            }
        }
                
        #region Constructors

        /// <summary>
        /// Creates an instance of an empty dynamic mixin.  The mixin will then be customized using
        /// the DynamicMixinBuilder.
        /// </summary>
        public DynamicMixin() {
            _resolutionOrder = new List<DynamicMixin>(1);
            _resolutionOrder.Add(this);
        }

        #endregion

        #region Public API Surface
      
        /// <summary>
        /// Looks up a slot on the dynamic type
        /// Just looks into the context sensitive slots.
        /// </summary>
        public bool TryLookupContextSlot(CodeContext context, SymbolId name, out DynamicTypeSlot slot) {
            Initialize();

            if (context.LanguageContext.ContextId == ContextId.Empty) {
                // Cannot pass the default context id here
                throw new ArgumentException();
            }

            SlotInfo si;
            bool success = _dict.TryGetValue(name, out si);

            if (success) {
                // check for a context-sensitive slot only
                if (si.SlotValues != null && si.SlotValues.Count > context.LanguageContext.ContextId.Id) {
                    slot = si.SlotValues[context.LanguageContext.ContextId.Id];
                    if (slot != null) return true;
                }
            }

            slot = null;
            return false;
        }

        /// <summary>
        /// Looks up a slot on the dynamic type
        /// </summary>
        public bool TryLookupSlot(CodeContext context, SymbolId name, out DynamicTypeSlot slot) {
            Initialize();

            SlotInfo si;
            if (_dict.TryGetValue(name, out si)) {
                return TryExtractVisibleSlot(context, si, out slot);
            } else {
                slot = null;
                return false;
            }
        }

        private bool TryExtractVisibleSlot(CodeContext context, SlotInfo si, out DynamicTypeSlot slot) {
            // check for a context-sensitive slot first...
            if (si.SlotValues != null && si.SlotValues.Count > context.LanguageContext.ContextId.Id) {
                slot = si.SlotValues[context.LanguageContext.ContextId.Id];
                if (slot != null) {
                    return slot.IsVisible(context, this);
                }
            }

            // then see if we have a normal slot
            if (si.DefaultValue != null && si.DefaultValue.IsVisible(context, this)) {
                slot = si.DefaultValue;
                return true;
            }

            slot = null;
            return false;
        }

        /// <summary>
        /// Searches the resolution order for a slot matching by name
        /// </summary>
        public bool TryResolveSlot(CodeContext context, SymbolId name, out DynamicTypeSlot slot) {
            for(int i = 0; i < _resolutionOrder.Count; i++) {
                DynamicMixin dt = _resolutionOrder[i];

                if (dt.TryLookupSlot(context, name, out slot)) {
                    return true;
                }
            }

            slot = null;
            return false;
        }

        private enum CaseInsensitiveMatch {
            NoMatch,                    // name doesn't exist on the type at all
            ExactMatch,                 // name exists with the exact casing
            InexactMatch,               // there's exactly one match, but with different casing
            AmbiguousMatch              // multiple instances of the different casings, none matches exactly
        }

        /// <summary>
        /// Searches the resolution order for the slots that match the name in case insensitive manner.
        /// </summary>
        public bool TryResolveSlotCaseInsensitive(CodeContext context, SymbolId name, out DynamicTypeSlot slot, out SymbolId actualName) {
            // Initialize the output parameters
            slot = null;
            actualName = SymbolId.Invalid;
            bool ambiguous = false;

            for (int i = 0; i < _resolutionOrder.Count; i++) {
                DynamicTypeSlot candidate;
                SymbolId candidateName;
                switch (_resolutionOrder[i].TryLookupSlotCaseInsensitive(context, name, out candidate, out candidateName)) {
                    case CaseInsensitiveMatch.ExactMatch:
                        // exact match - search is over
                        slot = candidate;
                        actualName = candidateName;
                        return true;

                    case CaseInsensitiveMatch.InexactMatch:
                        // inexact match. If we already have inexact candidate, we have ambiguous lookup,
                        // unless we find exact match later
                        if (slot == null) {
                            // first possible match encountered
                            slot = candidate;
                            actualName = candidateName;
                        } else {
                            // if the name is the same, we are ok, in that case continue to use the first
                            // match we found. If the name doesn't match, we have ambiguous result, unless
                            // we find exact match later.
                            if (candidateName != actualName) {
                                ambiguous = true;
                            }
                        }
                        break;

                    case CaseInsensitiveMatch.AmbiguousMatch:
                        // ambiguous match. We need to find an exact match to succeed.
                        ambiguous = true;
                        break;

                    case CaseInsensitiveMatch.NoMatch:
                        // no match - keep looking with the parent.
                        break;
                }
            }

            if (slot != null && !ambiguous) {
                return true;
            } else {
                slot = null;
                actualName = SymbolId.Invalid;
                return false;
            }
        }

        /// <summary>
        /// Looks up the slots on the dynamic mixin, using case insensitive comparison.
        /// Matching slots are added to the list.
        /// </summary>
        private CaseInsensitiveMatch TryLookupSlotCaseInsensitive(CodeContext context, SymbolId name, out DynamicTypeSlot slot, out SymbolId actualName) {
            bool ambiguous = false;

            // Initialize the result
            slot = null;
            actualName = SymbolId.Invalid;

            foreach (KeyValuePair<SymbolId, SlotInfo> kvp in _dict) {
                DynamicTypeSlot current;
                if (kvp.Key.CaseInsensitiveEquals(name) &&
                    TryExtractVisibleSlot(context, kvp.Value, out current)) {

                    // We have case insensitive match. Is it an exact match?
                    if (kvp.Key == name) {
                        slot = current;
                        actualName = kvp.Key;
                        return CaseInsensitiveMatch.ExactMatch;
                    }

                    // We have case insensitive match only. Is it the first one?
                    if (slot == null) {
                        slot = current;
                        actualName = kvp.Key;
                    } else {
                        // Already have case insensitive match, so unless we find exact match later,
                        // this is an ambiguous match.
                        ambiguous = true;
                    }
                }
            }

            // Do we have at least one candidate?
            if (slot != null) {
                // Do we have more than one?
                if (ambiguous) {
                    slot = null;
                    actualName = SymbolId.Invalid;
                    return CaseInsensitiveMatch.AmbiguousMatch;
                } else {
                    // Exactly one candidate based on insensitive match
                    return CaseInsensitiveMatch.InexactMatch;
                }
            } else {
                // nothing found
                return CaseInsensitiveMatch.NoMatch;
            }
        }

        #region Instance Access Helpers

        public object GetMember(CodeContext context, object instance, SymbolId name) {
            object res;
            if (TryGetMember(context, instance, name, out res)) {
                return res;
            }

            throw new MissingMemberException(String.Format(CultureInfo.CurrentCulture,
                Resources.CantFindMember, 
                SymbolTable.IdToString(name)));
        }

        public object GetBoundMember(CodeContext context, object instance, SymbolId name) {
            object value;
            if (TryGetBoundMember(context, instance, name, out value)) {
                return value;
            }

            throw new MissingMemberException(String.Format(CultureInfo.CurrentCulture,
                Resources.CantFindMember,
                SymbolTable.IdToString(name)));
        }

        public void SetMember(CodeContext context, object instance, SymbolId name, object value) {
            if (TrySetMember(context, instance, name, value)) {
                return;
            }

            throw new MissingMemberException(
                String.Format(CultureInfo.CurrentCulture,
                    Resources.Slot_CantSet, 
                    name));
        }

        public void DeleteMember(CodeContext context, object instance, SymbolId name) {
            if (TryDeleteMember(context, instance, name)) {
                return;
            }

            throw new MissingMemberException(String.Format(CultureInfo.CurrentCulture, "couldn't delete member {0}", name));
        }

        /// <summary>
        /// Gets a value from a dynamic type and any sub-types.  Values are stored in slots (which serve as a level of 
        /// indirection).  This searches the types resolution order and returns the first slot that
        /// contains the value.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1007:UseGenericsWhereAppropriate")]
        public bool TryGetMember(CodeContext context, object instance, SymbolId name, out object value) {
            Initialize();

            //if (_getmem != null) {
            //    return _getmem(context, instance, name, out value);
            //}

            return TryGetNonCustomMember(context, instance, name, out value);
        }

        /// <summary>
        /// Attempts to lookup a member w/o using the customizer.
        /// </summary>
        /// <returns></returns>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1007:UseGenericsWhereAppropriate")]
        public bool TryGetNonCustomMember(CodeContext context, object instance, SymbolId name, out object value) {
            ISuperDynamicObject sdo = instance as ISuperDynamicObject;
            if (sdo != null) {
                IAttributesCollection iac = sdo.Dict;
                if (iac != null && iac.TryGetValue(name, out value)) {
                    return true;
                }
            }

            for (int i = 0; i < _resolutionOrder.Count; i++) {
                DynamicMixin dt = _resolutionOrder[i];

                DynamicTypeSlot slot;
                if (dt.TryLookupSlot(context, name, out slot)) {
                    if (slot.TryGetValue(context, instance, this, out value))
                        return true;
                }
            }

            try {
                if (TryInvokeBinaryOperator(context, Operators.GetBoundMember, instance, name.ToString(), out value)) {
                    return true;
                }
            } catch (MissingMemberException) {
                //!!! when do we let the user see this exception?
            }

            value = null;
            return false;
        }

        /// <summary>
        /// Gets a value from a dynamic type and any sub-types.  Values are stored in slots (which serve as a level of 
        /// indirection).  This searches the types resolution order and returns the first slot that
        /// contains the value.
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1007:UseGenericsWhereAppropriate")]
        public bool TryGetBoundMember(CodeContext context, object instance, SymbolId name, out object value) {
            Initialize();

            if (_getboundmem != null) {
                return _getboundmem(context, instance, name, out value);
            }

            return TryGetNonCustomBoundMember(context, instance, name, out value);
        }


        /// <summary>
        /// Attempts to lookup a member w/o using the customizer.
        /// </summary>
        /// <returns></returns>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1007:UseGenericsWhereAppropriate")]
        public bool TryGetNonCustomBoundMember(CodeContext context, object instance, SymbolId name, out object value) {
            ISuperDynamicObject sdo = instance as ISuperDynamicObject;
            if (sdo != null) {
                IAttributesCollection iac = sdo.Dict;
                if (iac != null && iac.TryGetValue(name, out value)) {
                    return true;
                }
            }

            for (int i = 0; i < _resolutionOrder.Count; i++) {
                DynamicMixin dt = _resolutionOrder[i];

                DynamicTypeSlot slot;
                if (dt.TryLookupSlot(context, name, out slot)) {
                    if (slot.TryGetBoundValue(context, instance, this, out value))
                        return true;
                }
            }

            try {
                if (TryInvokeBinaryOperator(context, Operators.GetBoundMember, instance, name.ToString(), out value)) {
                    return true;
                }
            } catch (MissingMemberException) {
                //!!! when do we let the user see this exception?
            }

            value = null;
            return false;
        }


        /// <summary>
        /// Sets a value on an instance.  If a slot is available in the most derived type the slot
        /// is set there, otherwise the value is stored directly in the instance.
        /// </summary>
        public bool TrySetMember(CodeContext context, object instance, SymbolId name, object value) {
            Initialize();

            if (_setmem != null) {
                _setmem(context, instance, name, value);
                return true;
            }

            return TrySetNonCustomMember(context, instance, name, value);
        }

        /// <summary>
        /// Attempst to set a value w/o going through the customizer.
        /// 
        /// This enables languages to provide the "base" implementation for setting attributes
        /// so that the customizer can call back here.
        /// </summary>
        public bool TrySetNonCustomMember(CodeContext context, object instance, SymbolId name, object value) {
            DynamicTypeSlot slot;
            if (TryResolveSlot(context, name, out slot)) {
                if (slot.TrySetValue(context, instance, this, value)) {
                    return true;
                }
            }

            // set the attribute on the instance
            ISuperDynamicObject sdo = instance as ISuperDynamicObject;
            if (sdo != null) {
                IAttributesCollection iac = sdo.Dict;
                if (iac == null) {
                    iac = new SymbolDictionary();

                    if ((iac = sdo.SetDict(iac))==null) {
                        return false;
                    }
                }

                iac[name] = value;
                return true;
            }

            object dummy;
            return TryInvokeTernaryOperator(context, Operators.SetMember, instance, SymbolTable.IdToString(name), value, out dummy);
        }

        public bool TryDeleteMember(CodeContext context, object instance, SymbolId name) {
            Initialize();

            if (_delmem != null) {
                _delmem(context, instance, name);
                return true;
            }

            return TryDeleteNonCustomMember(context, instance, name);
        }

        public bool TryDeleteNonCustomMember(CodeContext context, object instance, SymbolId name) {
            DynamicTypeSlot slot;
            if (TryResolveSlot(context, name, out slot)) {
                if (slot.TryDeleteValue(context, instance, this)) {
                    return true;
                }
            }

            // set the attribute on the instance
            ISuperDynamicObject sdo = instance as ISuperDynamicObject;
            if (sdo != null) {
                IAttributesCollection iac = sdo.Dict;
                if (iac == null) {
                    iac = new SymbolDictionary();

                    if ((iac = sdo.SetDict(iac))==null) {
                        return false;
                    }
                }

                return iac.Remove(name);
            }

            try {
                object value;
                if (TryInvokeBinaryOperator(context, Operators.DeleteMember, instance, name.ToString(), out value)) {
                    return true;
                }
            } catch (MissingMemberException) {
                //!!! when do we let the user see this exception?
            }


            return false;
        }
        #endregion

        /// <summary>
        /// Returns a list of all slot names for this type.
        /// </summary>
        /// <param name="context">The context that is doing the inquiry of InvariantContext.Instance.</param>
        public IList<SymbolId> GetTypeMembers(CodeContext context) {
            Initialize();

            int ctxId = context.LanguageContext.ContextId.Id;
            Dictionary<SymbolId, SymbolId> keys = new Dictionary<SymbolId, SymbolId>();

            foreach (KeyValuePair<SymbolId, SlotInfo> kvp in _dict) {
                if (keys.ContainsKey(kvp.Key)) continue;

                if (kvp.Value.SlotValues != null) {
                    if (kvp.Value.SlotValues.Count > ctxId &&
                        kvp.Value.SlotValues[ctxId] != null) {

                        if (kvp.Value.SlotValues[ctxId].IsVisible(context, this)) {
                            keys[kvp.Key] = kvp.Key;
                            continue;
                        }
                    }
                }

                if (kvp.Value.DefaultValue != null && kvp.Value.DefaultValue.IsVisible(context, this))
                    keys[kvp.Key] = kvp.Key;
            }

            return new List<SymbolId>(keys.Keys);
        }

        /// <summary>
        /// Returns a list of all slot names for the type and any subtypes.
        /// </summary>
        /// <param name="context">The context that is doing the inquiry of InvariantContext.Instance.</param>
        public IList<SymbolId> GetMemberNames(CodeContext context) {
            return GetMemberNames(context, null);
        }

        /// <summary>
        /// Returns a list of all slot names for the type, any subtypes, and the instance.
        /// </summary>
        /// <param name="context">The context that is doing the inquiry of InvariantContext.Instance.</param>
        /// <param name="self">the instance to get instance members from, or null.</param>
        public IList<SymbolId> GetMemberNames(CodeContext context, object self) {
            //!!! context, object keys ? 
            Initialize();

            int ctxId = context.LanguageContext.ContextId.Id;
            Dictionary<SymbolId, SymbolId> keys = new Dictionary<SymbolId, SymbolId>();

            for (int i = 0; i < _resolutionOrder.Count; i++) {
                DynamicMixin dt = _resolutionOrder[i];
                dt.Initialize();

                foreach (KeyValuePair<SymbolId, SlotInfo> kvp in dt._dict) {                    
                    if (keys.ContainsKey(kvp.Key)) continue;

                    if (kvp.Value.SlotValues != null) {
                        if (kvp.Value.SlotValues.Count > ctxId &&
                            kvp.Value.SlotValues[ctxId] != null) {

                            if (kvp.Value.SlotValues[ctxId].IsVisible(context, this)) {
                                keys[kvp.Key] = kvp.Key;
                            }
                            continue;
                        }
                    }

                    if (kvp.Value.DefaultValue != null && kvp.Value.DefaultValue.IsVisible(context, this))
                        keys[kvp.Key] = kvp.Key;
                }                
            }

            ISuperDynamicObject dyno = self as ISuperDynamicObject;
            if (dyno != null) {
                IAttributesCollection iac = dyno.Dict;
                if (iac != null) {
                    lock (iac) {
                        foreach (SymbolId id in iac.SymbolAttributes.Keys) {
                            keys[id] = id;
                        }
                    }
                }
            }

            object names;
            if (TryInvokeUnaryOperator(context, Operators.GetMemberNames, self, out names)) {
                IList<SymbolId> symNames = names as IList<SymbolId>;
                if (symNames == null) throw new InvalidOperationException(String.Format("GetMemberNames returned bad list: {0}", names));

                foreach (SymbolId si in symNames) {
                    keys[si] = si;
                }
            }

            return new List<SymbolId>(keys.Keys);
        }        

        public IAttributesCollection GetMemberDictionary(CodeContext context) {
            Initialize();

            IAttributesCollection iac = new SymbolDictionary();
            foreach (SymbolId x in _dict.Keys) {             
                if(x.ToString() == "__dict__") continue;

                DynamicTypeSlot dts;
                if (TryLookupSlot(context, x, out dts)) {
                    //??? why check for DTVS?
                    object val;
                    if (dts.TryGetValue(context, null, this, out val)) {
                        if (dts is DynamicTypeValueSlot)
                            iac[x] = val;
                        else
                            iac[x] = dts;
                    }
                }
            }
            return iac;
        }

        public IAttributesCollection GetMemberDictionary(CodeContext context, object self) {
            if (self != null) {
                ISuperDynamicObject sdo = self as ISuperDynamicObject;
                if (sdo != null) return sdo.Dict;

                return null;
            }
            return GetMemberDictionary(context);
        }

        /// <summary>
        /// Attempts to invoke the specific unary operator with the given instance
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1007:UseGenericsWhereAppropriate")]
        public bool TryInvokeUnaryOperator(CodeContext context, Operators op, object self, out object ret) {
            Contract.RequiresNotNull(context, "context"); 
            
            Initialize();

            int opIndex = (int)op;

            if (_operators != null) {
                // check context specific version, if available
                if (_operators.ContextSpecific != null) {
                    ContextId ctxId = context.LanguageContext.ContextId;
                    if (_operators.ContextSpecific.Length > ctxId.Id) {
                        VTable vt = _operators.ContextSpecific[ctxId.Id];

                        if (vt != null && vt.UnaryOperators != null && opIndex < vt.UnaryOperators.Length) {
                            UnaryOperator ct = vt.UnaryOperators[opIndex].Operator;
                            if (ct != null && ct(context, self, out ret)) {
                                return true;
                            }
                        }
                    }
                }

                // check context neutral version
                if (_operators.UnaryOperators != null && opIndex < _operators.UnaryOperators.Length) {
                    UnaryOperator ct = _operators.UnaryOperators[opIndex].Operator;
                    if (ct != null && ct(context, self, out ret)) {
                        return true;
                    }
                }
            }

            ret = null;
            return false;
        }

        /// <summary>
        /// Attempts to invoke the specific binary operator with the given instance
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1007:UseGenericsWhereAppropriate")]
        public bool TryInvokeBinaryOperator(CodeContext context, Operators op, object self, object other, out object ret) {
            Contract.RequiresNotNull(context, "context"); 
            
            Initialize();

            int opIndex = (int)op;

            if (_operators != null) {
                // check context specific version, if available
                if (_operators.ContextSpecific != null) {
                    ContextId ctxId = context.LanguageContext.ContextId;
                    if (_operators.ContextSpecific.Length > ctxId.Id) {
                        VTable vt = _operators.ContextSpecific[ctxId.Id];

                        if (vt != null && vt.BinaryOperators != null && opIndex < vt.BinaryOperators.Length) {
                            BinaryOperator ct = vt.BinaryOperators[opIndex].Operator;
                            if (ct != null && ct(context, self, other, out ret)) {
                                return true;
                            }
                        }
                    }
                }

                // check context neutral version
                if (_operators.BinaryOperators != null && opIndex < _operators.BinaryOperators.Length) {
                    BinaryOperator ct = _operators.BinaryOperators[opIndex].Operator;
                    if (ct != null && ct(context, self, other, out ret)) {
                        return true;
                    }
                }
            }

            ret = null;
            return false;
        }

        /// <summary>
        /// Attempts to invoke the specific ternary operator with the given instance
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1007:UseGenericsWhereAppropriate")]
        public bool TryInvokeTernaryOperator(CodeContext context, Operators op, object self, object value1, object value2, out object ret) {
            Contract.RequiresNotNull(context, "context");

            Initialize();

            int opIndex = (int)op;

            if (_operators != null) {
                // check context specific version, if available
                if (_operators.ContextSpecific != null) {
                    ContextId ctxId = context.LanguageContext.ContextId;
                    if (_operators.ContextSpecific.Length > ctxId.Id) {
                        VTable vt = _operators.ContextSpecific[ctxId.Id];

                        if (vt != null && vt.TernaryOperators != null && opIndex < vt.TernaryOperators.Length) {
                            TernaryOperator ct = vt.TernaryOperators[opIndex].Operator;
                            if (ct != null && ct(context, self, value1, value2, out ret)) {
                                return true;
                            }
                        }
                    }
                }

                // check context neutral version    
                if (_operators.TernaryOperators != null &&
                    opIndex < _operators.TernaryOperators.Length) {

                    TernaryOperator ct = _operators.TernaryOperators[opIndex].Operator;
                    if (ct != null && ct(context, self, value1, value2, out ret)) {
                        return true;
                    }
                }
            }

            ret = null;
            return false;
        }

        public object InvokeUnaryOperator(CodeContext context, Operators op, object self) {
            object ret;
            if (TryInvokeUnaryOperator(context, op, self, out ret)) {
                return ret;
            }

            throw new MissingMemberException(String.Format(CultureInfo.CurrentCulture, "missing operator {0}", op.ToString()));
        }

        public object InvokeBinaryOperator(CodeContext context, Operators op, object self, object other) {
            object ret;
            if (TryInvokeBinaryOperator(context, op, self, other, out ret)) {
                return ret;
            }

            throw new MissingMemberException(String.Format(CultureInfo.CurrentCulture, "missing operator {0}", op.ToString()));
        }

        public object InvokeTernaryOperator(CodeContext context, Operators op, object self, object value1, object value2) {
            object ret;
            if (TryInvokeTernaryOperator(context, op, self, value1, value2, out ret)) {
                return ret;
            }

            throw new MissingMemberException(String.Format(CultureInfo.CurrentCulture, "missing operator {0}", op.ToString()));
        }


        public bool HasDynamicMembers(CodeContext context) {
            Initialize();

            if (_operators != null) {
                if (HasGetMem(_operators.BinaryOperators)) return true;

                int ctxId = context.LanguageContext.ContextId.Id;
                if (_operators.ContextSpecific != null &&
                    _operators.ContextSpecific.Length > ctxId && 
                    _operators.ContextSpecific[ctxId] != null) {
                    if (HasGetMem(_operators.ContextSpecific[ctxId].BinaryOperators))
                        return true;
                }
            }

            return false;
        }

        private bool HasGetMem(OperatorReference<BinaryOperator>[] table) {
            int getMemId = (int)Operators.GetBoundMember;
            int altGetMemId = (int)Operators.GetMember;

            if (table != null) {
                if (table.Length > getMemId && table[getMemId].Operator != null) {
                    return true;
                }
                if (table.Length > altGetMemId && table[altGetMemId].Operator != null) {
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        /// Provides languages an opportunity to track additional language-specific data related
        /// to the type.  Languages store information under the contexts they've allocated and
        /// get and set their own data.
        /// </summary>
        public object GetContextTag(ContextId context) {
            if (_contextTags != null && context.Id < _contextTags.Count) {
                return _contextTags[context.Id];
            }

            return null;
        }

        /// <summary>
        /// Sets the context tag for language independent data for the type.
        /// </summary>
        public void SetContextTag(ContextId context, object value) {
            if (_contextTags == null)
                _contextTags = new List<object>(context.Id+1);

            while (context.Id >= _contextTags.Count) {
                _contextTags.Add(null);                
            }

            _contextTags[context.Id] = value;
        }

        /// <summary>
        /// Gets the name of the dynamic type
        /// </summary>
        public string Name {
            get {
                return _name;
            }
            internal set {
                _name = value;
            }
        }
       
        public int Version {
            get {
                return _version;
            }
        }

        /// <summary>
        /// Temporary until DynamicVersion goes away and we handle dynamic cases in-line.  This provides
        /// a version number which can be used to disambiguate types with a version == DynamicVersion.
        /// </summary>
        public int AlternateVersion {
            get {
                return _altVersion;
            }
        }

        public bool IsNull {
            get { return Object.ReferenceEquals(this, _nullType); }
        }

        /// <summary>
        /// Gets the resolution order used for attribute lookup
        /// </summary>
        public IList<DynamicMixin> ResolutionOrder {
            get {
                Initialize();

                return _resolutionOrder;
            }
            internal set {
                lock (SyncRoot) {
                    _resolutionOrder = new List<DynamicMixin>(value);
                }
            }
        }
        
        /// <summary>
        /// True if the type is immutable, false if changes can be made
        /// </summary>
        public bool IsImmutable {
            get {
                Initialize();

                return (_attrs & DynamicTypeAttributes.Immutable) != 0;
            }
            internal set {
                if (value) _attrs |= DynamicTypeAttributes.Immutable;
                else _attrs &= (~DynamicTypeAttributes.Immutable);
            }
        }
        
        /// <summary>
        /// Gets the ContextId for the context that created this type.
        /// </summary>
        public ContextId TypeContext {
            get {
                Initialize();

                return _context;
            }
            internal set {
                _context = value;
            }
        }
        
        #endregion

        public TryGetMemberCustomizer CustomBoundGetter {
            get {                
                return _getboundmem;
            }
            set {
                if (value != null) {
                    _version = DynamicVersion;
                    _altVersion = GetNextAlternateVersion();
                } else {
                    _version = GetNextVersion();
                    _altVersion = 0;
                }
                _getboundmem = value;
            }
        }

        public SetMemberCustomizer CustomSetter {
            get {
                return _setmem;
            }
            internal set {
                _setmem = value;
            }
        }

        public DeleteMemberCustomizer CustomDeleter {
            get {
                return _delmem;
            }
            internal set {
                _delmem = value;
            }
        }

        public static DynamicType NullType {
            get {
                return _nullType;
            }
        }

        public bool HasGetAttribute {
            get {
                return _hasGetAttribute;
            }
            internal set {
                _hasGetAttribute = value;
            }
        }

        #region Internal API Surface

        /// <summary>
        /// Internal helper to add a new slot to the type
        /// </summary>
        /// <param name="name"></param>
        /// <param name="slot"></param>
        internal void AddSlot(SymbolId name, DynamicTypeSlot slot) {
            AddSlot(ContextId.Empty, name, slot);
        }

        /// <summary>
        /// Internal helper to add a new slot to the type
        /// </summary>
        /// <param name="name"></param>
        /// <param name="slot"></param>
        /// <param name="context">the context the slot is added for</param>
        internal void AddSlot(ContextId context, SymbolId name, DynamicTypeSlot slot) {
            EnsureDict();

            SlotInfo si;
            if (!_dict.TryGetValue(name, out si)) {
                if (context == ContextId.Empty) {
                    _dict[name] = new SlotInfo(slot);
                    return;
                } else {
                    _dict[name] = si = new SlotInfo();                    
                }
            } else if (context == ContextId.Empty) {
                si.DefaultValue = slot;
                return;
            }

            if (si.SlotValues == null) si.SlotValues = new List<DynamicTypeSlot>(context.Id + 1);
            if (si.SlotValues.Count <= context.Id) {
                while (si.SlotValues.Count < context.Id) {
                    si.SlotValues.Add(null);
                }
                si.SlotValues.Add(slot);
            } else {
                si.SlotValues[context.Id] = slot;
            }
        }


        /// <summary>
        /// Removes the provided symbol as published under the specified context.
        /// </summary>
        internal bool RemoveSlot(ContextId context, SymbolId name) {
            SlotInfo si;
            if (_dict != null && _dict.TryGetValue(name, out si)) {
                if (si.SlotValues != null && si.SlotValues.Count > context.Id && si.SlotValues[context.Id] != null) {
                    si.SlotValues[context.Id] = null;
                    return true;
                }

                if (si.SlotValues == null) {
                    _dict.Remove(name);
                } else {
                    si.DefaultValue = null;
                }
                return true;
            }

            return false;
        }


        /// <summary>
        /// Adds a new unary operator to the type.  
        /// </summary>
        internal void AddOperator(Operators op, UnaryOperator target) {
            if (_operators == null) _operators = new VTable();

            AddOperator<UnaryOperator>(op, ref _operators.UnaryOperators, target);
            if (_operators.ContextSpecific != null) {
                int opIndex = (int)op;

                for (int i = 0; i < _operators.ContextSpecific.Length; i++) {
                    VTable vt = _operators.ContextSpecific[i];
                    if (vt == null) continue;

                    if(vt.UnaryOperators != null && opIndex<vt.UnaryOperators.Length && vt.UnaryOperators[opIndex].Inheritance != 0) {
                        // non-inherited value hides inherited value
                        vt.UnaryOperators[opIndex].Inheritance = 0;
                        vt.UnaryOperators[opIndex].Operator = null;                        
                    }
                }
            }
        }

        /// <summary>
        /// Adds a new binary operator to the type.  
        /// </summary>
        internal void AddOperator(Operators op, BinaryOperator target) {
            if (_operators == null) _operators = new VTable();

            AddOperator<BinaryOperator>(op, ref _operators.BinaryOperators, target);
            if (_operators.ContextSpecific != null) {
                int opIndex = (int)op;

                for (int i = 0; i < _operators.ContextSpecific.Length; i++) {
                    VTable vt = _operators.ContextSpecific[i];
                    if (vt == null) continue;

                    if (vt.BinaryOperators != null && opIndex < vt.BinaryOperators.Length && vt.BinaryOperators[opIndex].Inheritance != 0) {
                        // non-inherited value hides inherited value
                        vt.BinaryOperators[opIndex].Inheritance = 0;
                        vt.BinaryOperators[opIndex].Operator = null;
                    }
                }
            }
        }

        /// <summary>
        /// Adds a new ternary operator to the type.  
        /// </summary>
        internal void AddOperator(Operators op, TernaryOperator target) {
            if (_operators == null) _operators = new VTable();

            AddOperator<TernaryOperator>(op, ref _operators.TernaryOperators, target);
            if (_operators.ContextSpecific != null) {
                int opIndex = (int)op;

                for (int i = 0; i < _operators.ContextSpecific.Length; i++) {
                    VTable vt = _operators.ContextSpecific[i];
                    if (vt == null) continue;

                    if (vt.TernaryOperators != null && opIndex < vt.TernaryOperators.Length && vt.TernaryOperators[opIndex].Inheritance != 0) {
                        // non-inherited value hides inherited value
                        vt.TernaryOperators[opIndex].Inheritance = 0;
                        vt.TernaryOperators[opIndex].Operator = null;                        
                    }
                }
            }
        }

        /// <summary>
        /// Adds a new unary operator to the type that is context-limited
        /// </summary>
        internal void AddOperator(ContextId context, Operators op, UnaryOperator target) {
            if (context == ContextId.Empty) {
                AddOperator(op, target);
                return;
            }

            EnsureContextSlots(context);

            AddOperator<UnaryOperator>(op, ref _operators.ContextSpecific[context.Id].UnaryOperators, target);
        }

        /// <summary>
        /// Adds a new binary operator to the type that is context-limited  
        /// </summary>
        internal void AddOperator(ContextId context, Operators op, BinaryOperator target) {
            if (context == ContextId.Empty) {
                AddOperator(op, target);
                return;
            }

            EnsureContextSlots(context);

            AddOperator<BinaryOperator>(op, ref _operators.ContextSpecific[context.Id].BinaryOperators, target);
        }

        /// <summary>
        /// Adds a new ternary operator to the type that is context-limited
        /// </summary>
        internal void AddOperator(ContextId context, Operators op, TernaryOperator target) {
            if (context == ContextId.Empty) {
                AddOperator(op, target);
                return;
            }

            EnsureContextSlots(context);

            AddOperator<TernaryOperator>(op, ref _operators.ContextSpecific[context.Id].TernaryOperators, target);
        }
        
        internal DynamicMixinBuilder Builder {
            get {
                return _builder;
            }
            set {
                Debug.Assert(_builder == null || value == null);
                _builder = value;
            }
        }

        internal CreateTypeSlot SlotCreator  {
            get {
                return _slotCreator;
            }
            set {
                _slotCreator = value;
            }
        }

        internal object SyncRoot {
            get { 
                // TODO: This is un-ideal, we should lock on something private.
                return this; 
            }
        }

        public event EventHandler<DynamicTypeChangedEventArgs> OnChange;

        /* 
         *                                  
         * class A(object):                 
         *      xyz=1 [ctx1]                
         *      abc=1 [allCtx]              
         *      baz=1 [ctx1]                
         *      baz=2 [allCtx]              
         *                                  
         * class B(A):                      
         *      xyz=2 [allCtx]              
         *      abc=2 [ctx1]                
         *                                      
         * class C(B):
         * 
         * x = B()
         * x.xyz from neutral ctx - 2           
         * x.xyz from ctx1        - 2
         * x.abc from neutral ctx - 1
         * x.abc from ctx1        - 2
         * 
         * y = C()
         * c.baz from ctx1        - 1
         * c.baz from allCtx      - 2
         * 
         */
        internal void Commit() {
            // update our vtable by propagating bases vtable up
            for (int i = 1; i < _resolutionOrder.Count; i++) {
                DynamicMixin curType = _resolutionOrder[i];
                curType.Initialize();

                PropagateOneSet(curType._operators, i, ref _operators);

                if (curType._operators != null && curType._operators.ContextSpecific != null) {
                    for (int j = 0; j < curType._operators.ContextSpecific.Length; j++) {
                        VTable ops = curType._operators.ContextSpecific[j];

                        if (ops != null) {
                            EnsureContextSlots(j);

                            if (_operators.ContextSpecific[j] == null) _operators.ContextSpecific[j] = new VTable();
                            VTable dest = _operators.ContextSpecific[j];

                            PropagateOperators(ops.UnaryOperators, _operators.UnaryOperators, i, ref dest.UnaryOperators);
                            PropagateOperators(ops.BinaryOperators, _operators.BinaryOperators, i, ref dest.BinaryOperators);
                            PropagateOperators(ops.TernaryOperators, _operators.TernaryOperators, i, ref dest.TernaryOperators);
                        }
                    }
                }
            }

        }

        #endregion

        #region Private implementation details

        internal void Initialize() {
            if (_builder == null) {
                Debug.Assert(_dict != null);
                Debug.Assert((_attrs & DynamicTypeAttributes.Initialized) != 0);
                return;
            }

            EnsureDict();

            InitializeWorker();

            if (_getboundmem != null || _setmem != null || _delmem != null) {
                _version = DynamicVersion;
                UpdateVersion();
            }
        }

        private void EnsureDict() {
            if (_dict == null) {
                Interlocked.CompareExchange<Dictionary<SymbolId, SlotInfo>>(
                    ref _dict,
                    new Dictionary<SymbolId, SlotInfo>(),
                    null);
            }
        }

        private void InitializeWorker() {
            lock (SyncRoot) {
                DynamicMixinBuilder dtb = _builder;

                if (dtb == null || 
                    (_attrs & DynamicTypeAttributes.Initializing)!=0) return;

                _attrs |= DynamicTypeAttributes.Initializing;
                try {
                    dtb.Initialize();
                } finally {
                    _attrs &= ~DynamicTypeAttributes.Initializing;
                }
                _attrs |= DynamicTypeAttributes.Initialized;

                _builder = null;
            }
        }

        /// <summary>
        /// private helper function to ensure that we've created slots for context-specific languages
        /// </summary>
        /// <param name="context"></param>
        private void EnsureContextSlots(ContextId context) {
            EnsureContextSlots(context.Id);
        }

        /// <summary>
        /// private helper function to ensure that we've created slots for context-specific languages
        /// </summary>
        /// <param name="id"></param>
        private void EnsureContextSlots(int id) {
            if (_operators == null) _operators = new VTable();

            if (_operators.ContextSpecific == null) _operators.ContextSpecific = new VTable[id + 1];
            else if (_operators.ContextSpecific.Length <= id) Array.Resize<VTable>(ref _operators.ContextSpecific, id + 1);

            if (_operators.ContextSpecific[id] == null)
                Interlocked.CompareExchange<VTable>(ref _operators.ContextSpecific[id], new VTable(), null);
        }

        /// <summary>
        /// Helper function for adding an operator to one of our tables
        /// </summary>
        private static void AddOperator<T>(Operators op, ref OperatorReference<T>[] table, T target) {
            int opCode = (int)op;

            if (table == null) {
                table = new OperatorReference<T>[opCode + 1];
            } else if (table.Length <= opCode) {
                Array.Resize<OperatorReference<T>>(ref table, opCode + 1);
            }

            table[opCode] = new OperatorReference<T>(target);
        }

        [Flags]
        internal enum DynamicTypeAttributes {
            None = 0x00,
            Immutable = 0x01,
            SystemType = 0x02,
            Initializing = 0x10000000,
            Initialized = 0x20000000,
        }

        private struct OperatorReference<T> {
            public T Operator;
            public int Inheritance;

            public OperatorReference(T op) {
                Operator = op;
                Inheritance = 0;
            }

            public OperatorReference(OperatorReference<T> op, int depth) {
                Operator = op.Operator;
                Inheritance = op.Inheritance + depth;
            }

            public override string ToString() {
                if (Operator == null) return "Undefined Operator";

                return String.Format("[Operator: {0}, Inheritance: {1}]", Operator, Inheritance);
            }
        }

        private class VTable {
            public OperatorReference<UnaryOperator>[] UnaryOperators;
            public OperatorReference<BinaryOperator>[] BinaryOperators;
            public OperatorReference<TernaryOperator>[] TernaryOperators;

            public VTable[] ContextSpecific;
        }

        private class SlotInfo {
            public SlotInfo() { }
            public SlotInfo(DynamicTypeSlot defaultValue) {
                DefaultValue = defaultValue;
            }

            public DynamicTypeSlot DefaultValue;
            public List<DynamicTypeSlot> SlotValues;
        }

        private static void PropagateOneSet(VTable operators, int depth, ref VTable destination) {
            if (operators != null) {
                if (destination == null) destination = new VTable();
                
                PropagateOperators(operators.UnaryOperators, depth, ref destination.UnaryOperators);
                PropagateOperators(operators.BinaryOperators, depth, ref destination.BinaryOperators);
                PropagateOperators(operators.TernaryOperators, depth, ref destination.TernaryOperators);
            }
        }

        private static void PropagateOperators<T>(OperatorReference<T>[] from, OperatorReference<T>[] nonCtx, int depth, ref OperatorReference<T>[] to) {
            if (from != null) {
                if (to == null) to = new OperatorReference<T>[from.Length];
                if (to.Length < from.Length) Array.Resize(ref to, from.Length);

                for (int i = 0; i < from.Length; i++) {
                    if(to[i].Operator == null && ContextualReplacesDefault<T>(from, nonCtx, i, depth)) 
                        to[i] = new OperatorReference<T>(from[i], depth);
                }
            }
        }

        private static bool ContextualReplacesDefault<T>(OperatorReference<T>[] from, OperatorReference<T>[] nonCtx, int opIndex, int depth) {
            if (nonCtx == null || opIndex >= nonCtx.Length || nonCtx[opIndex].Operator == null) return true;

            return nonCtx[opIndex].Inheritance >= from[opIndex].Inheritance+depth;
        }

        private static void PropagateOperators<T>(OperatorReference<T>[] from, int depth, ref OperatorReference<T>[] to) {
            if (from != null) {
                if (to == null) to = new OperatorReference<T>[from.Length];
                if (to.Length < from.Length) Array.Resize(ref to, from.Length);

                for (int i = 0; i < from.Length; i++) {
                    if (to[i].Operator == null)
                        to[i] = new OperatorReference<T>(from[i], depth);
                }
            }
        }


        #endregion

        #region ICustomMembers Members

        public bool TryGetCustomMember(CodeContext context, SymbolId name, out object value) {
            DynamicTypeSlot dts;
            if (TryResolveSlot(context, name, out dts)) {
                if(dts.TryGetValue(context, null, this, out value))
                    return true;
            }

            // search the type
            DynamicMixin myType = DynamicHelpers.GetDynamicType(this);
            if (myType.TryResolveSlot(context, name, out dts)) {
                if (dts.TryGetBoundValue(context, this, myType, out value)) {
                    return true;
                }
            }

            value = null;
            return false;
        }

        public bool TryGetBoundCustomMember(CodeContext context, SymbolId name, out object value) {
            DynamicTypeSlot dts;
            if (TryResolveSlot(context, name, out dts)) {
                if (dts.TryGetBoundValue(context, null, this, out value)) {
                    return true;
                }
            }

            // search the type
            DynamicMixin myType = DynamicHelpers.GetDynamicType(this);
            if(myType.TryResolveSlot(context, name, out dts)) {
                if (dts.TryGetBoundValue(context, this, myType, out value)) {
                    return true;
                }
            }

            value = null;
            return false;
        }

        public void SetCustomMember(CodeContext context, SymbolId name, object value) {
            DynamicTypeSlot dts;
            if (TryResolveSlot(context, name, out dts)) {
                if(dts.TrySetValue(context, null, this, value))
                    return;
            }

            if (DynamicType._dynamicTypeType.TryLookupSlot(context, name, out dts)) {
                if (dts.TrySetValue(context, this, DynamicType._dynamicTypeType, value))
                    return;            
            }

            if (IsImmutable) {
                throw new MissingMemberException(String.Format("'{0}' object has no attribute '{1}'", Name, SymbolTable.IdToString(name)));
            }

            EventHandler<DynamicTypeChangedEventArgs> dtc = OnChange;            
            object previous = null;

            if (dtc != null) {
                SlotInfo tmp;
                if (_dict.TryGetValue(name, out tmp)) {
                    previous = tmp.DefaultValue;
                }
            }

            dts = value as DynamicTypeSlot;
            if (dts != null) {
                _dict[name] = new SlotInfo(dts);
            } else if (_slotCreator == null) {
                _dict[name] = new SlotInfo(new DynamicTypeValueSlot(value));
            } else {
                _dict[name] = new SlotInfo(_slotCreator(value));
            }

            UpdateVersion();
            if (dtc != null) {
                dtc(this, new DynamicTypeChangedEventArgs(context, name, ChangeType.Added, previous, value));
            }
            
        }

        public bool DeleteCustomMember(CodeContext context, SymbolId name) {
            DynamicTypeSlot dts;
            if (TryResolveSlot(context, name, out dts)) {
                if (dts.TryDeleteValue(context, null, this))
                    return true;
            }

            if (IsImmutable) {                
                throw new MissingMemberException(String.Format("'{0}' object has no attribute '{1}'", Name, SymbolTable.IdToString(name)));
            }

            EventHandler<DynamicTypeChangedEventArgs> dtc = OnChange;
            object previous = null;

            if (dtc != null) {
                SlotInfo tmp;
                if (_dict.TryGetValue(name, out tmp)) {
                    previous = tmp.DefaultValue;
                }
            }

            if (!_dict.Remove(name)) {
                throw new MissingMemberException(String.Format(CultureInfo.CurrentCulture,
                    Resources.MemberDoesNotExist, 
                    name.ToString()));
            }

            UpdateVersion();
            if (dtc != null) {
                dtc(this, new DynamicTypeChangedEventArgs(context, name, ChangeType.Removed, previous, null));
            }
            return true;
        }

        public IList<object> GetCustomMemberNames(CodeContext context) {
            List<object> res = new List<object>();
            foreach(SymbolId x in GetMemberNames(context)){
                res.Add(x.ToString());
            }
            return res;
        }

        public IDictionary<object, object> GetCustomMemberDictionary(CodeContext context) {
            Dictionary<object, object> res = new Dictionary<object, object>();
            foreach (KeyValuePair<SymbolId, object> kvp in GetMemberDictionary(context).SymbolAttributes) {
                res[kvp.Key.ToString()] = kvp.Value;
            }
            return res;
        }

        #endregion        
    }
}
