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
using Microsoft.Scripting.Utils;

namespace Microsoft.Scripting.Types {
    /// <summary>
    /// DynamicMixinBuilder is the class that languages can use to create instances of DynamicMixin's
    /// that are customized to their language needs.
    /// </summary>
    public class DynamicMixinBuilder {
        internal DynamicMixin _building;
        private List<TypeInitializer> _inits;
        private static List<EventHandler<TypeCreatedEventArgs>> _notifications = new List<EventHandler<TypeCreatedEventArgs>>();

        /// <summary>
        /// Creates a new DynamicMixinBuilder for a DynamicMixin without a name that has an underlying
        /// system type of Object.
        /// </summary>
        public DynamicMixinBuilder() : this("<anonymous>") {            
        }
        
        /// <summary>
        /// Creates a new DynamicMixinBuilder for a DynamicMixin with the specified name that has an
        /// underlying system type of Object.
        /// </summary>
        public DynamicMixinBuilder(string name) {
            Assert.NotNull(name);

            _building = new DynamicMixin();
            _building.Name = name;
            _building.Builder = this;
        }
               
        internal DynamicMixinBuilder(DynamicMixin rebuild) {
            _building = rebuild;
        }

        /// <summary>
        /// Gets the DynamicMixinBuilder for a pre-existing DynamicMixin.
        /// </summary>
        public static DynamicMixinBuilder GetBuilder(DynamicMixin mixin) {
            Contract.RequiresNotNull(mixin, "mixin");

            lock (mixin.SyncRoot) {
                if (mixin.Builder == null) {
                    DynamicType type = mixin as DynamicType;
                    if (type != null) {
                        mixin.Builder = new DynamicTypeBuilder(type);
                    } else {
                        mixin.Builder = new DynamicMixinBuilder(mixin);
                    }
                    return mixin.Builder;
                }

                return mixin.Builder;
            }
        }

        /// <summary>
        /// Forces initialization of the build type and releases the builder
        /// object.  This should be called if a builder is created for a 
        /// pre-existing type and no new initializers were added.
        /// </summary>
        public void ReleaseBuilder() {
            lock (_building.SyncRoot) {
                if (_inits != null) {
                    _building.Initialize();
                }

                _building.Builder = null;
            }
        }

        /// <summary>
        /// Adds a new slot to the DynamicType being constructed
        /// </summary>
        public void AddSlot(SymbolId name, DynamicTypeSlot slot) {
            Contract.RequiresNotNull(slot, "slot");
            if (name == SymbolId.Empty) throw new ArgumentException(Resources.EmptySymbolId, "name");

            _building.AddSlot(name, slot);
        }

        public void AddSlot(ContextId context, SymbolId name, DynamicTypeSlot slot) {
            Contract.RequiresNotNull(slot, "slot");
            if (name == SymbolId.Empty) throw new ArgumentException(Resources.EmptySymbolId, "name");

            _building.AddSlot(context, name, slot);
        }

        /// <summary>
        /// Removes a slot from the DynamicType. Returns true if the slot is removed, false if the
        /// slot doesn't exist.
        /// </summary>
        public bool RemoveSlot(ContextId context, SymbolId name) {
            return _building.RemoveSlot(context, name);
        }

        public void SetHasGetAttribute(bool value) {
            _building.HasGetAttribute = value;
        }

        /// <summary>
        /// Sets a delegate this is used to intercept all member lookups.
        /// </summary>
        public void SetCustomBoundGetter(TryGetMemberCustomizer customizer) {
            _building.CustomBoundGetter = customizer;
        }

        /// <summary>
        /// Sets a delegate this is used to intercept all member sets.
        /// </summary>
        public void SetCustomSetter(SetMemberCustomizer customizer) {
            _building.CustomSetter = customizer;
        }

        /// <summary>
        /// Sets a delegate this is used to intercept all member deletes.
        /// </summary>
        public void SetCustomDeleter(DeleteMemberCustomizer customizer) {
            _building.CustomDeleter = customizer;
        }
       
        /// <summary>
        /// Sets a delegate that creates the default DynamicTypeSlot type
        /// for this type.
        /// </summary>
        public void SetDefaultSlotType(CreateTypeSlot creator) {
            _building.SlotCreator = creator;
        }

        /// <summary>
        /// Sets the name of the type.
        /// </summary>
        public void SetName(string name) {
            _building.Name = name;
        }        

        /// <summary>
        /// Adds a unary operator to the type being built.
        /// </summary>
        public void AddOperator(Operators op, UnaryOperator target) {
            _building.AddOperator(op, target);
        }

        /// <summary>
        /// Adds a binary operator to the type
        /// </summary>
        public void AddOperator(Operators op, BinaryOperator target) {
            _building.AddOperator(op, target);
        }

        /// <summary>
        /// Adds a ternary operator to the type.
        /// </summary>
        public void AddOperator(Operators op, TernaryOperator target) {
            _building.AddOperator(op, target);
        }


        /// <summary>
        /// Adds a unary operator for a specific context to the type being built.  
        /// </summary>
        public void AddOperator(ContextId context, Operators op, UnaryOperator target) {
            _building.AddOperator(context, op, target);
        }

        /// <summary>
        /// Adds a binary operator for a specific context to the type
        /// </summary>
        public void AddOperator(ContextId context, Operators op, BinaryOperator target) {
            _building.AddOperator(context, op, target);
        }

        /// <summary>
        /// Adds a ternary operator for a specific context to the type.
        /// </summary>
        public void AddOperator(ContextId context, Operators op, TernaryOperator target) {
            _building.AddOperator(context, op, target);
        }

        /// <summary>
        /// Sets the method resolution order that will be used for the type.
        /// </summary>
        public void SetResolutionOrder(IList<DynamicMixin> resolutionOrder) {
            Contract.RequiresNotNull(resolutionOrder, "resolutionOrder");

            _building.ResolutionOrder = resolutionOrder;
        }

        /// <summary>
        /// Sets the ContextId that this type was created from and belongs to.
        /// </summary>
        public void SetTypeContext(ContextId id) {
            _building.TypeContext = id;
        }

        /// <summary>
        /// Returns the DynamicMixin that is being built by this DynamicMixinBuilder.
        /// </summary>
        public DynamicMixin UnfinishedType {
            get {
                return _building;
            }
        }
       
        /// <summary>
        /// Finishes constructing the type and returns a newly created and immutable DynamicType object
        /// </summary>
        public DynamicMixin Finish() {
            return Finish(true);
        }

        /// <summary>
        /// Finishes constructing the type and returns the newly created DynamicType object
        /// </summary>
        public DynamicMixin Finish(bool frozen) {
            if (frozen) _building.IsImmutable = true;

            _building.Commit(); //TODO: Thread Safety?
            return _building;
        }

        /// <summary>
        /// Adds an initializer that runs the first time the type is used in a 
        /// substantial way
        /// </summary>
        public void AddInitializer(TypeInitializer init) {
            Contract.RequiresNotNull(init, "init");

            if (_inits == null) _inits = new List<TypeInitializer>();

            _inits.Add(init);
        }

        /// <summary>
        /// Called to perform the lazy initialization from a type.
        /// </summary>
        internal void Initialize() {
            if (_inits != null) {

                for (int i = 0; i < _inits.Count; i++) {
                    PerfTrack.NoteEvent(PerfTrack.Categories.OverAllocate, "DynamicTypeInit " + _building.Name);
                    _inits[i](this);
                }
                _inits = null;


                DynamicType dt = _building as DynamicType;
                EventHandler<TypeCreatedEventArgs> []notifys;
                lock (_notifications) notifys = _notifications.ToArray();
                foreach(EventHandler<TypeCreatedEventArgs> init in notifys) {
                    init(this, new TypeCreatedEventArgs(dt));
                }
            }
        }

        /// <summary>
        /// Fired when a new .NET type is initialialzed.
        /// </summary>
        public static event EventHandler<TypeCreatedEventArgs> TypeInitialized {
            add {
                List<DynamicType> inited = new List<DynamicType>();
                lock (_notifications) {
                    _notifications.Add(value);

                    int current = 0;
                    inited.Add(DynamicHelpers.GetDynamicTypeFromType(typeof(object)));

                    while(current < inited.Count) {
                        DynamicType dt = inited[current++];

                        IList<WeakReference> types = dt.SubTypes;
                        if(types != null) {
                            foreach(WeakReference wr in types) {
                                if (wr.IsAlive) {
                                    DynamicType wrtype = (DynamicType)wr.Target;

                                    if (wrtype != null) {
                                        inited.Add(wrtype);
                                    }
                                }
                            }
                        }
                    }

                }

                foreach (DynamicType dt in inited) {
                    value(typeof(DynamicMixinBuilder), new TypeCreatedEventArgs(dt));
                }
            }
            remove {
                lock (_notifications) {
                    _notifications.Remove(value);
                }
            }
        }
    }

    public delegate void TypeInitializer(DynamicMixinBuilder builder);
}
