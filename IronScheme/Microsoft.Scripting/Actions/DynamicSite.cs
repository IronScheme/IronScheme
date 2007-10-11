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
using System.Diagnostics;

using Microsoft.Scripting.Hosting;

namespace Microsoft.Scripting.Actions {

    //
    // A DynamicSite provides a fast mechanism for call-site caching of dynamic dispatch
    // behvaior. Each site will hold onto a delegate that provides a fast-path dispatch
    // based on previous types that have been seen at the call-site. This delegate will
    // call UpdateBindingAndInvoke if it is called with types that it hasn't seen
    // before.  Updating the binding will typically create (or lookup) a new delegate
    // that supports fast-paths for both the new type and for any types that 
    // have been seen previously.
    // 
    // DynamicSites are designed to replace a lot of currently hard-coded fast-paths,
    // for example, addition in IronPython before dynamic sites looked like this:
    // object Add(object x, object y) {
    //   if (x is int) return IntOps.Add((int)x, y);
    //   if (x is double) return DoubleOps.Add((double)x, y);
    //   [lots more special cases]
    //   Fall-back to dynamic case
    // }
    // 
    // and then in IntOps, we'd have
    // object Add(int x, object y) {
    //   if (y is int) return x + (int)y; // modulo overflow handling
    //   if (y is double) return (double)x + (double)y;
    //   [lots more special cases]
    // }
    // 
    // This was very fast for types that were in the fast-path; however, it was a lot
    // of code (~180K dll size) and it was still extremely slow on any types
    // that weren't in the fast-path, i.e. user-defined types that overloaded the
    // + operator.
    // 
    // DynamicSites will generate the same kind of fast-paths; however, they will
    // generate exactly the right amount of code for the types that are seen
    // in the program so that int addition will remain as fast as it was before,
    // and user-defined types can be as fast as ints because they will all have
    // the same optimal dynamically generated fast-paths.
    // 
    // DynamicSites don't encode any particular caching policy, but use their
    // CallSiteBinding to encode a caching policy.
    //

    
    /// <summary>
    /// Dynamic site which requires the code context to be passed in
    /// as a parameter into its Invoke method.
    /// </summary>
    public abstract class DynamicSite {
        private readonly DynamicAction _action;

        protected DynamicSite(DynamicAction action) {
            this._action = action;
        }

        public DynamicAction Action {
            get { return _action; }
        }


#if DEBUG
        // For debugging, keep track of language so that
        // invocations can be verified against the correct language
        private LanguageContext _lc;
#endif

        [Conditional("DEBUG")]
        protected void Validate(CodeContext context) {
#if DEBUG
            System.Threading.Interlocked.CompareExchange<LanguageContext>(ref _lc, context.LanguageContext, null);
            Debug.Assert(_lc.Engine == null || (_lc.GetType() == context.LanguageContext.GetType()));
#endif
        }
    }

    
    /// <summary>
    /// An optinized dynamic site which caches the value of CodeContext
    /// and therefore doesn't require it being passed into its Invoke method.
    /// </summary>
    public abstract class FastDynamicSite {
        private readonly DynamicAction _action;
        private CodeContext _context;

        protected FastDynamicSite(CodeContext context, DynamicAction action) {
            this._context = context;
            this._action = action;
        }

        public DynamicAction Action {
            get { return _action; }
        }

        public CodeContext Context {
            get { return _context; }
            internal set { _context = value; }
        }
    }
}
