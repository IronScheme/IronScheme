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
using System.Reflection;

namespace Microsoft.Scripting.Actions {
    public class MemberGroup : MemberTracker, IEnumerable<MemberTracker> {
        private MemberTracker[] _members;

        internal MemberGroup(MemberTracker[] members) {
            _members = members;
        }
        
        public int Count {
            get {
                return _members.Length;
            }
        }

        public MemberTracker this[int index] {
            get {
                return _members[index];
            }
        }

        public static implicit operator MemberGroup(MemberTracker[] members) {
            // TODO: Ensure only one member group per set of members.
            return new MemberGroup(members);
        }

        public static implicit operator MemberGroup(MemberInfo[] members) {
            MemberTracker[] trackers = new MemberTracker[members.Length];
            for(int i = 0; i<trackers.Length; i++) {
                trackers[i] = members[i];
            }
            return trackers;
        }

        #region IEnumerable<MemberTracker> Members

        public IEnumerator<MemberTracker> GetEnumerator() {
            foreach (MemberTracker tracker in _members) yield return tracker;
        }

        #endregion

        #region IEnumerable Members

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() {
            foreach (MemberTracker tracker in _members) yield return tracker;
        }

        #endregion

        public override TrackerTypes MemberType {
            get { return TrackerTypes.MemberGroup; }
        }

        public override Type DeclaringType {
            get { return _members[0].DeclaringType; }
        }

        public override string Name {
            get { return _members[0].Name; }
        }
    }
}
