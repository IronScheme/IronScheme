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
    public class EventTracker : MemberTracker {
        private EventInfo _event;

        public EventTracker(EventInfo eventInfo) {
            _event = eventInfo;
        }

        public override Type DeclaringType {
            get { return _event.DeclaringType; }
        }

        public override TrackerTypes MemberType {
            get { return TrackerTypes.Event; }
        }

        public override string Name {
            get { return _event.Name; }
        }

        public EventInfo Event {
            get {
                return _event;
            }
        }
      
        public override string ToString() {
            return _event.ToString();
        }
    }
}
