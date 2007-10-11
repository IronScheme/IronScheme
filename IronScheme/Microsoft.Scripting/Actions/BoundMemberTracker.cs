using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Ast;

namespace Microsoft.Scripting.Actions {
    public class BoundMemberTracker : MemberTracker {
        private Expression _instance;
        private MemberTracker _tracker;

        public BoundMemberTracker(MemberTracker tracker, Expression instance) {
            _tracker = tracker;
            _instance = instance;
        }

        public override TrackerTypes MemberType {
            get { return TrackerTypes.Bound; }
        }

        public override Type DeclaringType {
            get { return _tracker.DeclaringType; }
        }

        public override string Name {
            get { return _tracker.Name; }
        }

        public Expression Instance {
            get {
                return _instance;
            }
        }

        public override Expression GetValue(ActionBinder binder) {
            return _tracker.GetBoundValue(binder, _instance);
        }
    }
}
