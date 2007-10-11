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
using System.Globalization;
using System.IO;
using System.Collections;
using Microsoft.Scripting.Utils;
using System.Diagnostics;

#if SILVERLIGHT // Proxies

namespace System {

    namespace Diagnostics {

        namespace CodeAnalysis {
            // This class is stubbed out because it's easier to do this than to put #if !SILVERLIGHT
            // every place we use it.
            [AttributeUsage(AttributeTargets.All, Inherited = false, AllowMultiple = true), Conditional("CODE_ANALYSIS")]
            public class SuppressMessageAttribute : Attribute {
                private string _category, _checkId, _justification, _messageId, _scope, _target;
                public SuppressMessageAttribute(string category, string checkId) {
                    _category = category;
                    _checkId = checkId;
                }
                public string Category { get { return _category; } set { _category = value; } }
                public string CheckId { get { return _checkId; } set { _checkId = value; } }
                public string Justification { get { return _justification; } set { _justification = value; } }
                public string MessageId { get { return _messageId; } set { _messageId = value;  } }
                public string Scope { get { return _scope; } set { _scope = value; } }
                public string Target { get { return _target; } set { _target = value; } }
            }
        }
    }

    namespace Runtime.InteropServices {
        public sealed class DefaultParameterValueAttribute : Attribute {
            public DefaultParameterValueAttribute(object value) { }
        }
    }

    // We reference these namespaces via "using"
    // We don't actually use them because the code is #if !SILVERLIGHT
    // Rather than fix the usings all over the place, just define these here
    namespace Runtime.Remoting { class Dummy {} }
    namespace Security.Policy { class Dummy {} }
    namespace Xml.XPath { class Dummy {} }

    namespace Reflection {
        public enum PortableExecutableKinds {
            ILOnly = 0
        }

        public enum ImageFileMachine {
            I386 = 1
        }
    }

    namespace ComponentModel {

        public class WarningException : SystemException {
            public WarningException(string message) : base(message) { }
        }

        public enum EditorBrowsableState {
            Advanced
        }

        public class EditorBrowsableAttribute : Attribute {
            public EditorBrowsableAttribute(EditorBrowsableState state) { }
        }
    }

    namespace CodeDom.Compiler {
        public class GeneratedCodeAttribute : Attribute {
            public GeneratedCodeAttribute(string tool, string version) { }
        }
    }

    public class SerializableAttribute : Attribute {
    }

    public class NonSerializedAttribute : Attribute {
    }

    namespace Runtime.Serialization {
        public interface ISerializable {
        }
    }

    [Flags]
    public enum StringSplitOptions {
        None = 0,
        RemoveEmptyEntries = 1,
    }

    public enum ConsoleColor {
        Black = 0,
        DarkBlue = 1,
        DarkGreen = 2,
        DarkCyan = 3,
        DarkRed = 4,
        DarkMagenta = 5,
        DarkYellow = 6,
        Gray = 7,
        DarkGray = 8,
        Blue = 9,
        Green = 10,
        Cyan = 11,
        Red = 12,
        Magenta = 13,
        Yellow = 14,
        White = 15,
    }

    // BitArray, Queue<T>, Stack<T>, LinkedList<T> and LinkedListNode<T> were removed from CoreCLR
    // Recreating simple versions here.

    namespace Collections {
        #region BitArray
        
        public class BitArray {
            readonly int[] _data;
            readonly int _count;

            public int Length {
                get { return _count; }
            }

            public int Count {
                get { return _count; }
            }

            public BitArray(int count)
                : this(count, false) {
            }

            public BitArray(int count, bool value) {
                this._count = count;
                this._data = new int[(count + 31) / 32];
                if (value) {
                    Not();
                }
            }

            public BitArray(BitArray bits) {
                _count = bits._count;
                _data = (int[])bits._data.Clone();
            }

            public bool Get(int index) {
                if (index < 0 || index >= _count) {
                    throw new ArgumentOutOfRangeException();
                }
                int elem = index / 32, mask = 1 << (index % 32);
                return (_data[elem] & mask) != 0;
            }

            public void Set(int index, bool value) {
                if (index < 0 || index >= _count) {
                    throw new ArgumentOutOfRangeException();
                }
                int elem = index / 32, mask = 1 << (index % 32);
                if (value) {
                    _data[elem] |= mask;
                } else {
                    _data[elem] &= ~mask;
                }
            }

            public void SetAll(bool value) {
                int set = value ? -1 : 0;
                for (int i = 0; i < _data.Length; ++i) {
                    _data[i] = set;
                }
            }

            public BitArray And(BitArray bits) {
                if (bits == null) {
                    throw new ArgumentNullException();
                } else if (bits._count != _count) {
                    throw new ArgumentException("Array lengths differ");
                }
                for (int i = 0; i < _data.Length; ++i) {
                    _data[i] &= bits._data[i];
                }

                return this;
            }

            public BitArray Or(BitArray bits) {
                if (bits == null) {
                    throw new ArgumentNullException();
                } else if (bits._count != _count) {
                    throw new ArgumentException("Array lengths differ");
                }
                for (int i = 0; i < _data.Length; ++i) {
                    _data[i] |= bits._data[i];
                }

                return this;
            }

            public BitArray Not() {
                for (int i = 0; i < _data.Length; ++i) {
                    _data[i] = ~_data[i];
                }
                return this;
            }
        }
        #endregion
    }

    namespace Collections.Generic {

#region Stack<T>

        public class Stack<T> : ListStack<T> {
            public Stack() : base() {
            }

            public Stack(int capacity) : base(capacity) {
            }

            public Stack(IEnumerable<T> collection) : base(collection) {
            }
        }

#endregion

#region LinkedList<T>, LinkedListNode<T>
        public class LinkedListNode<T> {
            internal LinkedList<T> _list;
            internal LinkedListNode<T> _previous, _next;
            internal T _value;

            internal LinkedListNode(LinkedList<T> list, T value) {
                _list = list;
                _value = value;
            }

            public LinkedListNode(T value) {
                _value = value;
            }

            public LinkedList<T> List {
                get { return _list; }
            }

            public LinkedListNode<T> Previous {
                get { return _previous; }
            }

            public LinkedListNode<T> Next {
                get { return _next; }
            }

            public T Value {
                get { return _value; }
            }

        }

        public class LinkedList<T> {
            private LinkedListNode<T> _first;
            private LinkedListNode<T> _last;

            public LinkedList() { }

            public LinkedListNode<T> Last {
                get { return _last; }
            }

            public LinkedListNode<T> First {
                get { return _first; }
            }

            public void AddFirst(T value) {
                AddFirst(new LinkedListNode<T>(value));
            }

            public void AddFirst(LinkedListNode<T> node) {
                CheckInvariants();

                if (node == null) {
                    throw new ArgumentNullException("node");
                }
                if (node._list != null) {
                    throw new InvalidOperationException("node is already a member of another list");
                }

                node._list = this;
                node._next = _first;
                if (_first != null) {
                    _first._previous = node;
                }
                _first = node;
                if (_last == null) {
                    _last = node;
                }

                CheckInvariants();
            }

            public void AddLast(T value) {
                AddLast(new LinkedListNode<T>(value));
            }

            public void AddLast(LinkedListNode<T> node) {
                CheckInvariants();

                if (node == null) {
                    throw new ArgumentNullException("node");
                }
                if (node._list != null) {
                    throw new InvalidOperationException("node is already a member of another list");
                }

                node._list = this;
                node._previous = _last;
                if (_last != null) {
                    _last._next = node;
                }
                _last = node;
                if (_first == null) {
                    _first = node;
                }

                CheckInvariants();
            }

            public void Remove(LinkedListNode<T> node) {
                CheckInvariants();

                if (node == null) {
                    throw new ArgumentNullException("node");
                }
                if (node._list != this) {
                    throw new InvalidOperationException("node is not a member of this list");
                }

                if (node._previous == null) {
                    _first = node._next;
                } else {
                    node._previous._next = node._next;
                }

                if (node._next == null) {
                    _last = node._previous;
                } else {
                    node._next._previous = node._previous;
                }

                node._list = null;
                node._previous = null;
                node._next = null;

                CheckInvariants();
            }

            [Conditional("DEBUG")]
            private void CheckInvariants() {
                if (_first == null || _last == null) {
                    // empty list
                    Debug.Assert(_first == null && _last == null);
                } else if (_first == _last) {
                    // one element
                    Debug.Assert(_first._next == null && _first._previous == null && _first._list == this);
                } else {
                    Debug.Assert(_first._previous == null && _first._list == this);
                    Debug.Assert(_last._next == null && _last._list == this);
                    if (_first._next == _last || _last._previous == _first) {
                        // two elements
                        Debug.Assert(_first._next == _last && _last._previous == _first);
                    } else if (_first._next == _last._previous) {
                        // three elements
                        Debug.Assert(_first._next._next == _last && _last._previous._previous == _first);
                    }
                }
            }
        }

#endregion

#region Queue<T>

        public class Queue<T> {
            private readonly static T[] _EmptyArray = new T[0];
            private T[] _array = _EmptyArray;
            private int _head, _size;

            public void Enqueue(T obj) {
                // Expand the queue, if needed
                if (_size == _array.Length) {
                    int len = _array.Length * 2;
                    if (len < 4) len = 4;
                    T[] a = new T[len];

                    for (int i = 0; i < _size; i++) {
                        a[i] = _array[(_head + i) % _array.Length];
                    }

                    _array = a;
                    _head = 0;
                }

                _array[(_head + _size) % _array.Length] = obj;
                _size++;
            }

            public T Dequeue() {
                if (_size == 0) {
                    throw new InvalidOperationException("Queue is empty");
                }

                T obj = _array[_head];
                _array[_head] = default(T); // release the reference to obj (T might be a class)
                _size--;
                _head = (_head + 1) % _array.Length;
                return obj;
            }

            public int Count {
                get {
                    return _size;
                }
            }
        }

#endregion
    }
}

#endif