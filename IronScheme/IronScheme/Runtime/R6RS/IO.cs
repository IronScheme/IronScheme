#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/
#endregion


using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Generation;
using Microsoft.Scripting;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Reflection.Emit;
using System.Collections;
using System.IO;

namespace IronScheme.Runtime.R6RS
{
  public class IO : Builtins
  {
    static SymbolId bm_none = SymbolTable.StringToId("none");
    static SymbolId bm_line = SymbolTable.StringToId("line");
    static SymbolId bm_block = SymbolTable.StringToId("block");

    //(buffer-mode? obj )
    [Builtin("buffer-mode?")]
    public static object IsBufferMode(object s)
    {
      if (s is SymbolId)
      {
        SymbolId bm = RequiresNotNull<SymbolId>(s);
        return bm == bm_none || bm == bm_line || bm == bm_block;
      }
      return FALSE;
    }

    class Transcoder
    {
      public Encoding codec = Encoding.Default;
      public SymbolId eolstyle = eol_crlf;
      public SymbolId handlingmode = SymbolTable.StringToId("replace");

      public static readonly Transcoder native;

      static Transcoder()
      {
        native = new Transcoder();
      }
    }

    // transcoders
    //(latin-1-codec)
    //(utf-8-codec) 
    //(utf-16-codec)

    static Encoding latin1 = Encoding.GetEncoding("iso-8859-1"); 

    [Builtin("latin-1-codec")]
    public static object Latin1Codec()
    {
      return latin1;
    }

    [Builtin("utf-8-codec")]
    public static object Utf8Codec()
    {
      return Encoding.UTF8;
    }

    [Builtin("utf-16-codec")]
    public static object Utf16Codec()
    {
      return Encoding.Unicode;
    }


    static SymbolId eol_crlf = SymbolTable.StringToId("crlf");

    //(native-eol-style)
    [Builtin("native-eol-style")]
    public static object NativeEolStyle()
    {
      return eol_crlf;
    }

    //(make-transcoder codec) 
    //(make-transcoder codec eol-style) 
    //(make-transcoder codec eol-style handling-mode)

    [Builtin("make-transcoder")]
    public static object MakeTranscoder(object codec)
    {
      Transcoder tc = new Transcoder();
      tc.codec = codec as Encoding;
      return tc;
    }

    [Builtin("make-transcoder")]
    public static object MakeTranscoder(object codec, object eolstyle)
    {
      Transcoder tc = new Transcoder();
      tc.codec = codec as Encoding;
      tc.eolstyle = (SymbolId)eolstyle;
      return tc;
    }


    [Builtin("make-transcoder")]
    public static object MakeTranscoder(object codec, object eolstyle, object handlingmode)
    {
      Transcoder tc = new Transcoder();
      tc.codec = codec as Encoding;
      tc.eolstyle = (SymbolId)eolstyle;
      tc.handlingmode = (SymbolId)handlingmode;
      return tc;
    }


    //(native-transcoder)
    [Builtin("native-transcoder")]
    public static object NativeTranscoder()
    {
      return Transcoder.native;
    }

    //(transcoder-codec transcoder) 
    [Builtin("transcoder-codec")]
    public static object TranscoderCodec(object tc)
    {
      return RequiresNotNull<Transcoder>(tc).codec;
    }

    //(transcoder-eol-style transcoder) 
    [Builtin("transcoder-eol-style")]
    public static object TranscoderEolStyle(object tc)
    {
      return RequiresNotNull<Transcoder>(tc).eolstyle;
    }

    //(transcoder-error-handling-mode transcoder)
    [Builtin("transcoder-error-handling-mode")]
    public static object TranscoderErrorHandlingMode(object tc)
    {
      return RequiresNotNull<Transcoder>(tc).handlingmode;
    }


    //(bytevector->string bytevector transcoder)
    [Builtin("bytevector->string")]
    public static object ByteVectorToString(object bv, object tc)
    {
      return RequiresNotNull<Transcoder>(tc).codec.GetString(RequiresNotNull<byte[]>(bv));
    }

    //(string->bytevector string transcoder)
    [Builtin("string->bytevector")]
    public static object StringToByteVector(object s, object tc)
    {
      return RequiresNotNull<Transcoder>(tc).codec.GetBytes(RequiresNotNull<string>(s));
    }

    // input and output

    //(port-transcoder port)
    [Builtin("port-transcoder")]
    public static object PortTranscoder(object port)
    {
      throw new NotImplementedException();
    }


    //(textual-port? port) 
    [Builtin("textual-port?")]
    public static object IsTextualPort(object port)
    {
      return IsTrue(port is TextReader || port is TextWriter);
    }

    //(binary-port? port)
    [Builtin("binary-port?")]
    public static object IsBinaryPort(object port)
    {
      return IsTrue(port is Stream);
    }

    //(transcoded-port binary-port transcoder)
    [Builtin("transcoded-port")]
    public static object TranscodedPort(object binaryport, object transcoder)
    {
      Stream s = RequiresNotNull<Stream>(binaryport);
      Transcoder tc = RequiresNotNull<Transcoder>(transcoder);
      
      if (s.CanRead)
      {
        if (s is MemoryStream && s.CanWrite)
        {
          StreamWriter w = new StreamWriter(s, tc.codec);
          w.AutoFlush = true;
          return w;
        }
        return new StreamReader(s, tc.codec);
      }
      if (s.CanWrite)
      {
        return new StreamWriter(s, tc.codec);
      }
      return FALSE;
    }


    static object TranscodedOutputPort(object binaryport, object transcoder)
    {
      Stream s = RequiresNotNull<Stream>(binaryport);
      Transcoder tc = RequiresNotNull<Transcoder>(transcoder);
      if (s.CanWrite)
      {
        StreamWriter w = new StreamWriter(s, tc.codec);
        w.AutoFlush = true;
        return w;
      }
      return FALSE;
    }

    static object TranscodedInputPort(object binaryport, object transcoder)
    {
      Stream s = RequiresNotNull<Stream>(binaryport);
      Transcoder tc = RequiresNotNull<Transcoder>(transcoder);
      if (s.CanRead)
      {
        return new StreamReader(s, tc.codec);
      }
      return FALSE;
    }


    //(port-has-port-position? port) 
    [Builtin("port-has-port-position?")]
    public static object PortHasPortPosition(object port)
    {
      if (port is CustomTextWriter)
      {
        return GetBool(((CustomTextWriter)port).HasPosition);
      }
      if (port is CustomTextReader)
      {
        return GetBool(((CustomTextReader)port).HasPosition);
      }
      if (port is CustomStream)
      {
        return GetBool(((CustomStream)port).HasPosition);
      }
      if (port is Stream)
      {
        return TRUE;
      }
      if (port is StreamReader || port is StreamWriter)
      {
        return TRUE;
      }
      return FALSE;
    }

    //(port-position port)
    [Builtin("port-position")]
    public static object PortPosition(object port)
    {
      if (port is CustomTextWriter)
      {
        CustomTextWriter ctw = (CustomTextWriter)port;
        if (ctw.HasPosition)
        {
          return ctw.Position;
        }
        else
        {
          return AssertionViolation("port-position", "not supplied to custom port", port);
        }
      }
      if (port is CustomTextReader)
      {
        CustomTextReader ctr = (CustomTextReader)port;
        if (ctr.HasPosition)
        {
          return ctr.Position;
        }
        else
        {
          return AssertionViolation("port-position", "not supplied to custom port", port);
        }
      }
      if (port is CustomStream)
      {
        if (((CustomStream)port).HasPosition)
        {
          return AssertionViolation("port-position", "not supplied to custom port", port);
        }
      }
      if (port is Stream)
      {
        return (int)((Stream)port).Position;
      }
      if (port is StreamReader)
      {
        return (int)((StreamReader)port).BaseStream.Position;
      }
      if (port is StreamWriter)
      {
        return (int)((StreamWriter)port).BaseStream.Position;
      }

      return AssertionViolation("port-position", "not supported", port);
    }

    //(port-has-set-port-position!? port) 
    [Builtin("port-has-set-port-position!?")]
    public static object PortHasSetPortPosition(object port)
    {
      if (port is CustomTextWriter)
      {
        return GetBool(((CustomTextWriter)port).HasSetPosition);
      }
      if (port is CustomTextReader)
      {
        return GetBool(((CustomTextReader)port).HasSetPosition);
      }
      if (port is Stream)
      {
        return GetBool(((Stream)port).CanSeek);
      }
      if (port is StreamReader)
      {
        return GetBool(((StreamReader)port).BaseStream.CanSeek);
      }
      if (port is StreamWriter)
      {
        return GetBool(((StreamWriter)port).BaseStream.CanSeek);
      }
      return FALSE;
    }

    //(set-port-position! port pos)
    [Builtin("set-port-position!")]
    public static object SetPortPosition(object port, object pos)
    {
      int p = RequiresNotNull<int>(pos);

      if (port is CustomTextWriter)
      {
        CustomTextWriter ctw = (CustomTextWriter)port;
        if (ctw.HasPosition)
        {
          ctw.Position = p;
        }
        else
        {
          return AssertionViolation("set-port-position!", "not supplied to custom port", port);
        }
      }
      else if (port is CustomTextReader)
      {
        CustomTextReader ctr = (CustomTextReader)port;
        if (ctr.HasPosition)
        {
          ctr.Position = p;
        }
        else
        {
          return AssertionViolation("set-port-position!", "not supplied to custom port", port);
        }
      }
      else if (port is Stream)
      {
        ((Stream)port).Position = p;
      }
      else if (port is StreamReader)
      {
        ((StreamReader)port).BaseStream.Position = p;
      }
      else if (port is StreamWriter)
      {
        ((StreamWriter)port).BaseStream.Position = p;
      }
      else
      {
        return AssertionViolation("set-port-position!", "not supported", port);
      }
      return Unspecified;
    }

    //(close-port port)
    [Builtin("close-port")]
    public static object ClosePort(object port)
    {
      if (port is Stream)
      {
        ((Stream)port).Close();
      }
      if (port is TextReader)
      {
        ((TextReader)port).Close();
      }
      if (port is TextWriter)
      {
        ((TextWriter)port).Close();
      }
      return Unspecified;
    }


    //(call-with-port port proc)
    [Builtin("call-with-port")]
    public static object CallWithPort(object port, object proc)
    {
      ICallable p = RequiresNotNull<ICallable>(proc);
      try
      {
        object result = p.Call(port);
        ClosePort(port);
        return result;
      }
      catch (IOException ex)
      {
        return IOPortViolation("call-with-port", ex.Message, port);
      }
    }

    // input ports

    //(port-eof? input-port)
    [Builtin("port-eof?")]
    public static object PortIsEof(object inputport)
    {
      object eof = new object();
      if (inputport is Stream)
      {
        eof = LookAheadU8(inputport);
      }
      else if (inputport is TextReader)
      {
        eof = LookAheadChar(inputport);
      }

      return eof == EOF;
    }

    //(open-file-input-port filename) 
    //(open-file-input-port filename file-options)
    //(open-file-input-port filename file-options buffer-mode)
    //(open-file-input-port filename file-options buffer-mode maybe-transcoder)
    [Builtin("open-file-input-port")]
    public static object OpenFileInputPort(object filename)
    {
      return OpenFileInputPort(filename, 0);
    }

    [Builtin("open-file-input-port")]
    public static object OpenFileInputPort(object filename, object fileoptions)
    {
      return OpenFileInputPort(filename, fileoptions, bm_block);
    }

    [Builtin("open-file-input-port")]
    public static object OpenFileInputPort(object filename, object fileoptions, object buffermode)
    {
      return OpenFileInputPort(filename, fileoptions, bm_block, FALSE);
    }

    [Builtin("open-file-input-port")]
    public static object OpenFileInputPort(object filename, object fileoptions, object buffermode, object maybetranscoder)
    {
      string fn = RequiresNotNull<string>(filename);
      Transcoder tc = maybetranscoder as Transcoder;
      try
      {
        Stream s = File.OpenRead(fn);

        if (tc == null)
        {
          return s;
        }
        else
        {
          return TranscodedInputPort(s, tc);
        }
      }
      catch (FileNotFoundException ex)
      {
        return FileNotFoundViolation("open-file-input-port", ex.Message, filename);
      }
      catch (Exception ex)
      {
        return AssertionViolation("open-file-input-port", ex.Message, filename);
      }
    }

    //(open-bytevector-input-port bytevector) 
    //(open-bytevector-input-port bytevector maybe-transcoder)
    [Builtin("open-bytevector-input-port")]
    public static object OpenBytevectorInputPort(object bytevector)
    {
      return OpenBytevectorInputPort(bytevector, FALSE);
    }

    [Builtin("open-bytevector-input-port")]
    public static object OpenBytevectorInputPort(object bytevector, object maybetranscoder)
    {
      byte[] b = RequiresNotNull<byte[]>(bytevector);
      Transcoder tc = maybetranscoder as Transcoder;

      Stream s = new MemoryStream(b, false);

      if (tc == null)
      {
        return s;
      }
      else
      {
        return TranscodedPort(s, tc);
      }
    }

    //(open-string-input-port string)
    [Builtin("open-string-input-port")]
    public static object OpenStringInputPort(object str)
    {
      string s = RequiresNotNull<string>(str);
      return new StringReader(s); //lexer is broken...
    }

    //(standard-input-port)
    [Builtin("standard-input-port")]
    public static object StandardInputPort()
    {
      return Console.OpenStandardInput();
    }

    abstract class CustomStream : Stream
    {
      public abstract bool HasPosition { get ; }
    }

    class CustomBinaryInputStream : CustomStream
    {
      string id;
      ICallable read, get_pos, set_pos, close;

      public CustomBinaryInputStream(object id, object read, object get_pos, object set_pos, object close)
      {
        this.id = RequiresNotNull<string>(id);
        this.read = RequiresNotNull<ICallable>(read);
        // rest optional
        this.get_pos = get_pos as ICallable;
        this.set_pos = set_pos as ICallable;
        this.close = close as ICallable;
      }

      public override bool CanRead
      {
        get { return true; }
      }

      public override bool CanSeek
      {
        get { return set_pos != null; }
      }

      public override bool CanWrite
      {
        get { return false; }
      }

      public override void Flush()
      {

      }

      public override bool HasPosition
      {
        get { return get_pos != null; }
      }

      public override long Length
      {
        get { throw new Exception("The method or operation is not implemented."); }
      }

      public override long Position
      {
        get
        {
          if (get_pos != null)
          {
            return Convert.ToInt64(get_pos.Call());
          }
          return (int)AssertionViolation("port-position", "not supported");
        }
        set
        {
          if (set_pos != null)
          {
            set_pos.Call((int)value);
          }
        }
      }

      public override int Read(byte[] buffer, int offset, int count)
      {
        return (int)read.Call(buffer, offset, count);
      }

      public override long Seek(long offset, SeekOrigin origin)
      {
        throw new Exception("The method or operation is not implemented.");
      }

      public override void SetLength(long value)
      {
        throw new Exception("The method or operation is not implemented.");
      }

      public override void Write(byte[] buffer, int offset, int count)
      {
        AssertionViolation("write", "cannot write to input port");
      }

      public override string ToString()
      {
        return string.Format("#<custom-binary-input-port id: {0}>", id);
      }

      public override void Close()
      {
        if (close != null)
        {
          close.Call();
        }
      }
    }

    //(make-custom-binary-input-port id read! get-position set-position! close)
    [Builtin("make-custom-binary-input-port")]
    public static object MakeCustomBinaryInputPort(object id, object read, object get_pos, object set_pos, object close)
    {
      return new CustomBinaryInputStream(id, read, get_pos, set_pos, close);
    }

    class CustomTextReader : TextReader
    {
      string id;
      ICallable read, get_pos, set_pos, close;

      public CustomTextReader(object id, object read, object get_pos, object set_pos, object close)
      {
        this.id = RequiresNotNull<string>(id);
        this.read = RequiresNotNull<ICallable>(read);
        // rest optional
        this.get_pos = get_pos as ICallable;
        this.set_pos = set_pos as ICallable;
        this.close = close as ICallable;
      }

      public override int Read(char[] buffer, int index, int count)
      {
        StringBuilder sb = new StringBuilder(new string(buffer));

        int res = (int)read.Call(sb, index, count);

        for (int i = 0; i < res; i++)
        {
          buffer[i] = sb[i];
        }

        return res;
      }

      public bool HasPosition
      {
        get { return get_pos != null; }
      }

      public bool HasSetPosition
      {
        get { return set_pos != null; }
      }

      public int Position
      {
        get
        {
          if (get_pos != null)
          {
            return Convert.ToInt32(get_pos.Call());
          }
          return (int)AssertionViolation("port-position", "not supported");
        }
        set
        {
          if (set_pos != null)
          {
            set_pos.Call(value);
          }
        }
      }

      public override string ToString()
      {
        return string.Format("#<custom-textual-input-port id: {0}>", id);
      }

      public override void Close()
      {
        if (close != null)
        {
          close.Call();
        }
      }

    }


    //(make-custom-textual-input-port id read! get-position set-position! close)
    [Builtin("make-custom-textual-input-port")]
    public static object MakeCustomTextInputPort(object id, object read, object get_pos, object set_pos, object close)
    {
      return new CustomTextReader(id, read, get_pos, set_pos, close);
    }

    // binary input
    //(get-u8 binary-input-port)
    [Builtin("get-u8")]
    public static object GetU8(object binaryinputport)
    {
      Stream s = RequiresNotNull<Stream>(binaryinputport);

      try
      {

        int c = s.ReadByte();
        if (c == -1)
        {
          return EOF;
        }
        return c;
      }
      catch (IOException ex)
      {
        return IOPortViolation("get-u8", ex.Message, binaryinputport);
      }


    }

    //(lookahead-u8 binary-input-port)
    [Builtin("lookahead-u8")]
    public static object LookAheadU8(object binaryinputport)
    {
      Stream s = RequiresNotNull<Stream>(binaryinputport);
      try
      {
        if (s.CanSeek)
        {
          int c = s.ReadByte();
          s.Position--;
          if (c == -1)
          {
            return EOF;
          }
          return c;
        }
        return FALSE;
      }
      catch (IOException ex)
      {
        return IOPortViolation("lookahead-u8", ex.Message, binaryinputport);
      }

    }

    //(get-bytevector-n binary-input-port count)
    [Builtin("get-bytevector-n")]
    public static object GetBytevectorN(object binaryinputport, object count)
    {
      int k = RequiresNotNull<int>(count);
      Stream s = RequiresNotNull<Stream>(binaryinputport);

      try
      {

        byte[] buffer = new byte[k];

        int r = s.Read(buffer, 0, k);

        if (r == -1)
        {
          return EOF;
        }

        if (r != k)
        {
          byte[] nb = new byte[r];
          Array.Copy(buffer, nb, r);
          return nb;
        }
        return buffer;
      }
      catch (IOException ex)
      {
        return IOPortViolation("get-bytevector-n", ex.Message, binaryinputport);
      }

    }

    //(get-bytevector-n! binary-input-port bytevector start count)
    [Builtin("get-bytevector-n!")]
    public static object GetBytevectorNN(object binaryinputport, object bytevector, object start, object count)
    {
      int k = RequiresNotNull<int>(count);
      int j = RequiresNotNull<int>(start);
      Stream s = RequiresNotNull<Stream>(binaryinputport);
      byte[] b = RequiresNotNull<byte[]>(bytevector);

      try
      {

        int r = s.Read(b, j, k);

        if (r == -1)
        {
          return EOF;
        }

        return r;
      }
      catch (IOException ex)
      {
        return IOPortViolation("get-bytevector-n!", ex.Message, binaryinputport);
      }

    }

    //(get-bytevector-some binary-input-port)
    [Builtin("get-bytevector-some")]
    public static object GetBytevectorSome(object binaryinputport)
    {
      Stream s = RequiresNotNull<Stream>(binaryinputport);

      try
      {

        List<byte> some = new List<byte>();

        int c;
        while ((c = s.ReadByte()) != -1)
        {
          some.Add((byte)c);
        }

        if (some.Count == 0)
        {
          return EOF;
        }

        return some.ToArray();
      }
      catch (IOException ex)
      {
        return IOPortViolation("get-bytevector-some", ex.Message, binaryinputport);
      }

    }

    //(get-bytevector-all binary-input-port)
    [Builtin("get-bytevector-all")]
    public static object GetBytevectorAll(object binaryinputport)
    {
      Stream s = RequiresNotNull<Stream>(binaryinputport);

      try
      {

        List<byte> all = new List<byte>();

        int c;
        while ((c = s.ReadByte()) != -1)
        {
          all.Add((byte)c);
        }

        if (all.Count == 0)
        {
          return EOF;
        }

        return all.ToArray();
      }
      catch (IOException ex)
      {
        return IOPortViolation("get-bytevector-all", ex.Message, binaryinputport);
      }

    }

    //text input
    //(get-char textual-input-port)
    [Builtin("get-char")]
    public static object GetChar(object textinputport)
    {
      TextReader r = RequiresNotNull<TextReader>(textinputport);

      try
      {

        int c = r.Read();
        if (c == -1)
        {
          return EOF;
        }
        return (char)c;
      }
      catch (IOException ex)
      {
        return IOPortViolation("get-char", ex.Message, textinputport);
      }
    }

    //(lookahead-char textual-input-port)
    [Builtin("lookahead-char")]
    public static object LookAheadChar(object textinputport)
    {
      TextReader r = RequiresNotNull<TextReader>(textinputport);
      try
      {

        int c = r.Peek();
        if (c == -1)
        {
          return EOF;
        }
        return (char)c;
      }
      catch (IOException ex)
      {
        return IOPortViolation("lookahead-char", ex.Message, textinputport);
      }
    }

    //(get-string-n textual-input-port count)
    [Builtin("get-string-n")]
    public static object GetStringN(object textinputport, object count)
    {
      TextReader r = RequiresNotNull<TextReader>(textinputport);
      int k = RequiresNotNull<int>(count);

      try
      {

        char[] buffer = new char[k];

        int c = r.Read(buffer, 0, k);
        if (c == -1)
        {
          return EOF;
        }
        return new string(buffer, 0, c);
      }
      catch (IOException ex)
      {
        return IOPortViolation("get-string-n", ex.Message, textinputport);
      }

    }

    //(get-string-n! textual-input-port string start count)
    [Builtin("get-string-n!")]
    public static object GetStringNN(object textinputport, object str, object start, object count)
    {
      TextReader r = RequiresNotNull<TextReader>(textinputport);
      StringBuilder s = RequiresNotNull<StringBuilder>(str);
      int j = RequiresNotNull<int>(start);
      int k = RequiresNotNull<int>(count);

      try
      {

        char[] buffer = new char[s.Length];
        s.CopyTo(0, buffer, 0, s.Length);

        int c = r.Read(buffer, j, k);
        if (c == -1)
        {
          return EOF;
        }
        for (int i = 0; i < c; i++)
        {
          s[j + i] = buffer[j + i];
        }
        return c;
      }
      catch (IOException ex)
      {
        return IOPortViolation("get-string-n!", ex.Message, textinputport);
      }

    }

    //(get-string-all textual-input-port)
    [Builtin("get-string-all")]
    public static object GetStringAll(object textinputport)
    {
      TextReader r = RequiresNotNull<TextReader>(textinputport);
      try
      {

        string c = r.ReadToEnd();
        if (c == null)
        {
          return EOF;
        }
        return c;
      }
      catch (IOException ex)
      {
        return IOPortViolation("get-string-all", ex.Message, textinputport);
      }

    }

    //(get-line textual-input-port)
    [Builtin("get-line")]
    public static object GetLine(object textinputport)
    {
      TextReader r = RequiresNotNull<TextReader>(textinputport);
      try
      {

        string c = r.ReadLine();
        if (c == null)
        {
          return EOF;
        }
        return c;
      }
      catch (IOException ex)
      {
        return IOPortViolation("get-line", ex.Message, textinputport);
      }

    }

    //(get-datum textual-input-port)
    [Builtin("get-datum")]
    public static object GetDatum(object textinputport)
    {
      return Read(textinputport);
    }

    //output ports
    //(flush-output-port output-port)
    [Builtin("flush-output-port")]
    public static object FlushOutputPort(object outputport)
    {
      if (outputport is Stream)
      {
        ((Stream)outputport).Flush();
      }
      if (outputport is TextWriter)
      {
        ((TextWriter)outputport).Flush();
      }
      return Unspecified;
    }

    //(output-port-buffer-mode output-port)
    [Builtin("output-port-buffer-mode")]
    public static object OutputPortBufferMode(object outputport)
    {
      return bm_block;
    }

    //(open-file-output-port filename) 
    //(open-file-output-port filename file-options)
    //(open-file-output-port filename file-options buffer-mode)
    //(open-file-output-port filename file-options buffer-mode maybe-transcoder)
    [Builtin("open-file-output-port")]
    public static object OpenFileOutputPort(object filename)
    {
      return OpenFileOutputPort(filename, null);
    }

    [Builtin("open-file-output-port")]
    public static object OpenFileOutputPort(object filename, object fileoptions)
    {
      return OpenFileOutputPort(filename, fileoptions, bm_block);
    }

    [Builtin("open-file-output-port")]
    public static object OpenFileOutputPort(object filename, object fileoptions, object buffermode)
    {
      return OpenFileOutputPort(filename, fileoptions, bm_block, false);
    }

    [Flags]
    enum FileOptions
    {
      NoFail = 1, 
      NoCreate = 2, 
      NoTruncate = 4
    }

    static FieldInfo enum_value = null;

    static FileOptions ToFileOptions(object fo)
    {
      if (fo == null)
      {
        return 0;
      }
      if (enum_value == null)
      {
        enum_value = fo.GetType().GetField("value", BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
      }

      return (FileOptions)(int)enum_value.GetValue(fo);
    }

    static FileMode GetMode(FileOptions fo)
    {
      if ((fo & FileOptions.NoCreate) != 0)
      {
        return FileMode.Open;
      }
      if ((fo & FileOptions.NoTruncate) != 0)
      {
        return FileMode.Append;
      }

      return FileMode.CreateNew;
    }

    [Builtin("open-file-output-port")]
    public static object OpenFileOutputPort(object filename, object fileoptions, object buffermode, object maybetranscoder)
    {
      FileOptions fo = ToFileOptions(fileoptions);
      FileMode fm =  GetMode(fo);
      string fn = RequiresNotNull<string>(filename);
      Transcoder tc = maybetranscoder as Transcoder;
      try
      {
        Stream s = File.Open(fn, fm, FileAccess.Write);

        if (tc == null)
        {
          return s;
        }
        else
        {
          return TranscodedOutputPort(s, tc);
        }
      }
      catch (FileNotFoundException ex)
      {
        return FileNotFoundViolation("open-file-output-port", ex.Message, filename);
      }
      catch (Exception ex)
      {
        if ((fo & FileOptions.NoFail) != 0)
        {
          return FALSE;
        }
        if (ex.Message.StartsWith("The process cannot access the file"))
        {
          return FileInUseViolation("open-file-output-port", filename);
        }
        if (ex.Message.EndsWith("already exists."))
        {
          return FileAlreadyExistsViolation("open-file-output-port", filename);
        }
        else
        {
          return AssertionViolation("open-file-output-port", ex.Message, filename);
        }
      }
    }

    //(open-bytevector-output-port) 
    //(open-bytevector-output-port maybe-transcoder)
    [Builtin("open-bytevector-output-port")]
    public static object OpenBytevectorOutputPort()
    {
      return OpenBytevectorOutputPort(false);
    }

    [Builtin("open-bytevector-output-port")]
    public static object OpenBytevectorOutputPort(object maybetranscoder)
    {
      Transcoder tc = maybetranscoder as Transcoder;

      MemoryStream s = new MemoryStream();

      CallTarget0 extract = delegate
      {
        return s.ToArray();
      };

      return Values(tc == null ? s : TranscodedPort(s, tc), Closure.Make(Context, extract));
    }

    //(call-with-bytevector-output-port proc) 
    //(call-with-bytevector-output-port proc maybe-transcoder)
    [Builtin("call-with-bytevector-output-port")]
    public static object CallWithBytevectorOutputPort(object proc)
    {
      return CallWithBytevectorOutputPort(proc, false);
    }

    [Builtin("call-with-bytevector-output-port")]
    public static object CallWithBytevectorOutputPort(object proc, object maybetranscoder)
    {
      ICallable c = RequiresNotNull<ICallable>(proc);
      Transcoder tc = maybetranscoder as Transcoder;

      using (MemoryStream s = new MemoryStream())
      {
        object p = tc == null ? s : TranscodedOutputPort(s, tc);
        c.Call(p);
        return s.ToArray();
      }
    }

    //(open-string-output-port) ; in scheme lib

    //(call-with-string-output-port proc)
    [Builtin("call-with-string-output-port")]
    public static object CallWithStringOutputPort(object proc)
    {
      ICallable c = RequiresNotNull<ICallable>(proc);

      using (StringWriter w = new StringWriter())
      {
        c.Call(w);
        return w.ToString();
      }
    }


    //(standard-output-port)
    [Builtin("standard-output-port")]
    public static object StandardOutputPort()
    {
      return Console.OpenStandardOutput();
    }

    //(standard-error-port)
    [Builtin("standard-error-port")]
    public static object StandardErrorPort()
    {
      return Console.OpenStandardError();
    }


    class CustomBinaryOutputStream : CustomStream
    {
      string id;
      ICallable write, get_pos, set_pos, close;

      public CustomBinaryOutputStream(object id, object write, object get_pos, object set_pos, object close)
      {
        this.id = RequiresNotNull<string>(id);
        this.write = RequiresNotNull<ICallable>(write);
        // rest optional
        this.get_pos = get_pos as ICallable;
        this.set_pos = set_pos as ICallable;
        this.close = close as ICallable;
      }

      public override bool CanRead
      {
        get { return false; }
      }

      public override bool CanSeek
      {
        get { return set_pos != null; }
      }

      public override bool CanWrite
      {
        get { return true; }
      }

      public override bool HasPosition
      {
        get { return get_pos != null; }
      }

      public override void Flush()
      {

      }

      public override long Length
      {
        get { throw new Exception("The method or operation is not implemented."); }
      }

      public override long Position
      {
        get
        {
          if (get_pos != null)
          {
            return Convert.ToInt64(get_pos.Call());
          }
          return (int)AssertionViolation("port-position", "not supported");
        }
        set
        {
          if (set_pos != null)
          {
            set_pos.Call((int)value);
          }
        }
      }

      public override int Read(byte[] buffer, int offset, int count)
      {
        AssertionViolation("read", "cannot read from output port");
        return 0;
      }

      public override long Seek(long offset, SeekOrigin origin)
      {
        throw new Exception("The method or operation is not implemented.");
      }

      public override void SetLength(long value)
      {
        throw new Exception("The method or operation is not implemented.");
      }

      public override void Write(byte[] buffer, int offset, int count)
      {
        write.Call(buffer, offset, count);
      }

      public override string ToString()
      {
        return string.Format("#<custom-binary-output-port id: {0}>", id);
      }

      public override void Close()
      {
        if (close != null)
        {
          close.Call();
        }
      }
    }

    //(make-custom-binary-output-port id write! get-position set-position! close)
    [Builtin("make-custom-binary-output-port")]
    public static object MakeCustomBinaryOutputPort(object id, object read, object get_pos, object set_pos, object close)
    {
      return new CustomBinaryOutputStream(id, read, get_pos, set_pos, close);
    }

    class CustomTextWriter : TextWriter
    {
      string id;
      ICallable write, get_pos, set_pos, close;

      public CustomTextWriter(object id, object write, object get_pos, object set_pos, object close)
      {
        this.id = RequiresNotNull<string>(id);
        this.write = RequiresNotNull<ICallable>(write);
        // rest optional
        this.get_pos = get_pos as ICallable;
        this.set_pos = set_pos as ICallable;
        this.close = close as ICallable;
      }

      public override void Write(char[] buffer, int index, int count)
      {
        StringBuilder sb = new StringBuilder(new string(buffer));

        write.Call(sb, index, count);

        for (int i = 0; i < count; i++)
        {
          buffer[i] = sb[i];
        }
      }

      public bool HasPosition
      {
        get { return get_pos != null; }
      }

      public bool HasSetPosition
      {
        get { return set_pos != null; }
      }

      public int Position
      {
        get
        {
          if (get_pos != null)
          {
            return Convert.ToInt32(get_pos.Call());
          }
          return (int)AssertionViolation("port-position", "not supported");
        }
        set
        {
          if (set_pos != null)
          {
            set_pos.Call(value);
          }
        }
      }

      public override string ToString()
      {
        return string.Format("#<custom-textual-output-port id: {0}>", id);
      }

      public override void Close()
      {
        if (close != null)
        {
          close.Call();
        }
      }


      public override Encoding Encoding
      {
        get { return Encoding.Default; }
      }
    }


    //(make-custom-textual-input-port id read! get-position set-position! close)
    [Builtin("make-custom-textual-output-port")]
    public static object MakeCustomTextOutputPort(object id, object read, object get_pos, object set_pos, object close)
    {
      return new CustomTextWriter(id, read, get_pos, set_pos, close);
    }

    //binary output
    //(put-u8 binary-output-port octet)
    [Builtin("put-u8")]
    public static object PutU8(object binaryoutputport, object octet)
    {
      Stream s = RequiresNotNull<Stream>(binaryoutputport);
      byte b = (byte)RequiresNotNull<int>(octet);

      try
      {
        s.WriteByte(b);
        return Unspecified;
      }
      catch (Exception ex)
      {
        return AssertionViolation("put-u8", ex.Message, binaryoutputport, octet);
      }

    }


    //(put-bytevector binary-output-port bytevector)
    //(put-bytevector binary-output-port bytevector start)
    //(put-bytevector binary-output-port bytevector start count)
    [Builtin("put-bytevector")]
    public static object PutBytevector(object binaryoutputport, object bytevector)
    {
      Stream s = RequiresNotNull<Stream>(binaryoutputport);
      byte[] b = RequiresNotNull<byte[]>(bytevector);
      int j = 0;
      int k = b.Length;

      try
      {

        s.Write(b, j, k);

        return Unspecified;
      }
      catch (IOException ex)
      {
        return IOPortViolation("put-bytevector", ex.Message, binaryoutputport);
      }

    }

    [Builtin("put-bytevector")]
    public static object PutBytevector(object binaryoutputport, object bytevector, object start)
    {
      Stream s = RequiresNotNull<Stream>(binaryoutputport);
      byte[] b = RequiresNotNull<byte[]>(bytevector);
      int j = RequiresNotNull<int>(start);
      int k = b.Length - j;

      try
      {

        s.Write(b, j, k);

        return Unspecified;
      }
      catch (IOException ex)
      {
        return IOPortViolation("put-bytevector", ex.Message, binaryoutputport);
      }

    }

    [Builtin("put-bytevector")]
    public static object PutBytevector(object binaryoutputport, object bytevector, object start, object count)
    {
      Stream s = RequiresNotNull<Stream>(binaryoutputport);
      byte[] b = RequiresNotNull<byte[]>(bytevector);
      int j = RequiresNotNull<int>(start);
      int k = RequiresNotNull<int>(count);

      try
      {

        s.Write(b, j, k);

        return Unspecified;
      }
      catch (IOException ex)
      {
        return IOPortViolation("put-bytevector", ex.Message, binaryoutputport);
      }

    }

    //text output
    //(put-char textual-output-port char)
    [Builtin("put-char")]
    public static object PutChar(object textoutputport, object chr)
    {
      TextWriter s = RequiresNotNull<TextWriter>(textoutputport);
      char c = RequiresNotNull<char>(chr);

      try
      {

        s.Write(c);
        return Unspecified;
      }
      catch (IOException ex)
      {
        return IOPortViolation("put-char", ex.Message, textoutputport);
      }

    }

    //(put-string textual-output-port string) 
    //(put-string textual-output-port string start) 
    //(put-string textual-output-port string start count)
    [Builtin("put-string")]
    public static object PutString(object textoutputport, object str)
    {
      TextWriter s = RequiresNotNull<TextWriter>(textoutputport);
      string b = RequiresNotNull<string>(str);
      int j = 0;
      int k = b.Length;

      try
      {

        s.Write(b.ToCharArray(), j, k);

        return Unspecified;
      }
      catch (IOException ex)
      {
        return IOPortViolation("put-string", ex.Message, textoutputport);
      }

    }

    [Builtin("put-string")]
    public static object PutString(object textoutputport, object str, object start)
    {
      TextWriter s = RequiresNotNull<TextWriter>(textoutputport);
      string b = RequiresNotNull<string>(str);
      int j = RequiresNotNull<int>(start);
      int k = b.Length - j;

      try
      {

        s.Write(b.ToCharArray(), j, k);

        return Unspecified;
      }
      catch (IOException ex)
      {
        return IOPortViolation("put-string", ex.Message, textoutputport);
      }

    }

    [Builtin("put-string")]
    public static object PutString(object textoutputport, object str, object start, object count)
    {
      TextWriter s = RequiresNotNull<TextWriter>(textoutputport);
      string b = RequiresNotNull<string>(str);
      int j = RequiresNotNull<int>(start);
      int k = RequiresNotNull<int>(count);

      try
      {

        s.Write(b.ToCharArray(), j, k);

        return Unspecified;
      }
      catch (IOException ex)
      {
        return IOPortViolation("put-string", ex.Message, textoutputport);
      }

    }

    //(put-datum textual-output-port datum)
    [Builtin("put-datum")]
    public static object PutDatum(object textoutputport, object datum)
    {
      return Write(datum, textoutputport);
    }

    // input/output ports
    //(open-file-input/output-port filename) 
    //(open-file-input/output-port filename file-options)
    //(open-file-input/output-port filename file-options buffer-mode)
    //(open-file-input/output-port filename file-options buffer-mode transcoder)
    [Builtin("open-file-input/output-port")]
    public static object OpenFileInputOutputPort(object filename)
    {
      return OpenFileInputOutputPort(filename, 0);
    }

    [Builtin("open-file-input/output-port")]
    public static object OpenFileInputOutputPort(object filename, object fileoptions)
    {
      return OpenFileInputOutputPort(filename, fileoptions, bm_block);
    }

    [Builtin("open-file-input/output-port")]
    public static object OpenFileInputOutputPort(object filename, object fileoptions, object buffermode)
    {
      return OpenFileInputOutputPort(filename, fileoptions, bm_block, false);
    }

    [Builtin("open-file-input/output-port")]
    public static object OpenFileInputOutputPort(object filename, object fileoptions, object buffermode, object maybetranscoder)
    {
      string fn = RequiresNotNull<string>(filename);
      Transcoder tc = maybetranscoder as Transcoder;
      try
      {
        Stream s = File.Open(fn, FileMode.OpenOrCreate, FileAccess.ReadWrite);

        if (tc == null)
        {
          return s;
        }
        else
        {
          return TranscodedPort(s, tc);
        }
      }
      catch (FileNotFoundException ex)
      {
        return FileNotFoundViolation("open-file-input/output-port", ex.Message, filename);
      }
      catch (Exception ex)
      {
        return AssertionViolation("open-file-input/output-port", ex.Message, filename);
      }
    }



    class CustomBinaryInputOutputStream : CustomStream
    {
      string id;
      ICallable read, write, get_pos, set_pos, close;

      public CustomBinaryInputOutputStream(object id, object read, object write, object get_pos, object set_pos, object close)
      {
        this.id = RequiresNotNull<string>(id);
        this.read = RequiresNotNull<ICallable>(read);
        this.write = RequiresNotNull<ICallable>(write);
        // rest optional
        this.get_pos = get_pos as ICallable;
        this.set_pos = set_pos as ICallable;
        this.close = close as ICallable;
      }

      public override bool CanRead
      {
        get { return true; }
      }

      public override bool CanSeek
      {
        get { return set_pos != null; }
      }

      public override bool CanWrite
      {
        get { return true; }
      }

      public override void Flush()
      {

      }

      public override bool HasPosition
      {
        get { return get_pos != null; }
      }

      public override long Length
      {
        get { throw new Exception("The method or operation is not implemented."); }
      }

      public override long Position
      {
        get
        {
          if (get_pos != null)
          {
            return Convert.ToInt64(get_pos.Call());
          }
          AssertionViolation("get-position", "not supported");
          return 0;
        }
        set
        {
          if (set_pos != null)
          {
            set_pos.Call((int)value);
          }
        }
      }

      public override int Read(byte[] buffer, int offset, int count)
      {
        return (int)read.Call(buffer, offset, count);
      }

      public override long Seek(long offset, SeekOrigin origin)
      {
        throw new Exception("The method or operation is not implemented.");
      }

      public override void SetLength(long value)
      {
        throw new Exception("The method or operation is not implemented.");
      }

      public override void Write(byte[] buffer, int offset, int count)
      {
        write.Call(buffer, offset, count);
      }

      public override string ToString()
      {
        return string.Format("#<custom-binary-input/output-port id: {0}>", id);
      }

      public override void Close()
      {
        if (close != null)
        {
          close.Call();
        }
      }
    }

    //(make-custom-binary-input/output-port id read! write! get-position set-position! close)
    [Builtin("make-custom-binary-input/output-port")]
    public static object MakeCustomBinaryInputOutputPort(object id, object read, object write, object get_pos, object set_pos, object close)
    {
      return new CustomBinaryInputOutputStream(id, read, write, get_pos, set_pos, close);
    }


    //(make-custom-textual-input/output-port id read! write! get-position set-position! close)
  }
}

