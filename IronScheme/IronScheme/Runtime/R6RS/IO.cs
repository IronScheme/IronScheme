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
      public Encoding codec;
      public SymbolId eolstyle;
      public SymbolId handlingmode;

      public static readonly Transcoder native;

      static Transcoder()
      {
        native = new Transcoder();
        native.codec = Encoding.Default;
        native.eolstyle = eol_crlf;
      }
    }

    // transcoders
    //(latin-1-codec)
    //(utf-8-codec) 
    //(utf-16-codec)

    [Builtin("latin-1-codec")]
    public static object Latin1Codec()
    {
      return Encoding.Default;
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
      return port is TextReader || port is TextWriter;
    }
    
    //(binary-port? port)
    [Builtin("binary-port?")]
    public static object IsBinaryPort(object port)
    {
      return port is Stream;
    }

    //(transcoded-port binary-port transcoder)
    [Builtin("transcoded-port")]
    public static object TranscodedPort(object binaryport, object transcoder)
    {
      Stream s = RequiresNotNull<Stream>(binaryport);
      Transcoder tc = RequiresNotNull<Transcoder>(transcoder);
      if (s.CanRead)
      {
        return new StreamReader(s, tc.codec);
      }
      if (s.CanWrite)
      {
        return new StreamWriter(s, tc.codec);
      }
      return FALSE;
    }


    //(port-has-port-position? port) 
    [Builtin("port-has-port-position?")]
    public static object PortHasPortPosition(object port)
    {
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
      if (port is Stream)
      {
        return ((Stream)port).Position;
      }
      if (port is StreamReader)
      {
        return ((StreamReader)port).BaseStream.Position;
      }
      if (port is StreamWriter)
      {
        return ((StreamWriter)port).BaseStream.Position;
      }

      return -1;
    }

    //(port-has-set-port-position!? port) 
    [Builtin("port-has-set-port-position!?")]
    public static object PortHasSetPortPosition(object port)
    {
      if (port is Stream)
      {
        return ((Stream)port).CanSeek;
      }
      if (port is StreamReader )
      {
        return ((StreamReader)port).BaseStream.CanSeek;
      }
      if (port is StreamWriter)
      {
        return ((StreamWriter)port).BaseStream.CanSeek;
      }
      return FALSE;
    }

    //(set-port-position! port pos)
    [Builtin("set-port-position!")]
    public static object SetPortPosition(object port, object pos)
    {
      int p = RequiresNotNull<int>(pos);

      if (port is Stream)
      {
        ((Stream)port).Position = p;
      }
      if (port is StreamReader)
      {
        ((StreamReader)port).BaseStream.Position = p;
      }
      if (port is StreamWriter)
      {
        ((StreamWriter)port).BaseStream.Position = p;
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
      object result = p.Call(port);
      ClosePort(port);
      return result;
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

    static SymbolId fo_replace = SymbolTable.StringToId("replace");
    

    //(open-file-input-port filename) 
    //(open-file-input-port filename file-options)
    //(open-file-input-port filename file-options buffer-mode)
    //(open-file-input-port filename file-options buffer-mode maybe-transcoder)
    [Builtin("open-file-input-port")]
    public static object OpenFileInputPort(object filename)
    {
      return OpenFileInputPort(filename, fo_replace);
    }

    [Builtin("open-file-input-port")]
    public static object OpenFileInputPort(object filename, object fileoptions)
    {
      return OpenFileInputPort(filename, fileoptions, bm_block);
    }

    [Builtin("open-file-input-port")]
    public static object OpenFileInputPort(object filename, object fileoptions, object buffermode)
    {
      return OpenFileInputPort(filename, fileoptions, bm_block, false);
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
          return TranscodedPort(s, tc);
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
      return OpenBytevectorInputPort(bytevector, false);
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
      return new StringReader(s);
    }

    //(standard-input-port)
    [Builtin("standard-input-port")]
    public static object StandardInputPort()
    {
      return Console.OpenStandardInput();
    }

    class CustomBinaryInputStream : Stream
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
            set_pos.Call(value);
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

      int c = s.ReadByte();
      if (c == -1)
      {
        return EOF;
      }
      return (byte)c;
    }

    //(lookahead-u8 binary-input-port)
    [Builtin("lookahead-u8")]
    public static object LookAheadU8(object binaryinputport)
    {
      Stream s = RequiresNotNull<Stream>(binaryinputport);
      if (s.CanSeek)
      {
        int c = s.ReadByte();
        s.Position--;
        if (c == -1)
        {
          return EOF;
        }
        return (byte)c;
      }
      return FALSE;
    }

    //(get-bytevector-n binary-input-port count)
    [Builtin("get-bytevector-n")]
    public static object GetBytevectorN(object binaryinputport, object count)
    {
      int k = RequiresNotNull<int>(count);
      Stream s = RequiresNotNull<Stream>(binaryinputport);

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

    //(get-bytevector-n! binary-input-port bytevector start count)
    [Builtin("get-bytevector-n!")]
    public static object GetBytevectorNN(object binaryinputport, object bytevector, object start, object count)
    {
      int k = RequiresNotNull<int>(count);
      int j = RequiresNotNull<int>(start);
      Stream s = RequiresNotNull<Stream>(binaryinputport);
      byte[] b = RequiresNotNull<byte[]>(bytevector);

      int r = s.Read(b, j, k);

      if (r == -1)
      {
        return EOF;
      }

      return r;
    }

    //(get-bytevector-some binary-input-port)
    [Builtin("get-bytevector-some")]
    public static object GetBytevectorSome(object binaryinputport)
    {
      Stream s = RequiresNotNull<Stream>(binaryinputport);

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

    //(get-bytevector-all binary-input-port)
    [Builtin("get-bytevector-all")]
    public static object GetBytevectorAll(object binaryinputport)
    {
      Stream s = RequiresNotNull<Stream>(binaryinputport);

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

    //text input
    //(get-char textual-input-port)
    [Builtin("get-char")]
    public static object GetChar(object textinputport)
    {
      TextReader r = RequiresNotNull<TextReader>(textinputport);

      int c = r.Read();
      if (c == -1)
      {
        return EOF;
      }
      return (char)c;
    }

    //(lookahead-char textual-input-port)
    [Builtin("lookahead-char")]
    public static object LookAheadChar(object textinputport)
    {
      TextReader r = RequiresNotNull<TextReader>(textinputport);

      int c = r.Peek();
      if (c == -1)
      {
        return EOF;
      }
      return (char)c;
    }

    //(get-string-n textual-input-port count)
    [Builtin("get-string-n")]
    public static object GetStringN(object textinputport, object count)
    {
      TextReader r = RequiresNotNull<TextReader>(textinputport);
      int k = RequiresNotNull<int>(count);

      char[] buffer = new char[k];

      int c = r.Read(buffer, 0, k);
      if (c == -1)
      {
        return EOF;
      }
      return new string(buffer, 0, c);
    }

    //(get-string-n! textual-input-port string start count)
    [Builtin("get-string-n!")]
    public static object GetStringNN(object textinputport, object str, object start, object count)
    {
      TextReader r = RequiresNotNull<TextReader>(textinputport);
      StringBuilder s = RequiresNotNull<StringBuilder>(str);
      int j = RequiresNotNull<int>(start);
      int k = RequiresNotNull<int>(count);

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

    //(get-string-all textual-input-port)
    [Builtin("get-string-all")]
    public static object GetStringAll(object textinputport)
    {
      TextReader r = RequiresNotNull<TextReader>(textinputport);

      string c = r.ReadToEnd();
      if (c == null)
      {
        return EOF;
      }
      return c;
    }

    //(get-line textual-input-port)
    [Builtin("get-line")]
    public static object GetLine(object textinputport)
    {
      TextReader r = RequiresNotNull<TextReader>(textinputport);

      string c = r.ReadLine();
      if (c == null)
      {
        return EOF;
      }
      return c;
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
      return OpenFileOutputPort(filename, fo_replace);
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

    [Builtin("open-file-output-port")]
    public static object OpenFileOutputPort(object filename, object fileoptions, object buffermode, object maybetranscoder)
    {
      string fn = RequiresNotNull<string>(filename);
      Transcoder tc = maybetranscoder as Transcoder;
      try
      {
        Stream s = File.Create(fn);

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
        return FileNotFoundViolation("open-file-output-port", ex.Message, filename);
      }
      catch (Exception ex)
      {
        return AssertionViolation("open-file-output-port", ex.Message, filename);
      }
    }

    //(open-bytevector-output-port) 
    //(open-bytevector-output-port maybe-transcoder)
    [Builtin("open-bytevector-output-port")]
    public static object OpenBytevectorOutputPort(object bytevector)
    {
      return OpenBytevectorOutputPort(bytevector, false);
    }

    [Builtin("open-bytevector-output-port")]
    public static object OpenBytevectorOutputPort(object bytevector, object maybetranscoder)
    {
      byte[] b = RequiresNotNull<byte[]>(bytevector);
      Transcoder tc = maybetranscoder as Transcoder;

      Stream s = new MemoryStream(b);

      if (tc == null)
      {
        return s;
      }
      else
      {
        return TranscodedPort(s, tc);
      }
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
        c.Call(tc == null ? s : TranscodedPort(s, tc));
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

    
    class CustomBinaryOutputStream : Stream
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
          AssertionViolation("get-position", "not supported");
          return 0;
        }
        set
        {
          if (set_pos != null)
          {
            set_pos.Call(value);
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
      byte b = RequiresNotNull<byte>(octet);

      s.WriteByte(b);
      return Unspecified;
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

      s.Write(b, j, k);

      return Unspecified;
    }

    [Builtin("put-bytevector")]
    public static object PutBytevector(object binaryoutputport, object bytevector, object start)
    {
      Stream s = RequiresNotNull<Stream>(binaryoutputport);
      byte[] b = RequiresNotNull<byte[]>(bytevector);
      int j = RequiresNotNull<int>(start);
      int k = b.Length - j;

      s.Write(b, j, k);

      return Unspecified;
    }

    [Builtin("put-bytevector")]
    public static object PutBytevector(object binaryoutputport, object bytevector, object start, object count)
    {
      Stream s = RequiresNotNull<Stream>(binaryoutputport);
      byte[] b = RequiresNotNull<byte[]>(bytevector);
      int j = RequiresNotNull<int>(start);
      int k = RequiresNotNull<int>(count);

      s.Write(b, j, k);
      
      return Unspecified;
    }

    //text output
    //(put-char textual-output-port char)
    [Builtin("put-char")]
    public static object PutChar(object textoutputport, object chr)
    {
      TextWriter s = RequiresNotNull<TextWriter>(textoutputport);
      char c = RequiresNotNull<char>(chr);

      s.Write(c);
      return Unspecified;
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

      s.Write(b.ToCharArray(), j, k);

      return Unspecified;
    }

    [Builtin("put-string")]
    public static object PutString(object textoutputport, object str, object start)
    {
      TextWriter s = RequiresNotNull<TextWriter>(textoutputport);
      string b = RequiresNotNull<string>(str);
      int j = RequiresNotNull<int>(start);
      int k = b.Length - j;

      s.Write(b.ToCharArray(), j, k);

      return Unspecified;
    }

    [Builtin("put-string")]
    public static object PutString(object textoutputport, object str, object start, object count)
    {
      TextWriter s = RequiresNotNull<TextWriter>(textoutputport);
      string b = RequiresNotNull<string>(str);
      int j = RequiresNotNull<int>(start);
      int k = RequiresNotNull<int>(count);

      s.Write(b.ToCharArray(), j, k);

      return Unspecified;
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
      return OpenFileInputOutputPort(filename, fo_replace);
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

    

    class CustomBinaryInputOutputStream : Stream
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
            set_pos.Call(value);
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

