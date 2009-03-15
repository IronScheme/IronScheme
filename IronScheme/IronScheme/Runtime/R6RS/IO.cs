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
using System.Text.RegularExpressions;

namespace IronScheme.Runtime.R6RS
{
  public class IO : Builtins
  {
    static object bm_none = SymbolTable.StringToObject("none");
    static object bm_line = SymbolTable.StringToObject("line");
    static object bm_block = SymbolTable.StringToObject("block");

    //(buffer-mode? obj )
    [Builtin("buffer-mode?")]
    [Obsolete("Implemented in Scheme, do not use, remove if possible")]
    public static object IsBufferMode(object s)
    {
      if (s is SymbolId)
      {
        SymbolId bm = RequiresNotNull<SymbolId>(s);
        return GetBool(s == bm_none || s == bm_line || s == bm_block);
      }
      return FALSE;
    }

    internal class Transcoder
    {
      public Encoding codec = Encoding.Default;
      public object eolstyle = eol_crlf;
      public object handlingmode = SymbolTable.StringToObject("replace");

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
    static Encoding utf8 = new UTF8Encoding(false);
    static Encoding utf16 = new UnicodeEncoding(true, false);

    [Builtin("latin-1-codec")]
    public static object Latin1Codec()
    {
      return latin1;
    }

    [Builtin("utf-8-codec")]
    public static object Utf8Codec()
    {
      return utf8;
    }

    [Builtin("utf-16-codec")]
    public static object Utf16Codec()
    {
      return utf16;
    }

    static object eol_lf = SymbolTable.StringToObject("lf"); //10
    static object eol_cr = SymbolTable.StringToObject("cr"); //13
    static object eol_crlf = SymbolTable.StringToObject("crlf");
    static object eol_nel = SymbolTable.StringToObject("nel");
    static object eol_crnel = SymbolTable.StringToObject("crnel"); //194 133
    static object eol_ls = SymbolTable.StringToObject("ls"); // 226 128 168
    static object eol_none = SymbolTable.StringToObject("none");

    static string nel = "\u0085";
    static string ls = "\u2028";

    static string GetNewline(object symbolId, string current)
    {
      if (symbolId == eol_none)
      {
        return current;
      }
      else if (symbolId == eol_lf)
      {
        return "\n";
      }
      else if (symbolId == eol_cr)
      {
        return "\r";
      }
      else if (symbolId == eol_crlf)
      {
        return "\r\n";
      }
      else if (symbolId == eol_nel)
      {
        return nel;
      }
      else if (symbolId == eol_crnel)
      {
        return "\r" + nel;
      }
      else if (symbolId == eol_ls)
      {
        return ls;
      }
      else
      {
        return (string)AssertionViolation("transocde-port", "not a valid eol symbol", symbolId);
      }
    }

    //(native-eol-style)
    [Builtin("native-eol-style")]
    [Obsolete("Implemented in Scheme, do not use, remove if possible")]
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
      return MakeTranscoder(codec, NativeEolStyle(), SymbolTable.StringToObject("replace"));
    }

    [Builtin("make-transcoder")]
    public static object MakeTranscoder(object codec, object eolstyle)
    {
      return MakeTranscoder(codec, eolstyle, SymbolTable.StringToObject("replace"));
    }

    [Builtin("make-transcoder")]
    public static object MakeTranscoder(object codec, object eolstyle, object handlingmode)
    {
      Transcoder tc = new Transcoder();
      tc.codec = codec as Encoding;
      RequiresNotNull<SymbolId>(eolstyle);
      RequiresNotNull<SymbolId>(handlingmode);
      tc.eolstyle = eolstyle;
      tc.handlingmode = handlingmode;

      if (tc.handlingmode == SymbolTable.StringToObject("raise"))
      {
        tc.codec = Encoding.GetEncoding(tc.codec.WebName, new EncCB(tc), new DecCB(tc));
      }

      return tc;
    }

    class EncCB : EncoderFallback
    {
      Transcoder enc;

      public EncCB(Transcoder enc)
      {
        this.enc = enc;
      }

      public override EncoderFallbackBuffer CreateFallbackBuffer()
      {
        IOEncodingError();
        return null;
      }

      public override int MaxCharCount
      {
        get { return 1; }
      }
    }

    class DecCB : DecoderFallback
    {
      Transcoder enc;

      public DecCB(Transcoder enc)
      {
        this.enc = enc;
      }

      public override DecoderFallbackBuffer CreateFallbackBuffer()
      {
        IODecodingError();
        return null;
      }

      public override int MaxCharCount
      {
        get { return 1; }
      }
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

    static byte[] TrimFront(byte[] v, int k)
    {
      byte[] r = new byte[v.Length - k];
      Buffer.BlockCopy(v, k, r, 0, r.Length);
      return r;
    }

    //(bytevector->string bytevector transcoder)
    [Builtin("bytevector->string")]
    public static object ByteVectorToString(object bv, object tc)
    {
      byte[] b = RequiresNotNull<byte[]>(bv);
      Transcoder t = RequiresNotNull<Transcoder>(tc);
      byte[] preamble = t.codec.GetPreamble();
      if ((b[0] == 0xFF && b[1] == 0xFE) || (b[0] == 0xFE && b[1] == 0xFF))
      {
        b = TrimFront(b, 2);
      }

      string value = t.codec.GetString(b);

      if (t.eolstyle == eol_none)
      {
        value = value.Replace("\r", "");
      }
      else
      {
        value = eoltx.Replace(value, delegate(Match m)
        {
          return GetNewline(t.eolstyle, m.Value);
        });
      }

      return value;
    }

    //(string->bytevector string transcoder)
    [Builtin("string->bytevector")]
    public static object StringToByteVector(object s, object tc)
    {
      Transcoder t = RequiresNotNull<Transcoder>(tc);
      string value = RequiresNotNull<string>(s);

      value = lftx.Replace(value, delegate(Match m)
      {
        return GetNewline(t.eolstyle, m.Value);
      });

      return t.codec.GetBytes(value);
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
      return GetBool(port is TextReader || port is TextWriter || port is CustomTextReaderWriter);
    }

    //(binary-port? port)
    [Builtin("binary-port?")]
    public static object IsBinaryPort(object port)
    {
      return GetBool(port is Stream);
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
          TextWriter w = new TranscodedWriter(s, tc);
          return w;
        }
        return new TranscodedReader(s, tc);
      }
      if (s.CanWrite)
      {
        TextWriter w = new TranscodedWriter(s, tc);
        return w;
      }
      return FALSE;
    }


    static object TranscodedOutputPort(object binaryport, object transcoder)
    {
      Stream s = RequiresNotNull<Stream>(binaryport);
      Transcoder tc = RequiresNotNull<Transcoder>(transcoder);
      if (s.CanWrite)
      {
        TextWriter w = new TranscodedWriter(s, tc);
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
        return new TranscodedReader(s, tc);
      }
      return FALSE;
    }

    static object TranscodedInputOutputPort(object binaryport, object transcoder)
    {
      Stream s = RequiresNotNull<Stream>(binaryport);
      Transcoder tc = RequiresNotNull<Transcoder>(transcoder);
      return new CustomTextReaderWriter(SymbolTable.StringToObject("textual/input-output-port"), new TranscodedReader(s, tc), new TranscodedWriter(s, tc));
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
      if (port is CustomTextReaderWriter)
      {
        return PortHasPortPosition(((CustomTextReaderWriter)port).input);
      }
      if (port is TextWriter)
      {
        return FALSE;
      }
      return AssertionViolation("port-has-port-position?", "not a port", port);
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
        if (!((CustomStream)port).HasPosition)
        {
          return AssertionViolation("port-position", "not supplied to custom port", port);
        }
      }
      if (port is Stream)
      {
        return (int)((Stream)port).Position;
      }
      if (port is CustomTextReaderWriter)
      {
        return PortPosition(((CustomTextReaderWriter)port).input);
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
      if (port is CustomTextReaderWriter)
      {
        return PortHasSetPortPosition(((CustomTextReaderWriter)port).input);
      }
      return AssertionViolation("port-has-set-port-position!?", "not a port", port); ;
    }

    //(set-port-position! port pos)
    [Builtin("set-port-position!")]
    public static object SetPortPosition(object port, object pos)
    {
      int p = RequiresNotNull<int>(pos);

      if (port is CustomTextWriter)
      {
        CustomTextWriter ctw = (CustomTextWriter)port;
        if (ctw.HasSetPosition)
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
        if (ctr.HasSetPosition)
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
      else if (port is CustomTextReaderWriter)
      {
        return SetPortPosition(((CustomTextReaderWriter)port).input, pos);
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
      if (port is CustomTextReaderWriter)
      {
        return ClosePort(((CustomTextReaderWriter)port).input);
      }
      return Unspecified;
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
      else if (inputport is CustomTextReaderWriter)
      {
        return PortIsEof(((CustomTextReaderWriter)inputport).input);
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
      return OpenFileInputPort(filename, null);
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
          return TranscodedInputPort( new BufferedStream(s), tc);
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
    [Obsolete("Implemented in Scheme, do not use, remove if possible")]
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
          buffer[i + index] = sb[i + index];
        }

        return res;
      }

      public override int Peek()
      {
        int p = Position;
        int i = Read();
        Position = p;
        return i;
      }

      public override int Read()
      {
        char[] c = new char[1];
        int i = Read(c, 0, 1);
        if (i == 0)
        {
          return -1;
        }
        return c[0];
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
          
          if (c == -1)
          {
            return EOF;
          }
          // position does not advance on EOF
          s.Position--;
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

        if (r == 0)
        {
          return EOF;
        }
        else
        {
          while (r != k)
          {
            int rr = s.Read(buffer, r, k - r);

            if (rr == 0 )
            {
              if (r != 0)
              {
                break;
              }
              else
              {
                return EOF;
              }
            }

            r += rr;
          }
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
      byte[] buffer = RequiresNotNull<byte[]>(bytevector);

      try
      {

        int r = s.Read(buffer, j, k);

        if (r == 0)
        {
          return EOF;
        }
        else
        {
          while (r != k)
          {
            int rr = s.Read(buffer, j + r, k - r);

            if (rr == 0)
            {
              if (r != 0)
              {
                break;
              }
              else
              {
                return EOF;
              }
            }

            r += rr;
          }
        }

        return r;
      }
      catch (IOException ex)
      {
        return IOPortViolation("get-bytevector-n", ex.Message, binaryinputport);
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
        if (c == 0)
        {
          return EOF;
        }
        else
        {
          while (c < k)
          {
            int x = r.Read(buffer, c, k - c);
            if (x == 0)
            {
              break;
            }
            c += x;
          }
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

      string ss = GetStringN(textinputport, count) as string;

      for (int i = 0; i < ss.Length; i++)
      {
        s[j + i] = ss[i];
      }
      return ss.Length;
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
      if (outputport is CustomTextReaderWriter)
      {
        return FlushOutputPort(((CustomTextReaderWriter)outputport).output);
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
      return OpenFileOutputPort(filename, fileoptions, bm_block, FALSE);
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

    static FileMode GetMode(FileOptions fo, string filename)
    {
      if ((fo & FileOptions.NoCreate) != 0)
      {
        if ((fo & FileOptions.NoTruncate) != 0)
        {
          if (!File.Exists(filename))
          {
            return (FileMode)FileNotFoundViolation("open-file-output-port", "file does not exist", filename);
          }
          return FileMode.Open;
        }
        else
        {
          return FileMode.Truncate;
        }
      }
      else
      {
        if ((fo & FileOptions.NoTruncate) != 0)
        {
          if (File.Exists(filename) && (fo & FileOptions.NoFail) == 0)
          {
            return (FileMode)FileAlreadyExistsViolation("open-file-output-port", filename);
          }
          if ((fo & FileOptions.NoFail) != 0)
          {
            return FileMode.OpenOrCreate;
          }
          else
          {
            return FileMode.Append;
          }
        }
        else
        {
          if (File.Exists(filename) && (fo & FileOptions.NoFail) != 0)
          {
            return FileMode.Truncate;
          }
          else
          {
            return FileMode.CreateNew;
          }
        }
      }
    }

    [Builtin("open-file-output-port")]
    public static object OpenFileOutputPort(object filename, object fileoptions, object buffermode, object maybetranscoder)
    {
      string fn = RequiresNotNull<string>(filename);
      FileOptions fo = ToFileOptions(fileoptions);
      FileMode fm =  GetMode(fo, fn);
      
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
          return TranscodedOutputPort(new BufferedStream(s), tc);
        }
      }
      catch (Condition)
      {
        throw;
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
      return OpenBytevectorOutputPort(FALSE);
    }

    [Builtin("open-bytevector-output-port")]
    public static object OpenBytevectorOutputPort(object maybetranscoder)
    {
      Transcoder tc = maybetranscoder as Transcoder;

      MemoryStream s = new MemoryStream();

      CallTarget0 extract = delegate
      {
        byte[] r = s.ToArray();
        s.SetLength(0);
        return r;
      };

      return Values(tc == null ? s : TranscodedPort(s, tc), Closure.Make(Context, extract));
    }

    //(call-with-bytevector-output-port proc) 
    //(call-with-bytevector-output-port proc maybe-transcoder)
    [Builtin("call-with-bytevector-output-port")]
    public static object CallWithBytevectorOutputPort(object proc)
    {
      return CallWithBytevectorOutputPort(proc, FALSE);
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

    //(standard-output-port)
    [Builtin("standard-output-port")]
    [Obsolete("Implemented in Scheme, do not use, remove if possible")]
    public static object StandardOutputPort()
    {
      return Console.OpenStandardOutput();
    }

    //(standard-error-port)
    [Builtin("standard-error-port")]
    [Obsolete("Implemented in Scheme, do not use, remove if possible")]
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

      public override void Write(char value)
      {
        Write(new char[] { value }, 0, 1);
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

    static Regex eoltx = new Regex(string.Join("|", Array.ConvertAll<string,string>( new string[] { "\r\n", nel, "\r" + nel, ls, "\r", "\n", }, Regex.Escape)), RegexOptions.Compiled);
    static Regex lftx = new Regex("\\n", RegexOptions.Compiled);
    
    class TranscodedWriter : TextWriter, ITranscodedPort
    {
      Transcoder tc;
      Stream port;

      public TranscodedWriter(Stream s, Transcoder tc)
      {
        NewLine = "\n";
        this.tc = tc;
        this.port = s;
      }

      public override Encoding Encoding
      {
        get { return tc.codec; }
      }

      public override void Write(string value)
      {
        if (tc.eolstyle != eol_none)
        {
          value = lftx.Replace(value, delegate(Match m)
          {
            return GetNewline(tc.eolstyle, m.Value);
          });
        }
        base.Write(value);
      }

      public override void Write(char value)
      {
        byte[] bytes = Encoding.GetBytes(new char[] { value });
        port.Write(bytes, 0, bytes.Length);
      }

      public override void Close()
      {
        port.Close();
      }

      public override void Flush()
      {
        port.Flush();
      }


      public Stream BinaryPort
      {
        get { return port; }
      }
    }

    class TranscodedReader : TextReader, ITranscodedPort
    {
      Stream port;
      Transcoder tc;
      public TranscodedReader(Stream s, Transcoder tc)
      {
        this.tc = tc;
        this.port = s;
      }

      public override void Close()
      {
        port.Close();
      }

      public override int Peek()
      {
        long p = port.Position;
        int r = Read();
        port.Position = p;
        return r;
      }

      public override int Read()
      {
        if (port.Position == 0)
        {
          Encoding enc = ParsePreamble(port);
        }
        long p = port.Position;
        int max = tc.codec.GetMaxByteCount(1);
        byte[] buffer = new byte[max];
        int i = port.Read(buffer, 0, buffer.Length);
        if (i == 0)
        {
          return -1;
        }
        char[] chars = tc.codec.GetChars(buffer, 0, i);
        int len = tc.codec.GetByteCount(chars, 0, 1);
        port.Position = p + len;
        return chars[0];
      }

      Encoding ParsePreamble(Stream port)
      {
        byte[] b = new byte[4];
        int len = port.Read(b, 0, 4);

        if (b[0] == 0xff && b[1] == 0xfe)
        {
          port.Position = 2;
        }
        else if (b[0] == 0xfe && b[1] == 0xff)
        {
          port.Position = 2;
        }
        else if (b[0] == 0xfe && b[1] == 0xff)
        {
          port.Position = 4;
        }
        else if (b[0] == 0xfe && b[1] == 0xff)
        {
          port.Position = 4;
        }
        else
        {
          port.Position = 0;
        }

        return null;
      }
      
      public override string ReadToEnd()
      {
        string value = base.ReadToEnd();
        if (tc.eolstyle != eol_none)
        {
          value = eoltx.Replace(value, delegate(Match m)
          {
            return GetNewline(tc.eolstyle, "\n");
          });
        }
        else
        {
          value = value.Replace("\r", "");
        }
        return value;
      }

      public override int Read(char[] buffer, int index, int count)
      {
        return base.Read(buffer, index, count);
      }

      public override string ReadLine()
      {
        string value = base.ReadLine();
        if (tc.eolstyle != eol_none)
        {
          value = eoltx.Replace(value, delegate(Match m)
          {
            return GetNewline(tc.eolstyle, "\n");
          });
        }
        else
        {
          value = value.Replace("\r", "");
        }
        return value;
      }


      public Stream BinaryPort
      {
        get { return port; }
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

      try
      {
        s.Write(b);

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

      try
      {

        s.Write(b.Substring(j));

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

        s.Write(b.Substring(j, k));

        return Unspecified;
      }
      catch (IOException ex)
      {
        return IOPortViolation("put-string", ex.Message, textoutputport);
      }

    }

    // input/output ports
    //(open-file-input/output-port filename) 
    //(open-file-input/output-port filename file-options)
    //(open-file-input/output-port filename file-options buffer-mode)
    //(open-file-input/output-port filename file-options buffer-mode transcoder)
    [Builtin("open-file-input/output-port")]
    public static object OpenFileInputOutputPort(object filename)
    {
      return OpenFileInputOutputPort(filename, null);
    }

    [Builtin("open-file-input/output-port")]
    public static object OpenFileInputOutputPort(object filename, object fileoptions)
    {
      return OpenFileInputOutputPort(filename, fileoptions, bm_block);
    }

    [Builtin("open-file-input/output-port")]
    public static object OpenFileInputOutputPort(object filename, object fileoptions, object buffermode)
    {
      return OpenFileInputOutputPort(filename, fileoptions, bm_block, FALSE);
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
          return TranscodedInputOutputPort(new BufferedStream(s), tc);
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
    [Builtin("make-custom-textual-input/output-port")]
    public static object MakeCustomTextualInputOutputPort(object id, object read, object write, object get_pos, object set_pos, object close)
    {
      return new CustomTextReaderWriter(id, new CustomTextReader(id, read, get_pos, set_pos, close), new CustomTextWriter(id, write, get_pos, set_pos, close));
    }

    abstract class TextReaderWriter
    {
    }

    internal class CustomTextReaderWriter
    {
      public object id;
      public readonly TextWriter output;
      public readonly TextReader input;

      public CustomTextReaderWriter(object id, TextReader input, TextWriter output)
      {
        this.id = id;
        this.input = input;
        this.output = output;
      }

      public override string ToString()
      {
        return string.Format("#<custom-textual-input/output-port id: {0}>", id);
      }
    }
  }
}

