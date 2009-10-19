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

  #region  Support classes

  public class Transcoder
  {
    public Encoding codec = Encoding.Default;
    public object eolstyle = SymbolTable.StringToObject("crlf");
    public object handlingmode = SymbolTable.StringToObject("replace");

    public static readonly Transcoder native;

    static Transcoder()
    {
      native = new Transcoder();
    }

    internal Transcoder()
    {

    }

    public Transcoder(Encoding codec, object eolstyle, object handlingmode)
    {
      this.codec = codec;
      this.eolstyle = eolstyle;
      this.handlingmode = handlingmode;
    }
  }

  [CLSCompliant(false)]
  public class EncCB : EncoderFallback
  {
    Callable error;
    public EncCB(Callable error)
    {
      this.error = error;
    }

    public override EncoderFallbackBuffer CreateFallbackBuffer()
    {
      if (error == null)
      {
        Builtins.IOEncodingError();
      }
      else
      {
        error.Call();
      }
      return null;
    }

    public override int MaxCharCount
    {
      get { return 1; }
    }
  }

  [CLSCompliant(false)]
  public class DecCB : DecoderFallback
  {
    Callable error;
    public DecCB(Callable error)
    {
      this.error = error;
    }

    public override DecoderFallbackBuffer CreateFallbackBuffer()
    {
      if (error == null)
      {
        Builtins.IODecodingError();
      }
      else
      {
        error.Call();
      }
      return null;
    }

    public override int MaxCharCount
    {
      get { return 1; }
    }
  }



  public abstract class CustomStream : Stream
  {
    public abstract bool HasPosition { get; }
  }

  [CLSCompliant(false)]
  public class CustomBinaryInputStream : CustomStream
  {
    string id;
    Callable read, get_pos, set_pos, close;

    public CustomBinaryInputStream(string id, Callable read, object get_pos, object set_pos, object close)
    {
      this.id = id;
      this.read = read;
      // rest optional
      this.get_pos = get_pos as Callable;
      this.set_pos = set_pos as Callable;
      this.close = close as Callable;
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
        return (int)Builtins.AssertionViolation("port-position", "not supported");
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
      Builtins.AssertionViolation("write", "cannot write to input port");
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

  [CLSCompliant(false)]
  public class CustomTextReader : TextReader
  {
    string id;
    Callable read, get_pos, set_pos, close;

    public CustomTextReader(string id, Callable read, object get_pos, object set_pos, object close)
    {
      this.id = id;
      this.read = read;
      // rest optional
      this.get_pos = get_pos as Callable;
      this.set_pos = set_pos as Callable;
      this.close = close as Callable;
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
        return (int)Builtins.AssertionViolation("port-position", "not supported");
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

  [CLSCompliant(false)]
  public class CustomBinaryOutputStream : CustomStream
  {
    string id;
    Callable write, get_pos, set_pos, close;

    public CustomBinaryOutputStream(string id, Callable write, object get_pos, object set_pos, object close)
    {
      this.id = id;
      this.write = write;
      // rest optional
      this.get_pos = get_pos as Callable;
      this.set_pos = set_pos as Callable;
      this.close = close as Callable;
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
        return (int)Builtins.AssertionViolation("port-position", "not supported");
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
      Builtins.AssertionViolation("read", "cannot read from output port");
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

  [CLSCompliant(false)]
  public class CustomTextWriter : TextWriter
  {
    string id;
    Callable write, get_pos, set_pos, close;

    public CustomTextWriter(string id, Callable write, object get_pos, object set_pos, object close)
    {
      this.id = id;
      this.write = write;
      // rest optional
      this.get_pos = get_pos as Callable;
      this.set_pos = set_pos as Callable;
      this.close = close as Callable;
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
        return (int)Builtins.AssertionViolation("port-position", "not supported");
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

  [CLSCompliant(false)]
  public class TranscodedWriter : TextWriter
  {
    Transcoder tc;
    Stream port;

    public Transcoder Transcoder
    {
      get { return tc; }
    }

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

    public override void Write(char value)
    {
      string s = new string(new char[] { value });
      byte[] bytes = IO.StringToByteVector(s, tc) as byte[];
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

  [CLSCompliant(false)]
  public class TranscodedReader : TextReader
  {
    Stream port;
    Transcoder tc;

    public Transcoder Transcoder
    {
      get { return tc; }
    }

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

      if (b[0] == 0xff && b[1] == 0xfe && (len < 4 || !(b[2] == 0 && b[3] == 0)))
      {
        port.Position = 2;
        return new UnicodeEncoding(false, false);
      }
      else if (b[0] == 0xfe && b[1] == 0xff && (len < 4 || !(b[2] == 0 && b[3] == 0)))
      {
        port.Position = 2;
        return new UnicodeEncoding(true, false);
      }
      else if (len >= 4 && b[0] == 0 && b[1] == 0 && b[2] == 0xfe && b[3] == 0xff)
      {
        port.Position = 4;
        return new UTF32Encoding(false, false);
      }
      else if (len >= 4 && b[0] == 0xff && b[1] == 0xef && b[2] == 0 && b[3] == 0)
      {
        port.Position = 4;
        return new UTF32Encoding(true, false);
      }
      else
      {
        port.Position = 0;
        return Encoding.UTF8;
      }
    }

    public override string ReadToEnd()
    {
      string value = base.ReadToEnd();
      if (tc.eolstyle != IO.eol_none)
      {
        value = IO.eoltx.Replace(value, delegate(Match m)
        {
          return IO.GetNewline(tc.eolstyle, "\n");
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
      if (tc.eolstyle != IO.eol_none)
      {
        value = IO.eoltx.Replace(value, delegate(Match m)
        {
          return IO.GetNewline(tc.eolstyle, "\n");
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
  
  [CLSCompliant(false)]
  public class CustomBinaryInputOutputStream : CustomStream
  {
    string id;
    Callable read, write, get_pos, set_pos, close;

    public CustomBinaryInputOutputStream(string id, Callable read, Callable write, object get_pos, object set_pos, object close)
    {
      this.id = id;
      this.read = read;
      this.write = write;
      // rest optional
      this.get_pos = get_pos as Callable;
      this.set_pos = set_pos as Callable;
      this.close = close as Callable;
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
        Builtins.AssertionViolation("get-position", "not supported");
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


  [CLSCompliant(false)]
  public class CustomTextReaderWriter
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

  #endregion
  
  public class IO : Builtins
  {
    static object bm_line = SymbolTable.StringToObject("line");

    static object eol_lf = SymbolTable.StringToObject("lf"); //10
    static object eol_cr = SymbolTable.StringToObject("cr"); //13
    internal static object eol_crlf = SymbolTable.StringToObject("crlf");
    static object eol_nel = SymbolTable.StringToObject("nel");
    static object eol_crnel = SymbolTable.StringToObject("crnel"); //194 133
    static object eol_ls = SymbolTable.StringToObject("ls"); // 226 128 168
    internal static object eol_none = SymbolTable.StringToObject("none");

    static string nel = "\u0085";
    static string ls = "\u2028";

    internal static string GetNewline(object symbolId, string current)
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

    static object TranscodedInputOutputPort(object binaryport, object transcoder)
    {
      Stream s = RequiresNotNull<Stream>(binaryport);
      Transcoder tc = RequiresNotNull<Transcoder>(transcoder);
      return new CustomTextReaderWriter(SymbolTable.StringToObject("textual/input-output-port"), 
        new TranscodedReader(s, tc), new TranscodedWriter(s, tc));
    }

    [Builtin("open-file-output-port")]
    public static object OpenFileOutputPort(object filename)
    {
      return OpenFileOutputPort(filename, null);
    }

    [Builtin("open-file-output-port")]
    public static object OpenFileOutputPort(object filename, object fileoptions)
    {
      return OpenFileOutputPort(filename, fileoptions, bm_line);
    }

    [Builtin("open-file-output-port")]
    public static object OpenFileOutputPort(object filename, object fileoptions, object buffermode)
    {
      return OpenFileOutputPort(filename, fileoptions, buffermode, FALSE);
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
      if (fo == FALSE)
      {
        return FileOptions.NoFail;
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
      FileMode fm = GetMode(fo, fn);
      bool blockbuffer = buffermode == SymbolTable.StringToObject("block");
      bool nobuffer = buffermode == SymbolTable.StringToObject("none");

      Transcoder tc = maybetranscoder as Transcoder;
      try
      {
        Stream s = File.Open(fn, fm, FileAccess.Write);

        if (blockbuffer)
        {
          s = new BufferedStream(s);
        }

        if (tc == null)
        {
          return s;
        }
        else
        {
          return TranscodedOutputPort(s, tc);
        }
      }
      catch (Continuation)
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

    internal static Regex eoltx = new Regex(string.Join("|", 
      Array.ConvertAll<string, string>(
      new string[] { "\r\n", nel, "\r" + nel, ls, "\r", "\n", }, Regex.Escape)), RegexOptions.Compiled);
    internal static Regex lftx = new Regex("\\n", RegexOptions.Compiled);

    [Builtin("open-file-input/output-port")]
    public static object OpenFileInputOutputPort(object filename)
    {
      return OpenFileInputOutputPort(filename, null);
    }

    [Builtin("open-file-input/output-port")]
    public static object OpenFileInputOutputPort(object filename, object fileoptions)
    {
      return OpenFileInputOutputPort(filename, fileoptions, bm_line);
    }

    [Builtin("open-file-input/output-port")]
    public static object OpenFileInputOutputPort(object filename, object fileoptions, object buffermode)
    {
      return OpenFileInputOutputPort(filename, fileoptions, buffermode, FALSE);
    }

    [Builtin("open-file-input/output-port")]
    public static object OpenFileInputOutputPort(object filename, object fileoptions, object buffermode, object maybetranscoder)
    {
      string fn = RequiresNotNull<string>(filename);
      Transcoder tc = maybetranscoder as Transcoder;
      bool blockbuffer = buffermode == SymbolTable.StringToObject("block");
      try
      {
        Stream s = File.Open(fn, FileMode.OpenOrCreate, FileAccess.ReadWrite);

        if (blockbuffer)
        {
          s = new BufferedStream(s);
        }

        if (tc == null)
        {
          return s;
        }
        else
        {
          return TranscodedInputOutputPort(s, tc);
        }
      }
      catch (Continuation)
      {
        throw;
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
  }
}

