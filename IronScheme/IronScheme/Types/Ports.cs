using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

using ICallable = IronScheme.Runtime.ICallable;

namespace IronScheme.Types
{
  public sealed class Codec
  {
    readonly Encoding enc;
    readonly byte[] preamble;

    Codec(Encoding enc, byte[] preamble)
    {
      this.enc = enc;
      this.preamble = preamble;
    }

    public static readonly Codec Naitve = new Codec(Encoding.Default, new byte[0]);

    public static readonly Codec Latin1 = new Codec(Encoding.GetEncoding("iso-8859-1"), new byte[0]);
    public static readonly Codec Utf8 = new Codec(new UTF8Encoding(false), new byte[] { 0xff, 0xfe });
    public static readonly Codec Utf16 = new Codec(new UnicodeEncoding(false, false), new byte[0]);
  }

  public sealed class Transcoder
  {
    readonly Codec codec;
    readonly object eol;
    readonly object error;
  }

  public abstract class Port
  {
    protected virtual int LookaheadLength
    {
      get;
      set;
    }

    public virtual void Close()
    {
    }

    protected virtual bool HasGetPositionInternal()
    {
      return false;
    }

    public bool HasGetPosition()
    {
      return HasGetPositionInternal();
    }

    protected virtual bool HasSetPositionInternal()
    {
      return false;
    }

    public bool HasSetPosition()
    {
      return HasSetPositionInternal();
    }

    protected virtual int GetPositionInternal()
    {
      throw new NotSupportedException();
    }

    protected virtual void SetPositionInternal(int pos)
    {
      throw new NotSupportedException();
    }

    public int Position
    {
      get { return GetPositionInternal() - LookaheadLength; }
      set { SetPositionInternal(value); LookaheadLength = 0; }
    }

    public virtual bool CanRead
    {
      get { return false; }
    }

    public virtual bool CanWrite
    {
      get { return false; }
    }

    public virtual void Flush()
    {
    }

    public static readonly object EOF = new object();

  }

  public abstract class BinaryPort : Port
  {
    readonly object buffermode;



    public int ReadByte()
    {
      byte[] b = new byte[1];
      int i = Read(b, 0, 1);
      if (i == 0)
      {
        return -1;
      }
      return b[0];
    }

    public void WriteByte(int octet)
    {
      if (octet == -1)
      {
        // write EOF...
        byte[] b = { };
        Write(b, 0, 0);
      }
      else
      {
        byte[] b = new byte[] { (byte)octet };
        Write(b, 0, 1);
      }
    }

    public abstract int Read(byte[] data, int start, int count);
    public abstract int Write(byte[] data, int start, int count);
  }

  public class BinaryStreamPort : BinaryPort
  {
    readonly Stream port;

    public BinaryStreamPort(Stream s)
    {
      this.port = s;
    }

    public override int Read(byte[] data, int start, int count)
    {
      return port.Read(data, start, count);
    }

    public override int Write(byte[] data, int start, int count)
    {
      int pos = -1;
      if (HasGetPosition())
      {
        pos = Position;
      }
      port.Write(data, start, count);
      if (HasGetPosition())
      {
        return Position - pos;
      }
      return count;
    }

    public override void Close()
    {
      port.Close();
    }

    public override bool CanRead
    {
      get { return port.CanRead; }
    }

    public override bool CanWrite
    {
      get { return port.CanWrite; }
    }

    public override void Flush()
    {
      port.Flush();
    }

    protected override int GetPositionInternal()
    {
      return (int)port.Position;
    }

    protected override void SetPositionInternal(int pos)
    {
      port.Position = pos;
    }

    protected override bool HasGetPositionInternal()
    {
      try
      {
        long p = port.Position;
        return true;
      }
      catch (IOException)
      {
        return false;
      }
    }

    protected override bool HasSetPositionInternal()
    {
      return port.CanSeek;
    }
  }

  public sealed class TranscodedPort : TextualPort
  {
    readonly BinaryPort binaryport;
    readonly Transcoder transcoder;

    public TranscodedPort(BinaryPort port, Transcoder tc)
    {
      this.binaryport = port;
      this.transcoder = tc;
    }

    protected override int ReadInternal(UnicodeString s, int start, int count)
    {
      return 0;
    }

    protected override int WriteInternal(UnicodeString s, int start, int count)
    {
      return 0;
    }
  }

  public abstract class TextualPort : Port
  {
    bool haslookahead = false;
    readonly UnicodeString lookahead = new UnicodeString(1);

    protected abstract int ReadInternal(UnicodeString s, int start, int count);
    protected abstract int WriteInternal(UnicodeString s, int start, int count);

    protected override int LookaheadLength
    {
      get { return haslookahead ? 1 : 0; }
      set { haslookahead = value == 0 ? false : true; }
    }

    public object ReadString(int count)
    {
      UnicodeString s = new UnicodeString(count);
      if (count == 0)
      {
        return s;
      }
      object l = Read(s, 0, count);
      if (l == EOF)
      {
        return l;
      }

      return s.Substring(0, (int)l);
    }

    public object ReadLine()
    {
      List<int> chars = new List<int>();
      bool didread = false;

      while (true)
      {
        object c = ReadChar();

        if (c == EOF)
        {
          break;
        }

        didread = true;

        UnicodeScalar ch = (UnicodeScalar)c;

        if (ch == '\r')
        {
          object p = PeekChar();
          if (p != EOF)
          {
            UnicodeScalar ph = (UnicodeScalar)p;
            if (ph == '\n' || ph == '\u0085')
            {
              ReadChar();
            }
          }
          break;
        }
        else if (ch == '\n' || ch == '\u2028' || ch == '\u0085')
        {
          break;
        }
        else
        {
          chars.Add(ch.Value);
        }
      }

      if (chars.Count == 0 && !didread)
      {
        return EOF;
      }

      return new UnicodeString(chars, false);
    }

    public object ReadAll()
    {
      const int BUFFERSIZE = 1024;
      List<UnicodeString> all = new List<UnicodeString>();

      while (true)
      {
        UnicodeString s = new UnicodeString(BUFFERSIZE);

        object l = Read(s, 0, BUFFERSIZE);

        if (l == EOF)
        {
          break;
        }

        all.Add(s.Substring(0, (int)l));

      }

      if (all.Count == 0)
      {
        return EOF;
      }

      return UnicodeString.Concat(all.ToArray());
    }

    public object Read(UnicodeString s, int start, int count)
    {
      if (CanRead)
      {
        int diff = 0;
        if (haslookahead && count > 0)
        {
          s[0] = lookahead[0];
          start++;
          count--;
          diff = 1;
          haslookahead = false;

          if (count == 0)
          {
            return diff;
          }
        }
        int l = ReadInternal(s, start, count) + diff;
        if (l == 0)
        {
          return EOF;
        }
        return l;
      }
      else
      {
        throw new NotSupportedException();
      }
    }

    public void Write(UnicodeString s, int start, int count)
    {
      if (CanWrite)
      {
        if (LookaheadLength > 0)
        {
          throw new NotSupportedException();
        }
        WriteInternal(s, start, count);
      }
      else
      {
        throw new NotSupportedException();
      }
    }

    public object PeekChar()
    {
      if (CanRead)
      {
        if (HasGetPosition() && HasSetPosition())
        {
          int p = Position;
          object c = ReadChar();
          Position = p;
          return c;
        }
        else
        {
          if (haslookahead)
          {
            return lookahead[0];
          }
          object c = ReadChar();
          if (c == EOF)
          {
            return c;
          }
          lookahead[0] = (UnicodeScalar)c;
          haslookahead = true;
          return c;
        }
      }
      else
      {
        throw new NotSupportedException();
      }
    }

    public object ReadChar()
    {
      if (CanRead)
      {
        UnicodeString s = new UnicodeString(1);
        object r = Read(s, 0, 1);
        if (r == EOF)
        {
          return r;
        }
        return s[0];
      }
      else
      {
        throw new NotSupportedException();
      }
    }

    public void WriteChar(UnicodeScalar c)
    {
      if (CanWrite)
      {
        UnicodeString s = new UnicodeString(new int[] { c.Value }, true);
        Write(s, 0, 1);
      }
      else
      {
        throw new NotSupportedException();
      }
    }
  }

  public abstract class TextualOutputPort : TextualPort
  {
    public sealed override bool CanRead
    {
      get { return false; }
    }

    public sealed override bool CanWrite
    {
      get { return true; }
    }
  }

  public abstract class TextualInputPort : TextualPort
  {
    public sealed override bool CanRead
    {
      get { return true; }
    }

    public sealed override bool CanWrite
    {
      get { return false; }
    }

  }

  public abstract class TextualInputOutputPort : TextualPort
  {
    public sealed override bool CanRead
    {
      get { return true; }
    }

    public sealed override bool CanWrite
    {
      get { return true; }
    }
  }

  public abstract class CustomTextualPort : TextualPort
  {
    readonly string id;
    readonly ICallable read, write, get_pos, set_pos, close;

    internal CustomTextualPort(string id, object read, object write, object get_pos, object set_pos, object close)
    {
      this.id = id;
      this.read = read as ICallable;
      this.write = write as ICallable;
      this.get_pos = get_pos as ICallable;
      this.set_pos = set_pos as ICallable;
      this.close = close as ICallable;
    }

    public sealed override void Close()
    {
      close.Call();
    }

    protected override bool HasGetPositionInternal()
    {
      return get_pos != null;
    }

    protected override bool HasSetPositionInternal()
    {
      return set_pos != null;
    }

    public sealed override bool CanRead
    {
      get { return read != null; }
    }

    public sealed override bool CanWrite
    {
      get { return write != null; }
    }

    protected override int GetPositionInternal()
    {
      if (HasGetPosition())
      {
        return (int)get_pos.Call();
      }
      else
      {
        throw new NotSupportedException();
      }
    }

    protected override void SetPositionInternal(int pos)
    {
      if (HasSetPosition())
      {
        set_pos.Call(pos);
      }
      else
      {
        throw new NotSupportedException();
      }
    }

    protected override int ReadInternal(UnicodeString s, int start, int count)
    {
      if (CanRead)
      {
        return (int)read.Call(s, start, count);
      }
      else
      {
        throw new NotSupportedException();
      }
    }

    protected override int WriteInternal(UnicodeString s, int start, int count)
    {
      if (CanWrite)
      {
        return (int)write.Call(s, start, count);
      }
      else
      {
        throw new NotSupportedException();
      }
    }
  }


  public sealed class CustomTextualInputPort : CustomTextualPort
  {
    public CustomTextualInputPort(string id, object read, object get_pos, object set_pos, object close)
      : base(id, read, null, get_pos, set_pos, close)
    {
    }
  }

  public sealed class CustomTextualOutputPort : CustomTextualPort
  {
    public CustomTextualOutputPort(string id, object write, object get_pos, object set_pos, object close)
      : base(id, null, write, get_pos, set_pos, close)
    {
    }
  }

  public sealed class CustomTextualInputOutputPort : CustomTextualPort
  {
    public CustomTextualInputOutputPort(string id, object read, object write, object get_pos, object set_pos, object close)
      : base(id, read, write, get_pos, set_pos, close)
    {
    }
  }





  public class StringOutputPort : TextualOutputPort
  {
    int length;
    int position;
    UnicodeString buffer;

    public StringOutputPort(int initialsize)
    {
      buffer = new UnicodeString(initialsize);
    }

    protected override int GetPositionInternal()
    {
      return position;
    }

    protected override void SetPositionInternal(int pos)
    {
      position = pos;
    }

    protected override bool HasGetPositionInternal()
    {
      return true;
    }

    protected override bool HasSetPositionInternal()
    {
      return true;
    }

    protected override int WriteInternal(UnicodeString s, int start, int count)
    {
      EnsureCapacity(count);
      for (int i = 0; i < count; i++)
      {
        buffer[position++] = s[start + i];
        if (length < position + 1)
        {
          length = position + 1;
        }
      }
      return count;
    }

    protected override int ReadInternal(UnicodeString s, int start, int count)
    {
      throw new NotImplementedException();
    }

    void EnsureCapacity(int count)
    {
      if (position + count >= buffer.Length)
      {
        buffer = UnicodeString.Concat(buffer, new UnicodeString(buffer.Length));
      }
    }
  }

  public class StringInputPort : TextualInputPort
  {
    int position;
    UnicodeString buffer;

    public StringInputPort(UnicodeString str)
    {
      buffer = str.Copy();
    }

    protected override int ReadInternal(UnicodeString s, int start, int count)
    {
      for (int i = 0; i < count; i++)
      {
        if (position == buffer.Length)
        {
          return i;
        }
        s[start + i] = buffer[position++];
      }
      return count;
    }

    protected override int WriteInternal(UnicodeString s, int start, int count)
    {
      throw new NotImplementedException();
    }

    protected override int GetPositionInternal()
    {
      return position;
    }

    protected override void SetPositionInternal(int pos)
    {
      position = pos;
    }

    protected override bool HasGetPositionInternal()
    {
      return true;
    }

    protected override bool HasSetPositionInternal()
    {
      return true;
    }
  }
}
