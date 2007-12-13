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

#if R6RS
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
      SymbolId bm = RequiresNotNull<SymbolId>(s);
      return bm == bm_none || bm == bm_line || bm == bm_block;
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
      return false;
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
      return false;
    }


    //(port-has-port-position? port) 
    [Builtin("port-has-port-position?")]
    public static object PortHasPortPosition(object port)
    {
      if (port is Stream)
      {
        return true;
      }
      if (port is StreamReader || port is StreamWriter)
      {
        return true;
      }
      return false;
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
      return false;
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

    //(open-file-input-port filename) 
    //(open-file-input-port filename file-options)
    //(open-file-input-port filename file-options buffer-mode)
    //(open-file-input-port filename file-options buffer-mode maybe-transcoder)

    //(open-bytevector-input-port bytevector) 
    //(open-bytevector-input-port bytevector maybe-transcoder)

    //(open-string-input-port string)
    //(standard-input-port)

    //(make-custom-binary-input-port id read! get-position set-position! close)
    //(make-custom-textual-input-port id read! get-position set-position! close)

    // binary input
    //(get-u8 binary-input-port)
    //(lookahead-u8 binary-input-port)
    //(get-bytevector-n binary-input-port count)
    //(get-bytevector-n! binary-input-port bytevector start count)
    //(get-bytevector-some binary-input-port)
    //(get-bytevector-all binary-input-port)

    //text input
    //(get-char textual-input-port)
    //(lookahead-char textual-input-port)
    //(get-string-n textual-input-port count)
    //(get-string-n! textual-input-port string start count)
    //(get-string-all textual-input-port)
    //(get-line textual-input-port)
    //(get-datum textual-input-port)

    //output ports
    //(flush-output-port output-port)
    //(output-port-buffer-mode output-port)

    //(open-file-output-port filename) 
    //(open-file-output-port filename file-options)
    //(open-file-output-port filename file-options buffer-mode)
    //(open-file-output-port filename file-options buffer-mode maybe-transcoder)

    //(open-bytevector-output-port) 
    //(open-bytevector-output-port maybe-transcoder)

    //(call-with-bytevector-output-port proc) 
    //(call-with-bytevector-output-port proc maybe-transcoder)

    //(open-string-output-port)
    //(call-with-string-output-port proc)

    //(standard-output-port)
    //(standard-error-port)

    //(make-custom-binary-output-port id write! get-position set-position! close)
    //(make-custom-textual-output-port id write! get-position set-position! close)

    //binary output
    //(put-u8 binary-output-port octet)

    //(put-bytevector binary-output-port bytevector)
    //(put-bytevector binary-output-port bytevector start)
    //(put-bytevector binary-output-port bytevector start count)

    //text output
    //(put-char textual-output-port char)

    //(put-string textual-output-port string) 
    //(put-string textual-output-port string start) 
    //(put-string textual-output-port string start count)

    //(put-datum textual-output-port datum)

    // input/output ports
    //(open-file-input/output-port filename) 
    //(open-file-input/output-port filename file-options)
    //(open-file-input/output-port filename file-options buffer-mode)
    //(open-file-input/output-port filename file-options buffer-mode transcoder)

    //(make-custom-binary-input/output-port id read! write! get-position set-position! close)
    //(make-custom-textual-input/output-port id read! write! get-position set-position! close)
  }
}
#endif
