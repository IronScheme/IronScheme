#region License
/* Copyright (c) 2007-2015 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Threading;
using System.Web;
using System.Web.Hosting;

namespace IronScheme.Web.Hosting
{
  public class HttpListenerController
  {
    private Thread _pump, _ping;
    private bool _listening = false;
    private string _virtualDir;
    private string _physicalDir;
    private string[] _prefixes;
    private HttpListenerWrapper _listener;
    ManualResetEvent started;

    public HttpListenerController(string[] prefixes, string vdir, string pdir)
    {
      _prefixes = prefixes;
      _virtualDir = vdir;
      _physicalDir = pdir;
    }

    public void Start()
    {
      started = new ManualResetEvent(false);
      _listening = true;
      _pump = new Thread(new ThreadStart(Pump));
      _pump.Start();

      started.WaitOne();
    }

    public void Stop()
    {
      _listening = false;
      _listener.Stop();
      _pump.Join();
      _ping.Join();
    }

    void Ping()
    {
      while (_listening)
      {
        _listener.Ping();
        Thread.Sleep(1000);
      }
    }

    private void Pump()
    {
      try
      {
        _listener = (HttpListenerWrapper)ApplicationHost.CreateApplicationHost(
            typeof(HttpListenerWrapper), _virtualDir, _physicalDir);
        _listener.Configure(_prefixes, _virtualDir, _physicalDir);
        _listener.Start();

        _ping = new Thread(Ping);
        _ping.Start();

        Console.WriteLine("Listening on:");

        foreach (var pf in _prefixes)
        {
          Console.WriteLine(pf);
        }

        started.Set();

        while (_listening)
          _listener.ProcessRequest();
      }
      catch (AppDomainUnloadedException)
      {
        _listening = false;
        _ping.Join();
        Console.WriteLine("Restarting due to unloaded appdomain");
        Start();
      }
      catch (Exception ex)
      {
        Console.Error.WriteLine(ex);
      }
    }
  }

  public class HttpListenerWrapper : MarshalByRefObject
  {
    private HttpListener _listener;
    private string _virtualDir;
    private string _physicalDir;

    public void Configure(string[] prefixes, string vdir, string pdir)
    {
      _virtualDir = vdir;
      _physicalDir = pdir;
      _listener = new HttpListener();

      foreach (string prefix in prefixes)
        _listener.Prefixes.Add(prefix);
    }

    public override object InitializeLifetimeService()
    {
      return null;
    }

    public void Ping()
    {
      //Console.WriteLine("Ping: {0}", DateTime.Now.ToLongTimeString());
    }

    public void Start()
    {
      _listener.Start();
    }

    public void Stop()
    {
      _listener.Stop();
    }

    public void ProcessRequest()
    {
      HttpListenerContext ctx = _listener.GetContext();
      HttpListenerWorkerRequest workerRequest =
          new HttpListenerWorkerRequest(ctx, _virtualDir, _physicalDir);
      HttpRuntime.ProcessRequest(workerRequest);
    }
  }

  public class HttpListenerWorkerRequest : HttpWorkerRequest
  {
    private HttpListenerContext _context;
    private string _virtualDir;
    private string _physicalDir;

    public HttpListenerWorkerRequest(
        HttpListenerContext context, string vdir, string pdir)
    {
      if (null == context)
        throw new ArgumentNullException("context");
      if (null == vdir || vdir.Equals(""))
        throw new ArgumentException("vdir");
      if (null == pdir || pdir.Equals(""))
        throw new ArgumentException("pdir");

      _context = context;
      _virtualDir = vdir;
      _physicalDir = pdir;
    }

    // required overrides (abstract)
    public override void EndOfRequest()
    {
      _context.Response.OutputStream.Close();
      _context.Response.Close();
      //_context.Close();
    }

    public override void FlushResponse(bool finalFlush)
    {
      _context.Response.OutputStream.Flush();
    }

    public override string GetHttpVerbName()
    {
      return _context.Request.HttpMethod;
    }

    public override string GetHttpVersion()
    {
      return string.Format("HTTP/{0}.{1}",
          _context.Request.ProtocolVersion.Major,
          _context.Request.ProtocolVersion.Minor);
    }

    public override string GetLocalAddress()
    {
      return _context.Request.LocalEndPoint.Address.ToString();
    }

    public override int GetLocalPort()
    {
      return _context.Request.LocalEndPoint.Port;
    }

    public override string GetQueryString()
    {
      string queryString = "";
      string rawUrl = _context.Request.RawUrl;
      int index = rawUrl.IndexOf('?');
      if (index != -1)
        queryString = rawUrl.Substring(index + 1);
      return queryString;
    }

    public override string GetRawUrl()
    {
      return _context.Request.RawUrl;
    }

    public override string GetRemoteAddress()
    {
      return _context.Request.RemoteEndPoint.Address.ToString();
    }

    public override int GetRemotePort()
    {
      return _context.Request.RemoteEndPoint.Port;
    }

    public override string GetUriPath()
    {
      return _context.Request.Url.LocalPath;
    }

    public override void SendKnownResponseHeader(int index, string value)
    {
      _context.Response.Headers[
          HttpWorkerRequest.GetKnownResponseHeaderName(index)] = value;
    }

    public override void SendResponseFromMemory(byte[] data, int length)
    {
      _context.Response.OutputStream.Write(data, 0, length);
    }

    public override void SendStatus(int statusCode, string statusDescription)
    {
      _context.Response.StatusCode = statusCode;
      _context.Response.StatusDescription = statusDescription;
    }

    public override void SendUnknownResponseHeader(string name, string value)
    {
      _context.Response.Headers[name] = value;
    }

    public override void SendResponseFromFile(
        IntPtr handle, long offset, long length)
    { }

    public override void SendResponseFromFile(
        string filename, long offset, long length)
    {
      using (Stream s = File.OpenRead(filename))
      {
        byte[] buffer = new byte[length];
        int read = s.Read(buffer, (int)offset, buffer.Length);
        _context.Response.OutputStream.Write(buffer, 0, read);
      }
    }

    // additional overrides
    public override void CloseConnection()
    {
      //_context.Close();
    }

    public override string GetAppPath()
    {
      return _virtualDir;
    }

    public override string GetAppPathTranslated()
    {
      return _physicalDir;
    }

    public override int ReadEntityBody(byte[] buffer, int size)
    {
      return _context.Request.InputStream.Read(buffer, 0, size);
    }

    public override string GetUnknownRequestHeader(string name)
    {
      return _context.Request.Headers[name];
    }

    public override string[][] GetUnknownRequestHeaders()
    {
      string[][] unknownRequestHeaders;
      System.Collections.Specialized.NameValueCollection headers = _context.Request.Headers;
      int count = headers.Count;
      List<string[]> headerPairs = new List<string[]>(count);
      for (int i = 0; i < count; i++)
      {
        string headerName = headers.GetKey(i);
        if (GetKnownRequestHeaderIndex(headerName) == -1)
        {
          string headerValue = headers.Get(i);
          headerPairs.Add(new string[] { headerName, headerValue });
        }
      }
      unknownRequestHeaders = headerPairs.ToArray();
      return unknownRequestHeaders;
    }

    public override string GetKnownRequestHeader(int index)
    {
      switch (index)
      {
        case HeaderUserAgent:
          return _context.Request.UserAgent;
        default:
          return _context.Request.Headers[GetKnownRequestHeaderName(index)];
      }
    }

    public override string GetServerVariable(string name)
    {
      // TODO: vet this list
      switch (name)
      {
        case "HTTPS":
          return _context.Request.IsSecureConnection ? "on" : "off";
        case "HTTP_USER_AGENT":
          return _context.Request.Headers["UserAgent"];
        default:
          return null;
      }
    }

    public override string GetFilePath()
    {
      // TODO: this is a hack
      string s = _context.Request.Url.LocalPath;
      if (s.IndexOf(".aspx") != -1)
        s = s.Substring(0, s.IndexOf(".aspx") + 5);
      else if (s.IndexOf(".asmx") != -1)
        s = s.Substring(0, s.IndexOf(".asmx") + 5);
      return s;
    }

    public override string GetFilePathTranslated()
    {
      string s = GetFilePath();
      s = s.Substring(_virtualDir.Length);
      s = s.Replace('/', '\\');
      return _physicalDir + s;
    }

    public override string GetPathInfo()
    {
      string s1 = GetFilePath();
      string s2 = _context.Request.Url.LocalPath;
      if (s1.Length == s2.Length)
        return "";
      else
        return s2.Substring(s1.Length);
    }
  }
}