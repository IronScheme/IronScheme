namespace IronScheme.Runtime
{
  // this file is replaced by IL implementation. just here to keep VS intellisense happy
  public abstract class Glue
  {
    public static object AssertionViolation(object who, object message, params object[] irritants) => null;
    public static object Exact(object obj) => null;
    public static object Inexact(object obj) => null;
    public static object FileAlreadyExistsViolation(object who, object filename) => null;
    public static object FileInUseViolation(object who, object filename) => null;
    public static object FileNotFoundViolation(object who, object message, object filename) => null;
    public static object IODecodingError() => null;
    public static object IOEncodingError() => null;
    public static object IOPortViolation(object who, object message, object port) => null;
    public static object LexicalError(string msg, object what) => null;
    public static object SyntaxError(object who, object message, object form, object subform) => null;
    public static object UndefinedError(object sym) => null;
    public static object Raise(object obj) => null;
    public static object RaiseContinueable(object obj) => null;
    public static object WithExceptionHandler(object handler, object thunk) => null;
  }
}
