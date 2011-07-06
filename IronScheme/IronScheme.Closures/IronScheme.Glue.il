

.assembly extern ironscheme.boot
{
  .ver 0:0:0:0
}


.class public abstract auto ansi beforefieldinit IronScheme.Runtime.Glue
       extends [mscorlib]System.Object
{
  .method family hidebysig specialname rtspecialname 
          instance void  .ctor() cil managed
  {
    .maxstack  8
    ldarg.0
    call       instance void [mscorlib]System.Object::.ctor()
    ret
  } // end of method Glue::.ctor

  .method public hidebysig static object 
          AssertionViolation(object who,
                             object message,
                             object[] irritants) cil managed
  {
    .param [3]
    .custom instance void [mscorlib]System.ParamArrayAttribute::.ctor() = ( 01 00 00 00 ) 
    .maxstack  8
    ldarg.0
    ldarg.1
    ldsfld     class IronScheme.Runtime.ConsFromArrayHandler IronScheme.Runtime.Closure::ConsFromArray
    ldarg.2
    callvirt   instance object IronScheme.Runtime.ConsFromArrayHandler::Invoke(object[])
    tail.
    call object [ironscheme.boot]'#'::'ironscheme.exceptions::assertion-violation+'(object, object, object)
    ret
  } // end of method Glue::AssertionViolation

  .method public hidebysig static object 
          Exact(object obj) cil managed
  {
    .maxstack  8
    ldarg.0
    tail.
    call object [ironscheme.boot]'#'::'ironscheme.numbers::exact'(object)
    ret
  } // end of method Glue::Exact

  .method public hidebysig static object 
          Inexact(object obj) cil managed
  {
    .maxstack  8
    ldarg.0
    tail.
    call object [ironscheme.boot]'#'::'ironscheme.numbers::inexact'(object)
    ret
  } // end of method Glue::Inexact

  .method public hidebysig static object 
          FileAlreadyExistsViolation(object who,
                                     object filename) cil managed
  {
    .maxstack  8
    ldarg.0
    ldarg.1
    tail.
    call object [ironscheme.boot]'#'::'ironscheme.exceptions::file-already-exists-violation'(object, object)
    ret
  } // end of method Glue::FileAlreadyExistsViolation

  .method public hidebysig static object 
          FileInUseViolation(object who,
                             object filename) cil managed
  {
    .maxstack  8
    ldarg.0
    ldarg.1
    tail.
    call object [ironscheme.boot]'#'::'ironscheme.exceptions::file-in-use-violation'(object, object)
    ret
  } // end of method Glue::FileInUseViolation

  .method public hidebysig static object 
          FileNotFoundViolation(object who,
                                object message,
                                object filename) cil managed
  {
    .maxstack  8
    ldarg.0
    ldarg.1
    ldarg.2
    tail.
    call object [ironscheme.boot]'#'::'ironscheme.exceptions::file-not-found-violation'(object, object, object)
    ret
  } // end of method Glue::FileNotFoundViolation

  .method public hidebysig static object 
          IODecodingError() cil managed
  {
    .maxstack  8
    tail.
    call object [ironscheme.boot]'#'::'ironscheme.exceptions::io-decoding-error'()
    ret
  } // end of method Glue::IODecodingError

  .method public hidebysig static object 
          IOEncodingError() cil managed
  {
    .maxstack  8
    tail.
    call object [ironscheme.boot]'#'::'ironscheme.exceptions::io-encoding-error'()
    ret
  } // end of method Glue::IOEncodingError

  .method public hidebysig static object 
          IOPortViolation(object who,
                          object message,
                          object port) cil managed
  {
    .maxstack  8
    ldarg.0
    ldarg.1
    ldarg.2
    tail.
    call object [ironscheme.boot]'#'::'ironscheme.exceptions::i/o-port-violation'(object, object, object)
    ret
  } // end of method Glue::IOPortViolation

  .method public hidebysig static object 
          LexicalError(string msg,
                       object what) cil managed
  {
    .maxstack  8
    ldarg.0
    ldarg.1
    tail.
    call object [ironscheme.boot]'#'::'ironscheme.exceptions::lexical-error'(object, object)
    ret
  } // end of method Glue::LexicalError

  .method public hidebysig static object 
          SyntaxError(object who,
                      object message,
                      object form,
                      object subform) cil managed
  {
    .maxstack  8
    ldarg.0
    ldarg.1
    ldarg.2
    ldarg.3
    tail.
    call object [ironscheme.boot]'#'::'ironscheme.exceptions::syntax-error'(object, object, object, object)
    ret
  } // end of method Glue::SyntaxError

  .method public hidebysig static object 
          UndefinedError(object sym) cil managed
  {
    .maxstack  8
    ldarg.0
    tail.
    call object [ironscheme.boot]'#'::'ironscheme.exceptions::undefined-error'(object)
    ret
  } // end of method Glue::UndefinedError

  .method public hidebysig static object 
          Raise(object obj) cil managed
  {
    .maxstack  8
    ldarg.0
    tail.
    call object [ironscheme.boot]'#'::'ironscheme.exceptions::raise'(object)
    ret
  } // end of method Glue::Raise

  .method public hidebysig static object 
          RaiseContinueable(object obj) cil managed
  {
    .maxstack  8
     ldarg.0
    tail.
    call object [ironscheme.boot]'#'::'ironscheme.exceptions::raise-continuable'(object)
    ret
  } // end of method Glue::RaiseContinueable

  .method public hidebysig static object 
          WithExceptionHandler(object 'handler',
                               object thunk) cil managed
  {
    .maxstack  8
    ldarg.0
    ldarg.1
    tail.
    call object [ironscheme.boot]'#'::'ironscheme.exceptions::with-exception-handler'(object, object)
    ret
  } // end of method Glue::WithExceptionHandler

} // end of class IronScheme.Runtime.Glue
