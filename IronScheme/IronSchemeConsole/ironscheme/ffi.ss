(library (ironscheme ffi)
  (export
    make-pointer-setter
    make-pointer-getter
    make-ffi-callout
    make-ffi-callback
    ffi-callout
    ffi-callback
    pinvoke-call
    pointer?
    pointer=?
    pointer+
    null-pointer
    null-pointer?
  )
  (import 
    (ironscheme)
    (ironscheme clr))
    
  (clr-using System.Runtime.InteropServices)

  (define (write-int8! ptr ofs val) 
    (clr-static-call Marshal "WriteByte(IntPtr,Int32,Byte)" ptr ofs val))

  (define (write-int16! ptr ofs val) 
    (clr-static-call Marshal "WriteInt16(IntPtr,Int32,Int16)" ptr ofs val))

  (define (write-int32! ptr ofs val) 
    (clr-static-call Marshal "WriteInt32(IntPtr,Int32,Int32)" ptr ofs val))

  (define (write-int64! ptr ofs val) 
    (clr-static-call Marshal "WriteInt64(IntPtr,Int32,Int64)" ptr ofs val))

  (define (write-intptr! ptr ofs val) 
    (clr-static-call Marshal "WriteIntPtr(IntPtr,Int32,IntPtr)" ptr ofs val))
    
  (define (read-int8 ptr ofs) 
    (clr-static-call Marshal "ReadByte(IntPtr,Int32)" ptr ofs))

  (define (read-int16 ptr ofs) 
    (clr-static-call Marshal "ReadInt16(IntPtr,Int32)" ptr ofs))

  (define (read-int32 ptr ofs) 
    (clr-static-call Marshal "ReadInt32(IntPtr,Int32)" ptr ofs))

  (define (read-int64 ptr ofs) 
    (clr-static-call Marshal "ReadInt64(IntPtr,Int32)" ptr ofs))

  (define (read-intptr ptr ofs) 
    (clr-static-call Marshal "ReadIntPtr(IntPtr,Int32)" ptr ofs))
    
  (define (make-pointer-getter sym)
    (case sym
      [(int8 uint8)  read-int8]
      [(int16 uint16)  read-int16]
      [(int32 uint32)  read-int32]
      [(int64 uint64)  read-int64]
      [(intptr uintptr) read-intptr]
      [else (assertion-violation 'make-pointer-getter "unknown type" sym)]))
      
  (define (make-pointer-setter sym)
    (case sym
      [(int8 uint8)  write-int8!]
      [(int16 uint16)  write-int16!]
      [(int32 uint32)  write-int32!]
      [(int64 uint64)  write-int64!]
      [(intptr uintptr) write-intptr!]
      [else (assertion-violation 'make-pointer-setter "unknown type" sym)]))      
      
  (define (make-ffi-callout return-type arg-types)
    (eval `(ffi-callout ,return-type ,arg-types) 
           (environment '(ironscheme clr))))      

  (define (make-ffi-callback return-type arg-types)
    (eval `(ffi-callback ,return-type ,arg-types) 
           (environment '(ironscheme clr))))  
           
  (define (pointer? obj)
    (clr-is System.IntPtr obj))
    
  (define (pointer=? p1 p2)
    (unless (pointer? p1)
      (assertion-violation 'null-pointer "not a pointer" p1))
    (unless (pointer? p2)
      (assertion-violation 'null-pointer "not a pointer" p2))
    (clr-static-call System.IntPtr op_Equality p1 p2))
    
  (define (null-pointer)
    (clr-static-field-get System.IntPtr Zero))
    
  (define (null-pointer? obj)
    (unless (pointer? obj)
      (assertion-violation 'null-pointer "not a pointer" obj))
    (clr-static-call System.IntPtr op_Equality (null-pointer) obj))
)    
  