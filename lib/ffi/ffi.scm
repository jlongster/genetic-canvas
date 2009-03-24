
(c-declare "#include <sys/time.h>")
(c-declare "#include <time.h>")
(c-declare "#include <stdlib.h>")
(c-declare "#include <string.h>")

(include "ffi#.scm")

(define u8*->u8vector
  (c-lambda (u8* int) scheme-object #<<end-c-code
   ___U8* data = ___arg1;
   int len = ___arg2;
   ___SCMOBJ vec = ___EXT(___alloc_scmobj)(___sU8VECTOR, len, ___STILL);
   ___U8* ptr = ___CAST(___U8*, ___BODY(vec));

   memcpy(ptr, data, len*sizeof(___U8));
   ___result = vec;
end-c-code
))

(define u8vector->u8*
  (c-lambda (scheme-object) u8* #<<end-c-code
   ___SCMOBJ vec = ___arg1;
   int len = ___U8VECTORLENGTH(vec);
   ___U8* data = malloc(len*sizeof(___U8));
   memcpy(data, ___BODY(vec), len*sizeof(___U8));
   ___result_voidstar = data;
end-c-code
))

(define alloc-u8
  (c-lambda (int) u8*
            "___result_voidstar = malloc(___arg1*sizeof(unsigned char));"))

(define u8*->void*
  (c-lambda (u8*) (pointer void)
            "___result_voidstar = (void*)___arg1;"))

(define u8*-ref
  (c-lambda (u8* int) int
            "___result = ___arg1[___arg2];"))

(define u8*-set!
  (c-lambda (u8* int int) void
            "___arg1[___arg2] = ___arg3;"))

(define alloc-uint
  (c-lambda (int) (pointer unsigned-int)
            "___result_voidstar = malloc(___arg1*sizeof(unsigned int));"))

(define uint*-ref
  (c-lambda ((pointer unsigned-int) int) unsigned-int
            "___result = *(___arg1+___arg2);"))

(define free
  (c-lambda ((pointer void #f)) void "free((void*)___arg1);"))
