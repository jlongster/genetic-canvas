
(c-declare "#include <stdlib.h>")
(c-declare "#include <string.h>")

(define u8*->u8vector
  (c-lambda ((pointer unsigned-char) int) scheme-object #<<end-c-code
   ___U8* data = ___arg1;
   int len = ___arg2;
   ___SCMOBJ vec = ___EXT(___alloc_scmobj)(___sU8VECTOR, len, ___STILL);
   ___U8* ptr = ___CAST(___U8*, ___BODY(vec));

   memcpy(ptr, data, len*sizeof(___U8));
   ___result = vec;
end-c-code
))

(define u8vector->u8*
  (c-lambda (scheme-object) (pointer unsigned-char) #<<end-c-code
   ___SCMOBJ vec = ___arg1;
   int len = ___U8VECTORLENGTH(vec);
   ___U8* data = malloc(len*sizeof(___U8));
   memcpy(data, ___BODY(vec), len*sizeof(___U8));
   ___result_voidstar = data;
end-c-code
))

(define alloc-u8
  (c-lambda (int) (pointer unsigned-char)
            "___result_voidstar = malloc(___arg1*sizeof(unsigned char));"))

(define u8*->void*
  (c-lambda ((pointer unsigned-char)) (pointer void)
            "___result_voidstar = (void*)___arg1;"))

(define u8*-ref
  (c-lambda ((pointer unsigned-char) int) unsigned-char
            "___result = ___arg1[___arg2];"))

(define debug-u8*
  (c-lambda ((pointer unsigned-char)) void #<<end-c-code
   unsigned char *buf = ___arg1;
   int i=0;
   while(i<300*300) { if(buf[i]!=0) printf("%d:",buf[i]); i++; }
end-c-code
))

(define alloc-uint
  (c-lambda (int) (pointer unsigned-int)
            "___result_voidstar = malloc(___arg1*sizeof(unsigned int));"))

(define uint*-ref
  (c-lambda ((pointer unsigned-int) int) unsigned-int
            "___result = *(___arg1+___arg2);"))
