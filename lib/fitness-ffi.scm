
(include "ffi/ffi#.scm")
(c-declare "#include \"fitness.c\"")

(define %%calculate-fitness
  (c-lambda (u8* u8* int) unsigned-long "calculate_fitness"))
