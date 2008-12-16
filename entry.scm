;;;;
;;;; This is the main entry point of the program,
;;;; letting Gambit take control over the main() setup
;;;; since it does a bunch of special things such
;;;; as initializing its VM, configuring interrupts, and more.
;;;; It especially handles cross-platform main() procedures.
;;;;
;;;; Control is immediately passed to the Cocoa world.
;;;;

(c-declare "#include \"cocoa-entry.h\"")

(define (gambit-entry)
  ((c-lambda (scheme-object) void "cocoa_entry") (command-line)))

(gambit-entry)
