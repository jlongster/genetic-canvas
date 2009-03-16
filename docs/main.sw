
\documentclass{article}
\begin{document}

\title{MonaLisa}
\author{James Long}
\date{March 7, 2009}
\maketitle

First we load up the required modules.  We will be testing the main engine
module.

(load "lib/engine")
(init-engine 100 100)

\section{Images \& Textures (lib/images.scm)}

\subsection{image type}

An image type is defined with the following fields:

\begin{itemize}
\item width
\item height
\item rgb-bytes
\item gl-texture-id
\end{itemize}

\subsection{(load-image filename)}

Loads an image into memory, returning an image instance. FreeImage is
used to load images. Only JPG support has been implemented.

(define test-image #f)

(define-test load-image
  (set! test-image (load-image "docs/test.jpg"))
  (assert-equal (image-width test-image) 100)
  (assert-equal (image-height test-image) 100)
  (assert-equal (char->integer
                 (u8*-ref (image-bytes test-image) 0)) 25)
  (assert-equal (char->integer
                 (u8*-ref (image-bytes test-image) 1)) 51)
  (assert-equal (char->integer
                 (u8*-ref (image-bytes test-image) 2)) 76))

\subsection{(image-opengl-upload! image)}

Creates an OpenGL texture and uploads the image data to video memory.
Various OpenGL texture states are set to appropriate defaults. This
function will set the image's gl-texture-id field to the generated
OpenGL texture id.

TODO: need to implement testing under OpenGL.

\subsection{(image-fetch-rgb image x y)}

Retrieves the RGB color from the source image at the specified
position.  The bottom-left corner defines (0,0).

(define-test image-fetch-rgb
  (let ((color (image-fetch-rgb test-image 55 59)))
    (assert-equal (vec3-x color) 239)
    (assert-equal (vec3-y color) 255)
    (assert-equal (vec3-z color) 255)))

\section{Genetic Algorithm (lib/genetic.scm)}

\subsection{(selection-rws pop f)}

Implements roulette wheel selection. Given a number
\begin{math}f\end{math}, where \begin{math}f\end{math} is between 0
and the sum of all fitnesses, linearly search the genomes and find the
genome occupying the specified slot, where slot lengths are the genome's
fitness.

(define-test selection-rws
  (let ((pop (make-population 3)))
    (genotype-fitness-set! (car pop) 50)
    (genotype-fitness-set! (cadr pop) 25)
    (genotype-fitness-set! (caddr pop) 5)
    (assert-equal (selection-rws pop 25) (car pop))
    (assert-equal (selection-rws pop 50) (cadr pop))
    (assert-equal (selection-rws pop 70) (cadr pop))
    (assert-equal (selection-rws pop 76) (caddr pop))))

\subsection{(selection-sus pop f)}

Implements stochastic universal selection. This builds on top of
roulette wheel selection: instead of passing a random value to
\ref{selection-rws}, select \begin{math}n\end{math} genomes at once,
dividing the fitness space evenly and selecting the genomes at those
points. Typically, you would want to select a whole new population at
once, which means the spread of your fitness scores controls the
selection behaviour (higher spread, more variance, and vice versa).

(define-test selection-sus
  (let ((pop (make-population 3)))
    (genotype-fitness-set! (car pop) 50)
    (genotype-fitness-set! (cadr pop) 25)
    (genotype-fitness-set! (caddr pop) 5)
    (let ((vec-pop (list->vector (selection-sus pop 16))))
      (assert-equal (vector-ref vec-pop 0) (car pop))
      (assert-equal (vector-ref vec-pop 10) (cadr pop))
      (assert-equal (vector-ref vec-pop 15) (caddr pop)))))

\subsection{(genotype-crossover gt1 gt2 \#!optional rate-or-pt)}

Implements the crossover operator. It simple takes two genotypes,
randomly selects a point in the genotype with the longest solution,
and cuts both genotypes at this point and switches the second parts
from each genotype.

It takes an optional 3rd argument. If passed a floating point, it will
override the default crossover rate which is .01. If passed an
integer, it will be used as the point at which to cut, but if omitted
will generate this randomly.

(define-test genotype-crossover
  (let ((gt1 (make-genotype '(1 2 3 4 5)))
        (gt2 (make-genotype '(6 7 8 9 10 11 12 13))))
    (receive (new-gt1 new-gt2) (genotype-crossover gt1 gt2 3)
      (assert-equal (genotype-data new-gt1) '(1 2 3 9 10 11 12 13))
      (assert-equal (genotype-data new-gt2) '(6 7 8 4 5)))
    (receive (new-gt1 new-gt2) (genotype-crossover gt1 gt2 7)
      (assert-equal (genotype-data new-gt1) '(1 2 3 4 5 13))
      (assert-equal (genotype-data new-gt2) '(6 7 8 9 10 11 12)))))

\subsection{(mutate-polygon! polygon)}

Mutates a polygon. This selects one ``mutator'' from many and executes
it. Each mutator modifies some aspect of the polygon, such as adding
points, moving points, changing the red color, etc.

\subsection{(genotype-mutate-many gt \#!optional rate)}
      
Implements a mutation operator which mutates many of the solution's polygons.
The polygon mutation rate is .1, or 1/10th of the polygons are mutated.

This checks the genotype mutation rate automatically.  The default genotype
mutation rate is .5.  The 2nd parameter overrides this.

\subsection{(genotype-mutate-one gt \#!optional rate)}

Implements a mutation operater which mutates only one of the solution's polygons.

This checks the genotype mutation rate automatically.  The default genotype
mutation rate is .5.  The 2nd parameter overrides this.

\section{Vectors (lib/vectors.scm)}

Basic functionality for 2d and 3d vectors.

TODO:  add vector unit tests

\section{Engine (lib/engine.scm)}

Fires up the real engine.

\section{Triangulation}

\subsection{(triangulate c-vec2* int)}

(load "lib/util/triangulate-ffi")

(define-test triangulate
  (let ((buf (alloc-c-vec2 4)))
    (c-vec2-x-set! (c-vec2*-ref buf 0) 8.)
    (c-vec2-y-set! (c-vec2*-ref buf 0) 14.)
    
    (c-vec2-x-set! (c-vec2*-ref buf 1) -5.)
    (c-vec2-y-set! (c-vec2*-ref buf 1) 24.)

    (c-vec2-x-set! (c-vec2*-ref buf 2) 15.)
    (c-vec2-y-set! (c-vec2*-ref buf 2) 7.)
    
    (c-vec2-x-set! (c-vec2*-ref buf 3) 6.)
    (c-vec2-y-set! (c-vec2*-ref buf 3) -9.)

    (let ((res (triangulate buf 9)))
      (show (c-vec2-x (c-vec2*-ref res 0))
            " "
            (c-vec2-y (c-vec2*-ref res 0))
            "\n")
      (show (c-vec2-x (c-vec2*-ref res 1))
            " "
            (c-vec2-y (c-vec2*-ref res 1))
            "\n")
      (show (c-vec2-x (c-vec2*-ref res 2))
            " "
            (c-vec2-y (c-vec2*-ref res 2))
            "\n")
      (show (c-vec2-x (c-vec2*-ref res 3))
            " "
            (c-vec2-y (c-vec2*-ref res 3))
            "\n"))))

\section{Benchmarks}
    
(shutdown-engine)

\end{document}

