
\documentclass{article}
\begin{document}

\title{MonaLisa}
\author{James Long}
\date{March 7, 2009}
\maketitle

These documents are meant to provide a place for testing various
aspects of the system. Currently, these documents are pretty sloppy
and sorely lacking in code coverage. I have yet to add tests and
documentation for the major part of the system, but that will come
with time.

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
\item bytes
\item format
\item gl-texture-id
\end{itemize}

\subsection{(load-image filename)}

Loads an image into memory, returning an image instance. FreeImage is
used to load images. Only JPG support has been implemented.

(define test-image #f)

(define-test load-image
  (set! test-image (load-image "tests/test.jpg"))
  (assert-equal (image-width test-image) 100)
  (assert-equal (image-height test-image) 100)
  (assert-equal (u8*-ref (image-bytes test-image) 0) 25)
  (assert-equal (u8*-ref (image-bytes test-image) 1) 51)
  (assert-equal (u8*-ref (image-bytes test-image) 2) 76))

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

\subsection{Selection}

Elitist selection is the ONLY type of selection currently being used
by the system.

\subsubsection{(selection-elitist pop n)}

Using elitist selection, select \begin{math}n\end{math} number of
genotypes from the population \textit{pop}.  Elitist selection takes
the single best genotype and copies it.

\subsubsection{(selection-rws pop f)}

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

\subsubsection{(selection-sus pop f)}

Implements stochastic universal selection. This builds on top of
roulette wheel selection: instead of passing a random value to
selection-rws, select \begin{math}n\end{math} genomes at once,
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

\subsection{Crossover}

\subsubsection{(genotype-crossover gt1 gt2 \#!optional rate-or-pt)}

Currenty the genetic algorithm does NOT use any crossover procedures.

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


\subsection{Mutation}

Mutation is the key part of this genetic algorithm. There are several
ways to mutate a solution, which is a list of polygons, each polygon
having many specific attributes. We can add polygons, remove them, and
reorder them, as well as mutating each polygon's color and geometric
makeup. In order to find a solution and converge, we also need to
implement all of of these with different scales (i.e. it could either
move the point a little or a lot).

This operation happens for all but one genotype in the population.
This means that the best genotype (since we only use elitist
selection) is guaranteed to be copied over in pristine condition at
least once. Otherwise, when a genotype is mutated, it calls
\textit{mutate-genotype!} which mutates all aspects of the solution;
an important note is that this means it runs mutation operators over
every single polygon in its solution.

Other evolution strategies are possible, such as specializing some
genotypes to only be mutated with \textit{mutate-polygons!} or
\textit{mutate-geometry!} which would favor certain types of traversing.

\subsubsection{(mutate-genotype! gt)}

Runs both \textit{mutate-polygons!} and \textit{mutate-geometry!} on
the genotype, meaning it possibly mutates all aspects of the solution.

\subsubsection{(mutate-polygons! gt)}

Only changes properties of each polygon. This runs through all of the
mutators and tests to see if each one should be run. Either zero or
many mutators could run each time. Each mutator modifies some aspect
of the polygon, such as adding points, moving points, changing the red
color, etc.

\subsubsection{(mutate-geometry! gt)}

Only changes properties of the genotype, meaning it only adds,
removes, or reorders polygons. These mutations all happen according to
some set mutation rate, and either zero or many could happen at once.

\section{Settings (lib/settings.scm)}

The most important place in the code is in the settings file. All
operators and settings which determine the health and accuracy of the
genetic algorithm are placed here. The fundamental workflow of the
genetic algorith (when the mutators are run, the selection operators,
etc.) are not included here, but rather mutation rates and operators,
color settings and more.

Part of the settings file is also a few configuration procedures which
have a chance to optimize the inital solution. This is neat because it
can make it easier for the genetic algorithm to find a solution
faster.  Two things which are currently done are:

\begin{itemize}
\item Optimizing the initial color: the image is scanned through and
  the most frequent red, green, and blue values are found and set as
  the inital color of the canvas
\item Optimizing the available color ranges: the image is scanned
  through again and the highest and lowest RGB values are found and
  set as the available color ranges for polygons.
\end{itemize}

\section{Vectors (lib/vectors.scm)}

Basic functionality for 2d and 3d vectors.

\section{Util}

\subsection{(random-real-in-range minv maxv)}

(define-test random-real-in-range
  (let ((r (random-real-in-range 105.5 109.2)))
    (assert (and (>= r 105.5)
                 (<= r 109.2))))
  (let ((r (random-real-in-range 105.5 109.2)))
    (assert (and (>= r 105.5)
                 (<= r 109.2))))
  (let ((r (random-real-in-range 105.5 109.2)))
    (assert (and (>= r 105.5)
                 (<= r 109.2))))
  (let ((r (random-real-in-range 105.5 109.2)))
    (assert (and (>= r 105.5)
                 (<= r 109.2)))))

\section{Benchmarks}

Average evolution step in the genetic algorithm, when compiled, takes
about .054s. This means were able to evolve about 1111 times per 60 seconds.
    
(shutdown-engine)

\end{document}

