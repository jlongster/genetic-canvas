
\documentclass{article}
\begin{document}

\title{Engine}
\author{James Long}
\date{March 7, 2009}
\maketitle

First we load up the required modules.  We will be testing the main engine
module.

(load "lib/engine")

\section{Selection Procedures}

\subsection{selection-rws}

Implements roulette wheel selection. Given a number
\begin{math}N\end{math}, where \begin{math}N\end{math} is between 0
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

\subsection{selection-sus}

Implements stochastic universal selection. This builds on top of
roulette wheel selection: instead of passing a random value to
\ref{selection-rws}, select mutiple genomes at once, dividing the
fitness space evenly and selecting the genomes at those points.
Typically, you would want to select a whole new population at once,
which means the spread of your fitness scores controls the selection
behaviour (higher spread, more variance, and vice versa).

(define-test selection-sus
  (let ((pop (make-population 3)))
    (genotype-fitness-set! (car pop) 50)
    (genotype-fitness-set! (cadr pop) 25)
    (genotype-fitness-set! (caddr pop) 5)
    (let ((vec-pop (list->vector (selection-sus pop 16))))
      (assert-equal (vector-ref vec-pop 0) (car pop))
      (assert-equal (vector-ref vec-pop 10) (cadr pop))
      (assert-equal (vector-ref vec-pop 15) (caddr pop)))))

\section{Crossover procedures}

\subsection{genotype-crossover}

Implements the crossover operator.  It simple takes two genotypes, randomly selects
a point in the genotype with the longest solution, and cuts both genotypes at this point
and switches the second parts from each genotype.

It takes an optional 3rd argument, the point at which to cut, but if omitted will generate
this randomly.

(define-test genotype-crossover
  (let ((gt1 (make-genotype '(1 2 3 4 5)))
        (gt2 (make-genotype '(6 7 8 9 10 11 12 13))))
    (receive (new-gt1 new-gt2) (genotype-crossover gt1 gt2 3)
      (assert-equal (genotype-data new-gt1) '(1 2 3 9 10 11 12 13))
      (assert-equal (genotype-data new-gt2) '(6 7 8 4 5)))
    (receive (new-gt1 new-gt2) (genotype-crossover gt1 gt2 7)
      (assert-equal (genotype-data new-gt1) '(1 2 3 4 5 13))
      (assert-equal (genotype-data new-gt2) '(6 7 8 9 10 11 12)))))

\end{document}
