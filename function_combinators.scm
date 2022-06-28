;; To run mit scheme, type M-x run-scheme . It is already set as the default

;; Note to execute code that lies before the cursor in the mit-scheme window in Emacs
;; do C-x C-e

;; If an error occurs, to go into the debugger type (debug)
;; For help in the debugger type ? 

(define (restrict-arity proc nargs)
  (hash-table-set! arity-table proc nargs)
  proc)

(define arity-table (make-key-weak-eqv-hash-table))

(define (get-arity proc)
  (or (hash-table-ref/default arity-table proc #f)
      (let ((a (procedure-arity proc))) ; arity not in table
	(assert (eqv? (procedure-arity-min a)
		      (procedure-arity-max a)))
	(procedure-arity-min a))))

(define (compose f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (define (the-composition . args)
      (assert (= (length args) m))
      (f (apply g args)))
    (restrict-arity the-composition n)))

((compose (lambda (x) (list 'foo x))
	  (lambda (x) (list 'bar x)))
 'z)

(define ((iterate n) f)
  (if (= n 0)
      identity
      (compose f ((iterate (- n 1)) f))))


(define (identity x) x)

(define (square x) (* x x))
(((iterate 3) square) 5)



(define (parallel-combine h f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
	(assert (= (length args) m))
	(assert (= m n))
	(h (apply f args) (apply g args)))
      (restrict-arity the-combination m))))

((parallel-combine list
		   (lambda (x y z) (list 'foo x y z))
	           (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c)


(define (spread-combine h f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
	(assert (= (length args) t))
	(h (apply f (list-head args n))
	   (apply g (list-tail args n))))
      (restrict-arity the-combination t))))




((spread-combine list
		   (lambda (x y) (list 'foo x y))
	           (lambda (u v w) (list 'bar u v w)))
  'a 'b 'c 'd 'e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
