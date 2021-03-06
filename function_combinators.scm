;; To run mit scheme, type M-x run-scheme . It is already set as the default

;; Note to execute code that lies before the cursor in the mit-scheme window in Emacs
;; do C-x C-e

;; If an error occurs, to go into the debugger type (debug)
;; For help in the debugger type ? 

(define (restrict-arity proc nargs)
  (hash-table-set! arity-table proc nargs)
  proc)

(define arity-table (make-key-weak-eqv-hash-table))

(define max-possible-arity 256)

(define (get-arity proc)
  (or (hash-table-ref/default arity-table proc #f)
      (let ((a (procedure-arity proc))) ; arity not in table
	;;(display a)
	(if (not (procedure-arity-max a))
	    max-possible-arity
	    (begin
	      (assert (eqv? (procedure-arity-min a)
			    (procedure-arity-max a)))
	      (procedure-arity-min a))
	))))

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



;; Test code
((spread-combine list
		   (lambda (x y) (list 'foo x y))
	           (lambda (u v w) (list 'bar u v w)))
  'a 'b 'c 'd 'e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (spread-apply f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
	(assert (= (length args) t))
	(values (apply f (list-head args n))
		(apply g (list-tail args n))))
      (restrict-arity the-combination t))))

(define (spread-combine h f g)
  (compose h (spread-apply f g)))

(define (compose f g)
  (define (the-composition . args)
    (call-with-values (lambda () (apply g args))
      f))
  (restrict-arity the-composition (get-arity g)))


;; Test code
((spread-combine list
		   (lambda (x y) (list 'foo x y))
	           (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c 'd 'e)
;Value: ((foo a b) (bar c d e))

;; Generalizing further and allowing all the functions we are combining to return multiple values
(define (spread-apply f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
	(assert (= (length args) t))
	(let-values ((fv (apply f (list-head args n)))
		     (gv (apply g (list-tail args n))))
	  (apply values (append fv gv))))
      (restrict-arity the-combination t))))

;;Test
((spread-combine list
		 (lambda (x y) (values x y))
		 (lambda (u v w) (values w v u)))
 'a 'b 'c 'd 'e)
;;Value: (a b e d c)

;; Reformulation of parallel combine to be a composition of two parts and to allow the
;; parts to return multiple values

(define (parallel-apply f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
	(assert (= (length args) m))
	(assert (= m n))
	(values (apply f args) (apply g args)))

      (restrict-arity the-combination m))))

(define (parallel-combine h f g)
  (compose h (parallel-apply f g)))

;;Test
((parallel-combine list
		   (lambda (x y z) (list 'foo x y z))
	           (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c)

;;Value: ((foo a b c) (bar a b c))


;; A small library

(define (list-remove lst index)
  (let lp ((lst lst) (index index))
    (if (= index 0)
	(cdr lst)
	(cons (car lst) (lp (cdr lst) (- index 1))))))

;;Test
(list-remove '(1 2 3 4 5 6) 3)

;;Value: (1 2 3 5 6)


(define (discard-argument i)
  (assert (exact-nonnegative-integer? i))
  (lambda (f)
    (let ((m (+ (get-arity f) 1)))
      (define (the-combination . args)
	(assert (= (length args) m ))
	(apply f (list-remove args i)))
      (assert (< i m))
      (restrict-arity the-combination m))))

;;Test
(((discard-argument 2)
  (lambda (x y z) (list 'foo x y z)))
 'a 'b 'c 'd)
;;Value: (foo a b d)

(define ((curry-argument i) . args)
  (lambda (f)
    (assert (= (length args) (- (get-arity f) 1)))
    (lambda (x)
      (apply f (list-insert args i x)))))

(define (list-insert lst index value)
  (let lp ((lst lst) (index index))
    (if (= index 0)
	(cons value lst)
	(cons (car lst) (lp (cdr lst) (- index 1))))))

;;Test
((((curry-argument 2) 'a 'b 'c)
  (lambda (x y z w) (list 'foo x y z w)))
 'd)
;;Value: (foo a b d c)

(define (permute-arguments . permspec)
  (let ((permute (make-permuation permspec)))
    (lambda (f)
      (define (the-combination . args)
	(apply f (permute args)))
      (let ((n (get-arity f)))
	(assert (= n (length permspec)))
	(restrict-arity the-combination n)))))

(define (make-permuation permspec)
  (define (the-permuter lst)
    (map (lambda (p) (list-ref lst p)) permspec))
  the-permuter)

;;Test
(((permute-arguments 1 2 0 3)
  (lambda (x y z w) (list 'foo x y z w)))
 'a 'b 'c 'd)
;;Value: (foo b c a d)
