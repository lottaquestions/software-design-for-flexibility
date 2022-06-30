;; To run mit scheme, type M-x run-scheme . It is already set as the default

;; Note to execute code that lies before the cursor in the mit-scheme window in Emacs
;; do C-x C-e

;; If an error occurs, to go into the debugger type (debug)
;; For help in the debugger type ? 

(define (r:dot) ".")
(define (r:bol) "^")
(define (r:eol) "$")

;; A way to treat a given set of regular expression fragments as a self-contained element
(define (r:seq . exprs)
  (string-append "\\(" (apply string-append exprs) "\\)"))

(define (r:quote string)
  (r:seq
   (list->string
    (append-map (lambda (char)
		  (if (memv char chars-needing-quoting)
		      (list #\\ char)
		      (list char)))
		(string->list string)))))

(define chars-needing-quoting
  '(#\. #\[ #\\ #\^ #\$ #\*))

(define (r:alt . exprs)
  (if (pair? exprs)
      (apply r:seq
	     (cons (car exprs)
		   (append-map (lambda (expr)
				 (list "\\|" expr))
			       (cdr exprs))))
      (r:seq)))

;;Test
(r:alt (r:quote "foo") (r:quote "bar") (r:quote "baz"))

;;Value: "\\(\\(foo\\)\\\|\\(bar\\)\\\|\\(baz\\)\\)"

(define (r:repeat min max expr)
  (apply r:seq
	 (append (make-list min expr)
		 (cond ((not max) (list expr "*"))
		       ((= max min) '())
		       (else
			(make-list (- max min)
				   (r:alt expr "")))))))

;;Test
(r:repeat 3 5 (r:alt (r:quote "cat") (r:quote "dog")))

;;Value: "\\(\\(\\(cat\\)\\\|\\(dog\\)\\)\\(\\(cat\\)\\\|\\(dog\\)\\)\\(\\(cat\\)\\\|\\(dog\\)\\)\\(\\(\\(cat\\)\\\|\\(dog\\)\\)\\\|\\)\\(\\(\\(cat\\)\\\|\\(dog\\)\\)\\\|\\)\\)"

(define (r:char-from string)
  (case (string-length string)
    ((0) (r:seq))
    ((1) (r:quote string))
    (else
     (bracket string
	      (lambda (members)
		(if (lset= eqv? '(#\- #\^) members)
		    '(#\- #\^)
		    (quote-bracketed-contents members)))))))

(define (r:char-not-from string)
  (bracket string
	   (lambda (members)
	     (cons #\^ (quote-bracketed-contents members)))))

(define (bracket string procedure)
  (list->string
   (append '(#\[)
	   (procedure (string->list string))
	   '(#\]))))

(define (quote-bracketed-contents members)
  (define (optional char)
    (if (memv char members) (list char) '()))
  (append (optional #\])
	  (remove
	   (lambda (c)
	     (memv c chars-needing-quoting-in-brackets))
	   members)
	  (optional #\^)
	  (optional #\-)))

(define chars-needing-quoting-in-brackets '(#\] #\^ #\-))

(define (write-bourne-shell-grep-command expr filename)
  (display (bourne-shell-grep-command-string expr filename)))

(define (bourne-shell-grep-command-string expr filename)
  (string-append "grep -e "
		 (bourne-shell-quote-string expr)
		 " "
		 filename))

(define (bourne-shell-quote-string string)
  (list->string
   (append (list #\')
	   (append-map (lambda (char)
			 (if (char=? char #\')
			     (list #\' #\\ char #\')
			     (list char)))
		       (string->list string))
	   (list #\'))))



