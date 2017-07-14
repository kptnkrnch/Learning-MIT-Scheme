;; Assignment 3 - CMPT 383
;; 
;; Joshua Campbell
;; 301266191


;; Question 1 -------------------------------------------------------------------------------
(define my-last
	(lambda (lst)
		(cond
			((null? lst)
				(error "my-last: argument is an empty list" lst)
			)
			((eq? (lst-length lst) 1)
				(car lst)
			)
			(else
				(my-last (cdr lst))
			)
		)
	)
)
;;-------------------------------------------------------------------------------------------

;; Question 2 -------------------------------------------------------------------------------
(define snoc
	(lambda (x lst)
		(cond
			((null? lst)
				(list x)
			)
			((eq? (lst-length lst) 1)
				(list (car lst) x)
			)
			(else
				(cons (car lst) (snoc x (cdr lst)))
			)
		)
	)
)
;;-------------------------------------------------------------------------------------------

;; Question 3 -------------------------------------------------------------------------------
(define range
	(lambda (n)
		(cond
			((<= n 0)
				'()
			)
			((= n 1)
				'(0)
			)
			(else
				(snoc (- n 1) (range (- n 1)))
			)
		)
	)
)
;;-------------------------------------------------------------------------------------------

;; Question 4 -------------------------------------------------------------------------------
(define deep-sum
	(lambda (lst)
		(cond
			((null? lst)
				0
			)
			((list? (car lst))
				(+ (deep-sum (car lst)) (deep-sum (cdr lst)))
			)
			((number? (car lst))
				(+ (car lst) (deep-sum (cdr lst)))
			)
			(else
				(+ (deep-sum (cdr lst)) 0)
			)
		)
	)
)
;;-------------------------------------------------------------------------------------------

;; Question 5 -------------------------------------------------------------------------------
(define count-primes
	(lambda (n)
		(cond
			((<= n 0)
				0
			)
			((is-prime? n 2)
				(+ (count-primes (- n 1)) 1)
			)
			(else
				(+ (count-primes (- n 1)) 0)
			)
		)
	)
)

(define is-prime?
	(lambda (num comparitor)
		(cond 
			((<= num 1)
				#f
			)
			((= num comparitor)
				#t
			)
			((= (modulo num comparitor) 0)
				#f
			)
			(else
				(is-prime? num (+ comparitor 1))
			)
		)
	)
)
;;-------------------------------------------------------------------------------------------

;; Question 6 -------------------------------------------------------------------------------
(define is-bit?
	(lambda (x)
		(cond
			((eq? x 0)
				#t
			)
			((eq? x 1)
				#t
			)
			(else
				#f
			)
		)
	)
)
;;-------------------------------------------------------------------------------------------

;; Question 7 -------------------------------------------------------------------------------
(define is-bit-seq?
	(lambda (lst)
		(cond
			((null? lst)
				#t
			)
			((is-bit? (car lst))
				(is-bit-seq? (cdr lst))
			)
			(else
				#f
			)
		)
	)
)
;;-------------------------------------------------------------------------------------------

;; Question 8 -------------------------------------------------------------------------------
(define all-bit-seqs
	(lambda (n)
		(cond
			((< n 1)
				'()
			)
			(else
				(generate-bit-seqs (init-bit-seq n) '())
			)
		)
	)
)

;; generator to build all bit sequences.
;; generate-bit-seqs is called by all-bit-seqs
(define generate-bit-seqs
	(lambda (curseq sequences)
		(cond
			((is-end-bit-seq curseq)
				(cons (lst-reverse curseq '()) sequences)
			)
			(else
				(cons (lst-reverse curseq '()) (generate-bit-seqs (next-bit-seq curseq) sequences))
			)
		)
	)
)

;; next-bit-seq will return the next bit sequence (adding 1) based on the previous sequence
;; uses next-bit-seq-generator to build the next sequence
(define next-bit-seq
	(lambda (seq)
		(cond
			((null? seq)
				'()
			)
			(else
				(next-bit-seq-generator seq #f)
			)
		)
	)
)

;; creates the first all 0 bit seq
;; bitsremaining are the number of 0 bits that still need to be added to the sequence
(define init-bit-seq
	(lambda (bitsremaining)
		(cond
			((eq? bitsremaining 0)
				'()
			)
			(else
				(cons 0 (init-bit-seq (- bitsremaining 1)))
			)
		)
	)
)

;; generator of the next bit sequence based on the previous sequence "seq"
;; found is a flag representing if a 0 bit has been changed to a 1
(define next-bit-seq-generator
	(lambda (seq found)
		(cond
			((null? seq)
				'()
			)
			((and (eq? (car seq) 0) (not found))
				(cons 1 (next-bit-seq-generator (cdr seq) #t))
			)
			((eq? found #t)
				(cons (car seq) (next-bit-seq-generator (cdr seq) found))
			)
			(else
				(cons 0 (next-bit-seq-generator (cdr seq) found))
			)
		)
	)
)

;; checks if the sequence "seq" is all 1's
(define is-end-bit-seq
	(lambda (seq)
		(cond
			((null? seq)
				#t
			)
			((eq? (car seq) 1)
				(is-end-bit-seq (cdr seq))
			)
			(else
				#f
			)
		)
	)
)
;;-------------------------------------------------------------------------------------------

;; Helper Functions -------------------------------------------------------------------------
;; returns the length of a list
(define lst-length
	(lambda (lst)
		(cond
			((null? lst)
				0
			)
			(else
				(+ 1 (lst-length (cdr lst)))
			)
		)
	)
)

;; reverses a list
(define lst-reverse
	(lambda (lst newlst)
		(cond
			((null? lst)
				'()
			)
			((eq? (lst-length lst) 1)
				(cons (car lst) newlst)
			)
			(else
				(lst-reverse (cdr lst) (cons (car lst) newlst))
			)
		)
	)
)
;;-------------------------------------------------------------------------------------------