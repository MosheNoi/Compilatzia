(load "pattern-matcher.scm")


(define isInList? 
      (lambda (x lst)
	(if (null? lst)
	     #f
	     (if (equal? (car lst) x)
	     #t
	     (isInList? x (cdr lst))))))

(define var? 
      (lambda (x)
	    (and (symbol? x) (not (isInList? x '(and or if lambda let letrec))))))
	    
	    
(define simple-const?
      (lambda (x) 
	  (or (number? x) (string? x) (boolean? x) (char? x) (null? x) (vector? x))))

(define parseList (lambda (lst)
		  (if (null? lst)
		  '()
		  (cons (parse (car lst)) (parseList (cdr lst))))))	  
	  
(define parse
	(let ((run 
			(compose-patterns
				(pattern-rule
					(? 'c simple-const?)
					(lambda (c) `(const ,c)))
				(pattern-rule
					`(quote ,(? 'c))
					(lambda (c) `(const ,c)))
				(pattern-rule
					(? 'v var?)
					(lambda (v) `(var ,v)))
				(pattern-rule
					`(if ,(? 'test) ,(? 'dit))
					(lambda (test dit) `(if3 ,(parse test) ,(parse dit) (const ,(void)))))
				(pattern-rule
					`(if ,(? 'test) ,(? 'dit) ,(? 'dif))
					(lambda (test dit dif) `(if3 ,(parse test) ,(parse dit) ,(parse dif))))
				(pattern-rule 
					(cons 'or (? 'exps))
					(lambda (exps) `(or ,(parseList exps))))
				(pattern-rule 
					(cons 'lambda (cons (? 'params) (? 'exps)))
					(lambda (params exps) `(lambda-simple ,(parseList params) ,@(parseList exps))))
				
				))) 
			(lambda (e)
				(run e
						(lambda ()
							(error 'parse
									(format "I can't recognize this: ~s" e)))))))
