(load "pattern-matcher.scm")


(define isInList? 
      (lambda (x lst)
	(if (null? lst)
	     #f
	     (if (equal? (car lst) x)
	     #t
	     (isInList? x (cdr lst))))))

(define notAsavedWord? (lambda (x)
		    (not (isInList? x '(and or if lambda let letrec let* begin)))))
	  
(define var? 
      (lambda (x)
	    (and (symbol? x) (notAsavedWord? x))))
	    
	    
(define simple-const?
      (lambda (x) 
	  (or (number? x) (string? x) (boolean? x) (char? x) (null? x) (vector? x))))

(define parseList (lambda (lst)
		  (if (null? lst)
		  '()
		  (cons (parse (car lst)) (parseList (cdr lst))))))
		  
		  
		  
(define is-opt-args (lambda (lst)
	      (cond ((null? lst) #f)
		    ((not (pair? (cdr lst))) (not (list? lst)))
	      (else  (is-opt-args (cdr lst))))))
	      
(define get-last-element (lambda (lst)
		(cond ((null? lst) '())
		      ((not (pair? (cdr lst))) (cdr lst))
		(else  (get-last-element (cdr lst))))))
		
(define remove-tail (lambda (lst res flag)
      (if   (null? lst) '()
	    (if flag res
	    (if (not (pair? (cdr lst))) (remove-tail (cdr lst) (append res (list (car lst))) #t)
            (remove-tail (cdr lst) (append res (list (car lst))) #f))))))
            
(define getLetKeys (lambda (lst)
		      (if (null? lst)
		      '()
		      (cons (caar lst) (getLetKeys (cdr lst)))))) 

(define getLetVals (lambda (lst)
		      (if (null? lst)
		      '()
		      (cons (cadar lst) (getLetVals (cdr lst)))))) 		      

(define ChangeThisAndToANestedIf (lambda (lst)
				    (if (null? (cddr lst))
				      `(if ,(car lst) ,(cadr lst) #f)
				       `(if ,(car lst) ,(ChangeThisAndToANestedIf (cdr lst)) #f))))

(define AndToIf (lambda (lst)
		  (cond ((null? lst) #t)
			((= (length lst) 1) (car lst))
			((= (length lst) 2) `(if ,(car lst) ,(cadr lst) #f))
			(else (ChangeThisAndToANestedIf lst)))))
 
(define getBodies (lambda (lst)
		    (if (= (length lst) 1) 
			lst)))
			
            
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
					(cons 'lambda (cons (? 'params-var var?) (? 'exps)))
					(lambda (params-var exps)
					`(lambda-var ,params-var ,(parse `(begin ,exps)))))
				(pattern-rule 
					(cons 'lambda (cons (? 'params) (? 'exps)))
					(lambda (params exps)
					(if (is-opt-args params) `(lambda-opt ,(remove-tail params (list) #f) ,(get-last-element params) ,(parse `(begin ,exps))) `(lambda-simple  ,params ,(parse `(begin ,exps))))))
				(pattern-rule 
					`(define ,(? 'var var?) ,(? 'exps))
					(lambda (var exps)
					`(def ,(parse var) ,(parse exps))))
				(pattern-rule 
					(cons `define (cons (? 'var-args) (? 'exps)))
					(lambda (var-args exps)
					`(def ,(parse (car var-args)) ,(parse `(lambda ,(cdr var-args) ,@exps)))))
				(pattern-rule 
				        `(set! ,(? 'var) ,(? 'val))
				         (lambda (var val) `(set ,(parse var) ,(parse val))))
				(pattern-rule 
					(cons (? 'proc notAsavedWord? ) (? 'args))
					(lambda (proc args) `(applic ,(parse proc) ,(parseList args))))	
					
				(pattern-rule
					`(begin ,(? 'expr))
					 (lambda (expr) (if (> (length expr) 1) `(seq (,@(parseList expr))) (parse (car expr)))))
					 					
				#;----------------------------------------special-forms--------------------------------------------------
				
			        (pattern-rule
				        (cons 'let (cons (? 'keyAndValues) (? 'bodies)))
				        (lambda (keyAndValues bodies) 
						(parse (cons `(lambda ,(getLetKeys keyAndValues) ,@bodies) (getLetVals keyAndValues)))))
				
				(pattern-rule 
				       (cons 'and (? 'exprs))
				       (lambda (exprs)  (parse  (AndToIf exprs))))
						
				
					
				))) 
			(lambda (e)
				(run e
						(lambda ()
							(error 'parse
									(format "I can't recognize this: ~s" e)))))))
