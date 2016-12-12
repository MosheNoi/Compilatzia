(load "pattern-matcher.scm")
(load "qq.scm")



(define isInList? 
      (lambda (x lst)
	(if (null? lst)
	     #f
	     (if (equal? (car lst) x)
	     #t
	     (isInList? x (cdr lst))))))

(define notAsavedWord? (lambda (x)
		    (not (isInList? x '(and or if lambda let letrec let* begin begin-letrec cond quasiquote)))))
	  
(define var? 
      (lambda (x)
	    (and (symbol? x) (notAsavedWord? x))))
	    
	    
(define simple-const?
      (lambda (x) 
	  (or (number? x) (string? x) (boolean? x) (char? x) (vector? x))))
	  


(define else?
	(lambda (expr)
		(eq? (caar expr) 'else)))

(define parse2List (lambda (lst)
		  (if (null? lst)
		  '()
		  (cons (parse2 (car lst)) (parse2List (cdr lst))))))
		  
		  
		  
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
						
  
(define all-different? 
  (lambda (v)
    (if (pair? v)
        (and (not (member (car v) (cdr v)))
             (all-different? (cdr v)))
        #t)))
 
(define create-f-list
  (lambda (len res)
      (if   (equal? 0 len) res
	    (create-f-list (- len 1) (cons '#f res)))))

    
(define create-set-exp
    (lambda (keys vals res)
    (if (null? keys) res (create-set-exp (cdr keys) (cdr vals) (append res `((set ,(car keys) ,(car vals))))))))
    
(define OrParser 
	  (lambda (exps)
	    (cond ((null? exps) (parse2 #f))
		  ((= (length exps) 1) (parse2 (car exps)))
		  (else `(or ,(parse2List exps))))))
            
(define parse2
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
					(lambda (test dit) `(if3 ,(parse2 test) ,(parse2 dit) (const ,(void)))))
				(pattern-rule
					`(if ,(? 'test) ,(? 'dit) ,(? 'dif))
					(lambda (test dit dif) `(if3 ,(parse2 test) ,(parse2 dit) ,(parse2 dif))))
				(pattern-rule 
					(cons 'or (? 'exps))
					(lambda (exps) (OrParser exps)))
				(pattern-rule 
					(cons 'lambda (cons (? 'params-var var?) (? 'exps)))
					(lambda (params-var exps)
					`(lambda-var ,params-var ,(parse2 `(begin ,@exps)))))
				(pattern-rule 
					(cons 'lambda (cons (? 'params all-different?) (? 'exps)))
					(lambda (params exps)
					
					(if (is-opt-args params) `(lambda-opt ,(remove-tail params (list) #f) ,(get-last-element params) ,(parse2 `(begin ,exps))) `(lambda-simple  ,params ,(parse2 `(begin ,@exps))))))
				(pattern-rule 
					`(define ,(? 'var var?) ,(? 'exps))
					(lambda (var exps)
					`(def ,(parse2 var) ,(parse2 exps))))
				(pattern-rule 
					(cons `define (cons (? 'var-args) (? 'exps)))
					(lambda (var-args exps)
					`(def ,(parse2 (car var-args)) ,(parse2 `(lambda ,(cdr var-args) ,@exps)))))
				(pattern-rule 
				        `(set! ,(? 'var) ,(? 'val))
				         (lambda (var val) `(set ,(parse2 var) ,(parse2 val))))
				(pattern-rule 
					(cons (? 'proc notAsavedWord? ) (? 'args))
					(lambda (proc args) `(applic ,(parse2 proc) ,(parse2List args))))
					
				(pattern-rule
				       `(begin ,(? 'expr) . ,(? 'rest))
					(lambda (expr rest)
					(if (null? rest) (parse2 expr) `(seq (,(parse2 expr) ,@(parse2List rest))))))
					
				(pattern-rule
				        (cons 'begin-letrec (? 'expr))
					(lambda (expr)
					(display 'letrec)
					(cons 'seq `(,(map (lambda (e) (if (equal? (car e) 'set) `(set ,(parse2 (cadr e)) ,(parse2 (caddr e))) (parse2 e))) expr)))))
											
					 					
				#;----------------------------------------special-forms--------------------------------------------------
				
			        (pattern-rule
				        (cons 'let (cons (? 'keyAndValues) (? 'bodies)))
				        (lambda (keyAndValues bodies)
						(parse2 (cons `(lambda ,(getLetKeys keyAndValues) ,@bodies) (getLetVals keyAndValues)))))
				
				(pattern-rule 
				       (cons 'and (? 'exprs))
				       (lambda (exprs)  (parse2  (AndToIf exprs))))
				       
				(pattern-rule
				      `(let* () ,(? 'body) . ,(? 'other-bodies))
				       (lambda (body other-bodies)
				       (parse2 `(begin ,body ,@other-bodies))))
				       
				(pattern-rule
				      `(let* ((,(? 'key var?) ,(? 'value)) . ,(? 'other-bindings)) ,(? 'body))
					(lambda (key value other-bindings body)
					  (parse2 `(let ((,key ,value)) (let* ,other-bindings ,body)))))
					  
				(pattern-rule
				        (cons 'letrec (cons (? 'keyAndValues) (? 'bodies)))
				        (lambda (keyAndValues bodies) 
						(parse2 (cons `(lambda ,(getLetKeys keyAndValues) (begin-letrec ,@(create-set-exp (getLetKeys keyAndValues) (getLetVals keyAndValues) '()) ((lambda () ,@bodies)))) (create-f-list (length (getLetKeys keyAndValues)) '())))))
				(pattern-rule
					 (cons 'cond (cons (? 'con) (? 'els else?)))
					 (lambda (con els)
					 (parse2 `(if ,@con ,(car (cdr (car els)))))))
					   
				
				(pattern-rule
					(cons 'cond (? 'conds))
					(lambda (conds) 
					(parse2 `(if ,(caar conds) ,(car (cdr (car conds))) (cond ,@(cdr conds))))))
						
								 
				(pattern-rule
				       (cons 'quasiquote (? 'expr))
				       (lambda (expr) (parse2 (expand-qq (car expr)))))						
					
				))) 
			(lambda (e)
				(run e
						(lambda ()
							(error 'parse2
									(format "I can't recognize this: ~s" e)))))))
