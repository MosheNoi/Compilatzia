(load "~/Projects/Compilatzia/pc.scm")


#;this_is_the_numeric_part-------------------------------------------------------------------------------------------------

(define <digit-0-9>
  (range #\0 #\9))
  
(define <digit-1-9>
  (range #\1 #\9))
  
(define <nat>
  (new (*parser (char #\0))
       (*pack (lambda (_) 0))

       (*parser <digit-1-9>)
       (*parser <digit-0-9>) *star
       (*caten 2)
       (*pack-with
	(lambda (a s)
	  (string->number
	   (list->string
	    `(,a ,@s)))))

       (*disj 2)
       done))

(define <int>
  (new (*parser (char #\+))
       (*parser <nat>)
       (*caten 2)
       (*pack-with
	(lambda (++ n) n))

       (*parser (char #\-))
       (*parser <nat>)
       (*caten 2)
       (*pack-with
	(lambda (-- n) (- n)))

       (*parser <nat>)

       (*disj 3)

       done))
       
(define <rat>
  (new (*parser <int>)
       (*parser (char #\/))
       (*parser <nat>)
       (*guard (lambda (n) (not (zero? n))))
       (*caten 3)
       (*pack-with
	(lambda (num div den)
	  (/ num den)))
       done))
(define <Number>
   (new (*parser <int>)
	(*parser <rat>)
	(*disj 2)
	done))
	
#; this_is_the_char_part---------------------------------------------------------------------------------------------

(define <chars-a-f>
  (range #\a #\f))
  
(define <chars-a-z>
  (range #\a #\f))
  
(define <chars-A-Z>
  (range #\A #\Z))
  
(define get-ascii-code
  (lambda (ch) (cond 
	((equal? ch "nul") (char->integer #\nul))
	((equal? ch "lambda") 955)
	((equal? ch "newline") (char->integer #\newline))
	((equal? ch "page") (char->integer #\page))
	((equal? ch "space") (char->integer #\space))
	((equal? ch "tab") (char->integer #\tab))
	((equal? ch "return") (char->integer #\return)))))
  

       
(define <CharPrefix>
  (new (*parser (word "#\\"))
  (*pack (lambda (lst)
    (list->string lst)))
      done))
      
(define <StringLiteralChar>
  (new (*parser <any-char>)
       (*parser (char #\\))
	*diff
	done))
	
(define <NamedChar>
  (new (*parser (word "lambda"))
       (*parser (word "newline"))
       (*parser (word "nul"))
       (*parser (word "page"))
       (*parser (word "return"))
       (*parser (word "space"))
       (*parser (word "tab"))
       (*disj 7)
       (*pack
       (lambda (lst)
	  (list->string lst)))
      done))
       
(define <HexChar>
  (new (*parser <chars-a-f>)
       (*parser <digit-0-9>)
       (*disj 2)
	done))
	  	  
(define <HexUnicodeChar>
  (new (*parser (char #\x))
       (*parser <HexChar>)
       *plus
       (*caten 2)
       (*pack-with 
       (lambda (_ lst)
       (integer->char (string->number (list->string lst) 16))))
	  done))
	  	 
	  
(define <VisibleSimpleChar> 
  (range #\! #\delete))
	  
(define <Char>
  (new (*parser <CharPrefix>) 
       (*parser <NamedChar>)
       (*parser <VisibleSimpleChar>)
       (*parser <HexUnicodeChar>)
       (*disj 3)
       (*parser <end-of-input>)  
       (*caten 3)
       (*pack-with
	(lambda (_ ch end) 
	(if (char? ch) ch (integer->char (get-ascii-code ch)))))
     done))
     


#;this_is_the_symbol_part--------------------------------------------------------------------------------------------
	  
	  
(define <SymbolChar>
  (new (*parser <digit-0-9>)
       (*parser <chars-a-z>)
       (*parser <chars-A-Z>)
       (*parser (char #\^))
       (*parser (char #\*))
       (*parser (char #\-))
       (*parser (char #\_))
       (*parser (char #\=))
       (*parser (char #\+))
       (*parser (char #\<))
       (*parser (char #\>))
       (*parser (char #\?))
       (*parser (char #\/))
       (*disj 13)
       done))
       
(define <Symbol>
  (new (*parser <SymbolChar>) *plus
	done))
	
	
	
#;this_is_the_string_part--------------------------------------------------------------------------------------------
  
(define <StringLiteralChar>
  (range #\  #\delete))
 
 (define <StringHexChar>
  (new (*parser (char #\\))
       (*parser <HexUnicodeChar>)
       (*parser (char #\;))
       (*caten 3)
       (*pack-with
	  (lambda (a hex b) hex))
	  done))
 
(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
	 (*pack (lambda (_) ch))
	 done)))
   
 
(define <StringMetaChar>
  (new (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page)) 
       (*disj 6)
       done))

  
(define <StringChar>
  (new (*parser <StringMetaChar>)

       (*parser <StringLiteralChar>)

       (*parser (char #\"))
       (*parser (char #\\))
       (*disj 2)

       *diff
       
       (*parser  <StringHexChar>)
       
       (*disj 3)
       done))
       
(define <String>
  (new (*parser (char #\"))
       (*parser <StringChar>) *star
       (*parser (char #\"))
       (*caten 3)

       (*pack-with
	(lambda (open-delim chars close-delim)
	  (list->string chars)))

       done))
       

#;this_is_the_boolean_part--------------------------------------------------------------------------------------------

(define <Boolean>
    (new (*parser (word "#t"))
	 (*pack (lambda (_) #t))
	 
	 (*parser (word "#f"))
	 (*pack (lambda (_) #f))
	 
	 (*disj 2)
	 done))
	 

	 
#;this_is_the_lists_and_vectors_part--------------------------------------------------------------------------------------------
(define <ListElement>
  (new 
      (*delayed (lambda () <Sexpr>))
      (*parser (char #\ )) 
      *star
      
      (*caten 2)
      (*pack-with (lambda (ex spaces) ex))
    done))
    
(define <ProperList>
 (new (*parser  (char #\())
 
      (*parser <ListElement>)
      *star 
      
      (*parser  (char #\)))    
      (*caten 3)   
      (*pack-with
	(lambda (open exps close)
	  `(,@exps)))
			      
      done))

      
(define <ImproperList>
 (new (*parser  (char #\())
 
      (*parser <ListElement>)
      *plus
      
      (*parser (char #\.))
      (*parser (char #\ ))
      *star
      
      (*parser <ListElement>)
      
      (*parser  (char #\)))    
      (*caten 6)   
      (*pack-with
	(lambda (open exps dot spc ex close)
	  `(,@exps . ,ex)))
			      
      done))
      
(define <Vector>
  (new (*parser (char #\#))
       (*parser <ProperList>)
       (*caten 2)
       (*pack-with
	(lambda (sharp lst) 
	  (list->vector lst)))
  done))

 (define <Quoted>
    (new  (*parser  (char #\'))
	  (*delayed (lambda () <Sexpr>))
	  (*caten 2)
	  (*pack-with
	    (lambda (sign ex)
	     (list 'quote ex)))
	  done))    
	      
(define <QuasiQuoted>
    (new  (*parser  (char #\`))
	  (*delayed (lambda () <Sexpr>))
	  (*caten 2)
	  (*pack-with
	    (lambda (sign ex)
	     (list 'quasiquote ex)))
	  done))  	 

(define <Unquoted>
    (new  (*parser  (char #\,))
	  (*delayed (lambda () <Sexpr>))
	  (*caten 2)
	  (*pack-with
	    (lambda (sign ex)
	     (list 'unquote ex)))
	  done))  	 
	  
(define <UnquoteAndSpliced>
    (new  (*parser  (word ",@"))
	  (*delayed (lambda () <Sexpr>))
	  (*caten 2)
	  (*pack-with
	    (lambda (sign ex)
	     (list 'unquote-splicing ex)))
	  done)) 
       

       
#;this_is_the_InfixPart--------------------------------------------------------------------------------------------            
 
(define <Mul>
  (new  (*parser (char #\*))
  	(*pack (lambda (op) (string->symbol
			      (string op))))
			      done))
(define <Div>
  (new  (*parser (char #\/))
  	(*pack (lambda (op) (string->symbol
			      (string op))))
			      done))
			      
(define <Add>
  (new  (*parser (char #\+))
  	(*pack (lambda (op) (string->symbol
			      (string op))))
			      done))

(define <Sub>
  (new  (*parser (char #\-))
  	(*pack (lambda (op) (string->symbol
			      (string op))))
			      done))

			      
(define <PowerSymbol>
   (new  (*parser (char #\^))
  	 (*pack (lambda (op) (string->symbol
			      (string op))))
	(*parser (word "**"))
	(*pack (lambda (op) (string->symbol op)))
	
	(*disj 2)
			      done))  
(define <ops>
  (new (*parser <Mul>)
       (*parser <Add>)
       (*parser <Sub>)
       (*parser <Div>)
       (*parser <PowerSymbol>)
       (*disj 5)
       done))
       
     
       
(define OperationPrecedence 
    (lambda (ex)
      (cond ((not (pair? ex)) 0)
            ((or (equal? (car ex) (string->symbol "+")) (equal? (car ex) (string->symbol "-"))) 1)
	    ((or (equal? (car ex) (string->symbol "/")) (equal? (car ex) (string->symbol "*"))) 2)
	    ((or (equal? (car ex) (string->symbol "^")) (equal? (car ex) (string->symbol "**"))(equal? (car ex) (string->symbol "expt"))) 3)
	    ((list? (car ex)) 4)
	   )))


(define op->list (lambda (op)  (cons op '())))	

(define isExpt (lambda (op)(= (OperationPrecedence (op->list op)) 3)))
      
(define switchOrder (lambda (n op  ex2)
			(if (not (pair? ex2))
			  (if (isExpt op)
			  `(expt ,n ,ex2)
			  `(,op ,n ,ex2))
			   (if (>  (OperationPrecedence ex2) (OperationPrecedence (op->list op)))
			      (if (isExpt op)
				  `(expt ,n ,ex2)			
				  `(,op ,n ,ex2))
				  (let  ((op2 (car ex2))
				    (a (car (cdr ex2)))
				    (b (car (cddr ex2))))
				    (let ((recEx (switchOrder n op a))) 
				      `(,op2  ,recEx ,b))))))) 
				      
(define switchOrderCounter (lambda (n op  ex2)
			(if (not (pair? ex2))
			   1
			   (if (>  (OperationPrecedence ex2) (OperationPrecedence (op->list op)))
			         1
				  (let  ((op2 (car ex2))
				    (a (car (cdr ex2)))
				    (b (car (cddr ex2))))
				    (let ((counter (switchOrderCounter n op a))) 
				      (+ 1 counter)))))))   
				      

(define switchOrderLimited (lambda (n op  ex2 limit counter)
		    (if (< counter limit)
			(if (not (pair? ex2))
			  (if (isExpt op)
			  `(expt ,n ,ex2)
			  `(,op ,n ,ex2))
			   (if (>  (OperationPrecedence ex2) (OperationPrecedence (op->list op)))
			      (if (isExpt op)
				  `(expt ,n ,ex2)			
				  `(,op ,n ,ex2))
				  (let  ((op2 (car ex2))
				    (a (car (cdr ex2)))
				    (b (car (cddr ex2))))
				    (let ((recEx (switchOrderLimited n op a limit (+ 1 counter)))) 
				      `(,op2  ,recEx ,b)))))
			`(,op ,n ,ex2))))
				      
(define pack  (lambda (n op  ex2)
		 (switchOrder n op ex2)))

(define <InfixMul>
      (new 
	(*parser <Number>)
	(*parser <Mul>)
	(*delayed (lambda () <InfixExpression>))
	(*caten 3)
	(*pack-with pack)    
			done))
			
(define <InfixDiv>
      (new 
	(*parser <Number>)
	(*parser <Div>)
	(*delayed (lambda () <InfixExpression>))
	(*caten 3)
	(*pack-with pack)    
			done))
			
(define <InfixAdd>
      (new 
	(*parser <Number>)
	(*parser <Add>)
	(*delayed (lambda () <InfixExpression>))
	(*caten 3)
	(*pack-with pack)
			done))

			
(define <InfixSub>
      (new 
	(*parser <Number>)
	(*parser <Sub>)
	(*delayed (lambda () <InfixExpression>))
	(*caten 3)
	(*pack-with pack)
			done))

(define <InfixPow>
    (new
	(*parser <Number>)
	(*parser <PowerSymbol>)
	(*delayed (lambda () <InfixExpression>))
	(*caten 3)
	(*pack-with pack)
			done))	
(define <InfixParen>
      (new 
	(*parser (char #\())
	(*delayed (lambda () <InfixExpression>))
	(*parser (char #\)))
	(*parser <ops>)
	(*delayed (lambda () <InfixExpression>))
	(*caten 5)
	(*pack-with (lambda (open ex1  close op2 ex2)	  
			(cons (switchOrderCounter ex1 op2 ex2) (switchOrder ex1 op2 ex2))))
	
		 done))
		 
		 
(define <InfixParenEnd>
      (new 
	
	(*parser (char #\())
	(*delayed (lambda () <InfixExpression>))
	(*parser (char #\)))
	(*caten 3)
	(*pack-with (lambda (open ex close)	  
			   (cons 0  ex)))	
		 done))		 
			  
(define packParen (lambda (n op parenEx)
			      (let ((limit (car parenEx))
				    (ex (cdr parenEx)))
				    (switchOrderLimited n op ex limit 0))))
(define <Exp-Paren>
    (new 
	(*parser <Number>)
	(*parser <ops>)
	(*parser <InfixParen>)
	(*caten 3)
	(*pack-with packParen)
	done))
	
(define <Exp-Paren-End>
    (new 
	(*parser <Number>)
	(*parser <ops>)
	(*parser <InfixParenEnd>)
	(*caten 3)
	(*pack-with (lambda (n op ex)
			`(,op ,n ,(cdr ex))))
	done))
	
(define <Paren-On-Start>
    (new 
	(*parser <InfixParen>)
	(*pack-with (lambda (paren)
		      (cdr paren)))
	done))				   			
	      
(define  <InfixExpression>
  (new 
      (*parser <Exp-Paren>)
      (*parser <Exp-Paren-End>)
      (*parser <InfixAdd>)
      (*parser <InfixSub>)
      (*parser <InfixMul>) 
      (*parser <InfixDiv>)
      (*parser <InfixPow>)
      (*parser <Paren-On-Start>)
      (*parser <Number>)
      (*disj 9)
      
      done)) 
      
      
#;this_is_the_Sexpr--------------------------------------------------------------------------------------------     
      
(define <Sexpr>
    (new (*parser <Number>)
	 (*parser <String>)
	 (*parser <Boolean>)
	 (*parser <Symbol>)
	 (*parser <Char>)
	 (*parser <ProperList>)
	 (*parser <ImproperList>)
	 (*parser <Vector>)
	 (*parser <Quoted>)
	 (*parser <QuasiQuoted>)
	 (*parser <Unquoted>)
	 (*parser <UnquoteAndSpliced>)
	 
	 
	 (*disj 12)
	 
	 done))
	 