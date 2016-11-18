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

#;this_is_the_string_part--------------------------------------------------------------------------------------------
  
(define <StringLiteralChar>
  (range #\  #\delete))
  
(define <VisibleSimpleChar> 
  (range #\! #\delete))
 
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
       
       #;TODO:add_a_stringHexChar_function
       (*disj 2)
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
       
      
      
#;this_is_the_Sexpr--------------------------------------------------------------------------------------------     
      
(define <Sexpr>
    (new (*parser <Number>)
	 (*parser <String>)
	 (*parser <Boolean>)
	 (*parser <ProperList>)
	 (*parser <ImproperList>)
	 (*parser <Vector>)
	 (*parser <Quoted>)
	 (*parser <QuasiQuoted>)
	 (*parser <Unquoted>)
	 (*parser <UnquoteAndSpliced>)
	 
	 
	 (*disj 10)
	 
	 done))
	 