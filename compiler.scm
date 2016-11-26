(load "~/Projects/Compilatzia/pc.scm")

#;this_is_the_whitespaces_part----------------------------------------------------------------------------------------

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))


(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
	   (*parser <p>)
	   (*parser <wrapper>)
	   (*caten 3)
	   (*pack-with
	    (lambda (_left e _right) e))
	   done))))
 


(define <line-comment>
  (let ((<end-of-line-comment>
	 (new (*parser (char #\newline))
	      (*parser <end-of-input>)
	      (*disj 2)
	      done)))
    (new (*parser (char #\;))
	 
	 (*parser <any-char>)
	 (*parser <end-of-line-comment>)
	 *diff *star

	 (*parser <end-of-line-comment>)
	 (*caten 3)
	 done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       done))
      

(define <comment>
  (disj <line-comment>
	<sexpr-comment>))

(define <skip>
  (disj <comment>
	<whitespace>))

(define ^<skipped*> (^^<wrapped> (star <skip>)))	
#;this_is_the_symbol_part--------------------------------------------------------------------------------------------

	  

(define <SymbolChar>
  (new (*parser (range #\0 #\9))
       (*parser (range-ci #\a #\z))
       (*pack char-downcase)
       (*parser (char #\!))
       (*parser (char #\$))
       (*parser (char #\<))
       (*parser (char #\>))
       (*parser (char #\?))
       (*parser (char #\^))
       (*parser (char #\*))
       (*parser (char #\-))
       (*parser (char #\_))
       (*parser (char #\=))
       (*parser (char #\+))
       (*parser (char #\/))
       (*disj 14)
       (*pack (lambda (symch) (string symch)))
       done))      
       
       

(define SymbolCharsToSymbol (lambda (lst) 
      (if (null? lst)
	""
	(string-append (car lst) (SymbolCharsToSymbol (cdr lst))))))
	
(define <Symbol>
(^<skipped*>
  (new (*parser <SymbolChar>) *plus
       (*pack (lambda (str) 
		(string->symbol (SymbolCharsToSymbol str))))
       done)))
	
#;this_is_the_numeric_part-------------------------------------------------------------------------------------------------



(define <digit-0-9>
  (range #\0 #\9))
  
(define <digit-1-9>
  (range #\1 #\9))

(define <SymbolMinusDigits>
    (new 
       (*parser <Symbol>)
       (*parser (range #\0 #\9))
       (*parser (char #\ ))
       (*disj 2)
       *diff
	done))
	

(define <nat>
  (new (*parser (char #\0))
       (*parser <end-of-input>)
       (*caten 2)
       (*pack-with (lambda (ch end) 0))
       (*parser (char #\0)) *star
       (*parser <digit-1-9>)
       (*parser <digit-0-9>) *star
       (*caten 3)
       (*pack-with
	(lambda (z a s)
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
       (*parser (char #\0)) (*pack (lambda (ch) 0))
       (*disj 2)
       (*parser (char #\/))
       (*parser <nat>)
       (*guard (lambda (n) (not (zero? n))))
       (*caten 3)
       (*pack-with
	(lambda (num div den)
	  (/ num den)))
       done))
       
(define <Number>
(^<skipped*>
   (new (*parser <rat>)
	(*parser <int>)
	(*disj 2)
	(*parser <SymbolMinusDigits>)
	*not-followed-by
	done)))
	
#; this_is_the_char_part---------------------------------------------------------------------------------------------

(define <chars-a-f>
  (range #\a #\f))
  
(define <chars-a-z>
  (range #\a #\f))
  
(define <chars-A-Z>

  (range #\A #\Z))
  

        
(define <CharPrefix>
  (new (*parser (word "#\\"))
      done))
      

	
(define <NamedChar>
  (new (*parser (word "lambda"))
       (*pack (lambda (x) (integer->char 955)))
       (*parser (word "newline"))
       (*pack (lambda (x) #\newline))
       (*parser (word "nul"))
       (*pack (lambda (x) #\nul))
       (*parser (word "page"))
       (*pack (lambda (x) #\page))
       (*parser (word "return" ))
       (*pack (lambda (x) #\return))
       (*parser (word "space"))
       (*pack (lambda (x) #\space))
       (*parser (word "tab"))
       (*pack (lambda (x) #\tab))
       (*disj 7)
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
       (string->number (list->string lst) 16)))
       (*guard (lambda (x) (and (<= x 1114111) (>= x 0))))
       (*pack integer->char)
	  done))
	  	 
  
(define <VisibleSimpleChar>
  (new (*parser <any-char>)
       (*guard 
       (lambda (a) (char>? a #\space)))
       (*parser <digit-0-9>)
       (*parser <chars-a-z>)
       (*disj 2)
       *not-followed-by
       done)) 
	  
(define <Char>
(^<skipped*>
  (new (*parser <CharPrefix>) 
       (*parser <HexUnicodeChar>)
       (*parser <NamedChar>)
       (*parser <VisibleSimpleChar>)
       (*disj 3)
       (*caten 2)
       (*pack-with
	(lambda (pre ch) 
	 ch))
     done)))
     



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
(^<skipped*>
  (new (*parser (char #\"))
       (*parser <StringChar>) *star
       (*parser (char #\"))
       (*caten 3)

       (*pack-with
	(lambda (open-delim chars close-delim)
	  (list->string chars)))

       done)))
       

#;this_is_the_boolean_part--------------------------------------------------------------------------------------------

(define <Boolean>
(^<skipped*>
    (new (*parser (word "#t"))
	 (*pack (lambda (_) #t))
	 
	 (*parser (word "#f"))
	 (*pack (lambda (_) #f))
	 
	 (*disj 2)
	 done)))
	 

	 
#;this_is_the_lists_and_vectors_part--------------------------------------------------------------------------------------------

(define <ProperList>
 (new (*parser  (char #\())
 
      (*delayed (lambda () <Sexpr>))
      *star 
      
      (*parser  (char #\)))    
      (*caten 3)   
      (*pack-with
	(lambda (open exps close)
	  `(,@exps)))
			      
      done))

      
(define <ImproperList>
 (new 
      (*parser  (char #\())
      (*delayed (lambda () <Sexpr>))
      *plus      
      (*parser (char #\.))     
      (*delayed (lambda () <Sexpr>))      
      (*parser  (char #\)))    
      (*caten 5)   
      (*pack-with
	(lambda (open exps dot ex close)
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
       

       
#;this_is_the_InfixTerminals--------------------------------------------------------------------------------------------            

 
(define <InfixSymbolChar>
  (new (*parser (range #\0 #\9))
       (*parser (range-ci #\a #\z))
       (*pack char-downcase)
       (*parser (char #\!))
       (*parser (char #\$))
       (*parser (char #\<))
       (*parser (char #\>))
       (*parser (char #\?))
       (*parser (char #\_))
       (*parser (char #\=))
       (*disj 9)
       (*pack (lambda (symch) (string symch)))
       done))   
       
	
(define <InfixSymbol>
(^<skipped*>
  (new (*parser <InfixSymbolChar>) *plus
       (*pack (lambda (str) 
		(string->symbol (SymbolCharsToSymbol str))))
       (*parser (word "**"))
       *diff
       done)))

(define <InfixSymbolMinusDigitsAndOps>
    (new 
       (*parser <InfixSymbolChar>)
       (*parser (range #\0 #\9))
       *diff
	done)) 

	
(define <InfixNumber>
(^<skipped*>
  (new (*parser <rat>)
	(*parser <int>)
	(*disj 2)
	(*parser <InfixSymbolMinusDigitsAndOps>)
	*not-followed-by
	done)))
	
(define <InfixPrefixExtensionPrefix>
  (new 
       (*parser (word "##"))
       (*parser (word "#%"))
       (*disj 2)
         done))  
	
(define <InfixSexprEscape>
  (new 
       (*parser <InfixPrefixExtensionPrefix>)
       (*delayed (lambda () <Sexpr>))
       (*caten 2)
       (*pack-with (lambda (escape expr) expr))
       done))  
 
(define <InfixTerminals>
    (new 
     (*parser <InfixNumber>)
     (*parser <InfixSymbol>)
     (*parser <InfixSexprEscape>)
     (*disj 3)
      done))
      
   
       
#;-------------------------------------------------------------------------------------------------------------
(define <InfixParen>
(^<skipped*>
	(new    (*parser (char #\())
		(*delayed (lambda() <InfixExpression>))
		(*parser (char #\)))
		(*caten 3)
		(*pack-with 
		  (lambda (open ex close) ex))
		  
		(*parser  <InfixTerminals>)
		(*disj 2)
	   done)))


#;This_is_the_array_part---------------------------------------------------------------------------------------

(define SetArrayAccessOrder 
    (lambda (acc rest)
	(if (null? rest)
	      acc
	     (SetArrayAccessOrder `(vector-ref ,acc ,(car rest)) (cdr rest)))))


(define <InfixArrayGet>
    (new
	(*delayed (lambda () <InfixFunCall2>))
	(*parser <InfixParen>)
	(*disj 2)
	(*parser (char #\[))
        (*delayed (lambda () <InfixExpression>))
	(*parser (char #\]))
	(*caten 4)
	(*pack-with (lambda (array open ex close)
		      `(vector-ref ,array ,ex)))
		      
	(*parser (char #\[))
        (*delayed (lambda () <InfixExpression>))
	(*parser (char #\]))
	(*caten 3)
	(*pack-with (lambda (open ex close)
		      ex))
	*star
	
	(*caten 2)
	(*pack-with SetArrayAccessOrder)
	
	(*parser <InfixParen>)
	(*disj 2)
	
        done))


#;This_is_the_InfixFunctions_part-----------------------------------------------------------------------------------

(define <InfixArgList>
 (new 
    (*delayed (lambda () <InfixExpression>))
    (*parser (char #\,))
    (*delayed (lambda () <InfixExpression>))
    (*caten 2)
    (*pack-with (lambda (sign ex)
		  ex))
    *star
    (*caten 2)
    (*pack-with (lambda (first rest)
		  `(,first ,@rest)))
    
    (*parser (^<skipped*> (new (*parser <epsilon>)done)))
    (*disj 2)
		  done))

(define SetFuncCallsOrder 
	(lambda (acc rest)
		  (if (null? rest)
		      acc
		      (SetFuncCallsOrder `(,acc ,@(car rest)) (cdr rest)))))

		  
(define <InfixFunCall>
  (new 
	(*parser <InfixArrayGet>)
	(*parser (char #\())
	(*parser <InfixArgList>)
	(*parser (char #\)))
	(*caten 3)
	(*pack-with (lambda (open args close)
			  args))
			  
	(*caten 2)
	(*pack-with (lambda (func args)
			`(,func ,@args)))
			
	(*parser (char #\())
	(*parser <InfixArgList>)
	(*parser (char #\)))
	(*caten 3)
	(*pack-with (lambda (open args close)
			  args))
	*star
	(*caten 2)
	(*pack-with SetFuncCallsOrder)
        
        (*parser <InfixArrayGet>)			
	(*disj 2)
	done))  
	
(define <InfixFunCall2>
  (new 
	(*parser <InfixParen>)
	(*parser (char #\())
	(*parser <InfixArgList>)
	(*parser (char #\)))
	(*caten 3)
	(*pack-with (lambda (open args close)
			  args))
			  
	(*caten 2)
	(*pack-with (lambda (func args)
			`(,func ,@args)))
			
	(*parser (char #\())
	(*parser <InfixArgList>)
	(*parser (char #\)))
	(*caten 3)
	(*pack-with (lambda (open args close)
			  args))
	*star
	(*caten 2)
	(*pack-with SetFuncCallsOrder)

	done))  	
#;This_is_the_Power_part---------------------------------------------------------------------------------------------
(define <PowerSymbol>
(^<skipped*>
    (new
	(*parser (char #\^))
	(*parser (word-ci "**"))
	(*disj 2)
	done)))
		

(define SetPowersOrder 
    (lambda (bases acc)
	      (if (null? bases)
		  acc 
		(SetPowersOrder (cdr bases) (list 'expt (car bases) acc))))) 
	    

(define <InfixPow>
    (new
          
      (*parser <InfixFunCall>)
      (*parser <PowerSymbol>)
      (*caten 2)
      (*pack-with (lambda (base op)
		     base))
      *plus
      
      (*parser <InfixFunCall>)
      (*caten 2)
      (*pack-with (lambda (bases expo)
		    (SetPowersOrder (reverse bases) expo)))
		    
		    
      (*parser <InfixFunCall>)	    	    
      (*disj 2)
	done))

#;this_is_the_infix_neg----------------------------------------------------------------------------------------

(define <NegSign>
 (^<skipped*>
    (new (*parser (char #\-))
     done)))

(define <InfixNeg>
    (new (*parser <NegSign>)
         (*parser <InfixPow>)
         (*caten 2)
         (*pack-with (lambda (minus ex) 
				  `(- ,ex)))
	 (*parser <InfixNumber>) 
	 *diff		
	 
	 (*parser <InfixPow>)			  
         (*disj 2)
        done))
#;This_is_the_Multiplication_and_Division_part----------------------------------------------------------------


(define SetBinaryOperationsOrder
	(lambda (acc rest)
		(if (null? rest)
		  acc 
		  (SetBinaryOperationsOrder `(,(caar rest) ,acc ,@(cdar rest)) (cdr rest))))) 
		  
(define <MulDivOp>
(^<skipped*>
	  (new
	    (*parser (char #\*))
	    (*parser (char #\/))
	    (*disj 2)
	    (*pack (lambda (op) (string->symbol (string op)))) 
	    done)))

	    
(define <MulDivExps>
	 (new
	 (*parser <InfixNeg>)
	 
	 (*parser <MulDivOp>) 
	 (*parser <InfixNeg>)
	 (*caten 2)
	 (*pack-with (lambda (op ex)
			  `(,op ,ex)))
	 *star
	 (*caten 2)
	 (*pack-with SetBinaryOperationsOrder)
	 done))	 
	 
#;This_is_the_Addition_and_Subtraction_part-----------------------------------------------------------------

(define <AddSubOp>
(^<skipped*>
	  (new
	    (*parser (char #\+))
	    (*parser (char #\-))
	    (*disj 2)
	    (*pack (lambda (op) (string->symbol (string op)))) 
	    done)))
	    
	    
(define <AddSubExps>
	 (new
	 (*parser <MulDivExps>)
	 
	 (*parser <AddSubOp>) 
	 (*parser <MulDivExps>)
	 (*caten 2)
	 (*pack-with (lambda (op ex)
			  `(,op ,ex)))
	 *star
	 (*caten 2)
	 (*pack-with SetBinaryOperationsOrder)
	 done))	 



#;this_is_the_Sexpr--------------------------------------------------------------------------------------------     
(define <InfixExpression>
 (new
      (*parser <AddSubExps>)    
    done))
    
(define <InfixExtension>
  (new
    (*parser <InfixPrefixExtensionPrefix>)
    (*parser <InfixExpression>)
    (*caten 2)
    (*pack-with (lambda (prefix expr) expr))
    done))
		  
       
(define <Sexpr>
(^<skipped*>
    (new 
         (*parser <Number>)
         (*parser <Symbol>)
	 (*parser <Boolean>)
	 (*parser <String>)
	 (*parser <Char>)
	 (*parser <ProperList>)
	 (*parser <ImproperList>)
	 (*parser <Vector>)
	 (*parser <Quoted>)
	 (*parser <QuasiQuoted>)
	 (*parser <Unquoted>)
	 (*parser <UnquoteAndSpliced>)
	 (*parser  <InfixExtension>)
	 
	 
	 (*disj 13)
	 
	 done)))

(define <sexpr> <Sexpr>)	 

