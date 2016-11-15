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
       (*parser (^<meta-char> "\\f" #\page)) ; formfeed
       (*parser (^<meta-char> "\\{lambda}" (integer->char 955)))
       (*parser (^<meta-char> "\\{alef}" (integer->char 1488)))
       (*parser (^<meta-char> "\\{bismillah}" (integer->char 65021)))
       (*parser (^<meta-char> "\\{smiley}" (integer->char 9786)))

       (*disj 10)
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
  