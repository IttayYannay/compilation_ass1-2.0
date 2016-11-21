(load "pc.scm")

					;From Mayer's tutorial:
(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

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

(define ^<skipped*> (^^<wrapped> (star <skip>)))


; done dealing with whitespace & endline


(define <digit-0-9>
  (range #\0 #\9))

(define <digit-1-9>
  (range #\1 #\9))

(define <ValidChar>
  (range #\! #\~))

(define <a-f_Char>
  (range #\a #\f))

(define <ci-Char-a-z>
  (range-ci #\a #\z))


(define <Chars-for-SymbolChar>
  (new (*parser (char #\!))
       (*parser (char #\$))
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
       (*disj 12)
       done
       ))

(define <False>
  (new (*parser (char #\#))
       (*parser (char #\f))
       (*caten 2)
       done))

(define <True>
  (new (*parser (char #\#))
       (*parser (char #\t))
       (*caten 2)
       done))
		
(define <Boolean>
  (new (*parser <False>)
       (*parser <True>)
       (*disj 2)
       (*pack-with (lambda (a s)
	     (list->string `(,a ,s))))
       done))
       
(define <CharPrefix>
  (new (*parser (char #\#))
       (*parser (char #\\))
       (*caten 2)
	done))

(define  <VisibleSimpleChar>
  (new
   (*parser <ValidChar> )
   done
   ))

(define  <NamedChar>  
  (new  (*parser (word "lambda"))
	(*pack (lambda (_)  (integer->char 955)))
	(*parser (word "newline"))
       	(*pack (lambda (_)  (integer->char 10)))
       	(*parser (word "nul"))
     	(*pack (lambda (_)  (integer->char 0)))
       	(*parser (word "page"))
       	(*pack (lambda (_) (integer->char 12)))
       	(*parser (word "return"))
      	(*pack (lambda (_) (integer->char 13)))
       	(*parser (word "space"))
      	(*pack (lambda (_) (integer->char 32)))
	(*parser (word "tab"))
	(*pack (lambda (_) (integer->char 9)))
	(*disj 7)

	done ))


(define <HexChar>
  (new (*parser <digit-0-9>)
       (*parser <a-f_Char>)
       (*disj 2)
       done
       ))

(define  <HexUnicodeChar> 
  (new (*parser (char #\x))
       (*parser <HexChar>) *plus
       (*caten 2)
       (*pack-with (lambda(xChar list)
		    (integer->char
		     (string->number
		      (list->string list) 16))))
       done))

(define <Char>
  (new (*parser <CharPrefix>)
       (*parser <NamedChar>)
       (*parser <HexUnicodeChar>)
       (*parser <VisibleSimpleChar>)
       (*disj 3)
       (*caten 2)
       (*pack-with (lambda(pre ch)
		    ch))
      done))

(define <StringLiteralChar> 
  (new (*parser <any-char>)
       (*parser (char #\\)) *diff
       done))

(define <StringMetaChar>
  (new (*parser (word  "\\\\"))
       (*pack (lambda(_) "\\"))
       (*parser (word "\\\""))
       (*pack (lambda(_) "\""))
       (*parser (word "\\t"))
       (*pack (lambda(_) "\t"))
       (*parser (word "\\f"))
       (*pack (lambda(_) "\f"))
       (*parser (word "\\n"))
       (*pack (lambda(_) "\n"))
       (*parser (word "\\r"))
       (*pack (lambda(_) "\r"))
       (*disj 6)
       done))

	       
(define <StringHexChar>
  (new (*parser (char #\\))
       (*parser (char #\x))
       (*parser <HexChar>) *star
       (*caten 3)
       done))

(define <StringChar>
  (new (*parser <StringLiteralChar>)
     ;  (*parser <StringMetaChar>)
     ;  (*parser <StringHexChar>)
    ;   (*disj 2)
       done));

(define <String> 
  (new (*parser (char #\"))
       (*parser <StringChar>) *star
       (*parser (char #\"))
       (*caten 3)
       done))

(define <Natural>
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

(define <Integer>
  (new (*parser (char #\+))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
	(lambda (++ n) n))

       (*parser (char #\-))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
	(lambda (-- n) (- n)))

       (*parser <Natural>)

       (*disj 3)

       done))

(define <Fraction>
  (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*guard (lambda (n) (not (zero? n))))
       (*caten 3)
       (*pack-with
	(lambda (num div den)
	  (/ num den)))
       done))
(define <Number>
  (new (*parser <Fraction>)
       (*parser <Integer>)
       (*disj 2)
       done))

(define <SymbolChar>
  (new (*parser <digit-0-9>)
       (*parser <ci-Char-a-z>)
       (*parser <Chars-for-SymbolChar>)
       (*disj 3)
      done))

(define <Symbol>
  (new (*parser <SymbolChar>) *plus
       ;(*pack (lambda(x) (list->string x))) 
      done))


(define <open-Bra>
  (new (*parser (char #\( ))
       (*pack
	(lambda(_) #\( ))
       done))

(define <close-Bra>
  (new (*parser (char #\) ))
       (*pack
	(lambda(_) #\) ))
       done))

(define <ProperList>
  (new
   (*parser <open-Bra> )
   (*delayed (lambda () <Sexpr> )) *star
   (*parser <close-Bra> )
  (*caten 3)
       (*pack-with
	(lambda(a exprs c)
	  exprs))
       done))


(define <ImproperList>
  (new (*parser <open-Bra>)
       (*delayed (lambda () <Sexpr> ))  *plus
       (*parser (char #\. ))
       (*delayed (lambda () <Sexpr> ))
       (*parser <close-Bra>)
       (*caten 5)
       (*pack-with (lambda(a exps c last e)
		     (fold-right cons last exps)))
       done))

(define <Vector>
  (new (*parser (char #\# ))
       (*parser <open-Bra>)
      (*delayed (lambda () <Sexpr> )) *star
       (*parser <close-Bra> )
       (*caten 4)
       (*pack-with
	(lambda(a b sexprs d)
	  sexprs))
       done))

(define <Quoted>
  (new (*parser (char #\' ))
       (*delayed (lambda () <Sexpr>))
       (*caten 2)
       (*pack-with
	(lambda(ch sexp)
	  (list 'quote sexp)))
       done))

(define <QuasiQuoted>
  (new (*parser (char #\` ))
       (*delayed (lambda () <Sexpr>))
       (*caten 2)
       (*pack-with (lambda(ch sexpr)
		     (list 'quasiquote sexpr)))
       done))

(define <Unquoted>
  (new (*parser (char #\, ))
       (*delayed (lambda () <Sexpr>))
       (*caten 2)
       (*pack-with (lambda(ch sexpr)
		     (list 'unquote sexpr)))
	      done))

(define <UnquoteAndSpliced>
  (new (*parser (char #\, ))
       (*parser (char #\@ ))
        (*delayed (lambda () <Sexpr>))
       (*caten 3)
       (*pack-with (lambda(ch1 ch2 sexpr)
			    (list 'unquote-splicing sexpr)))
       done))



(define <Sexpr> 
(^<skipped*>  ;support for comment-line & whitespace 
 (new (*parser <Boolean>)
      (*parser <Char>)
      (*parser <Number>)
      (*parser <String>)  
      (*parser <Symbol>)
      (*parser <ProperList>)
      (*parser <ImproperList>)
      (*parser <Vector>)
      (*parser <Quoted>)
      (*parser <QuasiQuoted>)
      (*parser <Unquoted>)
      (*parser <UnquoteAndSpliced>)
      (*delayed (lambda () <InfixExtension>))
       (*disj 13)
       done
       )))



(define <InfixPrefixExtensionPrefix>
  (new (*parser (char #\#))
       (*parser (char #\#))
       (*caten 2)
       (*parser (char #\#))
       (*parser (char #\%))
       (*caten 2)
       (*disj 2)
      ; (*disj 2)
       ;(*caten 2)
       done))



(define <InfixMul>
  (new	 (*parser <Number>)
	 (*parser (char #\*))
	 (*parser <Number>)
	 (*caten 3)
	 (*pack-with (lambda(exp1 ch exp2)
		       (list ch exp1 exp2)))
	 done))


(define <InfixAdd>
  (new	 (*parser <InfixMul>)
	 (*parser (char #\+))
	 (*parser <InfixMul>)
	 (*caten 3)
	 (*pack-with (lambda(exp1 ch exp2)
		       (list ch exp1 exp2)))
	 done))

(define <InfixSub>
  (new	 (*parser <InfixAdd>)
	 (*parser (char #\-))
	 (*parser <InfixAdd>)
	 (*caten 3)
	 (*pack-with (lambda(exp1 ch exp2)
		       (list ch exp1 exp2)))
	 done))

(define <InfixNeg>
  (new (*parser (char #\-))
       (*delayed (lambda() <InfixExpression>))
       (*caten 2)
       (*pack-with (lambda(char exp)
		     (- exp)))
       done))


(define <InfixParen>
  (new    (*parser (char #\( ))
          (*delayed (lambda() <Initial>))
          (*parser (char #\)))
          (*caten 3)
          (*pack-with (lambda(bra1 exp bra2)
                        exp))
          done))


(define <PlusMinusChars>
  (new (*parser (char #\+))
       (*parser (char #\-))
       (*disj 2)
       done))
(define <MulDivChars>
  (new (*parser (char #\*))
       (*parser (char #\/))
       (*disj 2)
       done))

(define <PowChars>
  (new (*parser (char #\^))
       done))

(define <Pow_End> ;L3
  (new
   (*parser <Number>)
   (*parser <Symbol>)  
   (*parser <InfixParen>)
   (*disj 3)
   done))

(define <MulDiv> ;L2
  (new
   (*parser <Pow_End>)
   (*parser <PowChars>)
   (*parser <Pow_End>)
   (*caten 2)
   (*pack-with (lambda (sign exps)
                 (lambda (first_element)
                   `(,(string->symbol (string sign)) ,first_element ,exps))))
   *star
   (*caten 2)
     (*pack-with (lambda (first_exp lambda_rest_exps)
                   (fold-left (lambda (operator acc)
                                (acc operator)) first_exp lambda_rest_exps)))
   done))



(define <AddSub> ;L1
    (new
     (*parser <MulDiv>)
     (*parser <MulDivChars>)
     (*parser <MulDiv>)
     (*caten 2)
     (*pack-with (lambda (sign exps)
                    (lambda (first_element)
                      `(,(string->symbol (string sign)) ,first_element ,exps))))
     *star
     (*caten 2)
     (*pack-with (lambda (first_exp lambda_rest_exps)
                   (fold-left (lambda (operator acc)
                                (acc operator)) first_exp lambda_rest_exps)))
     done))


(define <Initial>  ;L0
  (new
   (*parser <AddSub>)
   (*parser <PlusMinusChars>)
   (*parser <AddSub>)
   (*caten 2)
        (*pack-with (lambda (sign exps)
                    (lambda (first_element)
                      `(,(string->symbol (string sign)) ,first_element ,exps))))
   *star
   (*caten 2)
     (*pack-with (lambda (first_exp lambda_rest_exps)
                   (fold-left (lambda (operator acc)
                                (acc operator)) first_exp lambda_rest_exps)))

;	 (*parser <InfixArrayGet>)
;	 (*parser <InfixFuncall>)
;	 (*parser <InfixSexprEscape>)
;	 (*parser <InfixSymbol>)
	 ;(*disj 2)
   done))

(define <InfixExtension>
  (new (*parser <InfixPrefixExtensionPrefix>)
       (*parser <Initial>)
       (*caten 2)
       (*pack-with
        (lambda(pre exp) exp))
done))



  
;---------------------------------------------------------------------------------------------------------------------------
;<infixExp>==><sub>
;*<sub>==> <add>(' - ' <add>)
;*<add>==> <div>(' + ' <div>)
;*<div>==> <mul>(' / ' <mul>)
;.............
;<theLastOne> ==>(number | infixSymbol | parenthesis |........)


;ביטוי infix-הכיוון לפי מה שהבנתי הוא
; + הופך לביטוי כפל, כפל הופך לחזקה, חזקה למס', מס' לסוגריים

;. זאת אומרת גזירה מהתעדוף הכי נמוך לגבוה
;. זה הכיוון, עוד לא מצאתי פיתרון.



#;(define <End>
  (new
   (*parser <Number>)
   (*parser  <InfixNeg>)
;  (*parser <Weak>)
   
   (*disj 2 )
   done))

#;(define <Sub>
  (new
   (*parser <End>)
   (*parser (char #\-))
   (*parser <End>)
   (*caten 3)
   (*pack-with (lambda(exp1 op exp2)
		 (list op exp1 exp2)))
   done))




  #;(define <Add>
  (new
   ;(*delayed (lambda() <Initial>))
   (*parser <End>)
  ; (*disj 2)
   (*parser (char #\+))
   (*delayed (lambda() <Initial>))
   ;(*parser <End>)
  ; (*disj 2)
   (*caten 3) 
   (*pack-with (lambda(exp1 op exp2)
		 (list op exp1 exp2)))
   done))