#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

; define lexer variables
(define-lex-abbrevs
  [letter (:or(char-range "A" "z")"_")]
  [number (char-range "0" "9")] 

  [identifier (:: letter ;identifier begins with letter or underscore (or just a single letter)
               (:*(:or letter number)))] ;identifier can have inf repetition of x y tokens
  [string-literal (:: (:or #\" #\”) (:* letter) (:or #\" #\”))] ; second quote is unioned just incase, as the example uses these 'weird?'quotes
  [keyword (:or "let" "add" "return" "fn" "if" "else")]
  [operator (:or "+" "-" "–" "*" "/" "=" "==" "!=")]
  [symbol (:or "{" "}" "(" ")" ":" ";" ",")]
  [comment (:: "//" any-string)] ;comment and anything on the line after it
  [integer (:+ number)]) ;integer defined as 1 to inf numbers
  
(define operator-hash-table
  (hash "+" 'OP_Add "-" 'OP_Subtract "/" 'OP_Divide "*" 'OP_Multiply "=" 'OP_Assign
        "==" 'OP_Equals "–" 'OP_Subtract))

(define symbol-hash-table
  (hash ":" 'Colon ";" 'Semi-colon "," 'Comma "(" 'Leftparen ")" 'Rightparen
        "{" 'Leftbrace "}" 'Rightbrace))

(define (lexeme->keyword l)
  (string->symbol (~a "KEYWORD_" l))) ; cocatenate keyword with lexeme

(define (lexeme->operator l)
  (hash-ref operator-hash-table l)) ; match key(l) to operator hash value

(define (lexeme->symbol l)
  (hash-ref symbol-hash-table l)) ; match key(l) to symbol hash value

(define (token name [value #f]) 
  (cons name (if value (list value) '()))) ; pair name and value
;if there is a value then turn the value into a list otherwise empty list


; whenever RE is encountered run X action where token fn is called
; and a corresponding name and value is returned
(define (lex input)
  (define my-lexer
    (lexer
     [integer (token 'Integer (string->number lexeme))]
     [keyword (token (lexeme->keyword  lexeme))]
     [operator (token (lexeme->operator lexeme))]
     [string-literal (token 'String  lexeme)]
     [symbol (token (lexeme->symbol   lexeme))]
     [comment #f]
     [whitespace #f]
     [identifier (token 'Identifier lexeme)]
     [(eof) (token 'End_of_input)]))
  
  (define (next-token) (my-lexer input)) ;define next token(recursive) lexer input as parameter
  next-token)
 
(define (string->tokens s) ;string->token defined by port->token return value
  (port->tokens (open-input-string s)))
 
(define (port->tokens input) ; port to token called by string to token
  (define next-token (lex input)) 
  (let loop ()(match (next-token)
         [#f(loop)] ; skip whitespace and comments
         [(list 'End_of_input)(list (list 'End_of_input))] ; eof encounter, function ends here
         [(list name value)(cons (list name value) (loop))] ; name and value encounter
         [(list name)(cons(list name)(loop))]))) ; just the name
;tokens as a list
(define token-list '())
(define (add bs)
  (for ([b bs])
    (for ([y b])
      (set! token-list(cons y token-list)))))
    
;display function
(define (display-tokens ts)
  (for ([t ts])
    (for ([x t])
      (display x) (display "\t\t"))
    (newline)))
(define p 3)
(provide p)

