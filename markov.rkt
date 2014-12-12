#lang typed/racket

(provide MarkovChain train-markov generate save-markov load-markov)

(module+ test (require typed/rackunit))

(struct markov ([count : Natural] [chain : MarkovChain]) #:transparent)
(define-type MarkovChain (HashTable (List Symbol Symbol) blob))
(struct blob 
  ([count : Natural] [data : (HashTable Symbol Natural)])
  #:transparent)
(: empty-markov : markov)
(define empty-markov (markov 0 ((inst hash (List Symbol Symbol) blob))))


(: train-markov : ((Listof String) [#:initial-chain markov]
                   [#:ignore-symbolic Boolean]
                   -> markov))
(define (train-markov strings #:initial-chain [m empty-markov] #:ignore-symbolic [ignore-sym? #f])
  (let loop ([strings strings] [m m])
    (if (null? strings)
        m
        (loop (rest strings)
              (add-to-chain m (first strings) ignore-sym?)))))

(: add-to-chain : markov String Boolean -> markov)
(define (add-to-chain m str ignore-sym?)
  (define words (map string->symbol (string-split str)))
  (let loop ([words words] [m m])
    (cond [(or 
            (null? words)
            (null? (cdr words))
            (null? (cddr words)))
           m]
          [else
           (define key (list (first words) (second words)))
           (define new-value (third words))
           (define cleaned (if ignore-sym? (clean new-value) new-value))
           (if (not cleaned)
               (loop (cdddr words) m)
               (loop (rest words)
                     (add-data m key cleaned)))])))

(: clean : Symbol -> (Option Symbol))
;; cleans out all symbols except . , : ; !
;; returns #f if only symbols remain
(define (clean s)
  (define symbols #rx"[][()@#$%^&*\"'/><\\|]")
  (define exceptions #rx"^[.,:;\\!].*$")
  (define new (regexp-replace* symbols (symbol->string s) ""))
  (and (not (regexp-match? exceptions new))
       (string->symbol new)))
(module+ test
  (check-equal? (clean '|a(|) 'a)
  (check-equal? (clean '|(if|) 'if)
  (check-equal? (clean '|a)|) 'a)
  (check-equal? (clean '|a).|) 'a.)
  (check-equal? (clean '|a].|) 'a.)
  (check-equal? (clean '|module).|) 'module.)
  (check-equal? (clean '!) #f))

(: add-data : markov (List Symbol Symbol) Symbol -> markov)
(define (add-data m key val)
  (define chain (markov-chain m))
  (markov
   (add1 (markov-count m))
   (cond [(not (hash-has-key? chain key))
          (hash-set chain key (blob 1 (hash-set ((inst hash Symbol Natural)) val 1)))]
         [else
          (define old (hash-ref chain key))
          (define hs (blob-data old))
          (define hs2
            (if (not (hash-has-key? hs val))
                (hash-set hs val 1)
                (hash-update hs val add1)))

          (define b2 (blob (add1 (blob-count old)) hs2))
          (hash-set chain key b2)])))

(: generate : (markov Natural [#:seed (Option Positive-Integer)] [#:prefix (Listof Symbol)]
                      [#:chunk? (Symbol -> Any)]
                      -> String))
(define (generate m limit #:seed [seed #f] #:prefix [in-prefix null] #:chunk? [chunk? (const #t)])
  (when seed
    (random-seed seed))
  
  (define prefix
    (reverse
     (cond [(<= 2 (length in-prefix))
            in-prefix]
           [else
            (choose (markov-count m)
                    (markov-chain m)
                    blob-count)])))

  (define chain (markov-chain m))
  (define old-limit limit)
  (define words
    (reverse
     (let loop : (Listof Symbol)
          ([out : (Listof Symbol) prefix] [limit : Natural limit])
          (define key (list (second out) (first out)))
          (define blob (hash-ref chain key #f))
          (cond [(zero? limit)
                 out]
                [(not blob)
                 (loop prefix old-limit)]
                [else
                 (define s (choose (blob-count blob) (blob-data blob) (lambda ([x : Natural]) x)))
                 (loop (cons s out)
                       (if (chunk? s) (sub1 limit) limit))]))))
  (apply
   string-append
   (symbol->string (first words))
   (map (lambda (s) (format " ~a" s)) (rest words))))

(: choose : (All (A B) (-> Natural (HashTable A B) (B -> Natural) A)))
(define (choose size dict freq)
  (define idx (random size))
  (let loop ([idx idx] [h (hash->list dict)])
    (define a (car (first h)))
    (define b (cdr (first h)))
    (define f (freq b))
    (if (<= idx f)
        a
        (loop (- idx f) (rest h)))))

;; use custom writing format because cast is super slow
(: save-markov : markov Path-String -> Void)
(define (save-markov markov path)
  (define o (open-output-file path #:exists 'replace))
  (write (markov->printable markov) o)
  (close-output-port o))

(: markov->printable : markov -> Any)
(define (markov->printable m)
  `(TOP ,(markov-count m)
    ,@(map (lambda ([x : (Pairof (List Symbol Symbol) blob)])
             (list (car x) (blob->printable (cdr x))))
           (hash->list (markov-chain m)))))
(: blob->printable : blob -> Any)
(define (blob->printable blob)
  `(blob ,(blob-count blob)
    (IN ,@(map (lambda (x) x) (hash->list (blob-data blob))))))

(: load-markov : Path-String -> markov)
(define (load-markov path)
  (define i (open-input-file path))
  (define v (read i))
  (close-input-port i)
  (printed->markov v))

(: printed->markov : Any -> markov)
(define (printed->markov printed)
  (define-predicate number? Natural)
  (match printed
    [`(TOP ,n ,(list prefix blob) ...)
     (define m empty-markov)
     (if (not (number? n))
         (error 'load "expected number, got ~s" n)
         (markov
          n
          (let loop ([chain (markov-chain m)] [ps prefix] [blobs blob])
            (cond [(null? ps) chain]
                  [else 
                   (define p1 (parse-prefix (first ps)))
                   (define b1 (parse-blob (first blobs)))
                   (loop (hash-set chain p1 b1)
                         (rest ps)
                         (rest blobs))]))))]))
(: parse-prefix : Any -> (List Symbol Symbol))
(define (parse-prefix p)
  (define-predicate prefix? (List Symbol Symbol))
  (if (prefix? p)
      p
      (error 'load "expected prefix, got ~s" p)))
(: parse-blob : Any -> blob)
(define (parse-blob b)
  (define-predicate symbols? (Listof Symbol))
  (define-predicate numbers? (Listof Natural))
  (define-predicate number? Natural)
  (match b
    [`(blob ,n (IN ,(cons w c) ...))
     (if (not (and (number? n) (symbols? w) (numbers? c)))
         (error 'load "expected valid blob, got ~s" b)
         (blob n
               (let loop ([x ((inst hash Symbol Natural))]
                          [ws : (Listof Symbol)  w]
                          [cs : (Listof Natural) c])
                 (if (or (null? w) (null? cs))
                     x
                     (loop (hash-set x (first ws) (first cs))
                           (rest ws)
                           (rest cs))))))]))

(module+ test
    (test-begin
   (define m (train-markov (list "a b a b c")))
   (define (test a b)
     (check-true
      (equal? a b)
      (format "Expected:~s\nActual:~s" b a)))
   (test
    m
    (markov
     3
     (hash-set
      (hash-set
       (markov-chain empty-markov)
       (list 'a 'b) (blob 2 #hash((a . 1) (c . 1))))
      (list 'b 'a) (blob 1 #hash((b . 1))))))))

