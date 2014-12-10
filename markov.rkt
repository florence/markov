#lang typed/racket

(provide MarkovChain train-markov generate save-markov load-markov)

(module+ test (require typed/rackunit))

(define-type MarkovChain (HashTable (List Symbol Symbol) blob))
(struct blob 
  ([count : Natural] [data : (HashTable Symbol Natural)])
  #:transparent)

(: train-markov : (Listof String) [#:initial-chain MarkovChain] -> MarkovChain)
(define (train-markov strings #:initial-chain [markov ((inst hash (List Symbol Symbol) blob))])
  (let loop ([strings strings] [markov markov])
    (if (null? strings)
        markov
        (loop (rest strings) (add-to-chain markov (first strings))))))

(: add-to-chain : MarkovChain String -> MarkovChain)
(define (add-to-chain markov str)
  (define words (map string->symbol (string-split str)))
  (if (or (null? words) (null? (rest words)))
      markov 
      (let loop ([words words] [markov markov])
        (cond [(empty? (cddr words)) markov]
              [else
               (define key (list (first words) (second words)))
               (define new-value (third words))
               (loop (rest words)
                     (add-data markov key new-value))]))))

(: add-data : MarkovChain (List Symbol Symbol) Symbol -> MarkovChain)
(define (add-data markov key val)
  (cond [(not (hash-has-key? markov key))
         (hash-set markov key (blob 1 (hash-set ((inst hash Symbol Natural)) val 1)))]
        [else
         (define old (hash-ref markov key))
         (define hs (blob-data old))
         (define hs2
           (if (not (hash-has-key? hs val))
               (hash-set hs val 1)
               (hash-update hs val add1)))

         (define b2 (blob (add1 (blob-count old)) hs2))
         (hash-set markov key b2)]))

(: generate : MarkovChain Natural [#:seed (Option Positive-Integer)] [#:prefix (Listof Symbol)]
   -> String)
(define (generate markov limit #:seed [seed #f] #:prefix [in-prefix null])
  (when seed
    (random-seed seed))
  
  (define prefix
    (reverse
     (cond [(<= 2 (length in-prefix))
            in-prefix]
           [else (choose markov blob-count)])))
  
  (define words
    (reverse
     (let loop : (Listof Symbol)
          ([out : (Listof Symbol) prefix] [limit : Natural limit])
          (define key (list (second out) (first out)))
          (define blob (hash-ref markov key #f))
          (if (or (not blob) (zero? limit))
              out
              (loop (cons (choose (blob-data blob) (lambda ([x : Natural]) x))
                          out)
                    (sub1 limit))))))
  (apply
   string-append
   (symbol->string (first words))
   (map (lambda (s) (format " ~a" s)) (rest words))))

(: save-markov : MarkovChain Path-String -> Void)
(define (save-markov markov path)
  (define o (open-output-file path #:exists 'replace))
  (write markov o)
  (close-output-port o))

(: load-markov : Path-String -> MarkovChain)
(define (load-markov path)
  (define i (open-input-file path))
  (define v (read i))
  (close-input-port i)
  (cast v MarkovChain))

(: choose : (All (A B) (-> (HashTable A B) (B -> Natural) A)))
(define (choose dict freq)
  (define idx (random (hash-count dict)))
  (let loop ([idx idx] [h (hash->list dict)])
    (define a (car (first h)))
    (define b (cdr (first h)))
    (define f (freq b))
    (if (<= idx f)
        a
        (loop (- idx f) (rest h))))
  #;
  (for/first : A
             ([([a : A] [b : B]) dict]
              #:when (let* ([f (freq b)] [r (<= idx b)])
                       (set! idx (- idx freq))
                       r))
    a))

(module+ test
  (: empty-markov : MarkovChain)
  (define empty-markov ((inst hash (List Symbol Symbol) blob)))
  (test-begin
   (define m (train-markov (list "a b a b c")))
   (define (test a b)
     (check-true
      (equal? a b)
      (format "Expected:~s\nActual:~s" b a)))
   (test
    m
    (hash-set
     (hash-set
      empty-markov
      (list 'a 'b) (blob 2 #hash((a . 1) (c . 1))))
     (list 'b 'a) (blob 1 #hash((b . 1)))))))

