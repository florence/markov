#lang typed/racket

(provide MarkovChain train-markov generate save-markov load-markov)

(module+ test (require typed/rackunit))

(define-type MarkovChain (HashTable (List Symbol Symbol) blob))
(struct blob 
  ([count : Natural] [data : (HashTable Symbol Natural)])
  #:transparent)
(: empty-markov : MarkovChain)
(define empty-markov ((inst hash (List Symbol Symbol) blob)))


(: train-markov : (Listof String) [#:initial-chain MarkovChain] -> MarkovChain)
(define (train-markov strings #:initial-chain [markov empty-markov])
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

;; use custom writing format because cast is super slow
(: save-markov : MarkovChain Path-String -> Void)
(define (save-markov markov path)
  (define o (open-output-file path #:exists 'replace))
  (write (markov->printable markov) o)
  (close-output-port o))

(: markov->printable : MarkovChain -> Any)
(define (markov->printable markov)
  `(TOP ,@(map (lambda ([x : (Pairof (List Symbol Symbol) blob)])
                 (list (car x) (blob->printable (cdr x))))
               (hash->list markov))))
(: blob->printable : blob -> Any)
(define (blob->printable blob)
  `(blob ,(blob-count blob)
    (IN ,@(map (lambda (x) x) (hash->list (blob-data blob))))))

(: load-markov : Path-String -> MarkovChain)
(define (load-markov path)
  (define i (open-input-file path))
  (define v (read i))
  (close-input-port i)
  (printed->markov v))

(: printed->markov : Any -> MarkovChain)
(define (printed->markov printed)
  (match printed
    [`(TOP ,(list prefix blob) ...)
     (define m empty-markov)
     (let loop ([m m] [ps prefix] [blobs blob])
       (cond [(null? ps) m]
             [else 
              (define p1 (parse-prefix (first ps)))
              (define b1 (parse-blob (first blobs)))
              (loop (hash-set m p1 b1)
                    (rest ps)
                    (rest blobs))]))]))
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

(: choose : (All (A B) (-> (HashTable A B) (B -> Natural) A)))
(define (choose dict freq)
  (define idx (random (hash-count dict)))
  (let loop ([idx idx] [h (hash->list dict)])
    (define a (car (first h)))
    (define b (cdr (first h)))
    (define f (freq b))
    (if (<= idx f)
        a
        (loop (- idx f) (rest h)))))

(module+ test
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

