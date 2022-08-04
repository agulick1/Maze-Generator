#lang racket

(provide maze-write-file build-maze); show-maze)

;; NOTE: This code creates a maze represented in the form required
;; However is does not create the .txt file for you so you have to manually
;; do that yourself


;; the structure representing a maze of size NxM
;; dimensions N and M and a HashMao tbl
(struct maze (N M tbl))
 
;; a hashmap that represents the connections of the maze
(define (connections tbl c) (dict-ref tbl c '()))


;; Used to mark two nodes as connected by adding it to the
;; connections hashmap
(define (connect! tbl c n) 
  (dict-set! tbl c (cons n (connections tbl c)))
  (dict-set! tbl n (cons c (connections tbl n))))

;; returns true if a and b are connected
;; false otherwise
;; two not calls because member returns #f or a list
;; so the not calls make the out #t or #f
(define (connected? tbl a b) (not (not(member a (connections tbl b)))))
 
;; Returns a maze of a given size
;; build-maze :: Index Index -> Maze
(define (build-maze N M)
  (define tbl (make-hash))

  ; denotes if youve already visited a node
  (define (visited? tbl c) (dict-has-key? tbl c))

  ;; Creates a list of all neighbors of a cell
  (define (neigbours c)
    (filter 
     (match-lambda [(list i j) (and (<= 0 i (- N 1)) (<= 0 j (- M 1)))])
     (for/list ([d '((0 1) (0 -1) (-1 0) (1 0))]) (map + c d))))

  ; generate the maze
  ; Picks a random cell
  (let move-to-cell ([c (list (random N) (random M))])
    ; Picks a random edge (that is not already connected to that cell)
    (for ([n (shuffle (neigbours c))] #:unless (visited? tbl n))
      ; Connects those two cells
      (connect! tbl c n)
      (move-to-cell n)))
  ; return the result
  (maze N M tbl))


;; Takes the maze that we built and writes it in node form
(define (get-node m i)
  (match-define (maze N M tbl) m)
  (cond
    [(< (* N M) (+ 1 i)) '()]
    [else
     ; a recursive call to create a list of all the nodes
     (cons (create-the-ith-node m i) (get-node m (+ 1 i)))]))



(define (create-the-ith-node m i)
  ; Creates the list of 4 bools that represents one node
  (match-define (maze N M tbl) m)
  (define edgeLeft (not(connected? tbl (index N M i)
                                   (index N M (- i M)))))
  (define edgeBottom (not(connected? tbl (index N M i)
                                     (index N M (+ i 1)))))
  (define edgeRight (not(connected? tbl (index N M i)
                                    (index N M (+ i M)))))
  (define edgeTop (not(connected? tbl (index N M i)
                                  (index N M (- i 1)))))
  (cond
    [(= i 0)
     (list edgeTop edgeRight edgeBottom #f)] ; makes start opening
    [(= i (- (* N M) 1))
     (list edgeTop #f edgeBottom edgeLeft)] ; makes end opening
    [else
     (list edgeTop edgeRight edgeBottom edgeLeft)]))


;; Helper Functions

;; replaces the 'place'th element of 'lst' with 'elmt'
(define (replace lst place elmt)
  (append (take lst place)
          (list elmt)
          (drop lst (+ place 1))))


;; makes the correct number of nodes, all with 4 walls  
(define (make-nodes num)
  (cond
    [(= 0 num)
     '()]
    [else
     (cons '(#t #t #t #t) (make-nodes (- num 1)))]))

;given
(define (bool_to_int bool)
  (if bool 1 0))

      
(define (node_to_text node)
  (integer->char (foldl (lambda (new acc) (+ acc (* (expt 2 new)
                                                    (bool_to_int (list-ref node (- 3 new)))))) 97
                                                                                               (build-list 4 identity))))

;; gets the index in an mxn of i
(define (index m n i)
  (let ([row_idx (floor (/ i n))])
    (list row_idx (- i  (* row_idx n)) )))




(define (maze-in-letter-form mz)
  (map node_to_text (get-node mz 0)))


(define m1 (build-maze 3 3))
(maze-in-letter-form m1)


(define (maze-write-file mz)
  (match-define (maze N M tbl) mz)
  (list (list N M) (maze-in-letter-form mz)))
 



