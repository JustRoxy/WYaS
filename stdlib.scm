(define (not x) (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))
(define (id obj) obj)
(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))
(define (curry func arg1) (lambda (arg2) (func arg1 arg2)))
(define (compose f g) (lambda (arg) (f (g arg))))
(define (foldl func accum lst) (if (null? lst) accum (foldl func (func accum (car lst)) (cdr lst))))
(define (foldr func accum lst) (if (null? lst) accum (func (car lst) (foldr func accum (cdr lst)))))
(define (unfold func init pred) (if (pred init) (cons init '()) (cons init (unfold func (func init pred)))))
(define fold foldl)
(define reduce fold)
(define zero? (curry = 0))
(define positive? (curry < 0))
(define negative? (curry > 0))
(define (odd? num) (= (mod num 2) 1))
(define (even? num) (= (mod num 2) 0))
(define (max x . num-list) (fold (lambda (y z) (if (> y z) y z)) x num-list))
(define (min x . num-list) (fold (lambda (y z) (if (< y z) y z)) x num-list))
(define (list . objs) objs)
(define (length lst) (fold (lambda (x y) (+ x 1)) 0 lst))
(define (append lst . lsts) (foldr (flip (curry foldr cons)) lst lsts))
(define (reverse lst) (fold (flip (curry foldr cons)) lst lsts))
(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (memq obj lst) (fold (mem-helper (curry eq? obj) id) #f lst))
(define (memv obj lst) (fold (mem-helper (curry eqv? obj) id) #f lst))
(define (member obj lst) (fold (mem-helper (curry equal? obj) id) #f lst))
(define (assq obj alist) (fold (mem-helper (curry eq? obj) car) #f alist))
(define (assv obj alist) (fold (mem-helper (curry eqv? obj) car) #f alist))
(define (assoc obj alist) (fold (mem-helper (curry equal? obj) car) #f alist))
(define (map func lst) (foldr (lambda (x y) (cons (func x) y)) '() lst))
(define (filter pred lst) (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))
(define (sum . lst) (fold + 0 lst))
(define (product . lst) (fold * 1 lst))
(define (and . lst) (fold && #t lst))
(define (or . lst) (fold || #f lst))
(define (any? pred . lst) (apply or (map pred lst)))
(define (every? pred . lst) (apply and (map pred lst)))