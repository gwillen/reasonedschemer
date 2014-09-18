#lang racket

(require (planet dfriedman/miniKanren/minikanren))

(define succeed (== #t #t))
(define fail (== #t #f))

(define (caro l x)
  (fresh (xs)
     (== (cons x xs) l)))
(define (cdro l xs)
  (fresh (x)
     (== (cons x xs) l)))
(define (conso x xs l)
  (== (cons x xs) l))

(define (nullo x)
  (== x '()))

(define (pairo p)
  (fresh (a b)
         (conso a b p)))

(define (listo l)
  (conde
      ((nullo l))
      ((pairo l)
       (fresh (d)
           (cdro l d)
           (listo d)))))

(define (membero x l)
  (conde
   ((caro l x))
   ((fresh (d)
           (cdro l d)
           (membero x d)))))

(define (rembero x l out)
  (conde
   ((nullo l) (nullo out))
   ((caro l x) (cdro l out))
   ((fresh (res d a)
           (conso a d l)
           (rembero x d res)
           (conso a res out)))))

(define (surpriseo s)
  (rembero s '(a b c) '(a b c)))

(define (appendo l s out)
  (conde
   ((nullo l) (== s out))
   ((fresh (a d res)
           (conso a d l)
           (conso a res out)
           (appendo d s res)))))

(define (unwrapo-bad x out)
  (conde
   ((pairo x) (fresh (a)
                     (caro x a)
                     (unwrapo-bad a out)))
   ((== x out))))
; Why does this work?
; > (run 1 (x) (unwrapo-bad x 'pizza))
; The book says it should fail, and I agree, but we get:
; = '(pizza)

(define (unwrapo x out)
  (conde
   ((== x out))
   ((pairo x) (fresh (a)
                     (caro x a)
                     (unwrapo a out)))))

(define (flatteno s out)
  (conde
   ((nullo s) (== '() out))
   ((pairo s)
    (fresh (a d res-a res-d)
           (conso a d s)
           (flatteno a res-a)
           (flatteno d res-d)
           (appendo res-a res-d out)))
   ((conso s '() out))))
