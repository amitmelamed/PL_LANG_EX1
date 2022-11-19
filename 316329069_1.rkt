#lang pl
#|
openList Get List of List of Numbers
Output is List contains all the numbers.
|#
(: open-list : (Listof (Listof Number)) -> (Listof Number))
> (define (open-list list)
   ( cond [(null? list) '()]
    [else (append (first list) (open-list(rest list)))]))
#|
Test for open-list function
|#
(test (open-list '((1) (1 2 3))) => (open-list '((1) (1 2 3))))
(test (open-list '((1) (1 2 3))) => '(1 1 2 3 ))
(test (open-list '(() (1 23))) => '(1 23 ))
(test (open-list '(() (1 2 3))) => '(1 2 3 ))
(test (open-list '((1 2 3) ())) => '(1 2 3 ))
(test (open-list '((1 2 3) (4 5 6))) => '(1 2 3 4 5 6))
(test (equal? (open-list '((1 2 3) (4 5 6))) '(1 2 3 4 5 6)))
(test (null? (open-list '())))

#|
min-lst function
Input: List of numbers
Output: Smallest number
Algorithm:
1.Check for Empty list -> return +inf.0
2.Sort the list by <
3.Pick the first element
4.Compare first element to +inf.0 to get float value
|#
(: min-lst : (Listof Number) -> Number)
(define (min-lst lst)
( cond [(null? lst) +inf.0]
    [else (min (car (sort lst <)) +inf.0)]))
  
#|
max-lst function
Input: List of numbers
Output: Biggest number
Algorithm:
1.Check for Empty list -> return -inf.0
2.Sort the list by >
3.Pick the first element
4.Compare first element to -inf.0 to get float value
|# 
(: max-lst : (Listof Number) -> Number)
(define (max-lst lst)
( cond [(null? lst) -inf.0]
    [else (max (car (sort lst >)) -inf.0)]))
  
#|
Tests for min-lst and max-lst functions
|#
(test (min-lst '(2 1 3 4 5)) => 1.0)
(test (max-lst '(2 1 3 4 5)) => 5.0)


(: min&max : (Listof (Listof Number)) -> (Listof Any))
(define (min&max lst)
  (define new_lst (open-list lst)) 
  (define min_num (min-lst new_lst))
  (define max_num (max-lst new_lst))
  (list min_num max_num)
  )

(test (min&max '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1.0 233.0))
(test (min&max '(( 90))) => '(90.0 90.0))
(test (min&max '(() () () ())) => '(+inf.0 -inf.0))


#|
min-lst_apply function
Input: List of numbers
Output: Smallest number
Algorithm:
1.Check for Empty list -> return +inf.0
2.Sort the list by <
3.Pick the first element

|#
(: min-lst_apply : (Listof Number) -> Number)
(define (min-lst_apply lst)
( cond [(null? lst) +inf.0]
    [else  (car (sort lst <))]))
  
#| 
max-lst_apply function
Input: List of numbers
Output: Biggest number
Algorithm:
1.Check for Empty list -> return -inf.0
2.Sort the list by >
3.Pick the first element

|# 
(: max-lst_apply : (Listof Number) -> Number)
(define (max-lst_apply lst)
( cond [(null? lst) -inf.0]
    [else  (car (sort lst >))]))
  
#|
Tests for min-lst_apply and max-lst_apply functions
|#
(test (min-lst_apply '(2 1 3 4 5)) => 1)
(test (max-lst_apply '(2 1 3 4 5)) => 5)


(: min&max_apply : (Listof (Listof Number)) -> (Listof Any))
(define (min&max_apply lst)
  (define new_lst (open-list lst)) 
  (define min_num (min-lst_apply new_lst))
  (define max_num (max-lst_apply new_lst))
  (list min_num max_num)
  )

(test (min&max_apply '((1 2 3) (2 3 3 4) (9 2 -1) (233 11 90))) => '(-1 233))
(test (min&max_apply '(( 90))) => '(90 90))
(test (min&max_apply '(() () () ())) => '(+inf.0 -inf.0))



#|
Q2
|#

(define-type Table
  [EmptyTbl]
  [Add Symbol String Table])

#|
The Search table function
input Symbol and table
returns the element if exist
else return false
|#

(: search-table : Symbol Table -> ( U String #f))
(define (search-table input_symbol table)
  (cases table ;;Check table cases
    [(EmptyTbl) #f] ;;Base case -> empty table -> return false
    [(Add symbol string table) ;;Add table -> Check for table element
    (cond[(eq? symbol input_symbol )string] ;;symbol found -> return symbol
    [else
     (search-table input_symbol table);;symbol not found-> recursive call to function
     ])]))
#|
remove-item function ->
input: key and table
output: table after removing the first LIFO key item
if the table does not contain key-> return the same table
if the table is empty -> return the empty table
|#


(: remove-item : Table Symbol -> Table)
(define ( remove-item table rmv_symbol)
  (cases table
    [(EmptyTbl) (EmptyTbl)] ;;Case 1-> empty table -> return empty table
    [(Add symT str table)   ;;Case 2-> Not empty Table -> Check for Item to remove is equal to current item table 
     (cond [(equal? symT rmv_symbol) table] ;;Case 2.1 -> Found Item to remove -> return the table without the Item
       [else(Add symT str (remove-item table rmv_symbol))])]))  ;;Case 2.3 Not Found Item to Remove -> Call the Recursive function to find it with the next table


(test (EmptyTbl) => (EmptyTbl))
(test (Add 'b "B" (Add 'a "A" (EmptyTbl))) => 
 (Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) => 
 (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))
(test (search-table 'c (Add 'a "AAA" (Add 'b "B" (Add 'a "A" 
(EmptyTbl)))))
=> #f)
(test (search-table 'a (Add 'a "AAA" (Add 'b "B" (Add 'a "A" 
(EmptyTbl)))))
=> "AAA")
(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A" 
(EmptyTbl)))) 'a)
=> (Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A" 
(EmptyTbl)))) 'b)
=> (Add 'a "AAA" (Add 'a "A" (EmptyTbl))))

