#lang racket
;;;; this module implements a double-entry account system
;;;; copyright 2024 (c) Roy E Lowrance
;;;; license: MIT    

;;; -------------------------------------------------------
;;; Identifies types and functions provided to using module
(provide
 ;; The types
 ;; - A ledger is formed by processing transactions
 transaction
 ;; - Transactions create postings in a ledger
 posting

 ;; The functions
 ;; - Convert a list of transactions into a ledger hash mapping accounts to a list of postings
 make-ledger
 ;; - Convert a ledger hash into a hash mapping acco  unts to balances in the account
 make-ledger-balances
 )

;;; -----------------------------
;;; Implements provided functions

;; Creates transaction struct
;; The only type constraint is the the accounts-amounts field be a sequence
;; Ex: (transaction "2024-01-02" "bought water" ((cash -2) (water 2)))
(struct transaction (date description accounts-amounts) #:transparent)

;; Create postings struct
;; The function make-ledger converts a list of transactions in a ledger
;; The ledger is a hash mapping accounts to lists of posting
(struct posting (date description amount) #:transparent)

;; Returns a new ledger containing the transactions
;; Arguments
;;  transaction-list: a list of transaction.
;;  starting-ledger-hash: optional hash; keys: account; values: list of posting
;;    If provided, the returned value is generated from this starting point.
(define (make-ledger transaction-list (starting-ledger-hash (hash)))
  ;; Returns a new general ledger containing the transaction
  (define (post-transaction date description account-amount-list ledger-hash)
    (cond
      ((empty? account-amount-list) ledger-hash)
      (#t (define account-amount (first account-amount-list))
          (define account (first account-amount))
          (define amount (second account-amount))
          (post-transaction date
                            description
                            (rest account-amount-list)
                            (hash-set ledger-hash
                                      account
                                      (cons (posting date description amount)
                                            (hash-ref ledger-hash account empty)))))))
  (cond
    ((empty? transaction-list) starting-ledger-hash)
    (#t (define transaction (first transaction-list))
        (make-ledger (rest transaction-list)
                     (post-transaction (transaction-date transaction)
                                       (transaction-description transaction)
                                       (transaction-accounts-amounts transaction)
                                       starting-ledger-hash)))))


;; Returns ledger balances, an immutable hash with
;;  key = acount
;;  value = amount formed form the postings for the account
;; Arguments
;;  ledger-hash: a hash from account to list of posting
;;  reduce-postings-to-balance: a function from a list of postings to an amount (oten a number)
(define (make-ledger-balances
         ledger-hash
         #:reduce-postings-to-balance (reduce-postings-to-balance default-reduce-postings-to-balance))
    (for/hash (((account postings) ledger-hash))
        (values account (reduce-postings-to-balance postings))))
           

;;; ----------------------------
;;; Implements private functions
;;; Ordered alphabetically

;; Returns a number, the sum of the amounts in a list of postings
;; Arguments
;;  postings: a list of posting: (date description amount)
(define (default-reduce-postings-to-balance postings-list)
  (define (add posting result) (+ result (posting-amount posting)))
  (foldl add 0 postings-list))
    

;; ----------------------------------
;; Performs unit tests
;; ordered so that if A calls B, B is tested before A

(require racket/trace)

(require rackunit)
(require rackunit/text-ui)

;; Define test data
(define test-transactions
  (list
   (transaction "2024-05-01" "fund business" (list (list 'cash 10) (list 'equity -10)))
   (transaction "2024-05-02" "buy equipment" (list (list 'gear 2) (list 'cash -2)))))

(define expected-general-ledger
  (hash 'cash (list (posting "2024-05-02" "buy equipment" -2)
                    (posting "2024-05-01" "fund business" 10))
        'equity (list (posting "2024-05-01" "fund business" -10))
        'gear (list (posting "2024-05-02" "buy equipment" 2))))

   
(define expected-balances
  (hash 'cash 8 'equity -10 'gear 2))

;; Tests
(trace default-reduce-postings-to-balance)
(test-case
 "default-reduce-postings-to-balance"
 (define date "date")       ; the code doesn't examine this value
 (define description "abc") ; the code doesn't examine this value
 (check-equal? (default-reduce-postings-to-balance '())
               0)
 (check-equal? (default-reduce-postings-to-balance (list (posting date description 2)))
               2)
 (check-equal? (default-reduce-postings-to-balance (list (posting date description 1) (posting date description 20) (posting date description -11)))
               10))

(trace make-ledger)
(test-case
 "make-general-ledger"
 ;; without a starting ledger
 (define result1 (make-ledger test-transactions))
 (check-equal? result1 expected-general-ledger)
 ;; with a starting ledger
 (define result2 (make-ledger test-transactions result1))
 (check-equal? (hash-count result1) (hash-count result2))) ; just check the count, not content

(trace make-ledger-balances)
(test-case
 "balances are as expected"
 (check-equal? (make-ledger-balances (make-ledger test-transactions))
               expected-balances))