(comment "CPSA 2.1.0")
(comment "All input read")

(defprotocol uniq-orig basic
  (defrole init (vars (n text)) (trace (send n)) (uniq-orig n))
  (defrole resp (vars (m n text)) (trace (send (enc m n)) (recv n))))

(defskeleton uniq-orig
  (vars (n m text))
  (defstrand init 1 (n n))
  (defstrand resp 2 (m m) (n n))
  (uniq-orig n)
  (traces ((send n)) ((send (enc m n)) (recv n)))
  (label 0)
  (unrealized (1 1)))

(defskeleton uniq-orig
  (vars (n m text))
  (defstrand init 1 (n n))
  (defstrand resp 2 (m m) (n n))
  (precedes ((0 0) (1 1)))
  (uniq-orig n)
  (traces ((send n)) ((send (enc m n)) (recv n)))
  (label 1)
  (parent 0)
  (unrealized)
  (shape))

(comment "Nothing left to do")
