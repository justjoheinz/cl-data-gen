(in-package #:cl-data-gen-tests)


(def-suite all-tests
  :description "All tests for cl-data-gen")

(in-suite all-tests)

(test check-gen-pos-int
  (let* ((result (funcall (gen-pos-int 10))))
    (is (or (zerop result)
            (plusp result)))))

(test check-gen-neg-int
  (let* ((result (funcall (gen-neg-int 10))))
    (is (or (zerop result)
            (minusp result)))))

(test check-gen-const
  (let* ((result (funcall (gen-const 10))))
    (is (equal 10 result))))

(test check-gen-choose
  (let* ((elements `(one two three))
         (result (funcall (gen-choose elements))))
    (is (find result `(one two three)))))

(test check-gen-while
  (let* ((result (funcall (gen-while (gen-pos-int 100) (lambda (g) (<= g 95))))))
    (is (listp result))))

(test check-gen-n-elems
  (let* ((result-empty (funcall (gen-n-elems 0 (gen-pos-int 10))))
         (result (funcall (gen-n-elems 20 (gen-pos-int 10)))))
    (is (null result-empty))
    (is (equal (length result) 20))))

(test check-gen-value
      (multiple-value-bind (one two three)
          (funcall
           (gen-values (list (gen-const 1) (gen-const 2) (gen-const 3))))
        (is (equal 1 one))
        (is (equal 2 two))
        (is (equal 3 three))))

;;(eval-when (:compile-toplevel :execute :load-toplevel)
;;  (run! 'all-tests)
;;  )
