;;;; cl-data-gen.lisp

(in-package #:cl-data-gen)

(defmacro defgenerator (name args &body body)
  "generate a lambda function with zero arguments and the body"
  (multiple-value-bind (forms decls doc) (parse-body body :documentation t)
    (declare (ignore forms decls))
    `(defun ,name ,args
       ,doc
       (lambda () ,@body))))

(defgenerator gen-bool ()
  "Generate a randomly distributed truth value."
  (let ((b (random 2)))
    (if (zerop b) nil t)))

(defgenerator gen-pos-int (max)
  "Generate a positive int in the range [0,max)."
  (random max))

(defgenerator gen-neg-int (min)
  "Generate a negative int in the range (-min, 0]"
  (- (funcall (gen-pos-int min))))

(defgenerator gen-int (&key min max)
  "Generate an int between min and max [min, max)]."
  (+ min (random (- max min))))

(defgenerator gen-choose (l)
  "Choose randomly one of the elements of the list l."
  (let* (
         (list-length (length l))
         (elem (funcall (gen-pos-int list-length)))
         )
    (nth elem l)))

(defgenerator gen-const (elem)
  "Generate a constant element."
  elem)

(defgenerator gen-n-elems (n gen)
  "Generate n elements using the generator gen."
  (loop repeat n
        collect (funcall gen)))

(defgenerator gen-while (gen predicate)
  "Generate an element while the predicate holds.

Returns a list of elements.
"
  (loop for new-value = (funcall gen)
        while (funcall predicate new-value)
        collect new-value))

(defgenerator gen-zip-with (list gen)
  "Generate elements which are associated with the elements of the list."
  (loop for x in list
        for elem = (funcall gen)
        collect `(,x ,elem)))

(defgenerator gen-values (gens)
  "Generates a value list from a list of generators"
  (let ((result (loop for g in gens
                      for r = (funcall g)
                      collect r)))
    (values-list result)))
