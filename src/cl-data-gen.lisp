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
  "Generates a randomly distributed truth value."
  (let ((b (random 2)))
    (if (zerop b) nil t)))

(defgenerator gen-pos-int (max)
  "Generates a positive int in the range [0,max)."
  (random max))

(defgenerator gen-neg-int (min)
  "Generate a negative int in the range (-min, 0]"
  (- (funcall (gen-pos-int min))))

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
  "Generates elements which are associated with the elements of the list."
  (loop for x in list
        for elem = (funcall gen)
        collect `(,x ,elem)))

(defclass person ()
  (
   (last-name :accessor last-name :initarg :last-name)
   (first-name :accessor first-name :initarg :first-name)
   (gender :accessor gender :initarg :gender)))

(defparameter *male-names* `("Elias" "Emil" "Liam" "Felix" "Theo" "Paul" "Jonas" "Anton" "Markus" "Linus"))

(defparameter *female-names* `("Emilia" "Ella" "Laura" "Lina" "Leonie" "Ida" "Sarah" "Anna" "Lena" "Julia"))

(defparameter *last-names* `("Weber" "Müller" "Schneider" "Fischer" "Meyer" "Wagner" "Becker" "Schulz" "Hoffmann" "Koch"))

(defgenerator gen-gender ()
  (if (funcall (gen-bool)) 'MALE 'FEMALE))

(defgenerator gen-firstname (gender)
  (let ((name-list (if (equal gender 'MALE) *male-names* *female-names*)))
    (funcall (gen-choose name-list))))

(defgenerator gen-person ()
  (let* ((gender (funcall (gen-gender)))
         (first-name (funcall (gen-firstname gender)))
         (last-name (funcall (gen-choose *last-names*))))
    (make-instance 'person
                   :gender gender
                   :last-name last-name
                   :first-name first-name)))
