;; I guess it needs tests

(require 'ert)
(require 'ido-grid-mode)
(require 'cl-lib)

(defmacro ido-grid-let (alist &rest body)
  `(let ,(let (bindings)
            (dolist (cell alist)
              (push (list (intern (concat
                                   "ido-grid-mode-"
                                   (symbol-name (car cell))
                                   ))
                          (cadr cell))
                    bindings))
            bindings)
     ,@body))

(ert-deftest ido-grid-columns-1 ()
    (ido-grid-let
     ((padding "")
      (jank-rows 0)
      (max-columns nil)
      (max-rows 1)
      (min-rows 1))
     (should (equal
              '[10]
              (ido-grid-mode-count-columns '(10 10) 19)))
     (should (equal
              '[10 10]
              (ido-grid-mode-count-columns '(10 10) 20)))
     (should (equal
              '[10 10]
              (ido-grid-mode-count-columns '(10 10) 21)))))

(ert-deftest ido-grid-columns-2 ()
  "Check that `ido-grid-mode-count-columns' works OK."
  (ido-grid-let
   ((padding "")
    (jank-rows 0)
    (max-columns 'nil)
    (min-rows 1)
    (max-rows 4))

   (should (equal
            '[11]
            (ido-grid-mode-count-columns '(10 11) 20)))

   (should (equal
            '[10 10]
            (ido-grid-mode-count-columns '(10 10) 20)))

   (should (equal
            '[10 10]
            (ido-grid-mode-count-columns '(10 10 10 10 10 10 10) 21)))
   (should (equal
            '[10]
            (ido-grid-mode-count-columns (list 10 10
                                         10 10
                                         10 10
                                         20) 21)))
   (should (equal
            '[10 10]
            (ido-grid-mode-count-columns (list 10 10
                                         10 10
                                         10 10
                                         10 10
                                         20 20) 21)))))

(ert-deftest ido-grid-packs-grid-vert ()
  "Test for grid packing function `ido-grid-mode-gen-grid'"
  (ido-grid-let
   ((min-rows 4)
    (max-rows 4)
    (max-columns nil)
    (padding "|")
    (prefix "")
    (order t))
   (should
    (equal
     '("hello|a   \nworld|test\nthis \nis   " 4 2)
     (ido-grid-mode-gen-grid
      (list "hello" "world" "this" "is" "a" "test")
      :name #'identity
      :decorate (lambda (n &rest s) n)
      :max-width 20)))))

(ert-deftest ido-grid-packs-grid-horiz ()
  "Test for grid packing function `ido-grid-mode-gen-grid'"
  (ido-grid-let
   ((min-rows 4)
    (max-rows 4)
    (max-columns nil)
    (padding "|")
    (prefix "")
    (order nil))
   (should
    (equal
     '("hello|world|this|is\na    |test " 4 4)
     (ido-grid-mode-gen-grid
      (list "hello" "world" "this" "is" "a" "test")
      :name #'identity
      :decorate (lambda (n &rest s) n)
      :max-width 20)))))

(ert-deftest ido-grid-misc ()
  "Check misc ido-grid functions"
  (let ((ido-grid-mode-order nil))
    (should (and (not (ido-grid-mode-column-major))
                 (ido-grid-mode-row-major))))
  (let ((ido-grid-mode-order t))
    (should (and (not (ido-grid-mode-row-major))
                 (ido-grid-mode-column-major)))))
