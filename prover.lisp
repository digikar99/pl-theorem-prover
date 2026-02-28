(in-package :pl-theorem-prover)

(defun traverse (formula function)
  "Function should take the following arguments:

OPERATOR: could be NIL, NOT, OR, AND
  When NIL, the formula was atomic

SUBFORMULAE:
  List of subformulae

"
  (cond ((atom formula)
         (funcall function nil formula))
        ((and (eq 'not (first formula))
              (second formula)
              (null (rest (rest formula))))
         (funcall function
                  'not
                  (list (second formula))))
        ((eq 'or (first formula))
         (funcall function
                  'or
                  (rest formula)))
        ((eq 'and (first formula))
         (funcall function
                  'and
                  (rest formula)))
        (t
         (error "~S is not a well-formed-(sub)formula" formula))))

(defun well-formed-formula-p (formula)
  (labels ((do-nothing (op subargs)
             (if (listp subargs)
                 `(,op ,@(loop :for sub :in subargs
                               :collect (traverse sub #'do-nothing)))
                 subargs)))
    (traverse formula #'do-nothing)))

(defclass node ()
  ((formula :reader node-formula
            :initarg :formula)
   (state :accessor node-state
          :initform :open
          :type (member :open :close))
   (expanded-p :accessor node-expanded-p
               :initarg :expanded-p
               :initform nil
               :type boolean)
   (truth-value :reader node-truth-value
                :initarg :truth-value
                :type boolean)
   (parent :accessor node-parent
           :initarg :parent
           :initform nil
           :type (or null node))
   (children :accessor node-children
             :initarg :children
             :initform nil
             :type list)))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (with-slots (truth-value formula) node
      (format stream "~A: ~A" truth-value formula))))

(defvar *pprint-node-indentation* 0)
(defun pprint-node (stream node)
  (with-slots (truth-value formula parent) node
    (terpri stream)
    (loop :repeat *pprint-node-indentation*
          :do (write-char #\space stream))
    (format stream "~A: ~A" truth-value formula)
    (when parent
      (pprint-logical-block (stream nil)
        (let ((*pprint-node-indentation* (+ 2 *pprint-node-indentation*)))
          (pprint-node stream parent))))))

(set-pprint-dispatch 'node 'pprint-node)
(set-pprint-dispatch 'node nil)

(defclass tableaux ()
  ((nodes :accessor tableaux-nodes :type list :initarg :nodes)))

(defun pprint-tableaux (stream tableaux)
  (loop :for node :in (remove-if-not #'node-leaf-p (tableaux-nodes tableaux))
        :do (pprint-node stream node)))


(defun node-leaf-p (node)
  (null (node-children node)))

(defun node-branch (node)
  (loop :while node
        :collect node
        :do (setf node (node-parent node))))

(defun node-may-be-close (node)
  (let ((literal-nodes (remove-if-not (lambda (node)
                                        (symbolp (node-formula node)))
                                      (node-branch node))))
    (when literal-nodes
      (loop :for (first . rest) := literal-nodes
            :with previous-nodes := nil
            :with closed-p := nil
            :do (let ((other-nodes (remove-if-not (lambda (n)
                                                    (eq (node-formula n)
                                                        (node-formula first)))
                                                  rest)))
                  (push first previous-nodes)
                  (unless (loop :for node :in other-nodes
                                :always (eq (node-truth-value first)
                                            (node-truth-value node)))
                    (setf (node-state first) :close
                          closed-p t)))
                (setf literal-nodes rest)
            :while (and literal-nodes
                        (eq :open (node-state first)))
            :finally (when closed-p
                       (loop :for node :in previous-nodes
                             :do (setf (node-state node) :close)))))))

(defun node-open-p (node)
  (and (node-leaf-p node)
       (eq :open (node-state node))))

(defun expand-tableaux (tableaux)
  (declare (type tableaux tableaux))
  (let* ((nodes (tableaux-nodes tableaux))
         (unexpanded-nodes (remove-if #'node-expanded-p nodes))
         (node (elt unexpanded-nodes (random (length unexpanded-nodes))))
         (formula (node-formula node))
         ;; Because these are unexpanded nodes, formula is necessarily
         ;; (not ...), (or ...), (and ...)
         (op (first formula))
         (truth-value (node-truth-value node))
         (leaf-node (find-if #'node-leaf-p nodes))
         (new-nodes))

    (assert leaf-node)

    ;; Do the expansion
    (cond ((and (eq 'not (first formula))
                (second formula)
                (null (cddr formula)))
           (push (make-instance 'node
                                :expanded-p (symbolp (second formula))
                                :truth-value (not truth-value)
                                :formula (second formula)
                                :children nil
                                :parent leaf-node)
                 new-nodes)
           (setf (node-children leaf-node) new-nodes))
          ((or (and (eq 'and op)
                    (eq t truth-value))
               (and (eq 'or op)
                    (eq nil truth-value)))
           ;; Set up chain with each subformula
           (setf (node-children leaf-node)
                 (loop :with branch := (node-branch leaf-node)
                       :for sub :in (reverse (rest formula))
                       :with p-node := nil
                       :for node := (make-instance 'node
                                                   :formula sub
                                                   :truth-value truth-value
                                                   :children nil
                                                   :expanded-p (symbolp sub))
                       :do (unless (member node branch
                                           :test (lambda (n1 n2)
                                                   (and (eq (node-truth-value n1)
                                                            (node-truth-value n2))
                                                        (equal (node-formula n1)
                                                               (node-formula n2)))))
                             (if p-node
                                 (setf (node-children p-node) (list node)
                                       (node-parent node) p-node)
                                 (setf (node-parent node) leaf-node))
                             (setf p-node node)
                             (push node new-nodes))
                       :finally (return (when p-node
                                          (list p-node))))))
          ((or (and (eq 'and op)
                    (eq nil truth-value))
               (and (eq 'or op)
                    (eq t truth-value)))
           ;; Set up multiple branches for each subformula
           (setf (node-children leaf-node)
                 (loop :for sub :in (rest formula)
                       :for node := (make-instance 'node
                                                   :formula sub
                                                   :truth-value truth-value
                                                   :children nil
                                                   :expanded-p (symbolp sub)
                                                   :parent leaf-node)
                       :do (push node new-nodes)
                       :collect node))))

    (setf nodes (append nodes new-nodes))
    (setf (tableaux-nodes tableaux) nodes)
    (setf (node-expanded-p node) t)
    (mapc #'node-may-be-close new-nodes)))

(defun completely-expand-tableaux (tableaux)
  (loop :while (remove-if #'node-expanded-p (tableaux-nodes tableaux))
        :do (expand-tableaux tableaux)))

(defun semantic-tableau-prover (&rest formulae)
  "Reference: https://en.wikipedia.org/wiki/Method_of_analytic_tableaux"
  (let ((initial-tableaux ;; Make a chain from the formulae
          (make-instance
           'tableaux
           :nodes (loop :for formula :in (reverse formulae)
                        :with p-node := nil
                        :for node
                          := (make-instance 'node
                                            :formula formula
                                            :truth-value t
                                            :children nil
                                            :expanded-p (symbolp formula))
                        :do (when p-node
                              (setf (node-children p-node) (list node))
                              (setf (node-parent node) p-node))
                            (setf p-node node)
                        :collect node))))
    (completely-expand-tableaux initial-tableaux)

    (let ((open-leaf-node
            (find-if #'node-open-p (tableaux-nodes initial-tableaux))))
      (if open-leaf-node
          (format t "Provable.~%")
          (format t "Unprovable.~%"))
      ;; (mapcar #'describe (tableaux-nodes initial-tableaux))
      (values open-leaf-node
              initial-tableaux))))

(defvar *prover* 'semantic-tableau-prover)

(defun prove (&rest formulae)
  (loop :for formula :in formulae
        :do (assert (well-formed-formula-p formula)))
  (apply *prover* formulae))

(defmacro prove* (&rest formulae)
  `(prove ,@(loop :for f :in formulae
                  :collect (list 'quote f))))
