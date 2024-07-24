;;; -*- lexical-binding: t -*-

(require 'orm-column)
(require 'orm-class)


(iter-defun orm--by-two (l)
  "Iterate over a list in chunks of two elements"
  (while l
    (iter-yield (list (car l) (cadr l)))
    (setq l (cddr l))))

(defun orm--filter-plist (plist keys)
  (apply '-concat
	 (cl-loop for x iter-by (orm--by-two plist)
		  if (member (car x) keys)
		  collect x)))

(defun orm--slot-spec-from-column-spec (sname soptions)
  (cons sname (orm--filter-plist soptions
				 (list :initform :initarg
				       :accessor :allocation
				       :writer :reader
				       :type :documentation
				       :custom :label
				       :group :printer))))

(defun orm--column-form-from-column-spec (sname soptions)
  `(orm-column :name (quote ,sname)
	       ,@(orm--filter-plist soptions
				    (list :type
					  :unique
					  :primary-key
					  :not-null))))

(defmacro deftable (name superclasses column-specs &rest options-and-doc)
  "Define a table, which is a class that maps to a relation."
  (let ((table-name (or (plist-get options-and-doc :table) name))
	slots columns)
    (pcase-dolist (`(,sname . ,soptions) column-specs)
      (push (orm--slot-spec-from-column-spec sname soptions) slots)
      (push (orm--column-form-from-column-spec sname soptions) columns))

    `(defclass ,name (orm-class)
       (,@(nreverse slots)

	(table :initform ,table-name)
	(columns :initform (list ,@(nreverse columns))))
       ,@options-and-doc)))

(provide 'orm-macros)
