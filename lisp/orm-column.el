;;; -*- lexical-binding: t -*-

(require 'orm-table)
(require 'eieio)

(defclass orm-column ()
  ((name :initarg :name)
   (type :initarg :type)
   (unique :initarg :unique
	   :initform nil)
   (primary-key :initarg :primary-key
		:initform nil)
   (foreign-key :initarg :foreign-key
		:initform nil)
   (not-null :initarg :not-null
	     :initform nil))
  :documentation
  "A class for the columns of an relation-mapped class.")

(cl-defmethod orm-column-constraint ((column orm-column))
  (cons (oref column name)
	(append
	 (when (oref column unique) '(:unique))
	 (when (oref column primary-key) '(:primary-key))
	 (when (oref column not-null) '(:not-null)))))

(defun orm-column--fk-constraint (name fk)
  (pcase-let* ((`(:references ,table ,id) fk)
	       (table-name (orm-table-name table))
	       (table-key (or (orm-table-primary-key table) [id])))
    (list :foreign-key (vector name) :references table-name table-key)))

(cl-defmethod orm-column-table-constraint ((column orm-column))
  (let ((name (oref column :name))
	(fk (oref column :foreign-key)))
    (cond
     (fk (orm-column--fk-constraint name fk))
     (t nil))))

;; For macros

(defun orm-column--get-slot-spec (sname soptions)
  (cons sname (orm--filter-plist soptions
				 (list :initform :initarg
				       :accessor :allocation
				       :writer :reader
				       :type :documentation
				       :custom :label
				       :group :printer))))

(defun orm-column--form-from-spec (sname soptions)
  `(orm-column :name (quote ,sname)
	       ,@(orm--filter-plist soptions
				    (list :type
					  :unique
					  :primary-key
					  :foreign-key
					  :not-null))))

(provide 'orm-column)
