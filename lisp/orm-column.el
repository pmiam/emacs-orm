;;; -*- lexical-binding: t -*-

(require 'eieio)

(defclass orm-column ()
  ((name :initarg :name)
   (type :initarg :type)
   (unique :initarg :unique
	   :initform nil)
   (primary-key :initarg :primary-key
		:initform nil)
   (not-null :initarg :not-null
	     :initform nil))
  :documentation
  "A class for the columns of an relation-mapped class.")

(cl-defmethod orm-get-column-schema ((column orm-column))
  (cons (oref column name)
	(append
	 (when (oref column unique) '(:unique))
	 (when (oref column primary-key) '(:primary-key))
	 (when (oref column not-null) '(:not-null)))))

(provide 'orm-column)
