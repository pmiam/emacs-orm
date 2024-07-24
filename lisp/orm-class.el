;;; -*- lexical-binding: t -*-

(require 'eieio)
(require 'emacsql)

(defclass orm-class ()
  ((table :initarg :table
	  :type symbol
	  :accessor orm-table-name
	  :allocation :class
	  :documentation
	  "The relation/table in which instances of this object are saved.")
   (columns :initarg :columns
	    :type list
	    :accessor orm-columns
	    :allocation :class
	    :documentation
	    "A list of column specifications."))
  :documentation
  "This special class enables persistence through a database."
  :abstract t)

;; Static Utils

(cl-defmethod orm-column-names ((class (subclass orm-class)))
  "Get class column names"
  (let* ((obj (make-instance class))
	 (cols (orm-columns obj)))
    (mapcar (lambda (x) (slot-value x 'name)) cols)))

(cl-defmethod orm-table-name ((class (subclass orm-class)))
  "Get class table name"
  (orm-table-name (make-instance class)))

(cl-defmethod orm-table-schema ((class (subclass orm-class)))
  "Get schema for table"
  (let* ((obj (make-instance class))
	 (cols (orm-columns obj)))
    (apply 'vector (mapcar 'orm-get-column-schema cols))))

;; Instance Utils

(cl-defmethod orm-object-values ((obj orm-class))
  "Get values vector for object"
  (let* ((class (class-of obj))
	 (column-names (orm-column-names class)))
    (apply 'vector
	   (cl-loop for slot in column-names
		    collect (slot-value obj slot)))))

;; Static Methods

(cl-defmethod orm-count ((class (subclass orm-class)))
  "Count rows of class table in database."
  (let ((conn orm-default-conn)
	(table (orm-table-name class)))
    (emacsql conn [:select (funcall count *) :from $i1] table)))

(cl-defmethod orm-create ((class (subclass orm-class)))
  "Create table for object class in database."
  (let ((conn orm-default-conn)
	(table (orm-table-name class))
	(schema (orm-table-schema class)))
    (emacsql-with-transaction conn
      (emacsql conn [:create-table $i1 $S2] table schema))))

;; Insert

(cl-defmethod orm-insert ((this orm-class))
  "Insert object into database."
  (let ((conn orm-default-conn)
	(table-name (orm-table-name this))
	(values (orm-object-values this)))
    (emacsql-with-transaction conn
      (emacsql conn [:insert :into $i1 :values $v2] table-name values))))

(provide 'orm-class)
