;;; -*- lexical-binding: t -*-

(require 'orm-column)
(require 'orm-class)
(require 'orm-assocs)


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

;; SLOTS

(defun orm--slot-spec-from-column-spec (sname soptions)
  (cons sname (orm--filter-plist soptions
				 (list :initform :initarg
				       :accessor :allocation
				       :writer :reader
				       :type :documentation
				       :custom :label
				       :group :printer))))

;; COLUMNS

(defun orm--column-form-from-column-spec (sname soptions)
  `(orm-column :name (quote ,sname)
	       ,@(orm--filter-plist soptions
				    (list :type
					  :unique
					  :primary-key
					  :not-null))))

;; ASSOCIATIONS

(defun orm--get-assoc-form (type class options)
  "Make assoc instance"
  `(orm-assoc ;; :type ,type
	      :class ,class
	      ,@options))

(defun orm--get-assoc-self-column (type class options)
  (let* ((fk (or (plist-get options :foreign_key)
		 (intern (format "%s-id" (symbol-name class)))))
	 (fk-keyword (intern (format ":%s" (symbol-name fk)))))
    (pcase type
      (:belongs_to `(,fk :initarg ,fk-keyword
			 ;; :foreign-key t
			 ))
      (_ nil))))

;; REST

(defun orm--filter-options-and-doc (options-and-doc)
  (cond ((and (stringp (car options-and-doc))
              (/= 1 (% (length options-and-doc) 2)))
         (error "Too many arguments to `defclass'"))
        ((and (symbolp (car options-and-doc))
              (/= 0 (% (length options-and-doc) 2)))
         (error "Too many arguments to `defclass'")))

  (if (stringp (car options-and-doc))
      (setq options-and-doc
            (cons :documentation options-and-doc)))

  (orm--filter-plist options-and-doc
		     (list :documentation
			   :allow-nil-initform
			   :custom-groups
			   :abstract
			   :method-invocation-order)))

;; DEFTABLE

(defmacro deftable (name mixins column-specs &rest options-and-doc)
  "Define a table, which is a class that maps to a relation."
  (let ((table-name (or (plist-get options-and-doc :table) name))
	(assoc-specs (plist-get options-and-doc :associations))
	slots columns assocs)
    (pcase-dolist (`(,sname . ,soptions) column-specs)
      (push (orm--slot-spec-from-column-spec sname soptions) slots)
      (push (orm--column-form-from-column-spec sname soptions) columns))

    (pcase-dolist (`(,type ,class . ,options) assoc-specs)
      (push (orm--get-assoc-form type class options) assocs)

      (when-let ((col-spec (orm--get-assoc-self-column type class options)))
	(pcase-let ((`(,sname . ,soptions) col-spec))
	  (push (orm--slot-spec-from-column-spec sname soptions) slots)
	  (push (orm--column-form-from-column-spec sname soptions) columns))))

    `(defclass ,name (orm-class ,@mixins)
       (,@(nreverse slots)

	(table :initform ,table-name)
	(columns :initform (list ,@(nreverse columns)))
	(associations :initform (list ,@(nreverse assocs)))

       ,@(orm--filter-options-and-doc options-and-doc)))))

(provide 'orm-macros)
