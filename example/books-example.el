(require 'orm-table)

(deftable author ()
	  ((name :initarg :name
		 :primary-key t
		 :not-null t)))

(orm-create author)
(setq ayn-rand (author :name "Ayn Rand"))
(orm-insert ayn-rand)

(deftable book ()
	  ((title :initarg :title
		  :primary-key t
		  :not-null t))
	  :associations
	  ((:belongs_to author)))

(orm-create book)
(setq fountain-head (book :title "Fountain Head" :author-id "Ayn Rand"))
(orm-insert fountain-head)
