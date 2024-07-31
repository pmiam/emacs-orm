;;; -*- lexical-binding: t -*-

(require 'eieio)

(defclass orm-assoc ()
  ((type :initarg :type)
   (class :initarg :class))
  :documentation
  "A class for associations of an relation-mapped class.")

(provide 'orm-assocs)
