;;; orm.el --- An object relational mapping for emacs lisp
;; -*- coding: utf-8; lexical-binding: t; -*-

(provide 'orm)

(cl-eval-when (load eval)
  (require 'orm-class)
  (require 'orm-column)
  (require 'orm-db-connection)
  (require 'orm-macros))
