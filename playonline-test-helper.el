;;; playonline-test.el --- Helper for testing playonline.el -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; Redefine `playonline' to apply proxy.
;;
;; Example of `proxy-conf.el':
;;
;;   (setq with-proxy-http-server "127.0.0.1:7890")
;;   (setq with-proxy-senders '(playonline-send-to-go-playground))
;;
(when (load "proxy-conf.el" t)
  (require 'with-proxy)
  (with-eval-after-load 'playonline
    (dolist (sym with-proxy-senders)
      (let* ((orig-fn (symbol-function sym))
             (wrapper (lambda (&rest args)
                        (with-proxy
                            (apply orig-fn args)))))
        (fset sym wrapper)))))

(provide 'playonline-test-helper)

;;; playonline-test-helper.el ends here
