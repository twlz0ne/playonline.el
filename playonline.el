;;; playonline.el --- Play code with online playgrounds -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/10/11
;; Version: 0.1.2
;; Package-Requires: ((emacs "24.4") (json "1.2") (dash "2.1") (request "0.2"))
;; URL: https://github.com/twlz0ne/playonline.el
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Play code with online playgrounds.
;; See README.md for more information.

;;; Change Log:

;;  0.1.0  2019/10/11  Initial version.
;;  0.1.1  2019/10/31  Add `playonline', delete `playonline{buffer,region,block}'.
;;  0.1.2  2019/11/29  Add request.el as an alternative http library.

;;; Code:

(require 'url-util)
(require 'url-http)
(require 'json)
(require 'dash)
(require 'request)

(defvar org-babel-src-block-regexp)
(declare-function org-element-at-point "org")
(declare-function org-element-property "org")
(declare-function markdown-code-block-lang "markdown-mode")
(declare-function markdown-get-lang-mode "markdown-mode")
(declare-function markdown-get-enclosing-fenced-block-construct "markdown-mode")

(defcustom playonline-buffer-name "*playonline*"
  "The name of the buffer to show output."
  :type 'string
  :group 'playonline)

(defcustom playonline-focus-p t
  "Whether to move focus to output buffer after `url-retrieve' responsed."
  :type 'boolean
  :group 'playonline)

(defcustom playonline-output-to-buffer-p t
  "Show output in buffer or not."
  :type 'boolean
  :group 'playonline)

;;; HTTP functions

(defconst playonline-http-send-fn
  (if (executable-find "curl")
      #'playonline-request-send
    #'playonline-url-send))

(cl-defun playonline-url-send (url &key (method "POST") headers data response-fn &allow-other-keys)
  (let* ((url-request-method method)
         (url-request-extra-headers headers)
         (url-request-data data)
         (content-buf (url-retrieve-synchronously url)))
    (playonline--handle-json-response content-buf
                                      response-fn)))

(cl-defun playonline-request-send (url &key (method "POST") headers data response-fn &allow-other-keys)
  (let ((response
         (request url
           :type method
           :headers headers
           :data data
           :sync t
           :parser #'buffer-string)))
    (let ((response-code (request-response-status-code response))
          (response-body (request-response-data response)))
      (if (= response-code 200)
          (funcall response-fn (json-read-from-string response-body))
        (error "Http response error: %s" response-body)))))

;;; playgrounds

(defconst playonline-ground-alist
  '((playonline-go-playground       . playonline-send-to-go-playground)
    (playonline-rust-playground     . playonline-send-to-rust-playground)
    (playonline-rextester-languages . playonline-send-to-rextester)
    (playonline-labstack-languages  . playonline-send-to-labstack)))

(defconst playonline-go-playground
  '((go-mode . (("go" . "Go")))))

(defconst playonline-rust-playground
  '((rust-mode . (("stable" . "Rust (stable)")))))

(defconst playonline-rextester-languages
  '((ada-mode          . (("39" . "Ada")))
    (bash-mode         . (("38" . "Bash")))
    (brainfuck-mode    . (("44" . "Brainfuck")))
    (c-mode            . (("26" . "C (clang)")
                          ("6"  . "C (gcc)")
                          ("29" . "C (vc)")))
    (c:gcc-mode        . (("6"  . "C (gcc)")))
    (c:clang-mode      . (("26" . "C (clang)")))
    (c:vc-mode         . (("29" . "C (vc)")))
    (lisp-mode         . (("18" . "Common Lisp")))
    (c++-mode          . (("27" . "C++ (clang)")
                          ("7"  . "C++ (gcc)")
                          ("28" . "C++ (vc++)")))
    (c++:clang-mode    . (("27" . "C++ (clang)")))
    (c++:gcc-mode      . (("7"  . "C++ (gcc)")))
    (c++:vc++-mode     . (("28" . "C++ (vc++)")))
    (csharp-mode       . (("1"  . "C#")))
    (d-mode            . (("30" . "D")))
    (elixir-mode       . (("41" . "Elixir")))
    (erlang-mode       . (("40" . "Erlang")))
    (fortran-mode      . (("45"  . "F#")))
    (fsharp-mode       . (("3" . "Fortran")))
    (go-mode           . (("20" . "Go")))
    (haskell-mode      . (("11" . "Haskell")))
    (java-mode         . (("4"  . "Java")))
    (javascript-mode   . (("17" . "Javascript")
                          ("23" . "Node.js")))
    (js-mode           . javascript-mode)
    (js2-mode          . javascript-mode)
    (js:node-mode      . (("23" . "Node.js")))
    (kotlin-mode       . (("43" . "Kotlin")))
    (lua-mode          . (("14" . "Lua")))
    (nasm-mode         . (("15" . "Assembly")))
    (objc-mode         . (("10" . "Objective-C")))
    (ocaml-mode        . (("42" . "Ocaml")))
    (octave-mode       . (("25" . "Octave")))
    (oracle-mode       . (("35" . "Oracle")))
    (pascal-mode       . (("9"  . "Pascal")))
    (perl-mode         . (("13" . "Perl")))
    (php-mode          . (("8"  . "Php")))
    (prolog-mode       . (("19" . "Prolog")))
    (python-mode       . (("24" . "Python 3")
                          ("5"  . "Python")))
    (python:3-mode     . (("24" . "Python 3")))
    (python:2-mode     . (("5"  . "Python")))
    (r-mode            . (("31" . "R")))
    (ruby-mode         . (("12" . "Ruby")))
    (scala-mode        . (("21" . "Scala")))
    (scheme-mode       . (("22" . "Scheme")))
    (sql-mode          . (("33" . "MySql")
                          ("34" . "PostgreSQL")
                          ("16" . "Sql Server")))
    (swift-mode        . (("37" . "Swift")))
    (tcl-mode          . (("32" . "Tcl")))
    (visual-basic-mode . (("2"  . "Visual Basic")))
    ))

(defconst playonline-rextester-compiler-args
  `((,(car (cadr (assoc 'c:gcc-mode     playonline-rextester-languages))) . "-Wall -std=gnu99 -O2 -o a.out source_file.c")
    (,(car (cadr (assoc 'c:clang-mode   playonline-rextester-languages))) . "-Wall -std=gnu99 -O2 -o a.out source_file.c")
    (,(car (cadr (assoc 'c:vc-mode      playonline-rextester-languages))) . "source_file.c -o a.exe")
    (,(car (cadr (assoc 'c++:gcc-mode   playonline-rextester-languages))) . "-Wall -std=c++14 -O2 -o a.out source_file.cpp")
    (,(car (cadr (assoc 'c++:clang-mode playonline-rextester-languages))) . "-Wall -std=c++14 -stdlib=libc++ -O2 -o a.out source_file.cpp")
    (,(car (cadr (assoc 'c++:vc++-mode  playonline-rextester-languages))) . "source_file.cpp -o a.exe /EHsc /MD /I C:\\boost_1_60_0 /link /LIBPATH:C:\\boost_1_60_0\\stage\\lib")
    (,(car (cadr (assoc 'd-mode         playonline-rextester-languages))) . "source_file.d -ofa.out")
    (,(car (cadr (assoc 'go-mode        playonline-rextester-languages))) . "-o a.out source_file.go")
    (,(car (cadr (assoc 'haskell-mode   playonline-rextester-languages))) . "-o a.out source_file.hs")
    (,(car (cadr (assoc 'objc-mode      playonline-rextester-languages))) . "-MMD -MP -DGNUSTEP -DGNUSTEP_BASE_LIBRARY=1 -DGNU_GUI_LIBRARY=1 -DGNU_RUNTIME=1 -DGNUSTEP_BASE_LIBRARY=1 -fno-strict-aliasing -fexceptions -fobjc-exceptions -D_NATIVE_OBJC_EXCEPTIONS -pthread -fPIC -Wall -DGSWARN -DGSDIAGNOSE -Wno-import -g -O2 -fgnu-runtime -fconstant-string-class=NSConstantString -I. -I /usr/include/GNUstep -I/usr/include/GNUstep -o a.out source_file.m -lobjc -lgnustep-base")
    ))

(defconst playonline-labstack-languages
  '((sh-mode           . (("bash"         . "Bash")))
    (c-mode            . (("c"            . "C")))
    (clojure-mode      . (("clojure"      . "Clojure")))
    (coffeescript-mode . (("coffeescript" . "CoffeeScript")))
    (c++-mode          . (("cpp"          . "C++")))
    (crystal-mode      . (("crystal"      . "Crystal")))
    (csharp-mode       . (("csharp"       . "C#")))
    (d-mode            . (("d"            . "D")))
    (dart-mode         . (("dart"         . "Dart")))
    (elixir-mode       . (("elixir"       . "Elixir")))
    (erlang-mode       . (("erlang"       . "Erlang")))
    (fsharp-mode       . (("fsharp"       . "F#")))
    (groovy-mode       . (("groovy"       . "Groovy")))
    (go-mode           . (("go"           . "Go")))
    (hack-mode         . (("hack"         . "Hack")))
    (haskell-mode      . (("haskell"      . "Haskell")))
    (java-mode         . (("java"         . "Java")))
    (javascript-mode   . (("javascript"   . "JavaScript")
                          ("node"         . "Node")))
    (js-mode           . javascript-mode)
    (js2-mode          . javascript-mode)
    (js:node-mode      . (("node"         . "Node")))
    (julia-mode        . (("julia"        . "Julia")))
    (kotlin-mode       . (("kotlin"       . "Kotlin")))
    (lua-mode          . (("lua"          . "Lua")))
    (nim-mode          . (("nim"          . "Nim")))
    (objc-mode         . (("objective-c"  . "Objective-C")))
    (ocaml-mode        . (("ocaml"        . "OCaml")))
    (octave-mode       . (("octave"       . "Octave")))
    (perl-mode         . (("perl"         . "Perl")))
    (php-mode          . (("php"          . "PHP")))
    (powershell-mode   . (("powershell"   . "PowerShell")))
    (python:3-mode     . (("python"       . "Python")))
    (ruby-mode         . (("ruby"         . "Ruby")))
    (r-mode            . (("r"            . "R")))
    (reason-mode       . (("reason"       . "Reason")))
    (rust-mode         . (("rust"         . "Rust")))
    (scala-mode        . (("scala"        . "Scala")))
    (swift-mode        . (("swift"        . "Swift")))
    (tcl-mode          . (("tcl"          . "TCL")))
    (typescript-mode   . (("typescript"   . "TypeScript")))
    ))

(defun playonline-send-to-go-playground (_ code)
  "Send CODE to `play.golang.org', return the execution result."
  (funcall playonline-http-send-fn "https://play.golang.org/compile"
           :headers
           '(("content-type"    . "application/x-www-form-urlencoded; charset=UTF-8")
             ("accept"          . "application/json, text/javascript, */*; q=0.01")
             ("accept-encoding" . "gzip"))
           :data
           (concat "version=2&body="
                   (url-hexify-string code)
                   "&withVet=true")
           :response-fn
           (lambda (resp)
             (let ((errors (assoc-default 'Errors resp)))
               (if (string= errors "")
                   (assoc-default 'Message (aref (assoc-default 'Events resp) 0))
                 errors)))))

(defun playonline-send-to-rust-playground (channel-id code)
  "Send CODE to `play.rust-lang.org', return the execution result."

  (funcall playonline-http-send-fn "https://play.rust-lang.org/execute"
           :headers
           '(("content-type"    . "application/json;charset=UTF-8")
             ("accept"          . "application/json")
             ("accept-encoding" . "gzip"))
           :data
           (encode-coding-string
            (json-encode-plist
             `(:channel ,channel-id
               :mode "debug"
               :edition "2018"
               :crateType "bin"
               :tests :json-false
               :code ,code
               :backtrace :json-false))
            'utf-8)
           :response-fn
           (lambda (resp)
             (if (eq :json-false (assoc-default 'success resp))
                 (assoc-default 'stderr resp)
               (assoc-default 'stdout resp)))))

(defun playonline-send-to-rextester (lang-id code)
  "Send CODE to `rextester.com', return the execution result.
LANG-ID to specific the language."
  (funcall playonline-http-send-fn "https://rextester.com/rundotnet/run"
           :headers
           '(("content-type"    . "application/x-www-form-urlencoded; charset=UTF-8")
             ("accept"          . "text/plain, */*; q=0.01")
             ("accept-encoding" . "gzip"))
           :data
           (let ((compiler-args (assoc-default lang-id playonline-rextester-compiler-args)))
             (concat "LanguageChoiceWrapper="
                     lang-id
                     "&EditorChoiceWrapper=1&LayoutChoiceWrapper=1&Program="
                     (url-hexify-string code)
                     "&CompilerArgs="
                     (when compiler-args (url-hexify-string compiler-args))
                     "&IsInEditMode=False&IsLive=False"))
           :response-fn
           (lambda (resp)
             (let* ((warnings (assoc-default 'Warnings resp))
                    (errors (assoc-default 'Errors resp))
                    (result (assoc-default 'Result resp)))
               (concat (unless (eq warnings :null) warnings)
                       (unless (eq errors :null) errors)
                       (unless (eq result :null) result))))))

(defun playonline-send-to-labstack (lang-id code)
  "Send CODE to `code.labstack.com', return the execution result.
LANG-ID to specific the language."
  (funcall playonline-http-send-fn "https://code.labstack.com/api/v1/run"
           :headers
           '(("content-type"    . "application/json;charset=UTF-8")
             ("accept"          . "application/json, text/plain, */*")
             ("accept-encoding" . "gzip"))
           :data
           (let ((name (capitalize lang-id)))
             (encode-coding-string
              (json-encode-plist
               `(:notes ""
                 :language (:id ,(format "%s" lang-id)
                            :name ,(format "%s" name)
                            :version ""
                            :code ,code
                            :text ,(format "%s (<version>)" name))
                 :content ,code))
              'utf-8))
           :response-fn
           (lambda (resp)
             (if (assoc-default 'code resp)
                 (assoc-default 'message resp)
               (concat (assoc-default 'stdout resp)
                       (assoc-default 'stderr resp))))))

;;; Wrapper

(defconst playonline-main-wrap-functions
  '((go-mode        . playonline--go-ensure-main-wrap)
    (c-mode         . playonline--c-ensure-main-wrap)
    (c:gcc-mode     . playonline--c-ensure-main-wrap)
    (c:clang-mode   . playonline--c-ensure-main-wrap)
    (c:vc-mode      . playonline--c-ensure-main-wrap)
    (c++-mode       . playonline--cpp-ensure-main-wrap)
    (c++:gcc-mode   . playonline--cpp-ensure-main-wrap)
    (c++:clang-mode . playonline--cpp-ensure-main-wrap)
    (c++:vc++-mode  . playonline--cpp-ensure-main-wrap)
    (csharp-mode    . playonline--csharp-ensure-main-wrap)
    (d-mode         . playonline--d-ensure-main-wrap)
    (objc-mode      . playonline--objc-ensure-main-wrap)
    (rust-mode      . playonline--rust-ensure-main-wrap)
    ))

(defun playonline--c-ensure-main-wrap (body)
  "Wrap c BODY in 'main' function if necessary."
  (if (string-match-p "^[ \t]*int[ \n\t]*main *(" body)
      body
    (concat "#include <stdio.h>\n"
            "int main() {\n"
            body
            "\nreturn 0;"
            "\n}\n")))

(defun playonline--cpp-ensure-main-wrap (body)
  "Wrap cpp BODY in 'main' function if necessary."
  (if (string-match-p "^[ \t]*int[ \n\t]*main *(" body)
      body
    (concat "#include <iostream>\n"
            "int main() {\n"
            body
            "\nreturn 0;"
            "\n}\n")))

(defun playonline--csharp-ensure-main-wrap (body)
  "Wrap csharp BODY in 'main' function if necessary."
  (if (string-match-p "^[ \t]*public static void Main *(" body)
      body
    (concat "using System;\n"
            "public class Code\n"
            "{\npublic static void Main(string[] args)\n{\n" body "\n}\n}\n")))

(defun playonline--d-ensure-main-wrap (body)
  "Wrap d BODY in 'main' function if necessary."
  (if (string-match-p "^[ \t]*void main *()" body)
      body
    (concat "import std.stdio;\n"
            "void main() {\n" body "\n}\n")))

(defun playonline--go-ensure-main-wrap (body)
  "Wrap go BODY in 'main' function if necessary."
  (if (string-match-p "^[ \t]*func main *() *{" body)
      body
    (concat "package main\n"
            (when (string-match-p "^[ \t]*fmt\." body)
              "import \"fmt\"\n")
            "func main() {\n" body "\n}\n")))

(defun playonline--objc-ensure-main-wrap (body)
  "Wrap objc BODY in 'main' function if necessary."
  (if (string-match-p "^[ \t]*int main *(" body)
      body
    (concat "#import <Foundation/Foundation.h>\n"
            "int main (int argc, const char * argv[])\n"
            "{\n"
            body
            "\nreturn 0;"
            "\n}\n")))

(defun playonline--rust-ensure-main-wrap (body)
  "Wrap rust BODY in 'main' function if necessary."
  (if (string-match-p "^[ \t]*fn main *(" body)
      body
    (concat "fn main() {\n" body "\n}\n")))

;;;

(defmacro playonline--fbound-and-true-p (fun)
  "Return the symbol FUN if it is bound, else nil."
  `(and (fboundp ,fun) ,fun))

(defun playonline--pop-to-buffer (buf)
  "Display buffer specified by BUF and select its window."
  (let ((win (selected-window)))
    (pop-to-buffer buf)
    (unless playonline-focus-p
      (select-window win))))

(defun playonline--handle-json-response (url-content-buf callback)
  "Handle json response in URL-CONTENT-BUF.
Function CALLBACK accept an alist, and return output string."
  (with-current-buffer url-content-buf
    (let ((http-code (save-excursion (url-http-parse-response)))
          (http-body (save-excursion (goto-char (point-min))
                                     (re-search-forward "\n\n")
                                     (buffer-substring (point) (point-max)))))
      ;; (message "==> http code:\n%s" http-code)
      ;; (message "==> http body:\n%s" http-body)
      (pcase http-code
        (200 (let* ((resp (json-read-from-string http-body))
                    (output (funcall callback resp))
                    (output-buf (get-buffer-create playonline-buffer-name)))
               (cond (playonline-output-to-buffer-p
                      (with-current-buffer output-buf
                        (read-only-mode -1)
                        (erase-buffer)
                        (insert output)
                        (read-only-mode 1)
                        (playonline--pop-to-buffer output-buf)))
                     (t output))))
        (_ (error http-body))))))

(defun playonline--get-shebang-command ()
  "Get shabang program."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "#![^ \t]* \\(.*\\)$")
      (match-string-no-properties 1))))

(defun playonline--nodejs-p ()
  "Detect require / exports statment in buffer."
  (save-excursion
    (goto-char (point-min))
    (and
     (or
      (re-search-forward
       "^[ \t]*\\(var\\|const\\)[ \t]*[[:word:]]+[ \t]*=[ \t]*require[ \t]*("
       nil t 1)
      (re-search-forward
       "^[ \t]*\\(module\\.\\|\\)[ \t]*exports[ \t]*="
       nil t 1))
     t)))

(defun playonline--get-mode-alias (&optional mode)
  "Return alias of MODE.
The alias naming in the form of `foo:<specifier>-mode' to
opposite a certain version of lang in `playonline-xxx-languags'."
  (let ((mode (or mode major-mode)))
    (pcase mode
      (`python-mode
       (pcase (list (playonline--get-shebang-command)
                    (file-name-extension (or (buffer-file-name) (buffer-name))))
         ((or `("python3" ,_) `(,_ "py3")) 'python:3-mode)
         ((or `("python2" ,_) `(,_ "py2")) 'python:2-mode)
         (_ mode)))
      ((or `js-mode `js2-mode `javascript-mode)
       (if (playonline--nodejs-p) 'js:node-mode mode))
      (_ mode))))

(defun playonline--get-ground (mode)
  "Return (((lang-id . lang-name) ...) . send-function) for MODE."
  (catch 'break
    (mapc
     (lambda (ground)
       (let ((lang (assoc mode (symbol-value (car ground)))))
         (let ((symbol (cdr lang)))
           (when (symbolp symbol)
             (setq lang (assoc symbol (symbol-value (car ground))))))
         (when lang
           (throw 'break (cons (cdr lang) (cdr ground))))))
     playonline-ground-alist)
    nil))

(defun playonline--get-lang-and-function (mode)
  "Return (lang sender wrapper) for MODE."
  (pcase-let*
      ((`(,langs . ,sender) (playonline--get-ground mode))
       (`(,lang . ,_desc)
        (cond ((> (length langs) 1)
               (rassoc
                (completing-read "Choose: " (mapcar (lambda (it) (cdr it)) langs))
                langs))
              (t (car langs))))
       (`(,_mode . ,wrapper) (assoc mode playonline-main-wrap-functions)))
    ;; (message "==> lang: %s, sender: %s, wrapper: %s" lang sender wrapper)
    (when (or (not lang)
              (not sender))
      (error "No lang-id or sender found"))
    (list lang sender wrapper)))

(defun playonline-orgmode-src-block (&optional beg end)
  "Return orgmode src block in the form of (mode code bounds)."
  (require 'org)
  (-if-let* ((src-element (org-element-at-point)))
      (list (funcall (or (playonline--fbound-and-true-p 'org-src--get-lang-mode)
                         (playonline--fbound-and-true-p 'org-src-get-lang-mode))
                     (org-element-property :language src-element))
            (if (region-active-p)
                (buffer-substring-no-properties beg end)
              (org-element-property :value src-element))
            (save-excursion
              (goto-char (plist-get (cadr src-element) :begin))
              (looking-at org-babel-src-block-regexp)
              (list (match-beginning 5) (match-end 5))))))

(defun playonline-markdown-src-block (&optional beg end)
  "Return markdown src block in the form of (mode code bounds)."
  (require 'markdown-mode)
  (save-excursion
    (-if-let* ((lang (markdown-code-block-lang))
               (bounds (markdown-get-enclosing-fenced-block-construct))
               (beg (if (region-active-p)
                        beg
                      (goto-char (nth 0 bounds)) (point-at-bol 2)))
               (end (if (region-active-p)
                        end
                      (goto-char (nth 1 bounds)) (point-at-bol 1))))
        (list (markdown-get-lang-mode lang)
              (buffer-substring-no-properties beg end)
              (list beg end)))))

;;;

;;;###autoload
(defun playonline (&optional beg end)
  "Play code online.

This function can be applied to:
- buffer
- region
- block (or region in block) ;; require org-mode / markdown
"
  (interactive "r")
  (pcase-let*
      ((`(,mode ,code ,bounds)
        (pcase major-mode
          (`org-mode (playonline-orgmode-src-block beg end))
          (`markdown-mode (playonline-markdown-src-block beg end))
          (_ (list major-mode
                   (if (region-active-p)
                       (buffer-substring-no-properties beg end)
                     (buffer-substring-no-properties (point-min) (point-max)))
                   nil))))
       (`(,lang ,sender ,wrapper)
        (playonline--get-lang-and-function
         (save-restriction
           (when bounds
             (apply 'narrow-to-region bounds))
           (playonline--get-mode-alias mode)))))
    (funcall sender lang (if wrapper
                             (funcall wrapper code)
                           code))))

(provide 'playonline)

;;; playonline.el ends here
