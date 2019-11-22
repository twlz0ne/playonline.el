;;; play-code-test.el --- Test play-code.el -*- lexical-binding: t; -*-

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

(require 'ert)
(require 'markdown-mode)
(require 'play-code)
(require 'play-code-test-helper)

;; Set to nil to print more detail but sometimes it becomes a disturbance.
;; (setq ert-batch-backtrace-right-margin nil)

(when noninteractive
  (transient-mark-mode))

(cl-defun --with-code-buffer
    (&key ground code mode choose expect expect-fn &allow-other-keys)
  (let ((play-code-output-to-buffer-p nil)
        (play-code-ground-alist (or ground play-code-ground-alist))
        (buf (generate-new-buffer "*test*")))
    (with-current-buffer buf
      (unless (fboundp mode)
        (eval `(define-derived-mode ,mode prog-mode "Dummy Mode" "For Test")))
      (if choose
          (progn
            (eval `(define-derived-mode ,choose ,mode "Dummy Mode" "For Test"))
            (funcall choose))
        (funcall mode))
      (insert code)
      (if expect
          (should (string= expect (play-code)))
        (if expect-fn
            (error "Not implement!"))))))

(defmacro --with-code-block (mode content &rest body)
  `(let ((buf (generate-new-buffer "*test*")))
    (with-current-buffer buf
      (funcall ,mode)
      (insert ,content)
      (goto-char (point-min))
      (when (re-search-forward "\<\>" nil t 1)
        (replace-match ""))
      (let ((noninteractive nil))
        (font-lock-mode 1)
        (font-lock-set-defaults)
        (jit-lock-fontify-now (point-min) (point-max)))
      ,@body)))

(defun --concat (&rest lines)
  (mapconcat 'identity lines "\n"))

;;; Wrapper

(ert-deftest play-code-test-c-wrapper ()
  (let* ((wrapper (assoc-default 'c-mode play-code-main-wrap-functions))
         (unwrapped-code "printf(\"Hello, C!\");")
         (wrapped-code (--concat
                        "#include <stdio.h>"
                        "int main() {"
                        unwrapped-code
                        "return 0;"
                        "}\n")))
    (should (string= wrapped-code (funcall wrapper unwrapped-code)))
    (should (string= wrapped-code (funcall wrapper wrapped-code)))))

(ert-deftest play-code-test-cpp-wrapper ()
  (let* ((wrapper (assoc-default 'c++-mode play-code-main-wrap-functions))
         (unwrapped-code "std::cout << \"Hello, C++!\";")
         (wrapped-code (--concat
                        "#include <iostream>"
                        "int main() {"
                        unwrapped-code
                        "return 0;"
                        "}\n")))
    (should (string= wrapped-code (funcall wrapper unwrapped-code)))
    (should (string= wrapped-code (funcall wrapper wrapped-code)))))

(ert-deftest play-code-test-csharp-wrapper ()
  (let* ((wrapper (assoc-default 'csharp-mode play-code-main-wrap-functions))
         (unwrapped-code "Console.WriteLine(\"Hello, C#!\");")
         (wrapped-code (--concat
                        "using System;"
                        "public class Code"
                        "{\npublic static void Main(string[] args)\n{"
                        unwrapped-code
                        "}\n}\n")))
    (should (string= wrapped-code (funcall wrapper unwrapped-code)))
    (should (string= wrapped-code (funcall wrapper wrapped-code)))))

(ert-deftest play-code-test-d-wrapper ()
  (let* ((wrapper (assoc-default 'd-mode play-code-main-wrap-functions))
         (unwrapped-code "writeln(\"Hello, D!\");")
         (wrapped-code (--concat
                        "import std.stdio;"
                        "void main() {"
                        unwrapped-code
                        "}\n")))
    (should (string= wrapped-code (funcall wrapper unwrapped-code)))
    (should (string= wrapped-code (funcall wrapper wrapped-code)))))

(ert-deftest play-code-test-go-wrapper ()
  (let* ((wrapper (assoc-default 'go-mode play-code-main-wrap-functions))
         (unwrapped-code "fmt.Println(\"Hello, Go!\")")
         (wrapped-code (--concat "package main"
                                 "import \"fmt\""
                                 "func main() {"
                                 unwrapped-code
                                 "}\n")))
    (should (string= wrapped-code (funcall wrapper unwrapped-code)))
    (should (string= wrapped-code (funcall wrapper wrapped-code)))))

(ert-deftest play-code-test-objc-wrapper ()
  (let* ((wrapper (assoc-default 'objc-mode play-code-main-wrap-functions))
         (unwrapped-code "NSLog (@\"Hello, Objc!\");")
         (wrapped-code (--concat "#import <Foundation/Foundation.h>"
                                 "int main (int argc, const char * argv[])"
                                 "{"
                                 unwrapped-code
                                 "return 0;"
                                 "}\n")))
    (should (string= wrapped-code (funcall wrapper unwrapped-code)))
    (should (string= wrapped-code (funcall wrapper wrapped-code)))))

(ert-deftest play-code-test-rust-wrapper ()
  (let* ((wrapper (assoc-default 'rust-mode play-code-main-wrap-functions))
         (unwrapped-code "println!(\"Hello, Rust!\")")
         (wrapped-code (--concat "fn main() {"
                                 unwrapped-code
                                 "}\n")))
    (should (string= wrapped-code (funcall wrapper unwrapped-code)))
    (should (string= wrapped-code (funcall wrapper wrapped-code)))))

;;; Send code directly

(ert-deftest play-code-test-c-code/labstack ()
  (--with-code-buffer
   :ground '((play-code-labstack-languages . play-code-send-to-labstack))
   :mode   'c-mode
   :code   "printf(\"Hello, C!\\n\");"
   :expect "Hello, C!\n"))

(ert-deftest play-code-test-cpp-code/labstack ()
  (--with-code-buffer
   :ground '((play-code-labstack-languages . play-code-send-to-labstack))
   :mode   'c++-mode
   :code   "std::cout << \"Hello, CPP(Gcc)\\n\";"
   :expect "Hello, CPP(Gcc)\n"
   ))

(ert-deftest play-code-test-c@clang-code ()
  (--with-code-buffer
   :ground '((play-code-rextester-languages . play-code-send-to-rextester))
   :mode   'c-mode
   :choose 'c:clang-mode
   :code   "printf(\"Hello, C(Clang)!\\n\");"
   :expect "Hello, C(Clang)!\n"))

(ert-deftest play-code-test-c@gcc-code ()
(--with-code-buffer
   :ground '((play-code-rextester-languages . play-code-send-to-rextester))
   :mode   'c-mode
   :choose 'c:gcc-mode
   :code   "printf(\"Hello, C(Gcc)!\\n\");"
   :expect "Hello, C(Gcc)!\n"))

(ert-deftest play-code-test-c@vc-code ()
  (--with-code-buffer
   :ground '((play-code-rextester-languages . play-code-send-to-rextester))
   :mode   'c-mode
   :choose 'c:vc-mode
   :code   "printf(\"Hello, C(VC)!\\n\");"
   :expect "Hello, C(VC)!\r\n"))

(ert-deftest play-code-test-cpp@gcc-code ()
  (--with-code-buffer
   :ground '((play-code-rextester-languages . play-code-send-to-rextester))
   :mode   'c++-mode
   :choose 'c++:gcc-mode
   :code   "std::cout << \"Hello, C++(GCC)!\\n\";"
   :expect "Hello, C++(GCC)!\n"))

(ert-deftest play-code-test-cpp@clang-code ()
  (--with-code-buffer
   :ground '((play-code-rextester-languages . play-code-send-to-rextester))
   :mode   'c++-mode
   :choose 'c++:clang-mode
   :code   "std::cout << \"Hello, C++(Clang)!\\n\";"
   :expect "Hello, C++(Clang)!\n"))

(ert-deftest play-code-test-cpp@vcpp-code ()
  (--with-code-buffer
   :ground '((play-code-rextester-languages . play-code-send-to-rextester))
   :mode   'c++-mode
   :choose 'c++:vc++-mode
   :code   "std::cout << \"Hello, C++(VC++)!\\n\";"
   :expect "Hello, C++(VC++)!\r\n"))

(ert-deftest play-code-test-go-code ()
  ;; golang.org
  (--with-code-buffer
   :mode 'go-mode
   :code "fmt.Println(\"Hello, Go\")"
   :expect "Hello, Go\n")
  ;; rextester
  (--with-code-buffer
   :ground '((play-code-rextester-languages . play-code-send-to-rextester))
   :mode 'go-mode
   :code "fmt.Println(\"Hello, Go\")"
   :expect "Hello, Go\n")
  ;; labstack
  (--with-code-buffer
   :ground '((play-code-labstack-languages . play-code-send-to-labstack))
   :mode 'go-mode
   :code "fmt.Println(\"Hello, Go\")"
   :expect "Hello, Go\n"))

(ert-deftest play-code-test-python3-code ()
  (--with-code-buffer
   :ground '((play-code-labstack-languages . play-code-send-to-labstack))
   :mode 'python-mode
   :choose 'python:3-mode
   :code "print('Hello, Python3')"
   :expect "Hello, Python3\n"))

(ert-deftest play-code-test-rust-code ()
  (let ((play-code-output-to-buffer-p nil)
        (code (play-code--rust-ensure-main-wrap "println!(\"Hello, Rust\");")))
    (pcase-let ((`(,lang ,func) (play-code--get-lang-and-function 'rust-mode)))
      (should (eq func 'play-code-send-to-rust-playground))
      (should (string= "Hello, Rust\n"
                       (funcall func lang code))))
    (let ((play-code-ground-alist '((play-code-labstack-languages . play-code-send-to-labstack))))
      (pcase-let ((`(,lang ,func) (play-code--get-lang-and-function 'rust-mode)))
        (should (eq func 'play-code-send-to-labstack))
        (should (string= "Hello, Rust\n"
                         (funcall func lang code)))))))

;;; Play code in block

(ert-deftest play-code-test-python3-code-in-markdown ()
  (let ((play-code-output-to-buffer-p nil))
    (--with-code-block
     'markdown-mode
     "```python
#!/usr/bin/env python3
print('hello, python3')<>
```"
     (should (string= "hello, python3\n" (play-code))))))

(ert-deftest play-code-test-rust-code-in-orgmode ()
  (let ((play-code-output-to-buffer-p nil))
    (--with-code-block
     'org-mode
     "#+BEGIN_SRC rust
fn main () { println!(\"hello, rust\"); }<>
#+END_SRC"
     (should (string= "hello, rust\n" (play-code))))))

(provide 'play-code-test)

;;; play-code-test.el ends here
