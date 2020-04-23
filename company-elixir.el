;;; Code:

(require 'ansi-color)

(defgroup company-elixir nil
  "Company-Elixir group."
  :prefix "company-elixir-"
  :group 'company-elixir)

(defcustom company-elixir-iex-command "iex -S mix"
  "Command used to start iex."
  :type 'string
  :group 'company-elixir)

(defun company-elixir--project-root ()
  "Find the root of the current mix project."
  (let ((closest-path (or buffer-file-name default-directory)))
    (if (string-match-p (regexp-quote "apps") closest-path)
        (let* ((potential-umbrella-root-parts (butlast (split-string closest-path "/apps/")))
               (potential-umbrella-root (mapconcat #'identity potential-umbrella-root-parts ""))
               (umbrella-app-root (mix--find-closest-mix-file-dir potential-umbrella-root)))
          (or umbrella-app-root (mix--find-closest-mix-file-dir closest-path)))
      (find-closest-mix-file-dir closest-path))))

(defun company-elixir--find-closest-mix-file-dir (path)
  "Find the closest mix file to the current buffer PATH."
    (let ((root (locate-dominating-file path "mix.exs")))
    (when root
      (file-truename root))))

(defun company-elixir--read-init-code ()
  "Read iex evaluator script and return it as string."
  (with-temp-buffer
    (insert-file-contents "./company_elixir_script.exs")
    (buffer-string)))

(defvar company-elixir--evaluator-init-code nil "Iex evaluator script code.")

(defvar company-elixir--process nil "Iex process.")

(defun company-elixir--init-code()
  "Read iex evaluator if it's not read or return if it's read."
  (or company-elixir--evaluator-init-code
      (setq company-elixir--evaluator-init-code (company-elixir--read-init-code))))

(defun company-elixir--start-iex-process ()
  "Start iex process."
  (let* ((process-name "iex")
         (default-directory (company-elixir--project-root)))
    (setq company-elixir--process (start-process-shell-command process-name "*iex*" company-elixir-iex-command))
    (set-process-query-on-exit-flag company-elixir--process nil)
    (process-send-string company-elixir--process company-elixir--evaluator-init-code)
    (set-process-filter company-elixir--process #'company-elixir--company-filter)))

(defun company-elixir--company-filter (_process output)
  "Filter OUTPUT from iex process and redirect them to company."
  (let ((output-without-ansi-chars (ansi-color-apply output)))
    (set-text-properties 0 (length output-without-ansi-chars) nil output-without-ansi-chars)
    (print output-without-ansi-chars)))

;; (process-send-string company-elixir--process "CompanyElixirServer.expand('String.')\n")
