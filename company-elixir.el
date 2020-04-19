
(defun project-root ()
  "Find the root of the current mix project."
  (let ((closest-path (or buffer-file-name default-directory)))
    (if (string-match-p (regexp-quote "apps") closest-path)
        (let* ((potential-umbrella-root-parts (butlast (split-string closest-path "/apps/")))
               (potential-umbrella-root (mapconcat #'identity potential-umbrella-root-parts ""))
               (umbrella-app-root (mix--find-closest-mix-file-dir potential-umbrella-root)))
          (or umbrella-app-root (mix--find-closest-mix-file-dir closest-path)))
      (find-closest-mix-file-dir closest-path))))

(defun find-closest-mix-file-dir (path)
  "Find the closest mix file to the current buffer PATH."
    (let ((root (locate-dominating-file path "mix.exs")))
    (when root
      (file-truename root))))

(defun start-iex ()
  "Start iex."
  (let* ((process-name "iex")
         (default-directory (project-root))
         (server-command "iex -S mix")
         (process (start-process-shell-command process-name "*iex*" server-command)))
    (set-process-filter process #'company-filter)
    (set-process-query-on-exit-flag process nil)
    (process-send-string process "2 + 2\n")))

(defun company-filter (_process output)
  (print output))
