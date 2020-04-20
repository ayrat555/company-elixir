
(require 'ansi-color)


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

(defvar process)

(defvar evaluator-init-code "
  evaluator = IEx.Server.start_evaluator([])
  Process.put(:evaluator, evaluator)

  defmodule CompanyElixirServer do
    def evaluator do
      {Process.get(:evaluator), self()}
    end

    def expand(expr) do
      case IEx.Autocomplete.expand(Enum.reverse(expr), __MODULE__) do
        {:yes, _, result} -> result
        _ -> :not_found
      end
    end
  end

  \n
")

(defun start-iex ()
  "Start iex."
  (let* ((process-name "iex")
         (default-directory (project-root))
         (server-command "iex -S mix"))
    (setq process (start-process-shell-command process-name "*iex*" server-command))
    (set-process-filter process #'company-filter)
    (set-process-query-on-exit-flag process nil)
    (process-send-string process "\n")
    (process-send-string process "Logger.remove_backend(:console)\n")
    (process-send-string process evaluator-init-code)
    (set-process-filter process #'company-filter)))

(defun company-filter (_process output)
  (let* ((output-without-ansi-chars (ansi-color-apply output))
         (output-without-props (set-text-properties 0 (length output-without-ansi-chars) nil output-without-ansi-chars)))
    (print output-without-ansi-chars)))


(process-send-string process "CompanyElixirServer.expand('String.re')\n")
  ;; (if (not (string-match-p "warn\\|debug\\|info\\|error" output))
;;     (print output)))


;;   setup do
;;     evaluator = IEx.Server.start_evaluator([])
;;     Process.put(:evaluator, evaluator)
;;     :ok
;;   end

;;   defmodule MyServer do
;;     def evaluator do
;;       {Process.get(:evaluator), self()}
;;     end
;;   end

;;   defp eval(line) do
;;     ExUnit.CaptureIO.capture_io(fn ->
;;       {evaluator, _} = MyServer.evaluator()
;;       Process.group_leader(evaluator, Process.group_leader())
;;       send(evaluator, {:eval, self(), line <> "\n", %IEx.State{}})
;;       assert_receive {:evaled, _, _}
;;     end)
;;   end

;;   defp expand(expr) do
;;     IEx.Autocomplete.expand(Enum.reverse(expr), MyServer)
;;   end

;; (process-send-string process "String.")
