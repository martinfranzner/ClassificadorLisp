(defun texto (file)
  (with-open-file (in file)
  (read-from-string (read-line file))
  )
)

; (get-file "/Users/martinfranzner/Documents/PUC_COMPUTACAO/5oSemestre/ProgramacaoFuncional/codes/teste.txt")
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (read-from-string(line)))
    )
)