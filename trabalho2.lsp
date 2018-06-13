;; Nomes: Eduardo Borsa, Martin, Antonio
;------------------Trabalho 2 de Lisp
; Execute os comandos a baixo
; (load "./trabalho2.lsp")
; (cria-classificador 'diabetes "./arquivoTesteTrab2.txt")
; (diabetes 6 148 72 35 0 33.6 0.627 50)
; (diabetes 1 85 66 29 0 26.6 0.351 31)
; (classificaCSV 'diabetes "./entrada.csv" "./saida.csv")



(defun cria-classificador (nomeDoClassificador arquivo)
  (let ( (arquivoCarregado (get-file arquivo)) )
    (setq argumentos (removeUltimoElemnto (string-to-list (car arquivoCarregado))))
    (setq corpo (cons 'COND (trataRegrasERetornaCorpo (cdr arquivoCarregado))))
    ;;esse eval serve pra criar a funcao e a lista eh a funcao
    (eval (list 'defun nomeDoClassificador argumentos corpo 'CLASS))
  )
)

(defun classificaCSV (classificador entrada saida)
  (let ( (entrada (get-file entrada)) (resultadosAtehEntao "") )
    (loop
      ;;Funcao com argumento
      (print (colocaNaPrimeiraPosicao (list classificador) (string-to-list (removeVirgulas (car entrada)))))
      (print (eval (colocaNaPrimeiraPosicao (list classificador) (string-to-list (removeVirgulas (car entrada))))))
      (setq resultadosAtehEntao 
        (concatenate 'string resultadosAtehEntao 
                (concatenate 
                  'string 
                  (car entrada)
                  ","
                  (write-to-string 
                    (eval (colocaNaPrimeiraPosicao (list classificador) (string-to-list (removeVirgulas (car entrada)))))
                  )
                  (colocaBarraN ",")
                )
        )
      )
      ;; Consome arquivoCarregado ateh virar null e sai
      (setq entrada (cdr entrada))
      (if (null entrada)
        (return resultadosAtehEntao)
      )
    )
    (writeToFile saida resultadosAtehEntao)
  )
)

(defun trataRegrasERetornaCorpo (arquivoCarregado)
  ;;Leia De DENTRO pra FORA
  (let ( (listaDeSaida) )
    (loop
      (setq listaDeSaida (append listaDeSaida 
                          (list 
                            (transformaEmExpressaoLisp 
                                (removeEqualsSymbol 
                                  (removeTodosOsANDsEColocaNoComeco                               ;; AQUI!!!!
                                    (removeUltimoElemnto (string-to-list (car arquivoCarregado))) ;; <------
                                  )
                                )
                              )
                          )
                        )
      )
      ;; Consome arquivoCarregado ateh virar null e sai
      (setq arquivoCarregado (cdr arquivoCarregado))
      (if (null arquivoCarregado)
        (return listaDeSaida)
      )
    )
    ;;----------------------------------------------------
    ;; Porque a Ultima Regra foge do Padrao, Trato Ela aqui
    (setq listaDeSaida (arrumaUltimoElemento listaDeSaida))
  )
)

(defun arrumaUltimoElemento (listaDeSaida)
  (setq novoUltimo (cons 'T (list (cadr (car (last listaDeSaida))))))
  (setq listaDeSaida (removeUltimoElemnto listaDeSaida))
  (setq listaDeSaida (append listaDeSaida (list novoUltimo)))
)

(defun transformaEmExpressaoLisp (condicoesDeCadaRegra)
  (let ( (resposta) )
    (dolist (x condicoesDeCadaRegra resposta)
      (if (listp x)
        (if (numberp (cadr (cdr x)))
          ;; Esse server pra transformar as condicoes
          (setq resposta (append resposta (list (list (cadr x) (car x) (cadr (cdr x))))))
          ;; Esse Else serve pra implicacao das Regras
          (if (equal (car x) 'SETQ)
            (progn
              (setq resposta (list resposta))
              (setq conclusaoDoExame (colocaQuoteNaFrente x))
              (setq conclusaoArrumada (list (car x) (cadr x) (car conclusaoDoExame )))
              (setq resposta ( append resposta (list conclusaoArrumada)))
            )
          )
        )
        ;Esse Else serve pro AND do comeco
        (setq resposta (append resposta (list x)))
      )
    )
  )
)

(defun removeEqualsSymbol (linhaComoLista)
  ;;Leia de DENTRO para Fora
  (let ( (resposta (removeUltimoElemnto linhaComoLista)) )
    (setq linhaQuasePronta 
      (append  '(setq) 
                (string-to-list 
                  (removeIguais                                   ; AQUI!!!!
                    (write-to-string (car (last linhaComoLista))) ; <----------
                  )
                )
      )
    )
    (setq resposta (append resposta (list linhaQuasePronta)))
  )
)

(defun removeTodosOsANDsEColocaNoComeco (linha)
  (let ( (resposta1) (resposta2) )
    ;; tira todos os AND
    (dolist (x linha )
      (if (not (equal x 'AND))
        (setq resposta1 ( append resposta1 (list x)))
      )
    )
    ;; Se n for a ultma regra coloca And na Frente
    (if (not (equal (car resposta1) '=>))
      (setq resposta1 (append '(AND) resposta1))
    )
    ;; Retira todos os '=>'
    (dolist (x resposta1 resposta2)
      (if (not (equal x '=>))
        (setq resposta2 (append resposta2 (list x)))
      )
    )
  )
)

(defun colocaQuoteNaFrente (elementoDaLista)
  (setq conclusaoDoExame (cadr (cdr elementoDaLista)))
  (setq conclusaoDoExame (write-to-string conclusaoDoExame ))
  (setq conclusaoDoExame (concatenate 'string "'" conclusaoDoExame ))
  (setq conclusaoDoExame (string-to-list conclusaoDoExame ))
)

(defun removeUltimoElemnto (lista)
  (let ( (tamanhoLista (length lista)) (resposta) (counter 1) )
    (dolist (x lista resposta)
      (if (not (equal counter tamanhoLista))
        (setq resposta (append resposta (list x)))
      )
      (setq counter (+ counter 1))
    )
  )
)

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
      while line
      collect line
    )
  )
)

(defun string-to-list (str)
  (if (not (streamp str))
     (string-to-list (make-string-input-stream str))
     (if (listen str)
       (cons (read str) (string-to-list str))
       nil
     )
  )
)

(defun trataSimboloIgual (char) 
  (let ( )
    (setq blop (char-code (code-char  (char-code char))))
    (if (not (= blop 61))
      (code-char  (char-code char))
      (code-char (char-code #\Space))
    )
  )
)
(defun removeIguais (string) (map 'string #'trataSimboloIgual string))

(defun trataVirgulas (char) 
  (let ( )
    (setq blop (char-code (code-char  (char-code char))))
    (if (not (= blop 44))
      (code-char  (char-code char))
      (code-char (char-code #\Space))
    )
  )
)

(defun removeVirgulas (string) (map 'string #'trataVirgulas string))

(defun trataVirgulaPorBarraN (char) 
  (let ( )
    (setq blop (char-code (code-char  (char-code char))))
    (if (not (= blop 44))
      (code-char  (char-code char))
      (code-char (char-code #\linefeed))
    )
  )
)

(defun colocaBarraN (string) (map 'string #'trataVirgulaPorBarraN string))

(defun trataLinhaDoArquivoCSV (string)
  (string-to-list (nextify-string string))
)

(defun colocaNaPrimeiraPosicao (elem lista)
  (let ( (resposta elem) )
    (dolist (x lista resposta)
      (setq resposta (append resposta (list x)))
    )
  )
)

(defun writeToFile (name content)
  (with-open-file (stream  name :external-format charset:iso-8859-1
                           :direction :output
                           :if-exists :overwrite
                           :if-does-not-exist :create )
  (format stream content))
  name
)

;; Isso Aqui eh o que sai
;(cria-classificador 'diabetes "arquivoTesteTrab2.txt")
;                 8   183  64   0    0    23.3 0.672 32
(DEFUN DIABETES (PREG PLAS PRES SKIN INSU MASS PEDI AGE)                   
 (COND 
   ((AND (>= PLAS 132) (>= MASS 30)) (SETQ CLASS 'TESTED_POSITIVE1)) 
   ((AND (>= AGE 29) (>= INSU 125) (<= PREG 3)) (SETQ CLASS 'TESTED_POSITIVE2)) 
   ((AND (>= AGE 31) (>= PEDI 0.529) (>= PREG 8) (>= MASS 25.9)) (SETQ CLASS 'TESTED_POSITIVE3)) 
   (T (SETQ CLASS 'TESTED_NEGATIVE)))                    
 CLASS
)
