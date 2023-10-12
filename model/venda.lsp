(load "model/item-venda.lsp")

(defstruct venda
  (codigo 0 )
  (valor-total 0.0 )
  (status nil )
  (cliente nil) ; Assume que "cliente" é a struct para informações do cliente
  (itensVenda (list)))

(defvar *vendas* (list))

(defun calc-valor-item (quantidade valor-produto)
  (* quantidade valor-produto))

(defun calc-valor-venda (itens)
  (reduce #'+ (mapcar #'item-venda-valor itens)))

(defvar *codigo-venda* 1) ; Variável global para manter o próximo código de venda

(defun gerar-codigo-venda ()
  (let ((codigo *codigo-venda*))
    (setq *codigo-venda* (+ *codigo-venda* 1))
    codigo))

(defun registrar-venda ()
  (loop
    (format t "Informe o código do produto: ")
    (let ((codigo (read-line)))
      (let ((produto (consultar-produto codigo)))
        (if produto
            (progn
              (format t "Produto encontrado: ~a~%" produto)
              (format t "Informe a quantidade: ")
              (let ((quantidade (read)))
                (if quantidade
                    (progn
                      (format t "Quantidade válida: ~D~%" quantidade)
                      (let ((valor-total (calc-valor-item quantidade (produto-valor produto))))
                        (format t "Valor total do item: ~a~%" valor-total)
                        (let ((item-venda (make-item-venda :produto-ref produto :quantidade quantidade :valor valor-total)))
                          (format t "Item-venda criado: ~a~%" item-venda)
                          (push item-venda *itemvendas*) ; Adicione o item-venda à lista de itens de venda atual.
                          (format t "Item-venda adicionado à venda atual.~%")
                        )
                      )
                    )
                    (format t "Quantidade inválida.~%")
                )
              )
            )
            (format t "Produto não encontrado.")
        )
      )
    )

    (format t "Deseja registrar outra venda? (S/N): ")
    (let ((resposta (read-line)))
      (when (string= resposta "N")

      (let ((valor-venda (calc-valor-venda *itemvendas*))) ; Calcula o valor total da venda com base nos itens de venda.
      (format t "Valor total da venda: ~a~%" valor-venda)
      (let ((venda (make-venda :codigo (gerar-codigo-venda) ; Gera o código da venda automaticamente
                               :valor-total valor-venda
                               :status t
                               :cliente nil
                               :itensVenda *itemvendas*)))
        (push venda *vendas*) ; Adiciona a venda à lista de vendas.
        (setf *itemvendas* (list)) ; Limpa a lista de itens de venda para a próxima venda.
        (format t "Venda registrada com sucesso.~%")
      )
    )
        (return)))
    )
)

(defun relatorio-venda (venda)
  (format t "===== Relatório de Venda =====~%")
  (format t "Cliente: ~a~%" (venda-cliente venda))
  (format t "Status: ~a~%" (if (venda-status venda) "Concluída" "Em andamento"))
  (format t "Itens da Venda:~%")
  (dolist (item (venda-itensVenda venda))
    (format t "Produto: ~a, Quantidade: ~a, Valor Total: ~a~%"
            (item-venda-produto-ref item)
            (item-venda-quantidade item)
            (item-venda-valor item)))
  (format t "Valor Total da Venda: ~a~%" (venda-valor-total venda))
  (format t "==============================~%"))

(defun listar-vendas()
  (format t "===== Lista de Vendas =====~%")
  (dolist (venda *vendas*)
    (relatorio-venda venda)
    (format t "--------------------------~%"))
  (format t "==============================~%"))
