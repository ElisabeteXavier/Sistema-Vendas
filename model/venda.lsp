(defstruct item-venda
  (produto-ref nil) ; Referência ao produto na lista *produtos*
  (quantidade 0 :type integer)
  (valor 0.0 :type double-float))

(defvar *itemvendas* (list))

(defstruct venda
  (valor-total 0.0 :type double-float)
  (status nil :type boolean)
  (cliente nil) ; Assume que "cliente" é a struct para informações do cliente
  (itensVenda '() :type (list item-venda)))

(defvar *vendas* (list))

(defun calc-valor-item (quantidade valor-produto)
  (* quantidade valor-produto))
  (defun calc-valor-venda (itens)
  (reduce #'+ (mapcar #'item-venda-valor itens)))

(defun registrar-venda ()
  (format t "Informe o código do produto: ")
  (let ((codigo (read-line)))
    (let ((produto (find-produto codigo)))
      (if produto
          (progn
            (format t "Produto encontrado: ~a~%" produto)
            (format t "Informe a quantidade: ")
            (let ((quantidade (parse-integer (read-line))))
              (if quantidade
                  (progn
                    (format t "Quantidade válida: ~a~" quantidade)
                    (let ((valor-total (calc-valor-item quantidade (produto-preco produto))))
                      (format t "Valor total do item: ~a~%" valor-total)
                      (let ((item-venda (make-item-venda :produto-ref produto :quantidade quantidade :valor valor-total)))
                        (format t "Item-venda criado: ~a~" item-venda)
                        (push item-venda *itemvendas) ; Adicione o item-venda à lista de itens de venda atual.
                        (format t "Item-venda adicionado à venda atual.")
                      )
                    )
                  )
                  (format t "Quantidade inválida.")
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
      (return))
  )
  (let ((valor-venda (calc-valor-venda *itemvendas*))) ; Calcula o valor total da venda com base nos itens de venda.
    (format t "Valor total da venda: ~a~%" valor-venda)
    (let ((venda (make-venda :valor-total valor-venda :status t :cliente nil :itensVenda *itemvendas*)))
      (push venda *vendas*) ; Adiciona a venda à lista de vendas.
      (setf *itemvendas* (list)) ; Limpa a lista de itens de venda para a próxima venda.
      (format t "Venda registrada com sucesso.")
    )
  )
)

