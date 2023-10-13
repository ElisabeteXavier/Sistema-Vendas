(defun menu-venda ()
  (loop
    (format t "Menu Venda~%")
    (format t "1 - Registrar Venda~%")
    (format t "2 - Consultar Venda~%")
    (format t "3 - Cancelar Venda~%")
    (format t "4 - Listar Todas as Vendas~%")
    (format t "0 - Voltar ao Menu Principal~%")
    (format t "Digite a opção desejada: ")
    (let ((venda-opcao (read)))
      (cond
       ((= venda-opcao 1)
        (registrar-venda)) ; Chame a função para registrar venda aqui
       ((= venda-opcao 2)
        (format t "Digite o número da venda para consulta: ")
        (let ((numero-venda (read)))
          (consultar-venda numero-venda))) ; Chame a função para consultar venda aqui
       ((= venda-opcao 3)
        (format t "Digite o número da venda para cancelar: ")
        (let ((numero-venda (read)))
        (cancelar-venda numero-venda))) ; Chame a função para atualizar venda aqui
       ((= venda-opcao 4)
        (listar-vendas)) ; Chame a função para listar vendas aqui
       ((= venda-opcao 0)
        (format t "Voltando ao Menu Principal~%")
        (return-from menu-venda)) ; Saia do menu de venda e retorne ao menu principal
       (t
        (format t "Opção inválida. Tente novamente.~%"))))))
