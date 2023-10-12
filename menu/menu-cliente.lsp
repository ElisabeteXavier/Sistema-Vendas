(defun menu-cliente ()
  (loop
    (format t "Menu Cliente~%")
    (format t "1 - Cadastrar Cliente~%")
    (format t "2 - Consultar Cliente~%")
    (format t "3 - Atualizar Cliente~%")
    (format t "4 - Apagar Cliente~%")
    (format t "5 - Listar Todos os Clientes~%")
    (format t "0 - Voltar ao Menu Principal~%")
    (format t "Digite a opção desejada: ")
    (let ((cliente-opcao (read)))
      (cond
       ((= cliente-opcao 1)        
        (cadastrar-cliente)) ; Chame a função para cadastrar cliente aqui
       ((= cliente-opcao 2)
        (consultar-cliente Nil t)) ; Chame a função para consultar cliente aqui
       ((= cliente-opcao 3)
        (atualizar-cliente)) ; Chame a função para atualizar cliente aqui
       ((= cliente-opcao 4)
        (apagar-cliente)) ; Chame a função para apagar cliente aqui
       ((= cliente-opcao 5)
        (listar-clientes)) ; Chame a função para listar clientes aqui
       ((= cliente-opcao 0)
        (format t "Voltando ao Menu Principal~%")
        (return-from menu-cliente)) ; Saia do menu do cliente e retorne ao menu principal
       (t
        (format t "Opção inválida. Tente novamente.~%"))))))