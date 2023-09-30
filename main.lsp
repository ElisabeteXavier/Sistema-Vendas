(load "cliente.lsp")

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
        (format t "Digite o CPF do cliente para consulta: ")
        (let ((cpf (read)))
          (consultar-cliente cpf))) ; Chame a função para consultar cliente aqui
       ((= cliente-opcao 3)
        (atualizar-cliente)) ; Chame a função para atualizar cliente aqui
       ((= cliente-opcao 4)
        (apagar-cliente)) ; Chame a função para apagar cliente aqui
       ((= cliente-opcao 5)
        (listar-clientes)) ; Chame a função para listar clientes aqui
       ((= cliente-opcao 0)
        (format t "Voltando ao Menu Principal~%")
        (return))
       (t
        (format t "Opção inválida. Tente novamente.~%"))))))

(defun main ()
  (loop
    (format t "Menu Principal~%")
    (format t "1 - Cliente~%")
    (format t "2 - Produto~%")
    (format t "3 - Venda~%")
    (format t "0 - Sair~%")
    (format t "Digite a opção desejada: ")
    (let ((opcao (read)))
      (cond
       ((= opcao 1) ; Opção Cliente
        (menu-cliente)) ; Chame o menu do Cliente
       ;; Resto do código para Produto e Venda, se necessário
       ((= opcao 0)
        (format t "Saindo do programa.~%")
        (return))
       (t
(format t "Opção inválida. Tente novamente.~%"))))))

(main)