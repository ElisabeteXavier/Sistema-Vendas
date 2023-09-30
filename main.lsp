;; Importe o módulo que contém a definição da struct cliente
(load "cliente.lsp")

(defun main ()
  (loop
    (format t "Menu Principal~%")
    (format t "1 - Cadastrar Cliente~%")
    (format t "2 - Consultar Cliente~%")
    (format t "3 - Atualizar Cliente~%")
    (format t "4 - Apagar Cliente~%")
    (format t "5 - Listar Todos os Clientes~%")
    (format t "0 - Sair~%")
    (format t "Digite a opção desejada: ")
    (let ((opcao (read)))
      (cond
       ((= opcao 1)
        (format t "Digite o nome do cliente: ")
        (let ((nome (read)))
          (format t "Digite o CPF do cliente: ")
          (let ((cpf (read)))
            (cadastrar-cliente nome cpf))))
       ((= opcao 2)
        (format t "Digite o CPF do cliente para consulta: ")
        (let ((cpf (read)))
          (consultar-cliente cpf)))
       ((= opcao 3)
        (format t "Digite o CPF do cliente para atualização: ")
        (let ((cpf (read)))
          (format t "Digite o novo nome do cliente: ")
          (let ((novo-nome (read)))
            (atualizar-cliente cpf novo-nome))))
       ((= opcao 4)
        (format t "Digite o CPF do cliente para apagar: ")
        (let ((cpf (read)))
          (apagar-cliente cpf)))
       ((= opcao 5)
        (listar-clientes))
       ((= opcao 0)
        (format t "Saindo do programa.~%")
        (return))
       (t
        (format t "Opção inválida. Tente novamente.~%"))))))

(main)