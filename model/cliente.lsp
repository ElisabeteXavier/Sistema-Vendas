(defstruct cliente
  (nome "")
  (cpf ""))

(defvar *clientes* (list))

(defun cadastrar-cliente ()
  (format t "Digite o nome do cliente: ")
  (let ((nome (read)))
    (format t "Digite o CPF do cliente: ")
    (let ((cpf (read-line)))
      (push (make-cliente :nome nome :cpf cpf) *clientes*)
      (format t "Cliente cadastrado: ~a (CPF: ~a)~%" nome cpf))))

(defun atualizar-cliente ()
  (format t "Digite o CPF do cliente para atualização: ")
  (let ((cpf (read-line)))
    (let ((cliente (consultar-cliente cpf)))
      (if cliente
          (progn
            (consultar-cliente cpf t) 
            (format t "Digite o novo nome do cliente: ")
            (let ((novo-nome (read)))
              (setf (cliente-nome cliente) novo-nome)
              (format t "Cliente atualizado: ~a (CPF: ~a)~%" novo-nome cpf)))
          (format t "Cliente com CPF ~a não encontrado~%" cpf)))))

(defun apagar-cliente ()
  (format t "Digite o CPF do cliente para apagar: ")
  (let ((cpf (read-line)))
    (let ((cliente (consultar-cliente cpf)))
      (if cliente
          (progn
            (format t "Deseja realmente apagar o cliente com CPF ~a? (s/n): " cpf)
            (let ((confirmacao (read-line)))
              (if (string= confirmacao "s")
                  (progn
                    (setq *clientes* (remove cliente *clientes* :test #'eq))
                    (format t "Cliente com CPF ~a apagado~%" cpf))
                  (format t "Operação cancelada. O cliente não foi apagado.~%"))))
          (format t "Cliente com CPF ~a não encontrado~%" cpf)))))

(defun listar-clientes ()
  (format t "Listagem de Clientes:~%")
  (dolist (cliente *clientes*)
    (format t "Nome: ~a, CPF: ~a~%" (cliente-nome cliente) (cliente-cpf cliente))))

(defun consultar-cliente (&optional cpf mostrar-mensagem)
  (unless cpf
    (format t "Digite o cpf do cliente: ")
    (setq cpf (read-line)))
  (let ((cliente (find cpf *clientes* :test #'equal :key #'cliente-cpf)))
    (if cliente
        (if mostrar-mensagem
            (format t "Cliente encontrado: ~A~%" cliente))
        (if mostrar-mensagem
            (format t "Cliente não encontrado.~%")))
    cliente))