(defstruct cliente
  (nome "")
  (cpf ""))

(defvar *clientes* (list))

(defun cadastrar-cliente (nome cpf)
  (push (make-cliente :nome nome :cpf cpf) *clientes*))

(defun consultar-cliente (cpf)
  (let ((cliente (find-cliente cpf)))
    (if cliente
        (format t "Cliente encontrado: ~a (CPF: ~a)~%" (cliente-nome cliente) (cliente-cpf cliente))
        (format t "Cliente com CPF ~a não encontrado~%" cpf))))

(defun atualizar-cliente (cpf novo-nome)
  (let ((cliente (find-cliente cpf)))
    (if cliente
        (setf (cliente-nome cliente) novo-nome)
        (format t "Cliente com CPF ~a não encontrado~%" cpf))))

(defun apagar-cliente (cpf)
  (let ((cliente (find-cliente cpf)))
    (if cliente
        (progn
          (setq *clientes* (remove cliente *clientes* :test #'eq))
          (format t "Cliente com CPF ~a apagado~%" cpf))
        (format t "Cliente com CPF ~a não encontrado~%" cpf))))

(defun listar-clientes ()
  (format t "Listagem de Clientes:~%")
  (dolist (cliente *clientes*)
    (format t "Nome: ~a, CPF: ~a~%" (cliente-nome cliente) (cliente-cpf cliente))))

(defun find-cliente (cpf)
  (find cpf *clientes* :test #'equal :key #'cliente-cpf))