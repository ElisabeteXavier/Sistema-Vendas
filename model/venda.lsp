(defstruct venda
  (valor-total 0.0 :type double-float)
  (status nil :type boolean)
  (cliente nil) ; Assume que "cliente" é a struct para informações do cliente
  (produtos '() :type list))


  (defvar *vendas* (list))
  
(defun registrar-venda ()
  (format t "Digite o cpf do cliente: ")
  (let ((cpf (read)))
    (let ((cliente (find-cliente cpf)))
      (if cliente
          (progn
            (format t "Cliente encontrado: ~a CPF: ~a ~%" (cliente-nome cliente) cpf)
            (terpri)) ; Adicionei esta linha para imprimir uma nova linha após as informações do cliente
          (format t "Cliente com CPF ~a não encontrado~%" cpf)))))

