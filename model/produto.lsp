
(defvar *LISTA-DE-PRODUTOS* '()) ; Inicializa a variável global com uma lista vazia


(defun cadastrar-produto ()
  (format t "Digite o código do produto: ")
  (let ((codigo (read-line)))

    ; Verifique se o código já existe na lista
    (if (produto-existe-p codigo)
        (format t "Código já cadastrado. Não é possível cadastrar um novo produto com o mesmo código.~%")
        (progn
          (format t "Digite o nome do produto: ")
          (let ((nome (read-line)))

            (format t "Digite a quantidade disponível: ")
            (let ((quantidade (read)))

              (format t "Digite o valor do produto: ")
              (let ((valor (read)))

                ; Crie um novo produto como uma lista associativa
                (let ((produto (list :codigo codigo
                                     :nome nome
                                     :quantidade quantidade
                                     :valor valor)))

                  ; Adicione o novo produto à lista de produtos
                  (push produto *LISTA-DE-PRODUTOS*) ; Use a variável global *LISTA-DE-PRODUTOS*

                  (format t "Produto cadastrado com sucesso!~%")
                  (format t "-------------------------------~%")))))))))

(defun produto-existe-p (codigo)
  ; Verifique se o código já existe na lista de produtos
  (find codigo *LISTA-DE-PRODUTOS* :key (lambda (produto) (getf produto :codigo)) :test #'string=))

(defun consultar-produto (&optional codigo-consulta-param mostrar-mensagem)
  (let ((codigo-consulta (if codigo-consulta-param
                            codigo-consulta-param
                            (progn
                              (format t "Digite o código do produto para consulta: ")
                              (read-line)))))
    (let ((produto-encontrado (find codigo-consulta
                                   *LISTA-DE-PRODUTOS*
                                   :key (lambda (produto) (getf produto :codigo))
                                   :test #'string=)))
      (if mostrar-mensagem
          (if produto-encontrado
              (format t "Produto encontrado:% Código: ~a% Nome: a% Quantidade: a% Valor: a%"
                      (getf produto-encontrado :codigo)
                      (getf produto-encontrado :nome)
                      (getf produto-encontrado :quantidade)
                      (getf produto-encontrado :valor))
              (format t "Produto com código a não encontrado.%" codigo-consulta)))
          produto-encontrado)))



(defun atualizar-produto ()
  (format t "Digite o código do produto que deseja atualizar: ")
  (let ((codigo-atualizar (read-line)))

    ; Procura o produto com o código informado
    (let ((produto-encontrado (find codigo-atualizar
                                     *LISTA-DE-PRODUTOS*
                                     :key (lambda (produto) (getf produto :codigo))
                                     :test #'string=)))
      (if produto-encontrado
          (progn
            (consultar-produto codigo-atualizar t )
            (format t "Produto encontrado. Insira as novas informações:~%")
            (format t "------------------------------------------------~%")

            (format t "Digite o novo nome do produto: ")
            (let ((novo-nome (read-line)))

              (format t "Digite a nova quantidade disponível: ")
              (let ((nova-quantidade (read)))

                (format t "Digite o novo valor do produto: ")
                (let ((novo-valor (read)))

                  ; Atualiza as informações do produto
                  (setf (getf produto-encontrado :nome) novo-nome)
                  (setf (getf produto-encontrado :quantidade) nova-quantidade)
                  (setf (getf produto-encontrado :valor) novo-valor)

                  (format t "Produto atualizado com sucesso!~%")))))
          (format t "Produto com código ~a não encontrado. Nenhum produto foi atualizado.~%" codigo-atualizar)))))

(defun listar-produtos ()
  (format t "Lista de Produtos Cadastrados:~%")
  (format t "------------------------------~%")
  (dolist (produto *LISTA-DE-PRODUTOS*)
    (format t "Código: ~a~% Nome: ~a~% Quantidade: ~a~% Valor: ~a~%~%"
            (getf produto :codigo)
            (getf produto :nome)
            (getf produto :quantidade)
            (getf produto :valor))
            (format t "-------------------------------~%"))
  (format t "Fim da Lista de Produtos~%"))


(defun deletar-produto ()
  (format t "Digite o código do produto que deseja remover: ")
  (let ((codigo-remover (read-line)))

    ; Procura o produto com o código informado
    (let ((produto-encontrado (find codigo-remover
                                     *LISTA-DE-PRODUTOS*
                                     :key (lambda (produto) (getf produto :codigo))
                                     :test #'string=)))
    (if produto-encontrado
        (progn
          (consultar-produto codigo-remover)
          (format t "Deseja realmente apagar o produto com o código ~a? (s/n): " codigo-remover)
          (let ((confirmacao (read-line)))
              (if (string= confirmacao "s")
                  (progn
                    (setq *lista-de-produtos* (remove produto-encontrado *lista-de-produtos* :test #'equal))
                    (format t "Produto com código ~a deletado com sucesso!~%" codigo-remover))
                  (format t "Operação cancelada. O produto não foi apagado.~%"))))
        (format t "Produto com código ~a não encontrado. Nenhum produto foi deletado.~%" codigo-remover)))))