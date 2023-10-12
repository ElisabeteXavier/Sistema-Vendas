(defstruct produto
  (codigo "")
  (nome "")
  (quantidade 0)
  (valor 0))
(defvar *LISTA-DE-PRODUTOS*(list)) ; Inicializa a variável global com uma lista vazia

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
                  (push (make-produto :codigo codigo :nome nome :quantidade quantidade :valor valor) *LISTA-DE-PRODUTOS*) ; Use a variável global *LISTA-DE-PRODUTOS*

                  (format t "Produto cadastrado com sucesso!~%")
                  (format t "-------------------------------~%")))))))))

(defun produto-existe-p (codigo)
  ; Verifique se o código já existe na lista de produtos
  (find codigo *LISTA-DE-PRODUTOS* :test #'string= :key #'produto-codigo))

(defun consultar-produto (&optional codigo-consulta-param mostrar-mensagem)
  (let ((codigo-consulta (if codigo-consulta-param
                            codigo-consulta-param
                            (progn
                              (format t "Digite o código do produto para consulta: ")
                              (read-line)))))
    (let ((produto-encontrado (find codigo-consulta
                                   *LISTA-DE-PRODUTOS*
                                   :test #'string=
                                   :key #'produto-codigo)))
      (if mostrar-mensagem
          (if produto-encontrado
              (format t "Produto encontrado: ~A~%" produto-encontrado)
              (format t "Produto com código a não encontrado.%" codigo-consulta)))
          produto-encontrado)))

(defun atualizar-produto ()
  (format t "Digite o código do produto que deseja atualizar: ")
  (let ((codigo-atualizar (read-line)))

    ; Procura o produto com o código informado
    (let ((produto-encontrado (consultar-produto codigo-atualizar)))
      (if produto-encontrado
          (progn
            
            (format t "Digite o novo nome do produto: ")
            (let ((novo-nome (read-line)))

              (format t "Digite a nova quantidade disponível: ")
              (let ((nova-quantidade (read)))

                (format t "Digite o novo valor do produto: ")
                (let ((novo-valor (read)))

                  ; Atualiza as informações do produto
                  (setf (produto-nome produto-encontrado) novo-nome)
                  (setf (produto-quantidade produto-encontrado) nova-quantidade)
                  (setf (produto-valor produto-encontrado) novo-valor)

                  (format t "Produto atualizado com sucesso!~%")))))
          (format t "Produto com código ~a não encontrado. Nenhum produto foi atualizado.~%" codigo-atualizar)))))

(defun listar-produtos ()
  (format t "Lista de Produtos Cadastrados:~%")
  (format t "------------------------------~%")
  (dolist (produto *LISTA-DE-PRODUTOS*)
    (format t "Código: ~a~% Nome: ~a~% Quantidade: ~a~% Valor: ~a~%~%"
            (produto-codigo produto)
            (produto-nome produto)
            (produto-quantidade produto)
            (produto-valor produto))
            (format t "-------------------------------~%"))
  (format t "Fim da Lista de Produtos~%"))



(defun baixa-saldo-produto (produto quantidade-a-baixar)
  (if (< (produto-quantidade produto) quantidade-a-baixar)
      nil ; Retorna nil se não houver saldo suficiente
      (progn
        ; Reduza a quantidade disponível no produto
        (setf (produto-quantidade produto) (- (produto-quantidade produto) quantidade-a-baixar))
        t))) ; Retorna true se a baixa for bem-sucedida



(defun deletar-produto ()
  (format t "Digite o código do produto que deseja remover: ")
  (let ((codigo-remover (read-line)))

    ; Procura o produto com o código informado
    (let ((produto-encontrado (consultar-produto codigo-remover)))
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