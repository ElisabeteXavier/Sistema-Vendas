(load "model/cliente.lsp")
(load "menu/menu-cliente.lsp")
(load "menu/menu-venda.lsp")

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
        (menu-cliente)) 
        ((= opcao 3) ; Opção Venda
        (menu-venda)) ; Chame o menu do Venda
       ;; Resto do código para Produto e Venda, se necessário
       ((= opcao 0)
        (format t "Saindo do programa.~%")
        (return)) ; Saia do programa
       (t
        (format t "Opção inválida. Tente novamente.~%"))))))

(main)