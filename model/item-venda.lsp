(defstruct item-venda
  (produto-ref nil) ; Referência ao produto na lista *produtos*
  (quantidade 0 )
  (valor 0.0 ))

(defvar *itemvendas* (list))