## Arrays e Vectors

**Arrays** possuem características de tempo de acesso constante.

Podem ser fixos ou ajustáveis. Um *simple array* não pode ser separado
(usando `:displaced-to, para apontar para outro array),
ajustável (`:adjust-array`), e também não possui um fill pointer (`full-pointer`, que se move conforme elementos são retirados ou adicionados).

Um **vetor** é um array de rank 1 (de uma dimensão). Ele também é uma sequência.

Um *simple vector* é um simple array que não é especializado 
(não é usado `:element-type`para definir o tipo dos elementos).

### Criar um array, com uma ou mais dimensões

`make-array` *(sizes-list :adjustable bool)*

`adjust-array` *(array, sizes-list, :element-type, :initial-element)*

### Acesso: aref (array i [j ...])

`aref` *(array i j k ...)* ou `row-major-aref` *(array i)* equivalente a `(aref i i i ...)`.

O resultado é passível de `setf`.

~~~lisp
(defparameter myarray (make-array '(2 2 2) :initial-element 1))
myarray
;; => #3A(((1 1) (1 1)) ((1 1) (1 1)))
(aref myarray 0 0 0)
;; => 1
(setf (aref myarray 0 0 0) 9)
;; => 9
(row-major-aref myarray 0)
;; => 9
~~~


### Tamanhos

`array-total-size` *(array i)*: quantos elementos cabem no array ?

`array-dimensions` *(array)*: uma lista contendo o tamanho das dimensões do array.

`array-dimension` *(array i)*: tamanho da *i*-ésima dimensão.

`array-rank`: número de dimensões do array.

~~~lisp
(defparameter myarray (make-array '(2 2 2)))
;; => MYARRAY
myarray
;; => #3A(((0 0) (0 0)) ((0 0) (0 0)))
(array-rank myarray)
;; => 3
(array-dimensions myarray)
;; => (2 2 2)
(array-dimension myarray 0)
;; => 2
(array-total-size myarray)
;; => 8
~~~


### Vectors

Crie com `vector` ou com a reader macro (macro de leitura) `#()`. Retorna um _simple vector._

~~~lisp
(vector 1 2 3)
;; => #(1 2 3)
#(1 2 3)
;; => #(1 2 3)
~~~


`vector-push` *(foo vector)*:  substitui o elemento de vector assinalado pelo fill pointer por foo. Pode ser destrutiva.

`vector-push-extend` *(foo vector [extension-num])*t

`vector-pop` *(vector)*: retorna o elemento de vector que fill pointer aponta.

`fill-pointer` *(vector)*. Passível de `setf`.

Veja também as funções de *sequence*.

### Transformando um vector em uma list.

Se você está mapeando a list, veja a função `map` que tem como primeiro
parâmetro o tipo resultante.

Ou use `(coerce vector 'list)`.
