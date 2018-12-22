## Set

### `interseção` de listas

Quais elementos estão tanto em list-a quanto em list-b ?

~~~lisp
(defparameter list-a '(0 1 2 3))
(defparameter list-b '(0 2 4))
(intersection list-a list-b)
;; => (2 0)
~~~

### Remover os elementos de list-b de uma list-a

`set-difference`

~~~lisp
(set-difference list-a list-b)
;; => (3 1)
(set-difference list-b list-a)
;; => (4)
~~~

### Juntar duas listas

`union`

~~~lisp
(union list-a list-b)
;; => (3 1 0 2 4) ;; a ordem pode ser diferente no seu Lisp
~~~

### Remover elementos que estão em ambas as listas

`set-exclusive-or`

~~~lisp
(set-exclusive-or list-a list-b)
;; => (4 3 1)
~~~

e sua forma "reciclável" (`nintersection`,...).

Veja também as funções `setp`, `set-equal`,... em
[Alexandria](https://common-lisp.net/project/alexandria/draft/alexandria.html#Conses).
