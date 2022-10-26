## Alist

Umas lista associativa é uma lista de cons cells.

Este simples exemplo:

~~~lisp
(defparameter *my-alist* (list (cons 'foo "foo")
                             (cons 'bar "bar")))
;; => ((FOO . "foo") (BAR . "bar"))
~~~

fica assim:

```
[o|o]---[o|/]
 |       |
 |      [o|o]---"bar"
 |       |
 |      BAR
 |
[o|o]---"foo"
 |
FOO
```
Nós podemos construir uma alist de acordo com sua representação:


~~~lisp
(setf *my-alist* '((:foo . "foo")
                 (:bar . "bar")))
~~~
O construtor `pairlis` associa uma lista de chaves a uma lista de valores:

~~~lisp
(pairlis '(:foo :bar)
         '("foo" "bar"))
;; => ((:BAR . "bar") (:FOO . "foo"))
~~~

Para pegar uma chave, nós temos `assoc` (use `:test 'equal` quando suas chaves
forem strings, como sempre). Ela retorna a toda a cons cell, então você pode usar
`cdr` ou `second` para obter o valor, ou ainda melhor, `assoc-value list key` de `Alexandria`.

~~~lisp
(alexandria:assoc-value *my-alist* :foo)
;; it actually returns 2 values
;; "foo"
;; (:FOO . "FOO")
~~~

Existem `assoc-if`, e `rassoc` para obter uma cons cell a partir do seu valor.

Para adicionar uma chave, nós fazemos `push` para outra cons cell:

~~~lisp
(push (cons 'team "team") *my-alist*)
;; => ((TEAM . "team") (FOO . "foo") (BAR . "bar"))
~~~

Podemos usar `pop` e outras funções que operam sobre listas, como `remove`:

~~~lisp
(remove :team *my-alist*)
;; => ((:TEAM . "team") (FOO . "foo") (BAR . "bar")) ;; didn't remove anything
(remove :team *my-alist* :key 'car)
;; => ((FOO . "foo") (BAR . "bar")) ;; returns a copy
~~~

Remover apenas um elemento com `:count`:

~~~lisp
(push (cons 'bar "bar2") *my-alist*)
;; => ((BAR . "bar2") (TEAM . "team") (FOO . "foo") (BAR . "bar")) ;; twice the 'bar key
(remove 'bar *my-alist* :key 'car :count 1)
;; => ((TEAM . "team") (FOO . "foo") (BAR . "bar"))
;; because otherwise:
(remove 'bar *my-alist* :key 'car)
;; => ((TEAM . "team") (FOO . "foo")) ;; no more 'bar
~~~

Na biblioteca
[Alexandria](https://common-lisp.net/project/alexandria/draft/alexandria.html#Conses)
veja mais funções como `remove-from-plist`, `alist-plist`, ...
