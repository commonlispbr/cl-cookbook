## Plist

Uma property list (lista de propriedades) é simplesmente uma lista que alterna
uma chave, um valor, etc, onde suas chaves são simbolos (não é possível utiilizar `:test`).
Mais precisamente, a plist tem um cons cell em que `car` é a chave,
que `cdr` aponta para a próxima cons cell em que `car` seja o valor.

Esta plist por exemplo:

~~~lisp
(defparameter my-plist (list 'foo "foo" 'bar "bar"))
~~~

é representada assim:

```
[o|o]---[o|o]---[o|o]---[o|/]
 |       |       |       |
FOO     "foo"   BAR     "bar"

```

Acessamos o valor com `getf (list elt)` (retorna o valor)
(a lista é o primeiro elemento),

removemos um elemento com `remf`.

~~~lisp
(defparameter my-plist (list 'foo "foo" 'bar "bar"))
;; => (FOO "foo" BAR "bar")
(setf (getf my-plist 'foo) "foo!!!")
;; => "foo!!!"
~~~
