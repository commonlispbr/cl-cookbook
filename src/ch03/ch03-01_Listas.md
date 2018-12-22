## Listas 

### Montando listas, Cons cells e listas

_Uma lista também é uma sequência, portanto é possível usar as funções abaixo._

Cons cells são os elementos básicos de uma lista. Listas são construídas agrupando-se cons cells.

~~~lisp
(cons 1 2)
;; => (1 . 2) ;; representação com um ponto, um dotted pair.
~~~

Assim:

```
[o|o]--- 2
 |
 1
```

Se o `cdr` da primeira cell for outra cons cell, e se o `cdr` desta última cons cell for `nil`,
uma lista é montada:

~~~lisp
(cons 1 (cons 2 nil))
;; => (1 2)
~~~

Mais ou menos assim:

```
[o|o]---[o|/]
 |       |
 1       2
```

(ascii art criada com [draw-cons-tree](https://github.com/cbaggers/draw-cons-tree)).

Percebeu como a lista não foi representada como um dotted pair? O printer entende a convenção.

Por fim, é possível construir uma lista literal usando `list`:

~~~lisp
(list 1 2)
;; => (1 2)
~~~

ou usando uma aspa simples:

~~~lisp
'(1 2)
;; => (1 2)
~~~

que nada mais é do que uma simplificação para a chamada de função `(quote (1 2))`.

### Listas Circulares

O car ou o cdr de uma cons cell pode referenciar outros objetos,
incluindo a si mesmo ou a outras cells da mesma lista. Portanto `car` e `cdr`
podem ser usados para definir estruturas de auto-referência, tais como listas circulares.

Antes de começar a trabalhar com listas circulares, avise ao printer, para que ele
as reconheça e que não tente imprimir a lista completa setando
[\*print-circle\*](http://clhs.lisp.se/Body/v_pr_cir.htm)
para T`:

~~~lisp
(setf *print-circle* t)
~~~

Uma função que modifica uma lista, de forma que o último `cdr` aponte para o começo
da lista é:

~~~lisp
(defun circular! (items)
  "Modifica o último cdr de uma lista ITEMS, retornando uma lista circular"
  (setf (cdr (last items)) items))
(circular! (list 1 2 3))
;; => #1=(1 2 3 . #1#)

(fifth (circular! (list 1 2 3)))
;; => 2
~~~

A função [list-length](http://www.lispworks.com/documentation/HyperSpec/Body/f_list_l.htm#list-length)
reconhece uma lista circular, retornando `nil`.

O reader também pode criar uma lista circular, usando a notação 
[Sharpsign Equal-Sign](http://www.lispworks.com/documentation/HyperSpec/Body/02_dho.htm)
Um objeto, (como uma lista) pode ser prefixado com `#n=` onde `n`
é um número inteiro decimal sem sinal (um ou mais dígitos).
O label `#n#` pode ser depois ser usado para referenciar o objeto na expressão:

~~~lisp
'#42=(1 2 3 . #42#)
;; => #1=(1 2 3 . #1#)
~~~

Note que o label dado ao reader (`n=42`) é descartado após a leitura,
e que o printer define um novo label (`n=1`).

Leitura complementar

* [Let over Lambda](https://letoverlambda.com/index.cl/guest/chap4.html#sec_5) seção abordando expressões cíclicas


### car/cdr ou primeiro/resto (e segundo... ao décimo)

~~~lisp
(car (cons 1 2)) ;; => 1
(cdr (cons 1 2)) ;; => 2
(first (cons 1 2)) ;; => 1
(first '(1 2 3)) ;; => 1
(rest '(1 2 3)) ;; => (2 3)
~~~

É possível atribuir *qualquer* novo valor usando `setf`

### last, butlast, nbutlast (&optional n)

retorna a última cons cell em uma lista (ou a n-ésima última cons cell).

~~~lisp
(last '(1 2 3))
;; => (3)
(car (last '(1 2 3)) ) ;; ou (first (last …))
;; => 3
(butlast '(1 2 3))
;; => (1 2)
~~~

Em [Alexandria](https://common-lisp.net/project/alexandria/draft/alexandria.html#Conses), `lastcar` é equivalente a `(first (last …))`:

~~~lisp
(alexandria:lastcar '(1 2 3))
;; => 3
~~~

### reverse, nreverse

`reverse` e `nreverse` retorna uma nova sequência.

`nreverse` é destrutiva. O N significa **non-consing**, ou seja, não é necessário
alocar novas cons cells. Ela *pode* (e na prática, é isso que ela faz)
reusar e modificar a sequência original:

~~~lisp
(defparameter mylist '(1 2 3))
;; => (1 2 3)
(reverse mylist)
;; => (3 2 1)
mylist
;; => (1 2 3)
(nreverse mylist)
;; => (3 2 1)
mylist
;; => (1) in SBCL but implementation dependant.
~~~

### append

`append` recebe qualquer quantidade de listas como argumento e retorna uma
nova lista, contendo os elementos de todos os seus arguments:

~~~lisp
(append (list 1 2) (list 3 4))
;; => (1 2 3 4)
~~~
A nova lista compartilha de algumas cons cells com a `(3 4)`:

http://gigamonkeys.com/book/figures/after-append.png

__Nota__: `append` em [cl21](cl21.htm) é genérica (para listas, strings, vetores e suas abstract-sequence)

`nconc` é a equivalente reciclável

### push (item, place)

`push` insere *item* à lista armazenada em *place*, guarda a lista resultante em *place*, e retorna a lista.

~~~lisp
(defparameter mylist '(1 2 3))
(push 0 mylist)
;; => (0 1 2 3)
~~~

~~~lisp
(defparameter x ’(a (b c) d))
;; => (A (B C) D)
(push 5 (cadr x))
;; => (5 B C)
x
;; => (A (5 B C) D)
~~~

`push` é equivalente a `(setf place (cons item place))`, porém, as subforms de
*place* sofrem eval apenas uma vez, e *item* tem o eval antes de *place*.

Não existe uma função nativa para **adicionar ao fim de uma lista**. Esta é uma operação
mais custosa (é preciso percorrer toda a lista). Então, se você precisa realizar
essa operação faça o seguinte:
Considere utilizar outra esrutura de dados, ou ponha sua lista `ao contrário` quando necessário.

### pop

uma operação destrutiva.

### nthcdr (index, list)

Use-a se `first`, `second` e todas as outras até `tenth` não forem o suficiente.

### car/cdr e composites (cadr, caadr...) - acessando listas dentro de listas

Essas fazem sentido quando aplicadas a listas que contém outras listas.

~~~lisp
(caar (list 1 2 3))                  ==> error
(caar (list (list 1 2) 3))           ==> 1
(cadr (list (list 1 2) (list 3 4)))  ==> (3 4)
(caadr (list (list 1 2) (list 3 4))) ==> 3
~~~

### destructuring-bind (parameters*, list)

Liga os valores do parâmetro à lista de elementos. É possível desestruturar
árvores, plists, e até mesmo prover defaults.

Match simples:

~~~lisp
(destructuring-bind (x y z) (list 1 2 3)
  (list :x x :y y :z z))
;; => (:X 1 :Y 2 :Z 3)
~~~

Match dentro de sublistas:

~~~lisp
(destructuring-bind (x (y1 y2) z) (list 1 (list 2 20) 3)
  (list :x x :y1 y1 :y2 y2 :z z))
;; => (:X 1 :Y1 2 :Y2 20 :Z 3)
~~~

A lista de parâmetros pode fazer uso dos parâmetros
`&optional`, `&rest` e `&key`.

~~~lisp
(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2) 3)
  (list :x x :y1 y1 :y2 y2 :z z))
;; => (:X 1 :Y1 2 :Y2 NIL :Z 3)
~~~

~~~lisp
(destructuring-bind (&key x y z) (list :z 1 :y 2 :x 3)
  (list :x x :y y :z z))
;; => (:X 3 :Y 2 :Z 1)
~~~

O parâmetro `&whole` está ligado à lista inteira. Ele deve vir à frente
e pode ser prosseguido por outros parâmetros.

~~~lisp
(destructuring-bind (&whole whole-list &key x y z) (list :z 1 :y 2 :x 3)
  (list :x x :y y :z z :whole whole-list))
;; => (:X 3 :Y 2 :Z 1 :WHOLE-LIST (:Z 1 :Y 2 :X 3))
~~~

Desestruturando uma plist, provendo defaults:

(exemplo retirado de Common Lisp Recipes, de E. Weitz, Apress, 2016)

~~~lisp
(destructuring-bind (&key a (b :not-found) c
                     &allow-other-keys)
    ’(:c 23 :d "D" :a #\A :foo :whatever)
  (list a b c))
;; => (#\A :NOT-FOUND 23)
~~~

** TODO
If this gives you the will to do pattern matching, see
[pattern matching](pattern_matching.html).

### Predicados: null, listp

`null` é equivalente a `not`, mas é o mais recomendado.

`listp` veerifica se um objeto é uma cons cell ou nil.

e predicados de sequência.


### idiff, tailp, list*, make-list, fill, revappend, nreconc, consp, atom

~~~lisp
(make-list 3 :initial-element "ta")
;; => ("ta" "ta" "ta")
~~~

~~~lisp
(make-list 3)
;; => (NIL NIL NIL)
(fill * "hello")
;; => ("hello" "hello" "hello")
~~~
