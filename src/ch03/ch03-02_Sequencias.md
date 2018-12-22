## Sequências

**lists** e **vectors** (e, portanto **strings**) são sequências.

_Note_: veja também a página sobre [strings](strings.html).

Muitas das funções de sequência utilizam palavras-chave como argumentos.
Todos os argumentos podem ser são opcionais, e se especificados, podem
aparecer em qualquer ordem.

Atente-se ao argumento `:teste`. O seu padrão é `eql` (para strings use `:equal`).

Para o argumento `:key` deve-se passar nil, ou uma função com um argumento.
Essa função key é usada como umo filtro pelo qual os elementos da
sequência são visualizados. Por exemplo:

~~~lisp
(find x y :key 'car)
~~~

é similar a `(assoc* x y)`, ela busca por um elemento na lista em que
o car seja igual a x, oa invés de um elemento que seja igual a x.
Se `:key` for omitido ou nil, o filtro será efetivamente a funçãp identidade.

Exemplo com uma alist:

~~~lisp
(defparameter my-alist (list (cons 'foo "foo")
                             (cons 'bar "bar")))
;; => ((FOO . "foo") (BAR . "bar"))
(find 'bar my-alist)
;; => NIL
(find 'bar my-alist :key 'car)
;; => (BAR . "bar")
~~~

Para algo mais, use um `lambda` que receba um parâmetro.

~~~lisp
(find 'bar my-alist :key (lambda (it) (car it)))
~~~

_Nota_: [cl21](cl21.html#shorter-lambda) possui short lambdas:

~~~lisp
(find 'bar my-alist :key ^(car %))
(find 'bar my-alist :key (lm (it) (car it)))
~~~

### Predicados: every, some,...

`every, notevery (teste, sequência)`: retornam nil ou t, respectivamente,
no momento em que um teste ou qualquer conjunto de elementos correspondentes
a elementos de sequências retorne nil.

~~~lisp
(defparameter foo '(1 2 3))
(every #'evenp foo)
;; => NIL
(some #'evenp foo)
;; => T
~~~

com uma lista de strings:

~~~lisp
(defparameter str '("foo" "bar" "team"))
(every #'stringp str)
;; => T
(some #'(lambda (it) (= 3 (length it))) str)
;; => T
(some ^(= 3 (length %)) str) ;; in CL21
;; => T
~~~

`some`, `notany` *(teste, sequência)*: retorna o valor do teste ou nil.
`mismatch` *(sequence-a, sequence-b)*: Retorna a posição em sequênce-a
onde sequence-a sequence-b deixam de ser iguais. Retorna nil se as duas
sequências forem iguais. Outros parâmetros: `:from-end bool`, `:start1`, `:end[1,2]`.

### Functions

Veja também as funções de sequência definidas em (em inglês)
[Alexandria](https://common-lisp.net/project/alexandria/draft/alexandria.html#Sequences):
`starts-with`, `ends-with`, `ends-with-subseq`, `length=`, `emptyp`,…

#### length (sequence)

#### member (elt, sequence)

Retorna o tail de `sequence` a partir do primeiro elemento que satisfaça `eql`.
Aceita `:test`, `:test-not`, `:key`(funções ou símbolos).

~~~lisp
(member 2 '(1 2 3))
;; (2 3)
~~~

#### elt (sequence, index) - encontra a partir de index

Cuidado, neste caso a sequência vem primeiro.

#### count (foo sequence)

Retorna o número de elementos em sequence que dão match em *foo*.

Parâmetros adicionais: `:from-end`, `:start`, `:end`.

Veja também `count-if`, `count-not` *(test-function sequence)*.

#### subseq (sequence start, [end])

É passível de "setf", mas funciona apenas se a nova sequência tiver o mesmo
tamanho que a sequência a ser substituída.

#### sort, stable-sort (sequence, test [, key function])

Esta função é destrutiva, então você é de bom tom copiar a sequência
antes da ordenação:

    (sort (copy-seq seq) :test #'string<)

#### find, position (foo, sequence) - obter index

além de `find-if`, `find-if-not`, `position-if`, `position-if-not` *(teste sequência)*.
Veja também os parâmetros `:key`, `:test`.

~~~lisp
(find 20 '(10 20 30))
;; 20
(position 20 '(10 20 30))
;; 1
~~~

#### search (sequence-a, sequence-b)

Procura por uma subsequência em sequence-b que correspondente a sequence-a.
Retorna a posição da subsequência correspondente ou NIL. Possui os parâmetros
`from-end`, `end1/2` entre outros.

#### substitute, nsubstitute[if,if-not]

#### sort, stable-sort, merge

#### replace (sequence-a, sequence-b)

Substitui os elmentos de sequence-a pelos elementos de sequence-b.

#### remove, delete (foo sequence)
Faz uma cópia de sequence sem elementos que dêem match em foo. Possui os parâmetros
:start/end`, `:key` e `:count`.

`delete` faz o mesmo, pórem, dependendo do contexto ela pode destruir a sequência
original no processo.

~~~lisp
(remove "foo" '("foo" "bar" "foo") :test 'equal)
;; => ("bar")
~~~

Veja também `remove-if[-not]` a seguir.

### mapping (map, mapcar, remove-if[-not],...)

Se você já está familiarizado com map e filter em outras linguagens,
provavelmete o que você procura é `mapcar`. Mas ele funciona apenas em listas,
então para iterar em vetores e produzir tanto um vetor quanto uma lista, use `(map 'list function vector)`.

mapcar também aceita múltiplas listas com `&rest more-seqs`. O mapeamento termina
no momento em que a menor sequência chega ao fim.

_Nota: `map` do cl21 é um `mapcar` para listas e vetores._

`map` recebe o tipo de saída desejado como o primeiro argumento (`'list`, `'vector`, ou `'string`):

~~~lisp
(defparameter foo '(1 2 3))
(map 'list (lambda (it) (* 10 it)) foo)
~~~

**Filter**, aqui, é chamado de `remove-if-not`.

### Flatten a list (Alexandria)

Com
[Alexandria](https://common-lisp.net/project/alexandria/draft/alexandria.html),
temor a função `flatten`.


### Criando listas com variáveis

Este é um dos usos de `backquote`:

~~~lisp
(defparameter *var* "bar")
;; First try:
'("foo" *var* "baz") ;; sem backquote
;; => ("foo" *VAR* "baz") ;; errado
~~~

Segunda tentaiva, usando backquote interpolation.

~~~lisp
`("foo" ,*var* "baz")     ;; com backquote, vírgula
;; => ("foo" "bar" "baz") ;; correto
~~~

O backquote avisa que nós vamos fazer a interpolação e a vírgula repassa 
o valor da variável.

Se a nossa variável é uma lista:

~~~lisp
(defparameter *var* '("bar" "baz"))
;; First try:
`("foo" ,*var*)
;; => ("foo" ("bar" "baz")) ;; uma nested list
`("foo" ,@*var*)            ;; backquote, comma-@ to
;; => ("foo" "bar" "baz")
~~~

E. Weitz avisa que "objetos gerados desta forma muito provavelmete da mesma estrutura (ver Receita 2-7)".

### Comparando listas

É possível usar funções de set.
