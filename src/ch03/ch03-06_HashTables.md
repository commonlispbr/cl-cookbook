## Hash Table

Hash Tables são poderosas estruturas de dados, associando chaves com valores
de uma forma muito eficiente. Hash Tables são preferíveis à association lists 
em casos que performance é essencial, porém introduzem um pouco de overhead,
o que torna assoc lists melhores caso a quantidade de pares key-value seja baixa.

Mas às vezes Alists podem ser usadas de forma diferentes:

- podem ser ordenadas
- é possível fazer um push em cons cells que possuem a mesma key,
  remover a primeira e obter uma pilha
- possuem uma representação sã aos olhos humanos
- podem ser fácilmente (de)serializadas
- devido ao RASSOC, chaves e valores em alists são essêncialmente intercâmbiáveis;
  já em hash tables chaves e valores desempenham papéis bem diferentes 
  (Veja CLRecipes para mais).


### Criando uma Hash Table

Hash Tables são criadas usando a função
[`make-hash-table`](http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_has.htm).
Ela não requer argumentos, e o seu argumento mais usado é `:test`,
especificando a função usada para testar a equalidade das chaves.

Se a biblioteca do [cl21](http://cl21.org/) for usada, é possível criar a hash table e adicionar elementos ao mesmo tempo com a nova sintáxe do reader `#H` :

~~~lisp
(defparameter *my-hash* #H(:name "Eitaro Fukamachi"))
~~~
e acessamos um elemento com

~~~lisp
(getf *my-hash* :name)
~~~

### Obtendo um valor de uma Hash Table

A função 
[`gethash`](http://www.lispworks.com/documentation/HyperSpec/Body/f_gethas.htm)
recebe dois argumentos obrigatórios: uma chave, e uma hash table. E retorna dois valores:
o valor correspondente à chave na hash table (ou `nil` se não for encontrado),
e um boolean indicando se a chave foi encontrada ou não. O segundo valor é necessário,
já que nil é um valor válido em um par chave-valor, então receber `nil` como primeiro 
argumento de gethash não significa necessáriamente que a chave não foi encontrada.

#### Obtendo uma chave que não existe com um valor default


`gethash` poossui um terceiro argumento opcional

~~~lisp
(gethash 'bar *my-hash* "default-bar")
;; => "default-bar"
;;     NIL
~~~

#### Obtendo todas as chaves ou todos os valores de uma hash table

A biblioteca
[Alexandria](https://common-lisp.net/project/alexandria/draft/alexandria.html)
(em Quicklisp) tem as funções `hash-table-keys` e `hash-table-values` com esse propósito.

~~~lisp
(ql:quickload :alexandria)
;; […]
(alexandria:hash-table-keys *my-hash*)
;; => (BAR)
~~~

### Adicionando um elemento a uma Hash Table

Se deseja adicionar um elemento a uma hash table, você pode usar a função `gethash`
para obter os elementos da hash table, em conjunto com 
[`setf`](http://www.lispworks.com/documentation/HyperSpec/Body/m_setf_.htm).

~~~lisp
CL-USER> (defparameter *my-hash* (make-hash-table))
*MY-HASH*
CL-USER> (setf (gethash 'one-entry *my-hash*) "one")
"one"
CL-USER> (setf (gethash 'another-entry *my-hash*) 2/4)
1/2
CL-USER> (gethash 'one-entry *my-hash*)
"one"
T
CL-USER> (gethash 'another-entry *my-hash*)
1/2
T
~~~


### Testando a presença de uma chave em uma Hash Table

O primeiro valor retornado por `gethash` é o objeto da hash table associado à chave
provida por você como argumento a `gethash`, ou `nil` caso nenhum valor exista para esta chave.
Esse valor pode ser usado como um 
[boolean generalizado](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_g.htm#generalized_boolean">generalizedboolean) 
se você deseja testar a existência de chaves.

~~~lisp
CL-USER> (defparameter *my-hash* (make-hash-table))
*MY-HASH*
CL-USER> (setf (gethash 'one-entry *my-hash*) "one")
"one"
CL-USER> (if (gethash 'one-entry *my-hash*)
           "Key exists"
           "Key does not exist")
"Key exists"
CL-USER> (if (gethash 'another-entry *my-hash*)
           "Key exists"
           "Key does not exist")
"Key does not exist"
~~~

Mas perceba que isso _não_ funciona caso `nil` esteja entre os valores que você
deseja pôr no hash.

~~~lisp
CL-USER> (setf (gethash 'another-entry *my-hash*) nil)
NIL
CL-USER> (if (gethash 'another-entry *my-hash*)
           "Key exists"
           "Key does not exist")
"Key does not exist"
~~~

Neste caso, você deve verificar o _segundo_ valor de retorno de `gethash`
que sempre retornará `nil` se nenhum valor for encontrado, e T caso contrário.


~~~lisp
CL-USER> (if (nth-value 1 (gethash 'another-entry *my-hash*))
           "Key exists"
           "Key does not exist")
"Key exists"
CL-USER> (if (nth-value 1 (gethash 'no-entry *my-hash*))
           "Key exists"
           "Key does not exist")
"Key does not exist"
~~~

### Deletando de uma Hash Table

Use
[`remhash`](http://www.lispworks.com/documentation/HyperSpec/Body/f_remhas.htm)
para deletar uma entrada. Tanto a chave quanto seu valor associado serão removidos
da tabela. `remhash` retorna T se a entrada existe, e  `nil` se não.

~~~lisp
CL-USER> (defparameter *my-hash* (make-hash-table))
*MY-HASH*
CL-USER> (setf (gethash 'first-key *my-hash*) 'one)
ONE
CL-USER> (gethash 'first-key *my-hash*)
ONE
T
CL-USER> (remhash 'first-key *my-hash*)
T
CL-USER> (gethash 'first-key *my-hash*)
NIL
NIL
CL-USER> (gethash 'no-entry *my-hash*)
NIL
NIL
CL-USER> (remhash 'no-entry *my-hash*)
NIL
CL-USER> (gethash 'no-entry *my-hash*)
NIL
NIL
~~~

### Percorrendo uma Hash Table

Se você deseja realizar uma ação em cada par chave-valor em uma hash table você pode usar:

[`maphash`](http://www.lispworks.com/documentation/HyperSpec/Body/f_maphas.htm)
que itera sobre todas as entradas na tabela. Seu primeiro argumento deve ser
uma função que aceita _dois_ argumentos, a chave e o valor de cada entrada.

Perceba que pela natureza das Hash Tables é _impossível_ controlar a ordem em que
as entradas são providas pela `maphash` (ou outras funções que percorram hash tables).
`maphash` sempre retorna `nil`.

~~~lisp
CL-USER> (defparameter *my-hash* (make-hash-table))
*MY-HASH*
CL-USER> (setf (gethash 'first-key *my-hash*) 'one)
ONE
CL-USER> (setf (gethash 'second-key *my-hash*) 'two)
TWO
CL-USER> (setf (gethash 'third-key *my-hash*) nil)
NIL
CL-USER> (setf (gethash nil *my-hash*) 'nil-value)
NIL-VALUE
CL-USER> (defun print-hash-entry (key value)
    (format t "The value associated with the key ~S is ~S~%" key value))
PRINT-HASH-ENTRY
CL-USER> (maphash #'print-hash-entry *my-hash*)
The value associated with the key FIRST-KEY is ONE
The value associated with the key SECOND-KEY is TWO
The value associated with the key THIRD-KEY is NIL
The value associated with the key NIL is NIL-VALUE
~~~

Também é possível usar
[`with-hash-table-iterator`](http://www.lispworks.com/documentation/HyperSpec/Body/m_w_hash.htm),
uma macro que faz do seu primeiro argumento um iterador (usando 
[`macrolet`](http://www.lispworks.com/documentation/HyperSpec/Body/s_flet_.htm))
que retorna três valores por hash table para cada vez que for invocada - um
boolean generalizado que é true se uma entrada for retornada, a chave para a entrada,
e o valor da entrada. Se não houverem mais entradas, apenas o valor é rotornado - `nil`

~~~lisp
;;; same hash-table as above
CL-USER> (with-hash-table-iterator (my-iterator *my-hash*)
           (loop
              (multiple-value-bind (entry-p key value)
                  (my-iterator)
                (if entry-p
                    (print-hash-entry key value)
                    (return)))))
The value associated with the key FIRST-KEY is ONE
The value associated with the key SECOND-KEY is TWO
The value associated with the key THIRD-KEY is NIL
The value associated with the key NIL is NIL-VALUE
NIL
~~~

TODO: Consigo traduzir isso não moisés
Tome nota da seguinte ressalva do Hyperspec: "Não é especificado o que pode
acontecer se qualquer estado interior implícito de uma iteração for retornada
fora da extensão dinamica da form `with-hash-table-iterator`, como retornar um
término da form de invocação.

E sempre existe a opção do [`loop`](http://www.lispworks.com/documentation/HyperSpec/Body/06_a.htm):

~~~lisp
;;; same hash-table as above
CL-USER> (loop for key being the hash-keys of *my-hash*
           do (print key))
FIRST-KEY
SECOND-KEY
THIRD-KEY
NIL
NIL
CL-USER> (loop for key being the hash-keys of *my-hash*
           using (hash-value value)
           do (format t "The value associated with the key ~S is ~S~%" key value))
The value associated with the key FIRST-KEY is ONE
The value associated with the key SECOND-KEY is TWO
The value associated with the key THIRD-KEY is NIL
The value associated with the key NIL is NIL-VALUE
NIL
CL-USER> (loop for value being the hash-values of *my-hash*
           do (print value))
ONE
TWO
NIL
NIL-VALUE
NIL
CL-USER> (loop for value being the hash-values of *my-hash*
           using (hash-key key)
           do (format t "~&~A -> ~A" key value))
FIRST-KEY -> ONE
SECOND-KEY -> TWO
THIRD-KEY -> NIL
NIL -> NIL-VALUE
NIL
~~~

**TODO: Procurar link certo pro cl21
Por último, existe também o `(doeach ((key val) *hash*) ...)` de [cl21](cl21.htm).

#### Percorrendo chaves ou valores

Para fazer um map pelas chaves ou pelos valores, é possível usar
`maphash-keys` e `maphash-values` de Alexandria.

### Contando as entradas em uma Hash Table

Não é preciso usar seus dedos (rs) - Common Lisp já possui uma função para isso
[`hash-table-count`](http://www.lispworks.com/documentation/HyperSpec/Body/f_hash_1.htm).

~~~lisp
CL-USER> (defparameter *my-hash* (make-hash-table))
*MY-HASH*
CL-USER> (hash-table-count *my-hash*)
0
CL-USER> (setf (gethash 'first *my-hash*) 1)
1
CL-USER> (setf (gethash 'second *my-hash*) 2)
2
CL-USER> (setf (gethash 'third *my-hash*) 3)
3
CL-USER> (hash-table-count *my-hash*)
3
CL-USER> (setf (gethash 'second *my-hash*) 'two)
TWO
CL-USER> (hash-table-count *my-hash*)
3
CL-USER> (clrhash *my-hash*)
#<EQL hash table, 0 entries {48205F35}>
CL-USER> (hash-table-count *my-hash*)
0
~~~

### Problemas de Performance: Tamanho da Hash Table

A função `make-hash-table` possui alguns parâmetros opcionais que controlam o
tamanho inicial da sua Hash Table e de que forma ela vai crescer, caso preciso.
Esse pode ser um iimportante problema de performance se você está trabalhando com 
hash tables grandes. Aqui está um exemplo (não muito científico) usando 
[CMUCL](http://www.cons.org/cmucl) pre-18d em um sistema Linux:

~~~lisp
CL-USER> (defparameter *my-hash* (make-hash-table))
*MY-HASH*
CL-USER> (hash-table-size *my-hash*)
65
CL-USER> (hash-table-rehash-size *my-hash*)
1.5
CL-USER> (time (dotimes (n 100000) (setf (gethash n *my-hash*) n)))
Compiling LAMBDA NIL:
Compiling Top-Level Form:

Evaluation took:
  0.27 seconds of real time
  0.25 seconds of user run time
  0.02 seconds of system run time
  0 page faults and
  8754768 bytes consed.
NIL
CL-USER> (time (dotimes (n 100000) (setf (gethash n *my-hash*) n)))
Compiling LAMBDA NIL:
Compiling Top-Level Form:

Evaluation took:
  0.05 seconds of real time
  0.05 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  0 bytes consed.
NIL
~~~

Os valores para 
[`hash-table-size`](http://www.lispworks.com/documentation/HyperSpec/Body/f_hash_4.htm)
e para
[`hash-table-rehash-size`](http://www.lispworks.com/documentation/HyperSpec/Body/f_hash_2.htm)
são dependentes da implementação. No nosso caso, CMUL escolhe um tamanho inicial
de 65, e vai aumentar o tamanho da tabela em 50% sempre que ela tiver que crescer.
Vejamos quantas vezes nós vamos ter que redimensionar o tamanho da nossa hash table
até que ela chegue ao seu tamanho final...

~~~lisp
CL-USER> (log (/ 100000 65) 1.5)
18.099062
CL-USER> (let ((size 65)) (dotimes (n 20) (print (list n size)) (setq size (* 1.5 size))))
(0 65)
(1 97.5)
(2 146.25)
(3 219.375)
(4 329.0625)
(5 493.59375)
(6 740.3906)
(7 1110.5859)
(8 1665.8789)
(9 2498.8184)
(10 3748.2275)
(11 5622.3413)
(12 8433.512)
(13 12650.268)
(14 18975.402)
(15 28463.104)
(16 42694.656)
(17 64041.984)
(18 96062.98)
(19 144094.47)
NIL
~~~

Ele teve que crescer 19 vezes até ser grande o suficiente para comportar 100.000 entradas.
Isto explica o porque de tantos consings e a relativa demora para preencher a tabela.
Explica também porque a segunda vez foi bem mais rápida - 
a hash table já estava com seu tamanho correto.

Aqui está uma forma mais rápida de fazer esse redimensionamento:
Se já soubermos de antemão quão grande a nossa tabela será, nós podemos de cara
começar com o tamanho exato:

~~~lisp
CL-USER> (defparameter *my-hash* (make-hash-table :size 100000))
*MY-HASH*
CL-USER> (hash-table-size *my-hash*)
100000
CL-USER> (time (dotimes (n 100000) (setf (gethash n *my-hash*) n)))
Compiling LAMBDA NIL:
Compiling Top-Level Form:

Evaluation took:
  0.04 seconds of real time
  0.04 seconds of user run time
  0.0 seconds of system run time
  0 page faults and
  0 bytes consed.
NIL
~~~

Esta forma é obviamente muito mais rápida. Além disso não envolveu consing
de forma alguma, pois não houve redimensionamento.
Se não se sabe o tamanho final, mas é possível estimar a taxa de crescimento
da hash table, pode-se passar este valor para `make-hash-table`. Ao se passar um
número inteito especifica-se um crescimento absoluto, já um numero de ponto flutuante
um crescimento relativo.

~~~lisp
CL-USER> (defparameter *my-hash* (make-hash-table :rehash-size 100000))
*MY-HASH*
CL-USER> (hash-table-size *my-hash*)
65
CL-USER> (hash-table-rehash-size *my-hash*)
100000
CL-USER> (time (dotimes (n 100000) (setf (gethash n *my-hash*) n)))
Compiling LAMBDA NIL:
Compiling Top-Level Form:

Evaluation took:
  0.07 seconds of real time
  0.05 seconds of user run time
  0.01 seconds of system run time
  0 page faults and
  2001360 bytes consed.
NIL
~~~

Relativamente rápido (apenas um redimensionamento é necessário) mas com 
muito mais consing, pois quase toda a hash table (tirando os 65 elementos iniciais)
teve de ser construída durante o loop.

Note também que você pode especificar o `rehash-threshold` enquanto cria
uma nova hash table. E por último, sua implementação pode _ignorar completamente_
os valores os valores passados por `rehash-size` e `rehash-threshold` ...
