
## Structures

Estruturas oferecem um maneira de armazenar dados em espaços determinados.
Elas suportam single inheritance.

Classes providas pelo Common Lisp Object System (CLOS) são mais flexiveis,
porém estruturas podem oferecer melhor performance (veja o exemplo no manual do SBCL).

### Criação

`defstruct`

~~~lisp
(defstruct person
   id name age)
~~~

Determinar espaços na momento de criação é opcional e por default `nil`.

Para colocar um valor default:

~~~lisp
(defstruct person
   id
   (name "john doe")
   age)
~~~

E para especificar o tipo após o valor default:

~~~lisp
(defstruct person
  id
  (name "john doe" :type string)
  age)
~~~

Se cria uma instância usando o construtor gerado da forma `make-` + `<nome-da-estrutura>`,
ou seja, `make-person`:

~~~lisp
(defparameter *me* (make-person))
*me*
#S(PERSON :ID NIL :NAME "john doe" :AGE NIL)
~~~

note que representações com o print podem ser lidas de volta com o reader.

Com um nome de tipo inválido:

~~~lisp
(defparameter *bad-name* (make-person :name 123))
~~~

```
Invalid initialization argument:
  :NAME
in call for class #<STRUCTURE-CLASS PERSON>.
   [Condition of type SB-PCL::INITARG-ERROR]
```

Nós podemos definir o construtor da estrutura para que a estrutura seja criada sem
usar argumentos de keywords, o que às vezes pode ser mais útil. Nós passamos o nome
e a ordem deos argumentos:

~~~lisp
(defstruct (person (:constructor create-person (id name age)))
     id
     name
     age)
~~~

Nosso novo construtor é `create-person`:

~~~lisp
(create-person 1 "me" 7)
#S(PERSON :ID 1 :NAME "me" :AGE 7)
~~~

Porém, o default `make-person` deixa de funcionar:

~~~lisp
(make-person :name "me")
;; debugger:
obsolete structure error for a structure of type PERSON
[Condition of type SB-PCL::OBSOLETE-STRUCTURE]
~~~

### Slot access

É possível acessar os slots com accessors criados por `<name-of-the-struct>-` + `slot-name`:

~~~lisp
(person-name *me*)
;; "john doe"
~~~

### Setting


Slots são passíveis de `setf`:

~~~lisp
(setf (person-name *me*) "Cookbook author")
(person-name *me*)
;; "Cookbook author"
~~~

### Predicado

~~~lisp
(person-p *me*)
T
~~~

### Single inheritance

Usando o argumento `:include <struct>`:

~~~lisp
(defstruct (female (:include person))
     (gender "female" :type string))
(make-female :name "Lilie")
;; #S(FEMALE :ID NIL :NAME "Lilie" :AGE NIL :GENDER "female")
~~~

### Limitações

Depois de uma mudança, intânncias não sofrem updates.

Se nós tentarmos adicionar um slot (`email` abaixo), temos a escolha de perder
todas as instâncias ou de continuar usando a nova definição de `person`, mas os
efeitos de redefinir uma estrutura não são determinados pelo padrão, então o melhor
a se fazer é recompilar e rodar novamente a parte que sofreu mudança.

~~~lisp
(defstruct person
       id
       (name "john doe" :type string)
       age
       email)

attempt to redefine the STRUCTURE-OBJECT class PERSON
incompatibly with the current definition
   [Condition of type SIMPLE-ERROR]

Restarts:
 0: [CONTINUE] Use the new definition of PERSON, invalidating already-loaded code and instances.
 1: [RECKLESSLY-CONTINUE] Use the new definition of PERSON as if it were compatible, allowing old accessors to use new instances and allowing new accessors to use old instances.
 2: [CLOBBER-IT] (deprecated synonym for RECKLESSLY-CONTINUE)
 3: [RETRY] Retry SLIME REPL evaluation request.
 4: [*ABORT] Return to SLIME's top level.
 5: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1002A0FFA3}>)
~~~

Se escolhermos reiniciar `0`, para usar a nova definição, não poderemos acessar `*me*`:

~~~lisp
*me*
obsolete structure error for a structure of type PERSON
   [Condition of type SB-PCL::OBSOLETE-STRUCTURE]
~~~

Portable Common Lisp não define maneiras de descobrir super/sub-structures,
nem que slots uma estrutura possui.

Common Lisp Object System (que veio depois da linguagem) não tem tais limitações.
Veja a seção sobre [CLOS](clos.html).

* [structures on the hyperspec](http://www.lispworks.com/documentation/HyperSpec/Body/08_.htm)
* David B. Lamkins, ["Successful Lisp, How to Undertsand and Use Common Lisp"](http://www.communitypicks.com/r/lisp/s/17592186045679-successful-lisp-how-to-understand-and-use-common).
