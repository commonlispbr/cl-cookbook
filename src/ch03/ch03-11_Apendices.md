## Apêndice A - acesso genérico de alists, plists, hash-tables e CLOS slots

As soluções apresentadas abaixo pode lhe ajudar no começo, mas tenha em mente que elas terão
impactos em performance e mensagens de erro serão menos explícitas.

* [CL21](cl21.html) tem um `getf` genérico (bem como outras funções genéricas),
* [rutils](https://github.com/vseloved/rutils) como um `generic-elt` genérico, ou `?`,
* a biblioteca [access](https://github.com/AccelerationNet/access) (testada em batalha, usada pelo sistema de templating Djula) tem um generico `(access my-var :elt)` ([blog post](https://lisp-journey.gitlab.io/blog/generice-consistent-access-of-data-structures-dotted-path/)).

## Apêndice B - acessando estruturas de dados aninhadas

As vezes trabalhamos com estruturas de dados aninhadas, e queremos uma maneira
mais simples de acessar um elemento aninhado comparado a um intrincado "getf" e "assoc"
etc. Além disso, quando uma chave intermediária não existe seja retornado apenas um `nil`.

A biblioteca `access` mostrada acima provê isso, com `(accesses var key1 key2...)`.
