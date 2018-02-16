# Gerenciamento avançado de dependências

Perceba que estas informações não são necessárias para começar.

Quicklisp instala as *libraries* no diretório `~/quicklisp/local-projects/`.
Uma *library* ali instalada estará automaticamente disponível para qualquer projeto.

## Fornecendo sua própria versão de uma biblioteca: Clonando *projects*

Dada a propriedade acima, podemos clonar qualquer *library* para o diretório
`local-projects` e ele será encontrado pelo Quicklisp e disponível logo em seguida:

~~~lisp
(ql:quickload "package")
~~~

## Como trabalhar com versões locais de *libraries*

Se precisarmos de que bibliotecas sejam instaladas localmente, para apenas um projeto,
ou para facilmente embarcar uma lista de dependências com uma aplicação, podemos utilizar
o [Qlot](https://github.com/fukamachi/qlot).

Quicklisp também fornece [bundles Quicklisp](https://www.quicklisp.org/beta/bundles.html).
São conjuntos independentes de *system*s que são exportados do Quicklisp e carregáveis
sem envolvê-lo.

Por fim, há também o [Quicklisp controller](https://github.com/quicklisp/quicklisp-controller),
para nos ajudar a construir *dists*.
