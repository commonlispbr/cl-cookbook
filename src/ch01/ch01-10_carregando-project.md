# Como carregar um *project* existente

Você criou seu novo *project*, ou tem um *project* existente, e você gostaria de trabalhar com
ele no REPL, mas o Quicklisp não o conhece. O que fazer?

Primeiramente, se você criá-lo ou cloná-lo em `~/quicklisp/local-projects`, você poderá
`(ql:quickload ...)` seu *project*, sem mais ressalvas.

Usualmente, você vai querer "entrar" no *system*, através do REPL, neste estágio:

~~~lisp
(use-package :my-project)
~~~

Por fim, você poderá compilar ou interpretar os códigos-fonte (`my-project.lisp`) com
`C-c C-k` ou `C-c C-c`<sup id="a1">[1](#f1)</sup> (*slime-compile-defun*) em um *form*,
e ver seu resultado no REPL.

Outra solução é usar a lista de *projects* conhecidos do ASDF:

~~~lisp
(pushnew "~/caminho/para/raiz/do/projeto/" asdf:*central-registry* :test #'equal)
~~~

E, já que o ASDF é integrado ao Quicklisp, nós podemos dar `quickload` no *project*.


Feliz hacking!


<b id="f1">1</b> N. do T.: Estes atalhos dizem respeito ao Emacs ou ao Portacle.
Não se preocupe por ainda não compreendê-los. [↩](#a1)
