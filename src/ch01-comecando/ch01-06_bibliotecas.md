# Instalando *libraries*

No REPL:

~~~lisp
(ql:quickload "nome-do-package")
~~~

e *voilà*. Veja a documentação do Quicklisp para mais comandos.

Note, também, que dezenas de *libraries* Common Lisp estão empacotadas como pacotes
Debian. O nome dos pacotes normalmente começam com o prefixo `cl-` (use
`apt-cache search --names-only "^cl-.*"` para listar todos eles).

Por exemplo, para utilizar a *library* CL-PPCRE (para expressões regulares), deve-se,
primeiramente, instalar o pacote `cl-ppcre`.

Então, no SBCL ou no ECL, este pode ser utilizado com:

~~~lisp
(require "asdf")
(require "cl-ppcre")
(cl-ppcre:regex-replace "fo+" "foo bar" "frob")
~~~

Veja mais: [https://wiki.debian.org/CommonLisp](https://wiki.debian.org/CommonLisp)

