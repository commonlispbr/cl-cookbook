## Iniciando um REPL

Apenas abra o executável da implementação na linha de comando para entrar no
REPL (*Read Eval Print Loop*, ou *Laço de Leitura-Interpretação-Escrita*).

Saia com `(quit)` ou `ctrl-d)` (em algumas implementações).

Eis um exemplo de uma sessão:
```
user@debian:~$ sbcl
This is SBCL 1.3.14.debian, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (+ 1 2)

3
* (quit)
user@debian:~$
```

Você pode melhorar um pouco o REPL (as teclas de setas não funcionam, ele não tem
um histórico de comandos, ....) com o programa `rlwrap`:

	apt-get install rlwrap
	
E então:

	rlwrap sbcl

Mas nós configuraremos nosso editor para oferecer uma melhor experiência, ao invés
de trabalhar diretamente neste REPL. Veja [editor-support](#).

**TODO: subsituir o link acima.**
