# Instalando o Quicklisp

[Quicklisp](https://www.quicklisp.org/beta/) é mais que um gerenciador de pacotes,
ele também é um repositório central (um *dist*) que assegura que todas as bibliotecas
compilem juntas.

Ele providencia seu próprio *dist*, mas também é possível criar o seu próprio.

Para instalá-lo, nós podemos:

1 - Executar o seguinte comando, em qualquer lugar:

	curl -O https://beta.quicklisp.org/quicklisp.lisp
	
e entrar em um REPL Lisp e carregar o arquivo baixado:

	sbcl --load quicklisp.lisp
	

Ou:

2 - Instalar o pacote Debian:

	apt-get install cl-quicklisp

e carregá-lo, de um REPL:

~~~lisp
(load "/usr/share/cl-quicklisp/quicklisp.lisp")
~~~


E então, em ambos os casos, ainda através do REPL:

~~~lisp
(quicklisp-quickstart:install)
~~~

Isto criará o diretório `~/quicklisp/`, onde Quicklisp manterá seu estado e
seus projetos baixados.

Se você quer que o Quicklisp seja sempre carregado em suas sessões Lisp, execute
`(ql:add-to-init-file)`: isto adicionará os comandos certos ao arquivo de inicialização
da sua implementação de CL. Do contrário, você deverá executar `(load "~/quicklisp/setup.lisp")`
em cada sessão, se você quiser utilizar o Quicklisp ou uma das bibliotecas instaladas através do
mesmo.

Este comando adiciona o seguinte em (por exemplo) seu arquivo `~/.sbclrc`:

~~~lisp
#-quicklisp
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
~~~

