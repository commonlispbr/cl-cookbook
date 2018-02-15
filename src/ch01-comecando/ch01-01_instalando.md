## Instalando uma implementação

### com seu gerenciador de pacotes

*TL;DR*:

	apt-get install sbcl

Common Lisp foi padronizado através de um documento ANSI, então ele pode ser implementado de diversas
formas; veja a [lista de implementações da Wikipédia](https://en.wikipedia.org/wiki/Common_Lisp#Implementations).

As implementações a seguir são empacotadas para o Debian e provavelmente também para a sua distro:

* [SBCL](http://www.sbcl.org/)
* [ECL](https://gitlab.com/embeddable-common-lisp/ecl/)
  (compila para C)
* [CMUCL](https://gitlab.common-lisp.net/cmucl/cmucl)
* [GCL](https://en.wikipedia.org/wiki/GNU_Common_Lisp)
* [CLISP](https://clisp.sourceforge.io/)

Em dúvida, apenas use o SBCL.

Veja também:

* [ABCL](http://abcl.org/) (para a JVM),
* [ClozureCL](https://ccl.clozure.com/)
* [CLASP](https://github.com/drmeister/clasp) (C++ e LLVM)
* [AllegroCL](https://franz.com/products/allegrocl/) (proprietário)
* [LispWorks](http://www.lispworks.com/) (proprietário)

e este [pacote Debian para Clozure CL](http://mr.gy/blog/clozure-cl-deb.html).

### com Roswell

[Roswell](https://github.com/roswell/roswell/wiki) é:

* um gerenciador de implementações: torna mais fácil instalar uma implementação de Common Lisp
  (`ros install ecl`), uma exata versão de uma implementação (`ros install sbcl/1.2.0`),
  e mudar para uma implementação padrão (`ros use ecl`);
* um ambiente de scripting (ajuda a executar Lisp através do shell, obter argumentos de linha de comando, ...);
* um instalador de scripts;
* um ambiente para testes (para executar testes, incluindo em plataformas populares de Integração Contínua);
* uma ferramenta de compilação (para compilar imagens e executáveis de forma portátil).

Você encontrará diversas formas de instalação na Wiki do Roswell (pacote Debian, instalador Windows,
Brew/Linux Brew, etc).

### com Docker

Se você já conhece o [Docker](https://docs.docker.com), você pode começar a usar Common Lisp rapidamente.
A imagem [daewok/lisp-devel-docker](https://github.com/daewok/lisp-devel-docker) inclui as versões
recentes de SBCL, CCL, ECL e ABCL, além de Quicklisp instalado na pasta home (`/home/lisp`) para
que possamos executar `ql:quickload` logo de cara.

Funciona em GNU/Linux, Mac e Windows.

O comando a seguir baixará a imagem requerida (mais ou menos 400MB), colocará seus arquivos de código
locais dentro da imagem Docker, onde indicado, e mostrará o REPL do SBCL:

	docker run --rm -it -v /path/to/local/code:/usr/local/share/common-lisp/source daewok/lisp-devel:base sbcl`

Mas nos ainda queremos desenvolver usando Emacs e Slime, então precisamos conectar o Slime ao Lisp dentro do
Docker. Veja [slime-docker](https://github.com/daewok/slime-docker) para uma biblioteca que o ajudará a
configurar isto.
