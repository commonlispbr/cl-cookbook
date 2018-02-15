# Terminologia

* No mundo de Common Lisp, um **pacote** é uma forma de agrupar símbolos
  e prover encapsulamento. É similar a um namespace de C++, um módulo de
  Python ou a um pacote de Java.
  
* Um **sistema** é uma coleção de códigos-fonte de CL, agrupados com um
  arquivo .asd que informa como compilá-los e carregá-los. Às vezes, há
  um relacionamento próximo entre sistemas e pacotes, mas isto não é
  algo obrigatório. Um sistema pode declarar uma dependência por outro
  sistema. Sistemas são gerenciados pelo [ASDF](https://common-lisp.net/project/asdf/asdf.html)
  (Another System Definition Facility), que oferece funcionalidades similares
  ao make e ao ld.so, e se tornou um padrão.
  
* Uma biblioteca ou um projeto de Common Lisp normalmente consiste de um ou
  vários sistemas ASDF (e é distribuído como um projeto Quicklisp).
