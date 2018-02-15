# Terminologia

* No mundo de Common Lisp, um **package** (pacote) é uma forma de agrupar símbolos
  e prover encapsulamento. É similar a um *namespace* de C++, um *module* (módulo) de
  Python ou a um *package* de Java.
  
* Um **system** (sistema) é uma coleção de códigos-fonte de CL, agrupados com um
  arquivo .asd que informa como compilá-los e carregá-los. Às vezes, há
  um relacionamento próximo entre *system*s e *package*s, mas isto não é
  algo obrigatório. Um *system* pode declarar uma dependência por outro
  *system*. *System*s são gerenciados pelo [ASDF](https://common-lisp.net/project/asdf/asdf.html)
  (Another System Definition Facility), que oferece funcionalidades similares
  ao *make* e ao *ld.so*, e se tornou um padrão.
  
* Uma **library** (biblioteca) ou um **project** (projeto) de Common Lisp normalmente consiste de um ou
  vários *system*s ASDF (e é distribuído como um *project* Quicklisp).
