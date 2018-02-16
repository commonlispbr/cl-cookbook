# Criando um novo *project*

Alguns criadores de *projects* ajudam a criar a estrutura do projeto.
Gostamos de [cl-project](https://github.com/fukamachi/cl-project), que
também configura um esqueleto para testes.

Em resumo:

~~~lisp
(ql:quickload "cl-project")
(cl-project:make-project #P"./caminho/para/raiz/do/projeto/")
~~~

Isto criará uma estrutura de diretório como essa:


```
|-- my-project.asd
|-- my-project-test.asd
|-- README.markdown
|-- README.org
|-- src
|   `-- my-project.lisp
`-- tests
    `-- my-project.lisp
```

Onde `my-project.asd` lembra o seguinte:

~~~lisp
(defsystem "my-project"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()  ;; <== list of Quicklisp dependencies
  :components ((:module "src"
                :components
                ((:file "my-project"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "my-project-test"))))
~~~

E `src/my-project.lisp` lembra isto:


~~~lisp
(defpackage footest
  (:use :cl))
(in-package :footest)
~~~

- Documentação do ASDF: [definindo um sistema com `defsystem`](https://common-lisp.net/project/asdf/asdf.html#Defining-systems-with-defsystem)
