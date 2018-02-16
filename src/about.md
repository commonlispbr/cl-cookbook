> **ATENÇÃO: ESTE LIVRO ESTÁ ALTAMENTE INCOMPLETO!**
> O objetivo é prover uma tradução apropriada para o Common Lisp Cookbook
> original, em concordância com o licenciamento e com o texto do mesmo.
> Visite o repositório no GitHub e nos ajude a traduzi-lo.

# Common Lisp Cookbook
> Cookbook, subst.
> um livro contendo receitas e outras informações sobre a preparo e cozimento de comida.

*agora com Lisp extra (e com cobertura de Português do Brasil)*

## Informações

Este é um projeto colaborativo focado em prover, para Common Lisp, algo
similar ao [Perl Cookbook][perl], publicado pela O'Reilly. Mais detalhes
sobre o que este livro é e o que ele não é podem ser encontrados neste [tópico][thread]
encontrado em [comp.lang.lisp][cll].

Se você quer contribuir com o CL Cookbook, por favor, envie um pull request ou preencha
uma issue no GitHub!

Sim, estamos falando com você! Precisamos de mais contribuidores - escreva um capítulo
que está faltando e adicione-o, encontre uma pergunta em aberto e dê uma resposta,
encontre bugs e reporte-os (se você não tem ideia do que falta mas gostaria de ajudar,
dê uma olhada no [índice][toc] do Perl Cookbook). Não se preocupe com a formatação,
apenas envie texto simples se você quiser - cuidaremos disso depois.

Desde já, obrigado pela sua ajuda!


## Notas dos tradutores

Este livro é uma tradução direta, para o Português brasileiro, do [Common Lisp Cookbook][original],
pela comunidade [Common Lisp Brasil][clbr].

Caso haja erros de tradução ou de Português nestas páginas, você está livre para contribuir
no mesmo formato provido acima, ou poderá abrir uma issue no repositório desta tradução.

Nosso foco primário é na pura tradução do Cookbook original, portanto, se você está interessado
em contribuir com conteúdo novo, por favor, dirija-se ao [repositório original][orig-repo].


## Contribuidores

* Marco Antoniotti
* [Zach Beane](mailto:xach@xach.com)
* Pierpaolo Bernardi
* [Christopher Brown](mailto:skeptomai@mac.com)
* [Frederic Brunel](mailto:brunel@mail.dotcom.fr)
* [Jeff Caldwell](mailto:jdcal@yahoo.com)
* [Bill Clementson](mailto:bill_clementson@yahoo.com)
* Martin Cracauer
* [Gerald Doussot](mailto:gdoussot@yahoo.com)
* [Paul Foley](mailto:mycroft@actrix.gen.nz)
* Jörg-Cyril Höhle
* [Nick Levine](mailto:ndl@ravenbrook.com)
* [Austin King](mailto:shout@ozten.com)
* [Lieven Marchand](mailto:mal@wyrd.be)
* [Drew McDermott](mailto:drew.mcdermott@yale.edu)
* [Kalman Reti](mailto:reti@ai.mit.edu)
* [Alberto Riva](mailto:alb@chip.org)
* [Rudi Schlatte](mailto:rschlatte@ist.tu-graz.ac.at)
* [Emre Sevinç](mailto:emres@bilgi.edu.tr)
* Paul Tarvydas
* Kenny Tilton
* [Reini Urban](mailto:rurban@x-ray.at)
* [Matthieu Villeneuve](mailto:matthieu@matthieu-villeneuve.net)
* [Edi Weitz](mailto:edi@agharta.de)
* Fernando Borretti

Finalmente, o crédito por finalmente dar a luz ao projeto provavelmente
vai para "dj\_special\_ed", que postou [esta mensagem][msg] em [comp.lang.lisp][cll].

## Tradutores

* [Lucas Vieira](mailto:lucasvieira@protonmail.com)

## Outros recursos

* [lisp-lang.org](http://lisp-lang.org/)
* A lista [Awesome-cl](https://github.com/CodyReichert/awesome-cl)
* [O Common Lisp HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm), por Kent M. Pitman
* [O Common Lisp UltraSpec](http://phoe.tymoon.eu/clus/doku.php)
* [Practical Common Lisp](http://www.gigamonkeys.com/book/), por Peter Seibel
* [Common Lisp Recipes](http://weitz.de/cl-recipes/) por Edmund Weitz, publicado em 2016
* [Cliki](http://www.cliki.net/), a Wiki de Common Lisp
* [Articulate Common Lisp](http://articulate-lisp.com/), um tutorial de iniciação para os não-iniciados
* [Common Lisp](https://en.wikibooks.org/wiki/Common_Lisp) no Wikibooks
* [O velho FAQ do comp.lang.lisp](http://www-2.cs.cmu.edu/Groups//AI/html/faqs/lang/lisp/top.html), por Mark Kantrowitz
* [Common Lisp: A Gentle Introduction to Symbolic Computation](http://www-2.cs.cmu.edu/~dst/LispBook/), por David S. Touretzky
* [Successful Lisp: How to Understand and Use Common Lisp](http://www.psg.com/~dlamkins/sl/cover.html), por David B. Lamkins
* [On Lisp](http://www.paulgraham.com/onlisptext.html), por Paul Graham
* [Common Lisp the Language, 2nd Edition](http://www-2.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html), por Guy L. Steele
* [Common Lisp Hints](http://www.n-a-n-o.com/lisp/cmucl-tutorials/LISP-tutorial.html), por Geoffrey J. Gordon
* [A Guide to CLOS](http://www.aiai.ed.ac.uk/~jeff/clos-guide.html), por Jeff Dalton
* [Common Lisp Pitfalls](http://www.aiai.ed.ac.uk/~jeff/lisp/cl-pitfalls), por Jeff Dalton
* [Tutorial para o macro Loop de Common Lisp](http://www.ai.sri.com/~pkarp/loop.html), por Peter D. Karp
* [Um Tutorial para um bom estilo de Lisp](https://www.cs.umd.edu/%7Enau/cmsc421/norvig-lisp-style.pdf), por Peter Norvig e Kent Pitman
* [Lisp and Elements of Style](http://www.nicklevine.org/declarative/lectures/), por Nick Levine
* [Guia Altamente Opinativo de Lisp](http://www.p-cos.net/lisp/guide.html), de Pascal Constanza
* [Loving Lisp - the Savy Programmer's Secret Weapon](https://leanpub.com/lovinglisp/), por Mark Watson
* [FranzInc](https://franz.com/), uma empresa que vende soluções em Common Lisp e Bancos de Dados Gráficos.


[original]: https://lispcookbook.github.io/cl-cookbook/
[orig-repo]: https://github.com/LispCookbook/cl-cookbook/
[clbr]: https://lisp.com.br
[perl]: http://www.oreilly.com/catalog/cookbook/
[thread]: http://groups.google.com/groups?threadm=m3it9soz3m.fsf%40bird.agharta.de
[cll]: news:comp.lang.lisp
[msg]: http://groups.google.com/groups?selm=76be8851.0201222259.70ecbcb1%40posting.google.com
[toc]: http://www.oreilly.com/catalog/cookbook/toc.html
[zip]: https://github.com/LispCookbook/cl-cookbook/archive/master.zip
[gh]: https://github.com/LispCookbook/cl-cookbook
[clog]: https://github.com/can3p/cl-cookbook/blob/master/CHANGELOG
