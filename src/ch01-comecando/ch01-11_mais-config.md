# Mais configurações

Você pode querer definir o formato padrão de codificação do SBCL para UTF-8:

	(setf sb-impl::*default-external-format* :utf-8)
	
Você pode adicionar isso ao seu `~/sbclrc`.
