# rna-2023-1
Repositório para códigos da disciplina de Redes Neurais Artificais - UFMG 2023-1

# Configuração de ambiente para R e RStudio

- instale o R na máquina (para Windows) https://cran-r.c3sl.ufpr.br/ ou https://posit.co/download/rstudio-desktop/
- instale o RStudio (para Windows) https://posit.co/download/rstudio-desktop/
- altere o RStudio para o tema escuro em Tools -> Global Options -> Appearance
- localização do R instalado na máquina C:\Program Files\R\R-4.2.3\bin . Pegue esse local e adicione no PATH do sistema
- instale o RTools 4.2.x de https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html 
- instale a extensão do VS Code 'R'.
- rode `install.packages("langaugeserver")` pela extensão ou no CMD (entrar no R através de `r` no cmd, um vez adiciona no PATH)
- rode `install.packages("cli")` no terminal CMD, dentro do R.
- vefifique se o R instalado na máquina está vendo os libs instaladas através de RStudio -> Help -> RHelp -> References -> Packages
    - ali tem a lista de todos que ele está vendo

- ainda não consegui integrar o RStudio com LaTeX, mas está tranquilo (é o de menos nesse momento)

# Ambiente de Desenvolvimento R para RNA

- Usa o VS Code com as extensões e IntelliSense para desenvolver os arquivos .R
- Abra o RStudio no mesmo arquivo que está editando no VS Code para compilar e gerar gráficos
- Para documentar, abra uma segunda instância no VS Code para fazer o codigo LaTeX
    - integração RStudio + Miktex não funcionou, não posso perder muito mais tempo com isso.
