# Funções do pacote enatjus que este repositório usa ------------------------------
#
# O pacote R do laboratório (lab-dados/enatjus) é privado. Para que a análise
# rode a partir de um clone, as definições que os scripts usam, e as que essas
# usam por sua vez, foram copiadas para R/enatjus/, uma cópia por arquivo de
# origem, com o texto do pacote e a licença MIT dele. A única adaptação é a
# leitura da base (R/enatjus/read-base.R), que aponta a base pública no Hugging
# Face em vez da base interna da coleta.
#
# Os scripts dão source() neste arquivo em vez de library(enatjus). Dar source
# de novo só redefine as mesmas funções, então scripts que se carregam uns aos
# outros podem fazê-lo sem cuidado com a ordem.

# Commit do pacote de que as definições foram copiadas. Os carimbos de
# proveniência gravam este valor onde antes gravavam o commit instalado.
ENATJUS_COMMIT <- "d2ee1a07e8bd54fa0baa237bdee760a334958389"

for (arquivo_enatjus in sort(list.files("R/enatjus", pattern = "[.]R$", full.names = TRUE))) {
  source(arquivo_enatjus, encoding = "UTF-8")
}
rm(arquivo_enatjus)
