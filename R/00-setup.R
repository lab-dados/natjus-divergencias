# Confere que esta máquina tem o que a análise precisa: os pacotes R de
# replication-r-packages.csv e a base do artigo, baixada do Hugging Face por
# tools/baixar-base.sh. Rodar uma vez por máquina, antes de qualquer script em
# R/. Não instala nada: a lista diz o que instalar, e a versão registrada é a
# da rodada do artigo.

source("R/enatjus.R")

pacotes <- read.csv("replication-r-packages.csv", comment.char = "#")
pacotes <- pacotes[!pacotes$base_r, ]
instalados <- installed.packages()
faltando <- setdiff(pacotes$package, rownames(instalados))
if (length(faltando)) {
  stop("Pacotes R ausentes: ", paste(faltando, collapse = ", "),
       ". As versões da rodada do artigo estão em replication-r-packages.csv.", call. = FALSE)
}
# Versão diferente não impede a execução, mas o nível 2 compara o resultado
# byte a byte com os arquivos versionados, e uma versão diferente de um pacote
# de modelagem pode mudar a última casa decimal.
versao <- instalados[pacotes$package, "Version"]
diferentes <- pacotes$package[versao != pacotes$version]
if (length(diferentes)) {
  warning("Versões diferentes das da rodada do artigo: ",
          paste0(diferentes, " ", versao[diferentes], collapse = ", "), call. = FALSE)
}

# read_analysis_base() confere o SHA-256 e para se o arquivo não for o do artigo.
base <- read_analysis_base()

dir.create("logs", showWarnings = FALSE)
writeLines(
  c(paste("enatjus commit (código copiado em R/enatjus/):", ENATJUS_COMMIT),
    paste("base:", BASE_HF_DATASET, "revisão", BASE_HF_REVISION, BASE_HF_FILE),
    paste("base sha256:", BASE_SHA256), "", capture.output(utils::sessionInfo())),
  "logs/setup-session-info.txt"
)
message("Pacotes presentes e base conferida: ", format(nrow(base), big.mark = ".", decimal.mark = ","), " notas.")
