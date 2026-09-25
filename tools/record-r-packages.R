# Grava replication-r-packages.csv: a versão instalada de cada pacote R que os
# scripts carregam, lida dos próprios scripts. É o registro do ambiente enquanto
# o projeto não tem lockfile; não instala nada.
#
# Uso: Rscript tools/record-r-packages.R
scripts <- list.files(c("R", "tools"), pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
codigo <- unlist(lapply(scripts, readLines, warn = FALSE))
codigo <- codigo[!grepl("^\\s*#", codigo)]
# Duas formas de carregar: library(pacote) e pacote::objeto (ou :::).
padrao <- "(?<=library\\()[A-Za-z0-9.]+|[A-Za-z][A-Za-z0-9.]*(?=:::?[A-Za-z_.])"
pacotes <- sort(unique(unlist(regmatches(codigo, gregexpr(padrao, codigo, perl = TRUE)))))
instalados <- installed.packages()
linha <- match(pacotes, rownames(instalados))
saida <- data.frame(
  package = pacotes,
  version = unname(instalados[linha, "Version"]),
  base_r = !is.na(instalados[linha, "Priority"]) & instalados[linha, "Priority"] == "base"
)
cabecalho <- c(paste("# r_version:", R.version.string), paste("# platform:", R.version$platform))
writeLines(cabecalho, "replication-r-packages.csv")
suppressWarnings(write.table(saida, "replication-r-packages.csv", sep = ",", row.names = FALSE, quote = FALSE, append = TRUE))
cat(nrow(saida), "pacotes registrados;", sum(is.na(saida$version)), "não instalados\n")
