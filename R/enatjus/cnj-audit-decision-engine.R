# Copiado de lab-dados/enatjus, R/cnj-audit-decision-engine.R, no commit d2ee1a07e8bd54fa0baa237bdee760a334958389.
# Só as definições que os scripts deste repositório usam, direta ou indiretamente;
# o texto de cada uma é o do pacote. Licença MIT, em R/enatjus/LICENSE.md.

cnj_audit_glmmtmb_latent_parameters <- function(model) {
  parameter_names <- tryCatch(
    rownames(stats::vcov(model, full = TRUE)),
    error = function(condition) character()
  )
  unique(grep("^theta_", parameter_names, value = TRUE))
}
