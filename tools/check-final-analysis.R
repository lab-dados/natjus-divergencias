source("R/08-final-analysis.R")

call_heads <- function(expression) {
  if (is.call(expression)) {
    head <- if (is.symbol(expression[[1L]])) as.character(expression[[1L]]) else character()
    nested <- unlist(lapply(as.list(expression)[-1L], call_heads), use.names = FALSE)
    return(c(head, nested))
  }
  if (is.expression(expression) || is.pairlist(expression)) {
    return(unlist(lapply(as.list(expression), call_heads), use.names = FALSE))
  }
  character()
}

reachable_local_functions <- function(root) {
  pending <- root
  visited <- character()
  called <- character()
  while (length(pending)) {
    name <- pending[[1L]]
    pending <- pending[-1L]
    if (name %in% visited) next
    visited <- c(visited, name)
    function_object <- get(name, envir = globalenv(), inherits = FALSE)
    current_calls <- unique(call_heads(body(function_object)))
    called <- union(called, current_calls)
    local_calls <- current_calls[vapply(current_calls, function(candidate) {
      exists(candidate, envir = globalenv(), inherits = FALSE) &&
        is.function(get(candidate, envir = globalenv(), inherits = FALSE))
    }, logical(1))]
    pending <- union(pending, setdiff(local_calls, visited))
  }
  list(functions = visited, calls = called)
}

check_final_path_excludes_bootstrap <- function() {
  graph <- reachable_local_functions("run_final_analysis")
  forbidden_calls <- c(
    "checkpoint_compute", "fit_one_estimator", "fit_main_models",
    "fit_origin_explanatory_models", "fit_reporting_candidates",
    "profile_origin_sd_interval", "run_family_bootstrap",
    "bootstrap_one_family", "parametric_response", "origin_cluster_resample",
    "bootstrap_worker_default"
  )
  present <- intersect(forbidden_calls, graph$calls)
  if (length(present)) {
    stop(
      "Final publication path reaches bootstrap or checkpoint-writing calls: ",
      paste(present, collapse = ", "),
      call. = FALSE
    )
  }
  forbidden_rds <- c(
    "final-analysis-bootstrap-main-parametric.rds",
    "final-analysis-bootstrap-main-origin.rds",
    "final-analysis-bootstrap-origin-explanatory-parametric.rds",
    "final-analysis-bootstrap-origin-explanatory-origin.rds"
  )
  if (any(basename(model_output_paths()) %in% forbidden_rds)) {
    stop("Bootstrap RDS files remain in the final model bundle.", call. = FALSE)
  }
  cat("Final publication path check passed: no model fitting, bootstrap execution, replica checkpoint creation or bootstrap RDS publication is reachable.\n")
  invisible(graph)
}

arguments <- commandArgs(trailingOnly = TRUE)
allowed <- c("--synthetic-only", "--real-preflight")
if (length(arguments) > 1L || (length(arguments) == 1L && !arguments %in% allowed)) {
  stop(
    "Usage: Rscript tools/check-final-analysis.R [--synthetic-only|--real-preflight]",
    call. = FALSE
  )
}

check_final_path_excludes_bootstrap()
run_synthetic_checks()
if (!identical(arguments, "--synthetic-only")) {
  check_real_preflight()
}
