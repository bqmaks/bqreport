# Deterministic identifiers
#
# Registry, plan, and contrast identifiers are content digests of their
# defining components, so identical inputs compile to byte-identical plans
# and results across sessions. Random identifiers would make two runs of the
# same analysis incomparable, which contradicts the reproducibility contract
# of the package.

bq_id <- function(prefix, ...) {
  paste0(prefix, "_", digest::digest(list(...), algo = "xxhash64"))
}

# Namespace a base plan-row identifier by analysis type and any identity
# components that specialized planners assign after `analysis_plan_row()`.
refine_analysis_id <- function(row, ...) {
  row$analysis_id <- bq_id(
    "analysis", row$analysis_type[[1]], row$analysis_id[[1]], ...
  )
  row
}

# Deterministic variable identifiers reuse the column name under which a
# variable was first registered. A released name can later be reused by a
# new column, so freshly created registry entries are suffixed until unique.
uniquify_fresh_ids <- function(ids, fresh) {
  for (i in fresh) {
    candidate <- ids[[i]]
    suffix <- 1L
    while (candidate %in% ids[-i]) {
      suffix <- suffix + 1L
      candidate <- paste0(ids[[i]], "_", suffix)
    }
    ids[[i]] <- candidate
  }
  ids
}
