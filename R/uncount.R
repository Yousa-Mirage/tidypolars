#' Uncount a Data/LazyFrame
#'
#' This duplicates rows according to a weighting variable (or expression). This
#' is the opposite of `count()`.
#'
#' @param data A Polars Data/LazyFrame
#' @param weights A vector of weights. Evaluated in the context of `data`.
#' @inheritParams rlang::check_dots_empty0
#' @param .remove If `TRUE`, and weights is the name of a column in data, then
#' this column is removed.
#' @param .id Supply a string to create a new variable which gives a unique
#' identifier for each created row.
#'
#' @export
#' @examplesIf require("dplyr", quietly = TRUE) && require("tidyr", quietly = TRUE)
#' test <- polars::pl$DataFrame(x = c("a", "b"), y = 100:101, n = c(1, 2))
#' test
#'
#' uncount(test, n)
#'
#' uncount(test, n, .id = "id")
#'
#' # using constants
#' uncount(test, 2)
#'
#' # using expressions
#' uncount(test, 2 / n)
uncount.polars_data_frame <- function(
  data,
  weights,
  ...,
  .remove = TRUE,
  .id = NULL
) {
  data <- tag_frame(data, substitute(data))
  check_dots_empty()
  check_bool(.remove)
  check_name(.id, allow_null = TRUE)

  weights_quo <- enquo(weights)
  grps <- attributes(data)$pl_grps
  mo <- attributes(data)$maintain_grp_order %||% FALSE
  repeat_expr <- translate_expr(
    data,
    weights_quo,
    new_vars = NULL,
    env = rlang::current_env()
  )

  weight_name <- if (
    quo_is_symbol(weights_quo) &&
      quo_name(weights_quo) %in% repeat_expr$meta$root_names()
  ) {
    quo_name(weights_quo)
  }

  # Repeating rows doesn't depend on any user column: use a dummy column as
  # carrier of `repeat_by()` and drop it afterwards.
  dummy <- ".tidypolars__uncount_dummy"
  out <- data

  if (!is.null(.id)) {
    # Tag each original row so that `.id` can restart from 1 for each of them,
    # even if some rows have identical content.
    rowid <- ".tidypolars__uncount_rowid"
    out <- out$with_row_index(rowid)
  }

  # `empty_as_null = FALSE` drops rows whose weight is 0
  out <- out$with_columns(
    pl$lit(1L)$repeat_by(repeat_expr)$alias(dummy)
  )$explode(dummy, empty_as_null = FALSE)

  if (!is.null(.id)) {
    out <- out$with_columns(
      pl$col(dummy)$cum_count()$over(rowid)$cast(pl$Int32)$alias(.id)
    )
  }

  # Only a bare column name is removed, not a column used in an expression
  drop_cols <- c(dummy, if (!is.null(.id)) rowid)
  # If `.id` has the same name as the weights column, it already contains the
  # `.id` values above (as in tidyr), so don't drop it.
  if (
    isTRUE(.remove) && !is.null(weight_name) && !identical(weight_name, .id)
  ) {
    drop_cols <- c(drop_cols, weight_name)
    grps <- setdiff(grps, weight_name)
  }
  out <- out$drop(drop_cols)

  if (length(grps) > 0) {
    out <- group_by(out, all_of(grps), maintain_order = mo)
  }
  add_tidypolars_class(out)
}

#' @rdname uncount.polars_data_frame
#' @export
uncount.polars_lazy_frame <- uncount.polars_data_frame
