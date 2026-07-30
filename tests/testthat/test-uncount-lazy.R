### [GENERATED AUTOMATICALLY] Update test-uncount.R instead.

Sys.setenv('TIDYPOLARS_TEST' = TRUE)

test_that("basic behavior works", {
  test_df <- tibble(x = c("a", "b"), y = 100:101, n = c(1L, 2L))
  test_pl <- as_polars_lf(test_df)

  expect_is_tidypolars(uncount(test_pl, n))

  expect_equal_lazy(
    uncount(test_pl, n),
    uncount(test_df, n)
  )

  expect_equal_lazy(
    uncount(test_pl, n, .id = "id"),
    uncount(test_df, n, .id = "id")
  )

  expect_equal_lazy(
    uncount(test_pl, n, .remove = FALSE),
    uncount(test_df, n, .remove = FALSE)
  )
})

test_that("works with constant", {
  test_df <- tibble(x = c("a", "b"), y = 100:101, n = c(1L, 2L))
  test_pl <- as_polars_lf(test_df)

  expect_equal_lazy(
    uncount(test_pl, 2),
    uncount(test_df, 2)
  )

  expect_equal_lazy(
    uncount(test_pl, 0),
    uncount(test_df, 0)
  )
})

test_that("works with expression", {
  test_df <- tibble(x = c("a", "b"), y = 100:101, n = c(1L, 2L))
  test_pl <- as_polars_lf(test_df)

  expect_equal_lazy(
    uncount(test_pl, 2 / n),
    uncount(test_df, 2 / n)
  )
})

test_that("works without a column named `x`", {
  test_df <- tibble(a = c("a", "b"), n = c(1L, 2L))
  test_pl <- as_polars_lf(test_df)

  expect_equal_lazy(
    uncount(test_pl, n),
    uncount(test_df, n)
  )
})

test_that("rows with weight 0 are dropped", {
  test_df <- tibble(x = c("a", "b", "c"), n = c(1L, 0L, 2L))
  test_pl <- as_polars_lf(test_df)

  expect_equal_lazy(
    uncount(test_pl, n),
    uncount(test_df, n)
  )
  expect_equal_lazy(
    uncount(test_pl, n, .id = "id"),
    uncount(test_df, n, .id = "id")
  )

  # all weights are 0
  test_df <- tibble(x = c("a", "b"), n = c(0L, 0L))
  test_pl <- as_polars_lf(test_df)

  expect_equal_lazy(
    uncount(test_pl, n),
    uncount(test_df, n)
  )
})

test_that("works with logical weights", {
  test_df <- tibble(x = c("a", "b"), n = c(TRUE, FALSE))
  test_pl <- as_polars_lf(test_df)

  expect_equal_lazy(
    uncount(test_pl, n),
    uncount(test_df, n)
  )
})

test_that("`.remove` only drops a bare column name, not expressions", {
  test_df <- tibble(x = c("a", "b"), n = c(1L, 2L))
  test_pl <- as_polars_lf(test_df)

  expect_equal_lazy(
    uncount(test_pl, n + 0),
    uncount(test_df, n + 0)
  )
  expect_equal_lazy(
    uncount(test_pl, n * 2),
    uncount(test_df, n * 2)
  )
  expect_equal_lazy(
    uncount(test_pl, 2 * n),
    uncount(test_df, 2 * n)
  )
})

test_that("`.remove` works with a column named `literal`", {
  test_df <- tibble(x = c("a", "b"), literal = c(1L, 2L))
  test_pl <- as_polars_lf(test_df)

  expect_equal_lazy(
    uncount(test_pl, literal),
    uncount(test_df, literal)
  )
})

test_that("`.id` restarts from 1 for each original row", {
  test_df <- tibble(x = c("a", "a"), n = c(2L, 2L))
  test_pl <- as_polars_lf(test_df)

  expect_equal_lazy(
    uncount(test_pl, n, .id = "id"),
    uncount(test_df, n, .id = "id")
  )
})

test_that("`.id` overwrites an existing column", {
  test_df <- tibble(x = c("a", "b"), n = c(1L, 2L), id = c(10L, 20L))
  test_pl <- as_polars_lf(test_df)

  expect_equal_lazy(
    uncount(test_pl, n, .id = "id"),
    uncount(test_df, n, .id = "id")
  )
})

test_that("`.id` works when the first column contains missing values", {
  test_df <- tibble(x = c(NA_character_, "b"), n = c(2L, 2L))
  test_pl <- as_polars_lf(test_df)

  expect_equal_lazy(
    uncount(test_pl, n, .id = "id"),
    uncount(test_df, n, .id = "id")
  )
})

test_that("`.id` is an integer column", {
  test_df <- tibble(x = c("a", "b"), n = c(1L, 2L))
  test_pl <- as_polars_lf(test_df)

  expect_type(
    as_tibble(uncount(test_pl, n, .id = "id"))$id,
    "integer"
  )
})

test_that("`.id` can have the same name as the weights column", {
  test_df <- tibble(x = c("a", "b"), n = c(1L, 2L)) |>
    group_by(n)
  test_pl <- as_polars_lf(test_df) |>
    group_by(n)

  expect_equal_lazy(
    uncount(test_pl, n, .id = "n"),
    uncount(test_df, n, .id = "n")
  )
  expect_equal_lazy(
    group_vars(uncount(test_pl, n, .id = "n")),
    group_vars(uncount(test_df, n, .id = "n"))
  )
})

test_that("bare weights can come from the calling environment", {
  test_df <- tibble(x = c("a", "b"))
  test_pl <- as_polars_lf(test_df)
  weight <- 2L

  expect_equal_lazy(
    uncount(test_pl, weight),
    uncount(test_df, weight)
  )
})

test_that("works when the data only contains the weights column and `.id` is used", {
  test_df <- tibble(n = c(1L, 2L))
  test_pl <- as_polars_lf(test_df)

  expect_equal_lazy(
    uncount(test_pl, n, .id = "id"),
    uncount(test_df, n, .id = "id")
  )
})

test_that("arguments are checked", {
  test_df <- tibble(x = c("a", "b"), n = c(1L, 2L))
  test_pl <- as_polars_lf(test_df)

  expect_both_error(
    uncount(test_pl, n, .remove = 1),
    uncount(test_df, n, .remove = 1)
  )
  expect_both_error(
    uncount(test_pl, n, .id = 1),
    uncount(test_df, n, .id = 1)
  )
  expect_both_error(
    uncount(test_pl, n, .id = ""),
    uncount(test_df, n, .id = "")
  )
})

test_that("negative weights error", {
  test_df <- tibble(x = "a", n = -1L)
  test_pl <- as_polars_lf(test_df)

  expect_both_error(
    uncount(test_pl, n),
    uncount(test_df, n)
  )
})

test_that("grouping is preserved", {
  test_df <- tibble(g = c("a", "b"), x = 1:2, n = c(1L, 2L)) |>
    group_by(g)
  test_pl <- as_polars_lf(test_df) |>
    group_by(g, maintain_order = TRUE)

  expect_equal_lazy(
    uncount(test_pl, n),
    uncount(test_df, n)
  )
  expect_equal_lazy(
    group_vars(uncount(test_pl, n)),
    group_vars(uncount(test_df, n))
  )
  expect_true(attr(uncount(test_pl, n), "maintain_grp_order"))
})

test_that("works with empty data", {
  test_df <- tibble(x = character(), n = integer())
  test_pl <- as_polars_lf(test_df)

  expect_equal_lazy(
    uncount(test_pl, n, .id = "id"),
    uncount(test_df, n, .id = "id")
  )
})

test_that("weights column is removed from the groups if it is dropped", {
  test_df <- tibble(g = c("a", "b"), n = c(1L, 2L)) |>
    group_by(g, n)
  test_pl <- as_polars_lf(test_df) |>
    group_by(g, n)

  expect_equal_lazy(
    group_vars(uncount(test_pl, n)),
    group_vars(uncount(test_df, n))
  )

  # weights column was the only grouping column
  test_df <- tibble(n = c(1L, 2L)) |>
    group_by(n)
  test_pl <- as_polars_lf(test_df) |>
    group_by(n)

  expect_equal_lazy(
    group_vars(uncount(test_pl, n)),
    group_vars(uncount(test_df, n))
  )
})

# TODO: `NA` weights should error (as in tidyr) instead of producing a row of
# `NA`s. This requires computing the weights eagerly, which is not possible
# for a LazyFrame.
# test_that("missing weights error", {
#   test_df <- tibble(x = c("a", "b"), n = c(1L, NA))
#   test_pl <- as_polars_lf(test_df)
#
#   expect_both_error(
#     uncount(test_pl, n),
#     uncount(test_df, n)
#   )
# })

# TODO: fractional weights should error (as in tidyr) instead of being
# silently truncated. Same limitation as above.
# test_that("fractional weights error", {
#   test_df <- tibble(x = "a", n = 1.5)
#   test_pl <- as_polars_lf(test_df)
#
#   expect_both_error(
#     uncount(test_pl, n),
#     uncount(test_df, n)
#   )
# })

# TODO: tidyr can return a data.frame with rows but no column, which Polars
# cannot represent (it returns a 0x0 DataFrame instead).
# test_that("works when the data only contains the weights column", {
#   test_df <- tibble(n = c(1L, 2L))
#   test_pl <- as_polars_lf(test_df)
#
#   expect_equal_lazy(
#     uncount(test_pl, n),
#     uncount(test_df, n)
#   )
# })

Sys.setenv('TIDYPOLARS_TEST' = FALSE)
