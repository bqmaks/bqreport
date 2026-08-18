test_that("with_resampling_seed() isolates a fixed seed", {
  set.seed(81)
  state_before <- .Random.seed

  first <- with_resampling_seed(2026L, stats::runif(3L))
  second <- with_resampling_seed(2026L, stats::runif(3L))

  expect_identical(first, second)
  expect_identical(.Random.seed, state_before)
})

test_that("with_resampling_seed() lets a NULL seed advance the stream", {
  set.seed(82)
  state_before <- .Random.seed

  with_resampling_seed(NULL, stats::runif(1L))

  expect_false(identical(.Random.seed, state_before))
})

test_that("with_resampling_seed() restores an absent stream", {
  seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (seed_exists) {
    previous_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (seed_exists) {
      assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })

  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  with_resampling_seed(2026L, stats::runif(1L))

  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("with_resampling_seed() restores the stream after an error", {
  set.seed(83)
  state_before <- .Random.seed

  expect_error(
    with_resampling_seed(2026L, stop("boom")),
    "boom"
  )
  expect_identical(.Random.seed, state_before)
})
