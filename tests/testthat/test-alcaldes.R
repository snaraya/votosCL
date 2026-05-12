test_that("data loads", {
  data("alcaldes_2024_candidatos")
  expect_true(TRUE)
})

test_that("expected columns exist", {
  expect_true(all(c(
    "comuna",
    "partido",
    "votos"
  ) %in% names(alcaldes_2024_candidatos)))
})
