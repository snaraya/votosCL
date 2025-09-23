test_that("what_time funciona", {
  # Funciones que ponemos empiezan con expect_*
  expect_type(what_time(), "character")
  expect_snapshot(what_time("fr"), error = T)
})
