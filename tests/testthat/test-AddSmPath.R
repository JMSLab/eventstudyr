# Test
test_that("AddZerosCovar correctly adds zeros where normalized coefficient should go", {

  vcov_matrix_all <- diag(rep(3,10))
  eventstudy_coeffs  <- c(paste0("a", 1:2), paste0("a", 4:9))
  rownames(vcov_matrix_all) <- c(eventstudy_coeffs, "a10", "a11")
  colnames(vcov_matrix_all) <- c(eventstudy_coeffs, "a10", "a11")

  norm_column  <- "a3"
  coeffs_oder  <- paste0("a", 1:9)

  expected_matrix <- diag(c(rep(3,2), 0, rep(3,6)))
  rownames(expected_matrix) <- paste0("a", 1:9)
  colnames(expected_matrix) <- paste0("a", 1:9)

  covar <- AddZerosCovar(vcov_matrix_all, eventstudy_coeffs, norm_column, coeffs_oder)

  expect_equal(covar, expected_matrix)
})
