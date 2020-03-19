test_that("Check leaf value return", {
  unlink("files", recursive = TRUE)
  create_sample_dir(name = "files")
  path.chain <- create_path_chain("files")
  expect_equal(path.chain$data$example1.RData, "files/data/example1.RData")
  unlink("files", recursive = TRUE)

})

test_that("Check node return", {
  unlink("files", recursive = TRUE)
  create_sample_dir(name = "files")
  path.chain <- create_path_chain("files")
  expect_true(inherits(path.chain$data, "path_chain"))
  unlink("files", recursive = TRUE)
})

test_that("Check dot notation", {
  unlink("files", recursive = TRUE)
  create_sample_dir(name = "files")
  path.chain <- create_path_chain("files")
  expect_equal(path.chain$data$., "files/data/")
  unlink("files", recursive = TRUE)
})
