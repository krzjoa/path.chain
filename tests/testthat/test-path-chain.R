test_that("Check leaf value return", {
  unlink("files", recursive = TRUE)
  create_sample_dir(name = "files")
  path.chain <-create_path_chain("files")
  expect_equal(file.structure$data$example1.RData, "files/data/example1.RData")
  unlink("files", recursive = TRUE)

})

test_that("Check node return", {
  unlink("files", recursive = TRUE)
  create_sample_dir(name = "files")
  path.chain <-create_path_chain("files")
  expect_true(inherits(file.structure$data, "path_chain"))
  unlink("files", recursive = TRUE)
})

test_that("Check dot notation", {
  unlink("files", recursive = TRUE)
  create_sample_dir(name = "files")
  path.chain <-create_path_chain("files")
  expect_equal(file.structure$data$., "files/data/")
  unlink("files", recursive = TRUE)
})
