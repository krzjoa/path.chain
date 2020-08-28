test_that("Check leaf value return", {
  unlink(file.path(tempdir(), "files"), recursive = TRUE)
  create_sample_dir(create_temp_dir("files"))
  path.chain <- path_chain(file.path(tempdir(), "files"))
  expect_equal(path.chain$data$example1.RData, "files/data/example1.RData")
})

test_that("Check node return", {
  unlink(file.path(tempdir(), "files"), recursive = TRUE)
  create_sample_dir(create_temp_dir("files"))
  path.chain <- path_chain(file.path(tempdir(), "files"))
  expect_true(inherits(path.chain$data, "path_chain"))
})

test_that("Check dot notation", {
  unlink(file.path(tempdir(), "files"), recursive = TRUE)
  create_sample_dir(create_temp_dir("files"))
  path.chain <- path_chain(file.path(tempdir(), "files"))
  expect_equal(sub("/$", "", path.chain$data$.), "files/data")
})
