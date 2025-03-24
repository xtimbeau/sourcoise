dir <- tempdir()
set_sourcoise_root(dir)
fs::file_copy(
  fs::path_package("sourcoise", "synthetic.R"),
  dir,
  overwrite = TRUE)
# Force execution (root is set explicitly here, it is normally deduced from project)
data_100 <- sourcoise("synthetic.R", args = list(size=100))
data_1000 <- sourcoise("synthetic.R", args = list(size=1000))

## sourcoise() ----------------

test_that("Right with args (100)", {
  expect(
    nrow(data_100)==100,
    "Worng execution with size=100")
})

test_that("Right with args (1000)", {
  expect(
    nrow(data_1000)==1000,
    "Worng execution with size=1000")
})

## test clear

sourcoise_clear()

test_that("Right with args (100)", {
  expect(
    nrow(data_100)==100,
    "Worng execution with size=100")
})
