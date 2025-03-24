dir <- tempdir()
set_sourcoise_root(dir)
fs::file_copy(
  fs::path_package("sourcoise", "synthetic.R"),
  dir,
  overwrite = TRUE)
data_100 <- sourcoise("synthetic.R", args = list(size=100))
data_1000 <- sourcoise("synthetic.R", args = list(size=1000))

## sourcoise() ----------------
if(tibble::is_tibble(data_100)) {
  test_that("Right with args (100)", {
    expect(
      nrow(data_100)==100,
      "Wrong execution with size=100")
  })

  test_that("Right with args (1000)", {
    expect(
      nrow(data_1000)==1000,
      "Wrong execution with size=1000")
  })
}
## test clear

sourcoise_clear()

test_that("Right with args (100)", {
  expect(
    nrow(data_100)==100,
    "Wrong execution with size=100")
})
