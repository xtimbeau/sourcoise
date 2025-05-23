dir <- tempdir()
set_sourcoise_root(dir)
fs::file_copy(
  fs::path_package("sourcoise", "ipch", "prix_insee.R"),
  dir,
  overwrite = TRUE)
fs::file_copy(
  fs::path_package("sourcoise", "ipch", "slow.R"),
  dir,
  overwrite = TRUE)
fs::file_copy(
  fs::path_package("sourcoise", "test.qmd"),
  dir,
  overwrite = TRUE)

# Force execution (root is set explicitly here, it is normally deduced from project)

## find_src ----------------

# test_that("find_src ?", {
#   expect(
#     stringr::str_detect(find_src(dir, "prix_insee"), "prix_insee"),
#     "find_src fails")
# })

# test_that("try_find_src ?", {
#   expect(
#     stringr::str_detect(try_find_src(dir, "prix_insee"), "prix_insee"),
#     "try_find_src fails")
# })

# test_that("try_find_root ?", {
#   expect(try_find_root() == dir,
#          "try_find_root fails")
# })

test_that("unfreeze&uncache", {
  expect(is.null(unfreeze("test.qmd", root = dir)),
         "unfreeze fails")
  expect(is.null(uncache("test.qmd", root = dir)),
         "unfreeze fails")
})

test_that("what lapse", {
  expect(what_lapse("2 weeks") == "14d 0H 0M 0S",
         "week failed")
  expect(what_lapse("2 days") == "2d 0H 0M 0S",
         "day failed")
  expect(what_lapse("2 hours") == "2H 0M 0S",
         "hour failed")
  expect(what_lapse("2 quarters") == "6m 0d 0H 0M 0S",
         "quarter failed")
  expect(what_lapse("2 years") == "2y 0m 0d 0H 0M 0S",
         "year failed")
})

