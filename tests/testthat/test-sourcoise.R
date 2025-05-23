dir <- tempdir()
set_sourcoise_root(dir)
sourcoise_clear()
fs::file_copy(
  fs::path_package("sourcoise", "ipch", "prix_insee.R"),
  dir,
  overwrite = TRUE)
fs::file_copy(
  fs::path_package("sourcoise", "ipch", "slow.R"),
  dir,
  overwrite = TRUE)

# Force execution (root is set explicitly here, it is normally deduced from project)
options(sourcoise.log = "INFO")
data <- sourcoise("prix_insee.R", force_exec = TRUE, metadata = TRUE)

## sourcoise() ----------------

test_that("We get data", {
  expect(
    identical(x=names(data$data), y=c("ipcha", "ipchm", "ipch")),
    "Execution is wrong")
})

if(!is.na(data$json_file))
  cache_dir <- fs::path_dir(data$json_file)

if(!is.na(cache_dir))
  test_that("Cache dir is there", {
    expect(
      fs::dir_exists(cache_dir),
      "No cache dir")
  })

if(!is.na(data$data_file))
  test_that("Data cached is well named", {
    expect(
      stringr::str_detect(data$data_file, "prix_insee-4262323b.+\\.qs2"),
      "wrong name")
  })

if(!is.na(data$json_file))
  test_that("Data cached exists", {
    expect(
      fs::file_exists(fs::path_join(c(fs::path_dir(data$json_file), data$data_file))),
      "no data cached")
  })

test_that("prevent works", {
  expect( sourcoise("prix_insee.R", prevent_exec = TRUE, metadata = TRUE)$ok == "cache",
          "prevent fails")
})

## timing test

if(rlang::is_installed("bench")) {
  gc()
  timing_force <- bench::mark(sourcoise("slow.R", force_exec = TRUE), max_iterations = 5 )
  gc()
  timing <- bench::mark(sourcoise("slow.R"), max_iterations = 5)

  test_that("Timings", {
    expect(timing_force$median>=timing$median,
           "cache is too slow")
  })
}
## sourcoise_meta ----------------

meta <- sourcoise_meta("prix_insee.R")

test_that("sourcoise_meta", {
  expect(meta$ok == "cache ok&valid",
         "no metadata returned")
  expect(meta$data_date == stringr::str_remove(data$data_date, " CET$"),
         "date not the same")
  expect(meta$data_file == data$data_file,
         "data_file not the same")
})

test_that("meta ok when no cache", {
  expect(sourcoise_meta("toto.R")$ok == "no cache data",
         "meta fails when nothing found")
})

## track files

# write.csv("data", file = fs::path_join(c(dir, "data.csv")))
# zz <- sourcoise("prix_insee.R", track = "data.csv", metadata=TRUE)
#
# test_that(
#   "tracking", {
#     expect(sourcoise("prix_insee.R", track = "data.csv", metadata=TRUE)$ok == "cache", "cache ?")
#   })
#
# write.csv("data modified", file = fs::path_join(c(dir, "data.csv")))
#
# test_that(
#   "tracking", {
#     expect(sourcoise("prix_insee.R", track = "data.csv", metadata=TRUE)$ok == "exec",  "exec ?")
#   })
#
# test_that(
#   "tracking", {
#     expect(sourcoise("prix_insee.R", track = "data.csv", metadata=TRUE)$ok == "cache",  "cache ?")
#   })

## sourcoise_status ----------------

status <- sourcoise_status()

test_that("sourcoise_status", {
  expect(tibble::is_tibble(status)&nrow(status)>0,
         "status is not a non empty tibble")
})

test_that("sourcoise_status", {
  expect(all(c("src", "root", "args", "track", "lapse") %in% names(status)),
         "status is not well formed")
})

## sourcoise_refresh

sr <- sourcoise_refresh()

test_that("sourcoise_refresh", {
  expect(length(sr)==4 & sr$ok[[1]]=="exec",
         "Refresh did not worked")
})

## sourcoise_clear ----------------

sourcoise_clear()
status <- sourcoise_status()


test_that("sourcoise_status", {
  expect(tibble::is_tibble(status)&nrow(status)==0,
         "status is not an empty tibble")
})

sourcoise_reset()

test_that("sourcoise_reset", {
  expect(!fs::dir_exists(cache_dir),
         "cache dir was not removed")
})
