dir <- tempdir() |> fs::path_norm()
uid <- digest::digest(as.character(dir), algo="crc32")
subdir <- fs::path_join(c(dir,"S"))
set_sourcoise_root(dir)
sourcoise_reset()
fs::file_copy(
  fs::path_package("sourcoise", "ipch", "prix_insee.R"),
  dir,
  overwrite = TRUE)

options(sourcoise.src_in = "project")

data <- sourcoise("prix_insee.r", metadata=TRUE)

sourcoise_priority("prix_insee.r", 12)

test_that(
  "priority", {
    expect(
      sourcoise_meta("prix_insee.r")$priority==12,
    "priority not set")
    })

write.csv("data", file = stringr::str_c(dir, "/data.csv"))
data <- sourcoise("prix_insee.r", track = "data.csv")

test_that(
  "track", {
    expect(
      sourcoise_meta("prix_insee.r")$track=="data.csv",
      "not tracked")
  })

sourcoise_untrack("prix_insee.r")
test_that(
  "track", {
    expect(
      sourcoise_meta("prix_insee.r")$track |> unlist() |> is.null(),
      "not untracked")
  })
