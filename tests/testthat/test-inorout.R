dir <- tempdir() |> fs::path_norm()
uid <- digest::digest(as.character(dir), algo="crc32")
subdir <- fs::path_join(c(dir,"S"))
dir.create(subdir)
set_sourcoise_root(dir)
sourcoise_reset()
fs::file_copy(
  fs::path_package("sourcoise", "ipch", "prix_insee.R"),
  dir,
  overwrite = TRUE)
fs::file_copy(
  fs::path_package("sourcoise", "ipch", "prix_insee.R"),
  subdir,
  overwrite = TRUE)

options(sourcoise.src_in = "project")
test_that(
  "project", {
    expect(
      identical(x=sourcoise("S/prix_insee.r", metadata=TRUE)$ok, y="exec"),
      "No execution")
    expect(
      identical(x=sourcoise("S/prix_insee.r", metadata=TRUE)$ok, y="cache"),
      "No cache")
  })

r <- sourcoise_status(short = FALSE)$root
j <- sourcoise_status(short = FALSE)$json_file

test_that(
  "cache is ok",
  expect( j |> stringr::str_detect(".sourcoise/S/prix_insee"), "Wrong cache"))

test_that(
  "Files are ok", {
    expect(fs::file_exists(fs::path_join(c(r,j))), "Should be there")
    expect(!fs::file_exists(fs::path_join(c(r,stringr::str_replace(j, "-1", "-2")))), "should no be there")
  } )
sourcoise_refresh()
test_that(
  "Files are ok", {
    expect(fs::file_exists(fs::path_join(c(r,j))), "Should be there")
    expect(fs::file_exists(fs::path_join(c(r,stringr::str_replace(j, "-1", "-2")))), "should be there")
  } )

sourcoise_reset()
options(sourcoise.src_in = "file")

test_that(
  "file", {
    expect(
      identical(x=sourcoise("S/prix_insee.r", metadata=TRUE)$ok, y="exec"),
      "No execution")
    expect(
      identical(x=sourcoise("S/prix_insee.r", metadata=TRUE)$ok, y="cache"),
      "No cache")
  })

r <- sourcoise_status(short = FALSE)$root
j <- sourcoise_status(short = FALSE)$json_file

test_that(
  "cache is ok",
  expect( j |> stringr::str_detect("S/.sourcoise/prix_insee"), "Wrong cache"))

test_that(
  "Files are ok", {
    expect(fs::file_exists(fs::path_join(c(r,j))), "Should be there")
    expect(!fs::file_exists(fs::path_join(c(r,stringr::str_replace(j, "-1", "-2")))), "should no be there")
  } )
sourcoise_refresh()
test_that(
  "Files are ok", {
    expect(fs::file_exists(fs::path_join(c(r,j))), "Should be there")
    expect(fs::file_exists(fs::path_join(c(r,stringr::str_replace(j, "-1", "-2")))), "should be there")
  } )

sourcoise_reset()
options(sourcoise.src_in = "project")
exec <- sourcoise("S/prix_insee.r", metadata=TRUE)
options(sourcoise.src_in = "file")
exec <- sourcoise("S/prix_insee.r", metadata=TRUE)
options(sourcoise.src_in = "project")

setwd(dir)

test_that("In and Out", {
  expect(fs::file_exists("S/.sourcoise/prix_insee-4262323b_{uid}-1.json" |> glue::glue()), "Should be there")
  expect(fs::file_exists(".sourcoise/S/prix_insee-4262323b_{uid}-1.json"|> glue::glue()), "Should be there")

})

