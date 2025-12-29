dir <- tempdir()
library(tidyr)
set_sourcoise_root(dir)
sourcoise_clear_all()
fs::file_copy(
  fs::path_package("sourcoise", "ipch", "slow.R"),
  dir,
  overwrite = TRUE)

options(sourcoise.log = "INFO")

data0 <- sourcoise("slow.R", metadata = TRUE)
data1 <- sourcoise("slow.R", metadata = TRUE, force = TRUE)

ff <- fs::dir_info(fs::path_join(c(dir, ".sourcoise")))

test_that(
  "files ok", {
    expect_equal(nrow(ff), 4)
    expect_equal(nrow(ff |> filter(str_detect(path,".qs2"))), 2)
  })

data2 <- sourcoise("slow.r")

fs::file_delete(ff |>
                  filter(str_detect(path, ".qs2")) |>
                  filter(modification_time == max(modification_time)) |>
                  pull(path))

data3 <- sourcoise("slow.r", metadata = TRUE)

test_that(
  "cache is recent", {
    expect(all(data2$a == data1$data$a), "cache error")
    expect(all(data3$a == data0$data$a), "cache error")
  })

ff <- fs::dir_info(fs::path_join(c(dir, ".sourcoise")))
fs::file_delete(ff |>
                  filter(str_detect(path, ".qs2")) |>
                  pull(path))

data4 <- sourcoise("slow.r", metadata = TRUE)

test_that(
  "cache is new", {
    expect(data4$ok == "exec", "No execution!")
    expect(all(data4$data$a == data0$data$a), "good data")
    expect(!all(data4$data$b == data0$data$b), "old data")
  })

slow <- readLines(fs::path_join(c(dir, "slow.R")))
slow[6] <- "# a comment"
writeLines(slow, con = fs::path_join(c(dir, "slow.R")))

data5 <- sourcoise("slow.r", metadata = TRUE)

test_that(
  "cache is old", {
    expect(data5$ok == "exec", "No execution!")
    expect(all(data5$data$a == data4$data$a), "good data")
    expect(!all(data5$data$b == data4$data$b), "old data")
  })

slow <- readLines(fs::path_join(c(dir, "slow.R")))
slow[4] <- "Une erreur"
writeLines(slow, con = fs::path_join(c(dir, "slow.R")))

data6 <- sourcoise("slow.r", metadata = TRUE)

test_that(
  "cache is old", {
    expect(data6$ok == "Invalid cache", "Cache error")
    expect(all(data6$data$a == data5$data$a), "good data")
    expect(all(data6$data$b == data5$data$b), "old data")
  })
