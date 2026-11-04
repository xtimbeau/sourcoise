test_that("scripts whose names end alike do not share a cache (data / mydata)", {
  dir <- local_project()
  write_script(dir, "data.R", "data.frame(x = 1)")
  write_script(dir, "mydata.R", "data.frame(x = 2)")

  sourcoise("mydata.R")
  expect_equal(sourcoise("data.R")$x, 1)
  expect_equal(sourcoise("mydata.R")$x, 2)
  expect_length(cache_files(dir, "\\.qs2$"), 2)
})

test_that("script names are matched literally, not as regular expressions", {
  dir <- local_project()
  write_script(dir, "a.b.R", "data.frame(x = 1)")
  write_script(dir, "aXb.R", "data.frame(x = 2)")
  sourcoise("aXb.R")

  expect_equal(sourcoise("a.b.R")$x, 1)
  expect_equal(sourcoise("aXb.R")$x, 2)

  files <- ls_cache_files(dir, bn = "a.b")$jsons |> unlist()
  expect_length(files, 1)
  expect_match(basename(files), "^a\\.b-")
})

test_that("ls_cache_files() lists qs2 data files", {
  dir <- local_project()
  write_script(dir, "data.R", "data.frame(x = 1)")
  sourcoise("data.R")

  qs2 <- ls_cache_files(dir, qs2 = TRUE)$qs2 |> unlist()
  expect_length(qs2, 1)
  expect_match(basename(qs2), "\\.qs2$")
})

test_that("clean_caches() removes orphans but keeps live cache files", {
  dir <- local_project()
  write_script(dir, "data.R", "data.frame(x = 1)")
  sourcoise("data.R")
  before <- cache_files(dir, "\\.(qs2|json)$")
  orphan <- file.path(dir, ".sourcoise", "orphan.txt")
  writeLines("junk", orphan)

  clean_caches(root = dir)

  expect_false(file.exists(orphan))
  expect_setequal(cache_files(dir, "\\.(qs2|json)$"), before)
  expect_equal(sourcoise("data.R", metadata = TRUE)$ok, "cache")
})

test_that("sourcoise_clear() keeps the cache by default", {
  dir <- local_project()
  write_script(dir, "data.R", "data.frame(x = 1)")
  sourcoise("data.R")
  sourcoise_clear(root = dir)
  expect_equal(sourcoise("data.R", metadata = TRUE)$ok, "cache")
})

test_that("get_metadata() keeps the cache matching the current source", {
  dir <- local_project()
  write_script(dir, "alpha.R", "data.frame(x = 1)")
  sourcoise("alpha.R")
  write_script(dir, "alpha.R", "data.frame(x = 10)")
  sourcoise("alpha.R")

  m <- get_metadata(root = dir, filter = "all")
  expect_gt(nrow(m), 0)
  expect_true(all(m$valid))
  expect_equal(sourcoise("alpha.R", metadata = TRUE)$ok, "cache")
})

test_that("get_metadata() keeps all versions when none matches the source", {
  dir <- local_project()
  write_script(dir, "alpha.R", "data.frame(x = 1)")
  sourcoise("alpha.R")
  write_script(dir, "alpha.R", "data.frame(x = 10)")
  sourcoise("alpha.R")
  write_script(dir, "alpha.R", "data.frame(x = 100)")

  m <- get_metadata(root = dir, filter = "all")
  expect_equal(nrow(m), 2)
  expect_false(any(m$valid))
})

test_that("scripts with dots in their name are cached", {
  dir <- local_project()
  write_script(dir, "my.data.R", "data.frame(x = 5)")
  sourcoise("my.data.R")

  expect_equal(sourcoise("my.data.R", metadata = TRUE)$ok, "cache")
  expect_length(cache_files(dir, "^my\\.data-.*-1\\.json$"), 1)
  expect_length(cache_files(dir, "^my\\.json$"), 0)

  m <- get_metadata(root = dir)
  expect_equal(nrow(m), 1)
  expect_equal(tolower(m$src), "my.data.r")
  expect_true(m$exists)
  expect_true(m$valid)
})

test_that("hash_file() of a missing file does not touch the RNG stream", {
  set.seed(4817)
  expected <- runif(1)
  set.seed(4817)
  hash_file(file.path(tempdir(), "does_not_exist.R"))
  expect_equal(runif(1), expected)

  expect_equal(
    hash_file(file.path(tempdir(), "does_not_exist.R")),
    hash_file(file.path(tempdir(), "does_not_exist.R")))
})

test_that("fast_metadata() returns json paths relative to the root", {
  # root made of regex metacharacters
  dir <- local_project("p(1)+x")
  write_script(dir, "data.R", "data.frame(x = 1)")
  sourcoise("data.R")

  fm <- fast_metadata(root = dir)
  expect_length(fm$json_file, 1)
  expect_match(fm$short_json_file, "^\\.sourcoise/data-[0-9a-f]{8}_[0-9a-f]{8}-1\\.json$")
  expect_equal(file.path(dir, fm$short_json_file), as.character(fm$json_file))
})

test_that("fast_metadata() on an empty project returns an empty list", {
  dir <- local_project()
  expect_length(fast_metadata(root = dir), 0)
})

# failing scripts ------------------------------------------------------------

# pushes the date of every cached version back, so that lapse has expired
expire_cache <- function(dir) {
  jsons <- list.files(file.path(dir, ".sourcoise"), "\\.json$", full.names = TRUE)
  for(j in jsons) {
    txt <- readLines(j)
    txt <- sub('"date": \\["[^"]*"\\]', '"date": ["2020-01-01 00:00:00"]', txt)
    writeLines(txt, j)
  }
}

# script that fails as soon as a file named `fail` exists next to it
failing_code <- function(x) {
  c('if(file.exists("fail")) stop("boom")', sprintf("data.frame(x = %d)", x))
}

test_that("an expired cache is returned when a modified script then fails", {
  dir <- local_project()
  write_script(dir, "alpha.R", failing_code(1))
  sourcoise("alpha.R", lapse = "1 hour")
  write_script(dir, "alpha.R", failing_code(2))
  sourcoise("alpha.R", lapse = "1 hour")
  expire_cache(dir)
  writeLines("", file.path(dir, "fail"))

  res <- sourcoise("alpha.R", lapse = "1 hour", metadata = TRUE, quiet = TRUE)

  expect_equal(res$ok, "Invalid cache")
  expect_equal(res$data$x, 2)
  expect_match(res$error, "boom")
})

test_that("the latest cache is returned when the modified script fails with no cache of its own", {
  dir <- local_project()
  write_script(dir, "alpha.R", failing_code(1))
  sourcoise("alpha.R")
  write_script(dir, "alpha.R", failing_code(2))
  sourcoise("alpha.R")
  write_script(dir, "alpha.R", failing_code(3))
  writeLines("", file.path(dir, "fail"))

  res <- sourcoise("alpha.R", metadata = TRUE, quiet = TRUE)

  expect_equal(res$ok, "Invalid cache")
  expect_equal(res$data$x, 2)
})

test_that("failing with no cache at all is an error naming the script", {
  dir <- local_project()
  write_script(dir, "alpha.R", failing_code(1))
  writeLines("", file.path(dir, "fail"))

  # cli may wrap long messages, hence \\s+ between words
  err <- expect_error(sourcoise("alpha.R", quiet = TRUE), class = "rlang_error")
  expect_match(conditionMessage(err), "alpha\\.[rR]")
  expect_match(conditionMessage(err), "failed\\s+and\\s+no\\s+cache\\s+to\\s+return")
})

test_that("preventing execution with no cache is an error naming the script", {
  dir <- local_project()
  write_script(dir, "alpha.R", failing_code(1))

  err <- expect_error(
    sourcoise("alpha.R", prevent_exec = TRUE, quiet = TRUE), class = "rlang_error")
  expect_match(conditionMessage(err), "alpha\\.[rR]")
  expect_match(conditionMessage(err), "prevented\\s+and\\s+no\\s+cache\\s+to\\s+return")
})

test_that("a missing script is an error naming the script", {
  dir <- local_project()

  err <- expect_error(sourcoise("nope.R", quiet = TRUE), class = "rlang_error")
  expect_match(conditionMessage(err), "nope\\.R")
  expect_match(conditionMessage(err), "not\\s+found")
})

test_that("error messages keep braces found in paths as they are", {
  dir <- local_project("{proj}")
  write_script(dir, "alpha.R", failing_code(1))
  writeLines("", file.path(dir, "fail"))

  err <- expect_error(sourcoise("alpha.R", quiet = TRUE), class = "rlang_error")
  expect_match(conditionMessage(err), "{proj}", fixed = TRUE)
})

test_that("error messages are formatted when raised, not lazily when displayed", {
  dir <- local_project()
  write_script(dir, "alpha.R", failing_code(1))
  writeLines("", file.path(dir, "fail"))

  err <- expect_error(sourcoise("alpha.R", quiet = TRUE), class = "rlang_error")
  # a lazily formatted condition would still hold the raw `{ctxt$src}` template
  expect_false(isTRUE(err$use_cli_format))
  expect_no_match(err$message, "ctxt", fixed = TRUE)
  expect_match(err$message, "alpha[.][rR]")
})

# sourcoise_clear() ----------------------------------------------------------

# runs `script` successively with each code snippet, producing one cache version each
make_versions <- function(dir, script, codes, ...) {
  for(code in codes) {
    write_script(dir, script, code)
    sourcoise(script, ...)
  }
}

test_that("sourcoise_clear('recent') keeps only the latest version of each script", {
  dir <- local_project()
  make_versions(dir, "a.R", sprintf("data.frame(x = %d)", 1:3))
  make_versions(dir, "b.R", sprintf("data.frame(y = %d)", 1:2))
  expect_length(cache_files(dir, "\\.json$"), 5)
  expect_length(cache_files(dir, "\\.qs2$"), 5)

  deleted <- sourcoise_clear("recent", root = dir)

  expect_length(deleted, 6)
  expect_length(cache_files(dir, "\\.json$"), 2)
  expect_length(cache_files(dir, "\\.qs2$"), 2)
  a <- sourcoise("a.R", metadata = TRUE)
  b <- sourcoise("b.R", metadata = TRUE)
  expect_equal(c(a$ok, b$ok), c("cache", "cache"))
  expect_equal(a$data$x, 3)
  expect_equal(b$data$y, 2)
})

test_that("sourcoise_clear('recent') keeps the latest version for each set of arguments", {
  dir <- local_project()
  write_script(dir, "a.R", "data.frame(x = args$k)")
  for(k in c(1, 2, 3))
    sourcoise("a.R", args = list(k = k))
  write_script(dir, "a.R", "data.frame(x = args$k * 10)")
  for(k in c(1, 2))
    sourcoise("a.R", args = list(k = k))
  expect_length(cache_files(dir, "\\.json$"), 5)

  sourcoise_clear("recent", root = dir)

  expect_length(cache_files(dir, "\\.json$"), 3)
  expect_length(cache_files(dir, "\\.qs2$"), 3)
  expect_equal(sourcoise("a.R", args = list(k = 1))$x, 10)
  expect_equal(sourcoise("a.R", args = list(k = 2))$x, 20)
  expect_equal(sourcoise("a.R", args = list(k = 3))$x, 30)
})

test_that("sourcoise_clear('recent') handles scripts in sub-directories", {
  dir <- local_project()
  dir.create(file.path(dir, "sub"))
  make_versions(dir, file.path("sub", "s.R"), sprintf("data.frame(x = %d)", 1:3))
  make_versions(dir, "top.R", sprintf("data.frame(x = %d)", 1:2))

  sourcoise_clear("recent", root = dir)

  expect_length(cache_files(dir, "\\.json$"), 2)
  expect_length(cache_files(dir, "\\.qs2$"), 2)
  s <- sourcoise("sub/s.R", metadata = TRUE)
  expect_equal(s$ok, "cache")
  expect_equal(s$data$x, 3)
})

test_that("sourcoise_clear('recent') is a no-op on a single version and an empty cache", {
  dir <- local_project()
  expect_length(sourcoise_clear("recent", root = dir), 0)

  write_script(dir, "a.R", "data.frame(x = 1)")
  sourcoise("a.R")
  expect_length(sourcoise_clear("recent", root = dir), 0)
  expect_equal(sourcoise("a.R", metadata = TRUE)$ok, "cache")
})

test_that("sourcoise_clear('nothing') and sourcoise_clear_all() remove the whole cache", {
  dir <- local_project()
  make_versions(dir, "a.R", sprintf("data.frame(x = %d)", 1:2))
  sourcoise_clear("nothing", root = dir)
  expect_length(cache_files(dir, "\\.(json|qs2)$"), 0)

  make_versions(dir, "a.R", sprintf("data.frame(x = %d)", 1:2))
  sourcoise_clear_all(root = dir)
  expect_length(cache_files(dir, "\\.(json|qs2)$"), 0)
})

test_that("sourcoise_clear() rejects unknown values of what2keep", {
  dir <- local_project()
  expect_error(sourcoise_clear("last", root = dir), "what2keep")
})
