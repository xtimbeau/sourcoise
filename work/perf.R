fs::dir_create("test")

bench::mark(
  base::dir.exists("test"),
  base::file.exists("test"),
  fs::dir_exists("test"),
  fs::file_exists("test"), check = FALSE)
