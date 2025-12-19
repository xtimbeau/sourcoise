set_sourcoise_root("~/sourcoise/work")
sourcoise_clear_all()

library(tidyverse)

purrr::walk(1:1000,  ~sourcoise("work/noerror3.r", force=TRUE))

sourcoise("work/noerror3.r") |> bench::mark()
sourcoise("work/commun.r") |> bench::mark()
sourcoise("work/error.r")

sourcoise_status(short=FALSE)
sourcoise_refresh()
sourcoise_status(short=FALSE)

bench::mark(
  {
    jsons <- fs::dir_ls(".sourcoise/work", glob = "*.json")
    jj <- map_dfr(jsons, ~jsonlite::read_json(.x, simplifyVector = TRUE) |> flatten())
    }
  )


jj <- map_dfr(jsons, ~jsonlite::read_json(.x, simplifyVector = TRUE) |> flatten())
map(jj, ~ .x |> unname() |> unlist() %||% list()) |> bind_rows()
  jj |> unname() |> dplyr::bind_rows()

  jj |> bind_rows()
