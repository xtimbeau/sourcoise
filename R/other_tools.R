# fonctions --------------------

what_lapse <- function(check) {

  ext <- function(e) {
    num <- stringr::str_extract(e, "^([0-9]+)")
    if(is.na(num))
      num <- 1
    num <- as.numeric(num)
  }

  if(stringr::str_detect(check, "month"))
    return(lubridate::period(ext(check), unit = "months"))
  if(stringr::str_detect(check, "week"))
    return(lubridate::weeks(ext(check)))
  if(stringr::str_detect(check, "quarter"))
    return(lubridate::period(3*ext(check), unit = "months"))
  if(stringr::str_detect(check, "day"))
    return(lubridate::days(ext(check)))
  if(stringr::str_detect(check, "hour"))
    return(lubridate::hours(ext(check)))
  if(stringr::str_detect(check, "year"))
    return(lubridate::years(ext(check)))
}

unfreeze <- function(qmd_file, root, quiet=TRUE) {
  if(is.null(qmd_file))
    return(NULL)
  qmd_folder <- qmd_file |> fs::path_ext_remove()
  freeze_path <- fs::path_join(c(root, "_freeze", qmd_folder))
  if(fs::dir_exists(freeze_path)) {
    if(!quiet)
      cli::cli_alert_info("Unfreezing {.file {freeze_path}}")
    unlink(freeze_path, recursive=TRUE, force=TRUE)
  }
  return(NULL)
}

uncache <- function(qmd_file, root, quiet=TRUE) {
  if(is.null(qmd_file))
    return(NULL)
  qmd_bn <- qmd_file |> fs::path_file() |> fs::path_ext_remove()
  rel_path <- fs::path_dir(qmd_file)
  cache_path <- fs::path_join(c(root, rel_path, stringr::str_c(qmd_bn, "_cache")))
  files_path <- fs::path_join(c(root, rel_path, stringr::str_c(qmd_bn, "_files")))
  if(fs::dir_exists(cache_path)) {
    if(!quiet)
      cli::cli_alert_info("Uncaching {.file {cache_path}}")
    unlink(cache_path, recursive=TRUE, force=TRUE)
  }
  if(fs::dir_exists(files_path)) {
    if(!quiet)
      cli::cli_alert_info("Unfiles {.file {files_path}}")
    unlink(files_path, recursive=TRUE, force=TRUE)
  }
  return(NULL)
}

format_timespan <- function(to_datetime, from_datetime = now()) {
  require("lubridate", quietly = TRUE)
  textify <- function(span, i=1) {
    tspan <- glue::glue("{floor(span/is[[i]])}{names(is)[[i]]}")
    if(i>1) {
      mspan <- round((span/is[[i]] - floor(span/is[[i]]))*is[[i]]/is[[i-1]])
      tspan <- glue::glue("{tspan} {mspan}{names(is)[[i-1]]}")
    }
    return(tspan)
  }
  is <- c(s = dseconds(1),
          m = dminutes(1),
          h = dhours(1),
          j = ddays(1),
          sem = dweeks(1),
          mois = dmonths(1),
          an = dyears(1))

  purrr::map2_chr(to_datetime |> as_datetime(tz=Sys.timezone()), from_datetime, ~{
    span <- (.x %--% .y)
    if(span<seconds(60))
      tspan <- textify(span, 1) else
        if(span<minutes(60))
          tspan <- textify(span, 2) else
            if(span<hours(24))
              tspan <- textify(span, 3) else
                if(span<days(7))
                  tspan <- textify(span, 4) else
                    if(span<weeks(4))
                      tspan <- textify(span, 5) else
                        if(span<months(52))
                          tspan <- textify(span, 6) else
                            tspan <- textify(span, 7)
                          return(tspan)
  })
}
