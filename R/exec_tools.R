exec_source <- function(ctxt) {
  safe_source <- purrr::safely(\(src, args) {
    args <- args
    if(ctxt$inform)
      res <- base::source(src, local=TRUE, encoding = getOption("sourcoise.encoding"))
    else
      (res <- base::source(src, local=TRUE, encoding = getOption("sourcoise.encoding"))) |>
      utils::capture.output(file = nullfile(), type = "output") |>
      utils::capture.output(file = nullfile(), type = "message") |>
      suppressMessages() |>
      suppressWarnings()
    return(res)
  })

  current_wd <- getwd()
  on.exit(setwd(current_wd))
  setwd(ctxt$exec_wd)
  start <- Sys.time()
  res <- safe_source(ctxt$src, args = ctxt$args)
  timing <- difftime(Sys.time() , start, units = "secs") |> as.numeric()
  setwd(current_wd)
  if(!is.null(res$error)) {
    return(
      list(
        ok=FALSE,
        error = res$error,
        args =ctxt$args,
        lapse = ctxt$lapse,
        src = ctxt$relname,
        src_hash = ctxt$src_hash,
        arg_hash = ctxt$arg_hash,
        track_hash = ctxt$track_hash,
        track = ctxt$track,
        wd = ctxt$wd,
        qmd_file = ctxt$new_qmds,
        src_in = ctxt$src_in,
        ok = "exec",
        log_file = ctxt$log_file))
  }
  data <- res$result$value
  if(!"S7_object"%in%class(data))
    size <- lobstr::obj_size(data) |> as.numeric()
  else
    size <- utils::object.size(data) |> as.numeric()
  list(
    data = data,
    timing = timing,
    date = lubridate::now(),
    size = size,
    args =ctxt$args,
    lapse = ctxt$lapse,
    src = ctxt$relname,
    src_hash = ctxt$src_hash,
    arg_hash = ctxt$arg_hash,
    track_hash = ctxt$track_hash,
    track = ctxt$track,
    wd = ctxt$wd,
    qmd_file = ctxt$new_qmds,
    src_in = ctxt$src_in,
    ok = "exec",
    log_file = ctxt$log_file)
}

super_exec_source <- function(ctxt) {

  safe_eval <- purrr::safely(\(src, args) {
    args <- args
    code <- c("func_tmp <- function() {", readLines(src), "}")
    func_tmp <- NULL
    eval(parse(text = code, encoding = getOption("sourcoise.encoding")))
    res <- func_tmp() |>
        suppressMessages() |>
        suppressWarnings() |>
        suppressPackageStartupMessages()
    return(res)
  })

  current_wd <- getwd()
  on.exit(setwd(current_wd))
  setwd(ctxt$exec_wd)
  start <- Sys.time()
  res <- safe_eval(ctxt$src, args = ctxt$args)
  timing <- difftime(Sys.time() , start, units = "secs") |> as.numeric()
  setwd(current_wd)
  if(!is.null(res$error)) {
    return(
      list(
        ok=FALSE,
        error = res$error,
        args =ctxt$args,
        lapse = ctxt$lapse,
        src = ctxt$relname,
        src_hash = ctxt$src_hash,
        arg_hash = ctxt$arg_hash,
        track_hash = ctxt$track_hash,
        track = ctxt$track,
        wd = ctxt$wd,
        qmd_file = ctxt$new_qmds,
        src_in = ctxt$src_in,
        ok = "exec",
        log_file = ctxt$log_file))
  }
  data <- res$result
  if(!"S7_object"%in%class(data))
    size <- lobstr::obj_size(data) |> as.numeric()
  else
    size <- utils::object.size(data) |> as.numeric()
  list(
    data = data,
    timing = timing,
    date = lubridate::now(),
    size = size,
    args =ctxt$args,
    lapse = ctxt$lapse,
    src = ctxt$relname,
    src_hash = ctxt$src_hash,
    arg_hash = ctxt$arg_hash,
    track_hash = ctxt$track_hash,
    track = ctxt$track,
    wd = ctxt$wd,
    qmd_file = ctxt$new_qmds,
    src_in = ctxt$src_in,
    ok = "exec",
    log_file = ctxt$log_file)
}
