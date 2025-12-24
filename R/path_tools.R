remove_ext <- function(name) {
  stringr::str_remove(name, "\\.[r|R]$")
}

find_src <- function(root, name, paths=NULL) {
  path <- fs::path_join(c(root, name)) |>
    fs::path_norm()
  if(!stringr::str_detect(name, "^/(?!/)"))
    if(!is.null(paths))
      if(paths$in_quarto)
        path <- fs::path_join(c(paths$project_path, paths$doc_path, name)) |>
          Qfs::path_norm()
  fn <- stringr::str_c(path, ".r")
  if(file.exists(fn)) return(fn)
  fn <- stringr::str_c(path, ".R")
  if(file.exists(fn)) return(fn)
  return(NULL)
}

try_find_src <- function(root, name) {
  pat <- glue::glue("{name}\\.[Rr]$")
  ff <- fs::dir_ls(path = root, regexp=pat, recurse=TRUE, ignore.case = TRUE)
  ff |> purrr::discard(~ stringr::str_detect(.x, "/_"))
}

try_find_root <- function(root=NULL, src_in = getOption("sourcoise.src_in"), quiet = TRUE) {
  if(!is.null(root)) {
    return(root)
  }
  if(!is.null(getOption("sourcoise.root"))) {
    if(fs::dir_exists(getOption("sourcoise.root")))
      return(getOption("sourcoise.root") |> fs::path_abs())
  }
  if(src_in == "project") {
    if(Sys.getenv("QUARTO_PROJECT_DIR") == "") {
      safe_find_root <- purrr::safely(rprojroot::find_root)
      root <- safe_find_root(
        rprojroot::is_quarto_project | rprojroot::is_r_package | rprojroot::is_rstudio_project)
      if(is.null(root$error))
        return(root$result |>
                 fs::path_expand() |>
                 fs::path_abs())
      else {
        if(!quiet)
          cli::cli_alert_warning("{root$error}")
        return(getwd() |>
                 fs::path_expand() |>
                 fs::path_abs())
      }
    }
    return(Sys.getenv("QUARTO_PROJECT_DIR") |> fs::path_expand() |> path_abs())
  }

  if(src_in == "file") {
    paths <- find_project_root(NULL, NULL)
    return( fs::path_join(c(paths$project_path, paths$doc_path)) |>
              fs::path_expand() |>
              path_abs() )
  }
  getwd()
}


find_project_root <- function(root=NULL, project_path = NULL, doc_path = NULL) {
  in_quarto <- FALSE
  if(is.null(doc_path)) {
    if(Sys.getenv("QUARTO_DOCUMENT_PATH") != "" | quarto::is_using_quarto()) {
      in_quarto <- TRUE
      doc_path <- Sys.getenv("QUARTO_DOCUMENT_PATH") |> path_abs()
    }
    else
      doc_path <- getwd() |> path_abs() |> fs::path_norm()
  }
  if(is.null(project_path)) {
    project_path <- Sys.getenv("QUARTO_PROJECT_DIR")
    if(project_path == "") {
      safe_find_root <- purrr::safely(rprojroot::find_root)
      project_path <- safe_find_root(rprojroot::is_quarto_project)
      if(!is.null(project_path$error))
        project_path <- safe_find_root(rprojroot::is_rstudio_project)
      else
        in_quarto <- TRUE
      if(!is.null(project_path$error))
        project_path$result <- getwd() |> fs::path_expand()
      project_path <- project_path$result |> fs::path_expand()
    }
  }
  project_path <- project_path |> fs::path_norm() |> path_abs()
  doc_path <- doc_path |>
    fs::path_expand() |>
    fs::path_abs() |>
    fs::path_rel(project_path)
  the_root <- NULL
  if(!is.null(root))
    if(fs::dir_exists(root))
      the_root <- root |>
    fs::path_expand() |>
    fs::path_abs()
  if(!is.null(getOption("sourcoise.root"))&is.null(root))
    if(fs::dir_exists(getOption("sourcoise.root")))
      the_root <- getOption("sourcoise.root") |>
    fs::path_expand() |>
    fs::path_abs()
  if(is.null(the_root))
    the_root <- project_path

  return(list(project_path = project_path, doc_path = doc_path, in_quarto = in_quarto, root = the_root))
}

path_abs <- function(path)
  path |> fs::path_expand() |> fs::path_abs()
