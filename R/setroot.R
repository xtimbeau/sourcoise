#' Force root
#'
#' `sourcoise()` mechanism to find root of the project automatically can be bypassed.
#' This function is equivalent to setting the `sourcoise.root` option, except for storage of cache at the level of the file. To allow thus behaviour, root should be set to `NULL`.
#'
#' @param root (default `NULL`, character) path of the root
#' @param quiet (default `TRUE` boolean) displays messages
#'
#' @returns root set (character)
#' @export
#'
#' @examples
#' dir <- tempdir()
#' set_sourcoise_root(dir)
#'

set_sourcoise_root <- function(root=NULL, quiet = TRUE) {
  root <- try_find_root(root, src_in = "project", quiet = quiet)
  options(sourcoise.root = root)
  root
}
