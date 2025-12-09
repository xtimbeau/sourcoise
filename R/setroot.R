#' Set the Root Directory for Sourcoise
#'
#' This function allows you to manually set the root directory for the sourcoise package,
#' bypassing the automatic root detection mechanism used by `sourcoise()`. Setting the root
#' directory affects where sourcoise looks for files and stores cache data.
#'
#' By default, sourcoise automatically detects the project root. This function is equivalent
#' to setting the `sourcoise.root` option directly, except when dealing with file-level cache
#' storage. To enable file-level cache storage behavior, set root to `NULL`.
#'
#' @param root Path to the desired root directory. If `NULL` (default), sourcoise will
#'   attempt to automatically detect the project root. Can be an absolute or relative path.
#' @param quiet Logical value indicating whether to suppress messages during root detection.
#'   Default is `TRUE` (messages suppressed).
#'
#' @returns The root path that was set (character string), invisibly returned by
#'   `try_find_root()`.
#'
#' @export
#'
#' @examples
#' # Set root to a temporary directory
#' dir <- tempdir()
#' set_sourcoise_root(dir)
#'
#' # Reset to automatic detection
#' set_sourcoise_root(NULL)
#'
#' # Set root with messages enabled
#' set_sourcoise_root(dir, quiet = FALSE)
#'

set_sourcoise_root <- function(root=NULL, quiet = TRUE) {
  root <- try_find_root(root, src_in = "project", quiet = quiet)
  options(sourcoise.root = root)
  root
}
