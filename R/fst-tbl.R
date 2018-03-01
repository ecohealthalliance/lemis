#' Create a tbl from an fst file
#'
#' A convenience wrapper to create a table from a single-file fst source
#'
#' @param path Path to the fst file
#' @export
#' @return A tbl with an fst source
#' @importFrom fstplyr src_fst tbl
#' @importFrom tools file_path_sans_ext
fst_tbl <- function(path) {
  path <- normalizePath(path)
  dir <- dirname(path)
  tblname <- basename(tools::file_path_sans_ext(path))
  fst_src <- fstplyr::src_fst(dir)
  fst_tbl <- fstplyr::tbl(fst_src, tblname)
  return(fst_tbl)
}
