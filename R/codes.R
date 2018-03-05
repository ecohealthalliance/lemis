#' LEMIS Code Values
#'
#' This function returns a data frame with descriptions of all the code values
#' used in [lemis_data()].  This is useful for lookup
#' as well as merging with the data for more descriptive summaries.
#'
#' USFWS made some changes in the data dictionary in Febrary 2013.  Most
#' are changes slight tweaks in wording and in these cases we use the post-2013
#' description.  Some fields codes are not used post-2013 and these have `FALSE`
#' values in the `post_feb_2013` column.
#'
#' Original as-published PDFs of codes from pre- and post-2013 can be found in this package's
#' [\code{extdata/} directory](https://github.com/ecohealthalliance/lemis/tree/master/inst/extdata)
#'
#' \if{html}{
#'   \Sexpr[echo=FALSE, results=rd, stage=build]{
#'   in_pkgdown <- any(grepl("as_html.tag_Sexpr", sapply(sys.calls(), function(a) paste(deparse(a), collapse = "\n"))))
#'     if(in_pkgdown) {
#'       mytext <- c('In RStudio, this help file includes a searchable table of values.')
#'     } else {
#'     tmp <- tempfile(fileext=".html")
#'       htmlwidgets::saveWidget(DT::datatable(lemis::lemis_codes(), rownames = FALSE, width=700), tmp)
#'       mytext <- paste('Below is a searchable version of the LEMIS codes.',
#'       '\\\out{<div style="width:100\%">',
#'          paste(stringi::stri_subset_regex(readLines(tmp), "^</?(!DOCTYPE|meta|body|html)",negate=TRUE), collapse="\n"),
#'       '</div>}\n',
#'       sep="\n")
#'     }
#'     mytext
#'   }
#' }
#'
#' \if{text,latex}{The HTML version of this help file includes a searchable table of the LEMIS codes.}
#'
#' @return A tibble with field, code, code value, and whether the field is present post-2013.
#' @importFrom DT datatable
#' @importFrom htmlwidgets saveWidget
#' @importFrom stringi stri_subset_regex
#' @aliases codes
#' @seealso [lemis_metadata()] [lemis_data()]
#' @export
lemis_codes <- function() {
  lemis_codes_
}

#' LEMIS Field Descriptions
#'
#' This function returns a data frame field descriptions for [lemis_data()].
#'
#' \if{html}{
#'   \Sexpr[echo=FALSE, results=rd, stage=build]{
#'   in_pkgdown <- any(grepl("as_html.tag_Sexpr", sapply(sys.calls(), function(a) paste(deparse(a), collapse = "\n"))))
#'     if(in_pkgdown) {
#'       mytext <- lemis:::tabular(lemis::lemis_metadata())
#'     } else {
#'     tmp <- tempfile(fileext=".html")
#'       htmlwidgets::saveWidget(DT::datatable(lemis::lemis_codes(), rownames = FALSE, width=700), tmp)
#'       mytext <- paste('Below is a searchable version of the LEMIS codes.',
#'       '\\\out{<div style="width:100\%">',
#'          paste(stringi::stri_subset_regex(readLines(tmp), "^</?(!DOCTYPE|meta|body|html)",negate=TRUE), collapse="\n"),
#'       '</div>}',
#'       sep="\n")
#'     }
#'     mytext
#'   }
#' }
#'
#' \if{text,latex}{ \Sexpr[echo=FALSE, results=rd, stage=build]{lemis:::tabular(lemis::lemis_metadata())}}
#'
#' @return A tibble with field, code, code value, and whether the field is present post-2013.
#' @importFrom DT datatable
#' @importFrom htmlwidgets saveWidget
#' @importFrom stringi stri_subset_regex
#' @aliases metadata
#' @seealso [lemis_codes()] [lemis_data()]
#' @export
lemis_metadata <- function() {
  lemis_metadata_
}


# From https://cran.r-project.org/web/packages/roxygen2/vignettes/formatting.html#tables

tabular <- function(df, col_names = TRUE, ...) {
  stopifnot(is.data.frame(df))

  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))

  cols <- lapply(df, format, ...)
  contents <- do.call(
    "paste",
    c(cols, list(sep = " \\tab ", collapse = "\\cr\n  "))
  )

  if (col_names) {
    header <- paste0("\\bold{", colnames(df), "}", collapse = " \\tab")
    contents <- paste0(header, "\\cr\n  ", contents)
  }

  paste(
    "\\tabular{", paste(col_align, collapse = ""), "}{\n  ",
    contents, "\n}\n", sep = ""
  )
}
