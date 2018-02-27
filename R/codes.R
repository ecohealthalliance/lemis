#' LEMIS Code Values
#'
#' This function returns a data frame with descriptions of all the code values
#' used in [lemis()].  This is useful for lookup (see searchable table in this help file)
#' as well as merging with the data for more descriptive summaries.
#'
#' There are some changes to coding pre- and post- Febrary 2013.  Most
#' are just slight changes in wording and in these cases we use the post-2013
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
#'       htmlwidgets::saveWidget(DT::datatable(lemis::lemis_codes(), rownames = FALSE), tmp)
#'       mytext <- paste('Below is a searchable version of the LEMIS codes.',
#'       '\\\out{<div style="width:100\%">',
#'          paste(stringi::stri_subset_regex(readLines(tmp), "^</?(!DOCTYPE|meta|body|html)",negate=TRUE), collapse="\n"),
#'       '</div>}',
#'       sep="\n")
#'     }
#'     mytext
#'   }
#' }
#' @return A tibble with field, code, code value, and whether the field is present post-2013.
#' @importFrom DT datatable
#' @importFrom htmlwidgets saveWidget
#' @export
lemis_codes <- function() {
  lemis_codes_
}
