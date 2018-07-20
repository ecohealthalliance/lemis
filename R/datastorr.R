#' Load the LEMIS database
#'
#' This function loads the LEMIS database as an out-of-memory **dplyr**
#' table.  It will download the latest database from the remote repo the first
#' time you use it or if you specify a version of the database not on your
#' computer.  Available versions can be found at
#' \url{https://github.com/ecohealthalliance/lemis/releases}
#'
#'
#' @param version Version number.  The default will load the most
#'   recent version on your computer or the most recent version known
#'   to the package if you have never downloaded the data before.
#'   With \code{lemis_del}, specifying \code{version=NULL} will
#'   delete \emph{all} data sets.
#'
#' @param path Path to store the data at.  If not given,
#'   \code{datastorr} will use \code{rappdirs} to find the best place
#'   to put persistent application data on your system.  You can
#'   delete the persistent data at any time by running
#'   \code{lemis_del(NULL)} (or \code{lemis_del(NULL, path)} if you
#'   use a different path).
#'
#' @export
lemis_data <- function(version = NULL, path = NULL) {
  datastorr::github_release_get(lemis_info(path), version)
}

#' @export
#' @rdname lemis_data
#'
#' @param local Logical indicating if local or github versions should
#'   be polled.  With any luck, \code{local=FALSE} is a superset of
#'   \code{local=TRUE}.  For \code{lemis_version_current}, if
#'   \code{TRUE}, but there are no local versions, then we do check
#'   for the most recent github version.
#'
#' @importFrom datastorr github_release_versions
lemis_versions <- function(local = TRUE, path = NULL) {
  datastorr::github_release_versions(lemis_info(path), local)
}

#' @export
#' @rdname lemis_data
lemis_version_current <- function(local = TRUE, path = NULL) {
  datastorr::github_release_version_current(lemis_info(path), local)
}

#' @export
#' @rdname lemis_data
lemis_del <- function(version, path = NULL) {
  datastorr::github_release_del(lemis_info(path), version)
}

## Core data:
lemis_info <- function(path) {
  datastorr::github_release_info(
    "ecohealthalliance/lemis",
    private = TRUE,
    filename = NULL,
    read = lemis::fst_tbl,
    path = path
  )
}

#' Maintainer-only function for releasing data.  This will look at
#' the version in the DESCRIPTION file and make a data release if the
#' GitHub repository contains the same version as we have locally.
#' Requires the \code{GITHUB_TOKEN} environment variable to be set.
#'
#' @title Make a data release.
#' @param ... Parameters passed through to \code{\link{github_release_create}}
#' @param path Path to the data (see \code{\link{lemis}}).
lemis_release <- function(..., path = NULL) {
  datastorr::github_release_create(lemis_info(path), ...)
}
