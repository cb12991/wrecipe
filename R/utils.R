`%||%` <- function(x, y) {
  if (is.null(x)) {
    return(y)
  } else {
    return(x)
  }
}

`%|%` <- function(x, y) {
  if (is.na(x)) {
    return(y)
  } else {
    return(x)
  }
}

namespace_references <- function() {
  R6::R6Class
  rlang::exec
  jpeg::readJPEG
  png::readPNG
  mime::guess_type
  xml2::download_html
}
