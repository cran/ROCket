.onLoad <- function(libname, pkgname) {
  rkt_options <- list(
    rkt_pride_colors = c('#FF0018', '#FFA52C', '#FFFF41', '#008018', '#0000F9', '#86007D')
  )
  options(rkt_options)
  
  invisible()
}
