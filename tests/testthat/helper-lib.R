# Linking to Extdata

FolderLocation <- function(subdir = NULL) {
  base <- system.file("extdata", package = "autospill")
  if (base == "") {stop("extdata not found — is the package installed?")}
  if (!is.null(subdir)) {base <- file.path(base, subdir)}
  return(base)
}

FolderPath <- FolderLocation(subdir="MM1")
MetadataPath <- list.files(FolderPath, pattern=".csv", full.names=TRUE)

