# Linking to Extdata

FolderLocation <- function(subdir = NULL) {
  base <- system.file("extdata", package = "autospill")
  if (base == "") {stop("extdata not found — is the package installed?")}
  if (!is.null(subdir)) {base <- file.path(base, subdir)}
  return(base)
}

FolderPath <- FolderLocation(subdir="MM1")
MetadataPath <- list.files(FolderPath, pattern=".csv", full.names=TRUE)

# The FlowSet Objects

#asp <- get.autospill.param("paper", outpath)
#flow.scatter.parameter <- autospill:::read.scatter.parameter(asp)

# Internal Generator

FlowSetReturn <- function(control.dir, control.def.file, asp, flow.scatter.parameter){

flow.set.marker.table <- read.marker(control.dir, control.def.file, asp)
flow.set.marker <- flow.set.marker.table[[ 1 ]]
flow.set.marker.corrected <- flow.set.marker.table[[ 2 ]]
control.table <- read.csv( control.def.file, na.strings = "",
                                 stringsAsFactors = FALSE )
control.table.order <- order( control.table$dye )
flow.file.name <- control.table$filename[ control.table.order ]
flow.marker <- control.table$dye[ control.table.order ]
flow.antigen <- control.table$antigen[ control.table.order ]
flow.wavelength <- control.table$wavelength[ control.table.order ]
names(flow.wavelength) <- flow.marker
flow.marker.n <- length(flow.marker)
flow.autof.marker.idx <- which( flow.antigen == asp$antigen.autof )
if (length( flow.autof.marker.idx ) != 1){flow.autof.marker.idx <- NULL}

flow.scatter.and.marker <- c( flow.scatter.parameter, flow.marker )
flow.scatter.and.marker.matched.bool <- flow.scatter.and.marker %in% flow.set.marker
flow.scatter.parameter.original <- flow.scatter.parameter
flow.scatter.parameter <- flow.set.marker.corrected[
  match( flow.scatter.parameter.original, flow.set.marker ) ]
flow.marker.original <- flow.marker
flow.marker <- flow.set.marker.corrected[match( flow.marker.original, flow.set.marker ) ]
flow.scatter.and.marker.original <- flow.scatter.and.marker
flow.scatter.and.marker <- flow.set.marker.corrected[match( flow.scatter.and.marker.original, flow.set.marker ) ]
flow.scatter.and.marker.label <- c( flow.scatter.parameter.original,
        ifelse( ! is.na( flow.antigen ),
            paste0( flow.marker.original, " - ", flow.antigen ),
            flow.marker.original ) )
    names( flow.scatter.and.marker.label ) <- flow.scatter.and.marker
flow.sample <- flow.marker
flow.sample.n <- length( flow.sample )

# read fcs files
flow.set <- lapply( flow.file.name, function( ff ) {
      if(is(control.dir, "flowSet")) {
        control.dir[[ff]]
      } else {
        flowCore::read.FCS( file.path( control.dir, ff ), transformation = NULL,
          truncate_max_range=FALSE)  
      }
    })

flow.set.resolution.read <- sapply( flow.set, function( fs )
        as.numeric( keyword( fs )[["$P1R" ]] ) )

flow.set.resolution <- max( flow.set.resolution.read )
flow.expr.data.min <- 0
flow.expr.data.max <- flow.set.resolution
flow.expr.data.max.ceil <- ceiling( flow.expr.data.max / asp$data.step ) * asp$data.step
flow.sample.event.number.max <- 0

    for ( fs.idx in 1 : flow.sample.n )
    {
        flow.sample.data <- flow.set[[ fs.idx ]]

        flow.sample.event.number <- nrow( exprs( flow.sample.data ) )

        if ( flow.sample.event.number > flow.sample.event.number.max )
            flow.sample.event.number.max <- flow.sample.event.number
    }

    flow.event.number.width <-
        floor( log10( flow.sample.event.number.max ) ) + 1
    flow.event.regexp <- sprintf( "\\.[0-9]{%d}$", flow.event.number.width )

    # make preliminary control info

    flow.control <- list(
        antigen = flow.antigen,
        autof.marker.idx = flow.autof.marker.idx,
        event.number.width = flow.event.number.width,
        expr.data.max = flow.expr.data.max,
        expr.data.max.ceil = flow.expr.data.max.ceil,
        expr.data.min = flow.expr.data.min,
        flow.set = flow.set,
        marker = flow.marker,
        marker.n = flow.marker.n,
        marker.original = flow.marker.original,
        sample = flow.sample,
        scatter.and.marker = flow.scatter.and.marker,
        scatter.and.marker.label = flow.scatter.and.marker.label,
        scatter.and.marker.original = flow.scatter.and.marker.original,
        scatter.parameter = flow.scatter.parameter,
        wavelength = flow.wavelength
    )

  ReturnObject <- list(flow.set=flow.set, flow.control=flow.control)
  return(ReturnObject)
}


  