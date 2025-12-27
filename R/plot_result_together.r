# plot_result_together.r
#
# Copyright (c) 2020 VIB (Belgium) & Babraham Institute (United Kingdom)
#
# Software written by Carlos P. Roca, as research funded by the European Union.
#
# This software may be modified and distributed under the terms of the MIT
# license. See the LICENSE file for details.


#' Plot slope error and skewness results together
#'
#' Plots results of slope error and skewness, together for initial and final
#' steps of AutoSpill, and calculation with positive and negative populations.
#'
#' @param flow.control List with data and metadata of a set of controls.
#' @param asp List with AutoSpill parameters.
#' 
#' @importFrom utils read.csv
#'
#' @return \code{NULL}.
#'
#' @references Roca \emph{et al}:
#'     AutoSpill is a principled framework that simplifies the analysis of multichromatic
#'     flow cytometry data.
#'     \emph{Nature Communications} 12, 2890 (2021);
#'     \href{https://doi.org/10.1038/s41467-021-23126-8}{https://doi.org/10.1038/s41467-021-23126-8}
#'     (2021).
#'
#' @seealso \code{\link{read.flow.control}} and
#'     \code{\link{get.autospill.param}}.
#'
#' @export
#' 
#' @examples A <- 2+2
plot_result.together <- function(flow.control, asp)
{
    # define table filenames for slope error and skewness

    rs.iter.width <- floor( log10( asp$rs.iter.max ) ) + 1

    slope.error.table <- file.path( asp$table.slope.error.dir,
        paste0( c(
            sprintf( "%s_%0*d", asp$slope.error.file.name, rs.iter.width, 0 ),
            asp$slope.error.file.name,
            asp$slope.error.posnegpop.file.name
        ), ".csv" ) )

    skewness.table <- file.path( asp$table.skewness.dir,
        paste0( c(
            sprintf( "%s_%0*d", asp$skewness.file.name, rs.iter.width, 0 ),
            asp$skewness.file.name,
            asp$skewness.posnegpop.file.name
        ), ".csv" ) )

    # read table data and plot densities together

    x.table.file.name <- list( slope.error.table, skewness.table )
    x.table.label <- c( "compensation error", "spillover skewness" )

    plot.dir <- c( asp$figure.slope.error.dir, asp$figure.skewness.dir )
    plot.file.name <- c( asp$slope.error.file.name, asp$skewness.file.name )

    for ( idx in 1:2 )
    {
        x.table <- lapply( x.table.file.name[[ idx ]], function ( xtfn )
            if ( ! is.null( xtfn ) && file.exists( xtfn ) )
                data.matrix( read.csv( xtfn )[ , -1 ] )
            else
                NA
        )

        x.table[ is.na( x.table ) ] <- NULL

        # remove skewness values in autofluorescence channel
        if ( idx == 2 )
            x.table <- lapply( x.table, function( xt ) {
                if ( ! is.null( flow.control$autof.marker.idx ) )
                    xt.plot <- xt[ - flow.control$autof.marker.idx,
                        - flow.control$autof.marker.idx ]
                else
                    xt.plot <- xt
                xt.plot
            } )

        x.label <- x.table.label[ idx ]

        plot.file.path <- file.path( plot.dir[ idx ],
            sprintf( "%s_together.png", plot.file.name[ idx ] ) )

        plot.density.log.together( x.table, x.label, plot.file.path, asp )
    }
}

