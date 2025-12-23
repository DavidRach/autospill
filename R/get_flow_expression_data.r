# get_flow_expression_data.r
#
# Copyright (c) 2020 VIB (Belgium) & Babraham Institute (United Kingdom)
#
# Software written by Carlos P. Roca, as research funded by the European Union.
#
# This software may be modified and distributed under the terms of the MIT
# license. See the LICENSE file for details.


#' Returns a matrix with flow expression data.
#' 
#' @param flow.set TODOLIST
#' @param flow.control TODOLIST
#' 
#' @importFrom flowCore colnames exprs
#' 
#' @return TODOLIST
#' 
#' @examples A <- 2+2
#' 
#' @noRd
get.flow.expression.data <- function(flow.set, flow.control)
{
    flow.expr.data <- lapply( 1 : length( flow.control$sample ),
        function( fs.idx ) {
            expr.data <- exprs( flow.set[[ fs.idx ]] )

            flow.the.sample <- flow.control$sample[ fs.idx ]
            flow.the.event <- sprintf( "%s.%0*d", flow.the.sample,
                flow.control$event.number.width, 1 : nrow( expr.data ) )
            rownames( expr.data ) <- flow.the.event

            expr.data <- expr.data[ , flow.control$scatter.and.marker.original ]
            colnames( expr.data ) <- flow.control$scatter.and.marker

            expr.data
        } )

    do.call( rbind, flow.expr.data )
}

