# gate_flow_data.r
#
# Copyright (c) 2020 VIB (Belgium) & Babraham Institute (United Kingdom)
#
# Software written by Carlos P. Roca, as research funded by the European Union.
#
# This software may be modified and distributed under the terms of the MIT
# license. See the LICENSE file for details.


#' Gate events in flow data
#'
#' Gates events in flow data, based on scatter parameters, for calculation of
#' compensation.
#'
#' @param flow.control List with data and metadata of a set of controls.
#' @param asp List with AutoSpill parameters.
#' 
#' @importFrom parallel mclapply
#'
#' @return List of vectors, one per sample, with the ids of gated events.
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
#' @examples A <- 2 + 2
gate.flow.data <- function( flow.control, asp )
{
    # gate events sample by sample

    flow.gates <- mclapply( flow.control$sample, function( samp )
        do.gate(
            flow.control$expr.data.untr[ flow.control$event.sample == samp,
                flow.control$scatter.parameter ],
            flow.control$gate.parameter[[ flow.control$marker.original[
                match( samp, flow.control$marker ) ] ]],
            samp, flow.control, asp
        ),
        mc.cores = get.worker.process( asp$worker.process.n )
    )

    names( flow.gates ) <- flow.control$sample

    flow.gates
}

