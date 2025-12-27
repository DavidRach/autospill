# get_autospill_param.r
#
# Copyright (c) 2020 VIB (Belgium) & Babraham Institute (United Kingdom)
#
# Software written by Carlos P. Roca, as research funded by the European Union.
#
# This software may be modified and distributed under the terms of the MIT
# license. See the LICENSE file for details.


#' Get AutoSpill parameters
#'
#' Returns parameters for running a calculation of compensation with AutoSpill.
#'
#' @param param.set Character string with the name of a parameter set. Posible
#'     values are:
#'     \describe{
#'         \item{\code{"minimal"}}{No figures or tables.}
#'         \item{\code{"final.step"}}{With figures and tables at final step.}
#'         \item{\code{"paper"}}{With all figures and tables used in AutoSpill
#'             paper.}
#'         \item{\code{"website"}}{With all figures and tables used in AutoSpill
#'             website.}
#'     }
#'
#' @return List of AutoSpill parameters. As reference, see source file
#'     \file{get_autospill_param_minimal.r}
#'
#' @references Roca \emph{et al}:
#'     AutoSpill is a principled framework that simplifies the analysis of multichromatic
#'     flow cytometry data.
#'     \emph{Nature Communications} 12, 2890 (2021);
#'     \href{https://doi.org/10.1038/s41467-021-23126-8}{https://doi.org/10.1038/s41467-021-23126-8}
#'     (2021).
#' @export
#' 
#' @examples A <- 2 + 2
get.autospill.param <- function( param.set = "minimal" )
{
    get.param.function <- get0( sprintf( "get.autospill.param.%s", param.set ) )

    check.critical( ! is.null( get.param.function ), "bad param set" )

    get.param.function()
}

