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
#' @param outpath Default NULL so autospill folders will be created inside your
#' current working directory. Provide a file.path to another folder if this is
#' not desired. 
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
#' @examples
#' 
#' library(autospill)
#' asp <- get.autospill.param("paper")
#' 
get.autospill.param <- function( param.set = "minimal", outpath=NULL){

    get.param.function <- get0( sprintf( "get.autospill.param.%s", param.set ) )

    check.critical( ! is.null( get.param.function ), "bad param set" )

    asp <- get.param.function()

    if (!is.null(outpath) & param.set != "minimal"){

        figure.scatter.dir.base <- file.path(outpath, asp$figure.scatter.dir.base)
        asp$figure.scatter.dir.base <- figure.scatter.dir.base

        figure.gate.dir <- file.path(outpath, asp$figure.gate.dir)
        asp$figure.gate.dir <- figure.gate.dir

        figure.compensation.dir <- file.path(outpath, asp$figure.compensation.dir)
        asp$figure.compensation.dir <- figure.compensation.dir

        figure.convergence.dir <- file.path(outpath, asp$figure.convergence.dir)
        asp$figure.convergence.dir <- figure.convergence.dir

        figure.spillover.dir <- file.path(outpath, asp$figure.spillover.dir)
        asp$figure.spillover.dir <- figure.spillover.dir

        figure.slope.error.dir <- file.path(outpath, asp$figure.slope.error.dir)
        asp$figure.slope.error.dir <- figure.slope.error.dir

        figure.skewness.dir <- file.path(outpath, asp$figure.skewness.dir)
        asp$figure.skewness.dir <- figure.skewness.dir

        table.compensation.dir <- file.path(outpath, asp$table.compensation.dir)
        asp$table.compensation.dir <- table.compensation.dir

        table.convergence.dir <- file.path(outpath, asp$table.convergence.dir)
        asp$table.convergence.dir <- table.convergence.dir

        table.spillover.dir <- file.path(outpath, asp$table.spillover.dir)
        asp$table.spillover.dir <- table.spillover.dir

        table.slope.error.dir <- file.path(outpath, asp$table.slope.error.dir)
        asp$table.slope.error.dir <- table.slope.error.dir

        table.skewness.dir <- file.path(outpath, asp$table.skewness.dir)
        asp$table.skewness.dir <- table.skewness.dir

    asp
    }
}

