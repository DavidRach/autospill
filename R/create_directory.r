# create_directory.r
#
# Copyright (c) 2020 VIB (Belgium) & Babraham Institute (United Kingdom)
#
# Software written by Carlos P. Roca, as research funded by the European Union.
#
# This software may be modified and distributed under the terms of the MIT
# license. See the LICENSE file for details.


#' Creates figure and table directories.
#' Returns directories for scatter figures.
#'
#' @param flow.control Autospill informed parameters relating to the flowframes in the flowSet.
#' @param asp List with AutoSpill parameters.
#' @param outpath Default NULL which results in the folders being created in the 
#'     current working directory. Provide a file.path if you wish the created 
#'     created folders to end up elsewhere.
#' 
#' @return Creates the folders and returns the location information parameters
#' 
#' @examples A <- "See unit test, this is an internal function for read_flow_control"
#' 
#' @noRd
create.directory <- function(flow.control, asp, outpath){
    prefix <- function(x) {
        if (is.null(x)) return(NULL)
        if (is.null(outpath)) x else file.path(outpath, x)
    }

    if (!is.null(asp$figure.scatter.dir.base)) {
        figure.scatter.dir <- sprintf(
            "%s_%s",
            asp$figure.scatter.dir.base,
            flow.control$sample
        )
        figure.scatter.dir <- prefix(figure.scatter.dir)
        names(figure.scatter.dir) <- flow.control$sample
    } else {
        figure.scatter.dir <- NULL
    }

    figure.dir <- c(
        prefix(asp$figure.compensation.dir),
        prefix(asp$figure.convergence.dir),
        prefix(asp$figure.gate.dir),
        prefix(asp$figure.skewness.dir),
        prefix(asp$figure.slope.error.dir),
        prefix(asp$figure.spillover.dir),
        figure.scatter.dir
    )

    table.dir <- c(
        prefix(asp$table.compensation.dir),
        prefix(asp$table.convergence.dir),
        prefix(asp$table.skewness.dir),
        prefix(asp$table.slope.error.dir),
        prefix(asp$table.spillover.dir)
    )

    for (ftd in c(figure.dir, table.dir)) {
        if (!is.null(ftd) && !file.exists(ftd)) {
            dir.create(ftd, recursive = TRUE)
        }
    }

    figure.scatter.dir
}
