# check_critical.r
#
# Copyright (c) 2020 VIB (Belgium) & Babraham Institute (United Kingdom)
#
# Software written by Carlos P. Roca, as research funded by the European Union.
#
# This software may be modified and distributed under the terms of the MIT
# license. See the LICENSE file for details.


#' Checks condition, if not true prints error message and stops execution.
#' @param condition TODOLIST
#' @param error.msg TODOLIST
#' 
#' @return When FALSE, prints an error and stops. 
#' 
#' @examples
#' Folder <- system.file("extdata",package="autospill")
#' FolderPath <- file.path(Folder, "MM1")
#' MetadataPath <- list.files(Folder, pattern=".csv", full.names=TRUE)
#' control <- read.csv(MetadataPath, stringsAsFactors = FALSE)
#' NoDuplicates <- autospill:::check.critical(!duplicated(control), "Data contains duplicates")
#' 
#' @noRd
check.critical <- function(condition, error.msg)
{
    if ( ! all( condition ) )
    {
        cat( sprintf( "ERROR: %s\n", error.msg ), file = stderr() )
        stop()
    }
}

