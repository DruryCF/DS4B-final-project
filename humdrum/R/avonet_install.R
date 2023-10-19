#' Installs avonet dataset
#'
#' @param filename A character defining the file destination
#'
#' @return Downloads avonet dataset at specified location
#' @export
#'
#' @examples
#' avonet_install(filename = "raw_data/avonet_dataset.xlsx")
avonet_install <- function(filename){

  message("Downloading AvoNet...\n")

  utils::download.file(url = "https://figshare.com/ndownloader/files/34480856?private_link=b990722d72a26b5bfead", destfile = filename, mode = "wb")

}
