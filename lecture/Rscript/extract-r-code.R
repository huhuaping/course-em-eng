#' Extract R code lines from a Rmarkdown file's code chunks
#'
#' @param rmd_file characters. file path of the Rmarkdown file 
#' @param out_file characters. file path of the output file
#'
#' @return a file containing the R code chunks
#' @export extract_r_code
#'
#' @examples
#' library(here)
#' library(fs)
#' rmd_file <- here("slide-reveal/lecture-20-sem-estimation.Rmd")
#' out_file <- here("slide-reveal/code/chpt20-sem-estimation.R")
#' extract_r_code(rmd_file = rmd_file, out_file = out_file)
#' 
extract_r_code <- function(rmd_file, out_file) {
  # check if the file exists
  if(!fs::file_exists(rmd_file)) {
    stop("The file does not exist.")
  }
  # read the Rmarkdown file
  rmd_content <- readLines(rmd_file)
  # detect the R code chunks
  r_chunk_start <- grep("^```\\{r", rmd_content)
  r_chunk_end <- grep("^```$", rmd_content)
  # check the length of the two vectors to make sure they are the same
  stopifnot(length(r_chunk_start) == length(r_chunk_end))
  # concatenate the two vectors
  r_chunk <- cbind(r_chunk_start, r_chunk_end)
  # extract R code chunks by using the start and end positions
  #i  <- 10
  r_code <- lapply(
    1:nrow(r_chunk), 
    function(i) {
      rmd_content[(r_chunk[i, 1] + 1):(r_chunk[i, 2] - 1)]
      }
    )
  # unlist the list
  r_code <- unlist(r_code)
  # write the R code chunks to a new file
  writeLines(r_code, out_file)
}



