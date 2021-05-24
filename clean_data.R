clean_data <- function(author, year) {
  author <- remove_accents(tolower(stringr::str_extract(dat$authors, "\\w+")))
  
  create_code_for_equals <- function(author, year) {
    combo <- paste(author, year)
    # Missing values in both author and year should not be considered
    i <- !(is.na(author) & is.na(year))
    nm <- unique(combo[i])
    lookup <- setNames(seq_along(nm), nm)
    lookup[combo]
  }
  
  code <- unname(create_code_for_equals(author, year))
  
  results_repeated <- function(code) {
    freq <- table(code)
    repeated <- freq[freq > 1]
    repeated <- as.numeric(names(repeated))
    lapply(repeated, function(x) {
      which(code == x)
    })
  }
  # You can add 1 to the results of the function to work with the spreadsheet
  
  res <- vector("list", 3L)
  
  res <- list(
    repeated_references = results_repeated(code),
    without_year = which((!is.na(author)) & is.na(year)),
    without_author = which(is.na(author) & (!is.na(year)))
  )
  
  res
  
}

remove_accents <- function(string) {
  stringi::stri_trans_general(string, id = "Latin-ASCII")
}
