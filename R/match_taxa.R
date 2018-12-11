#' Taxon matching using Dyntaxa (https://dyntaxa.se)
#' matches dyntaxa_id in data with TaxonId in the supplied dyntaxa list (dyntaxa_list)
#' @param names Vector of scientific names.
#' @param ask Ask user in case of multiple matches.
#' @return Data frame with scientific name, scientific name ID and match type.
#' @export
match_taxa <- function(names, ask = TRUE) {

  matches <- unlist(lapply(names, dyntaxa_list$TaxonId), recursive = FALSE)

  results <- data.frame(scientificName = character(), scientificNameID = character(), stringsAsFactors = FALSE)

  # count no matches

  no <- NULL
   for (i in 1:length(matches)) {
    if (is.data.frame(matches[[i]])) {
      } else {
      no <- c(no, unames[i])
    }
  }

  message(sprintf("%s names, %s without matches, length(unames), length(no)))

  # populate data frame

  for (i in seq_along(matches)) {

    row <- list(scientificName = NA, scientificNameID = NA, match_type = NA)

    match <- matches[[i]]
    if (is.data.frame(match)) {

      if (nrow(match) == 1) {

        # single match

        row$scientificName = match$scientificname
        row$scientificNameID = match$lsid
        row$match_type = match$match_type

      } else if (ask) {

        # multiple matches

        print(match %>% select(AphiaID, scientificname, authority, status, match_type))
        message(unames[i])
        n <- readline(prompt = "Multiple matches, pick a number or leave empty to skip: ")
        s <- as.integer(n)
        if (!is.na(n) & n > 0 & n <= nrow(match)) {
          row$scientificName = match$scientificname[s]
          row$scientificNameID = match$lsid[s]
          row$match_type = match$match_type[s]
        }

      }

    }

    results <- bind_rows(results, row)

  }

  return(results[indices,])
}
