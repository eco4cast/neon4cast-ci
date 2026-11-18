
# Update `scientificName`, `taxonID`, `taxonRank` and `morphospeciesID` using assignments from parataxonomy and expert taxonomy.
#
library(dplyr)
library(stringi)

clean_names <- function (x)
{
  s <- stringi::stri_split_regex(x, "/", simplify = TRUE)[,1]
  s <- stringi::stri_extract_all_words(s, simplify = TRUE)
  if (dim(s)[2] > 1)
    stringi::stri_trim(paste(s[, 1], s[, 2]))
  else stringi::stri_trim(s[, 1])
}

resolve_taxonomy <- function(sorting, para, expert){

  taxonomy <-
    left_join(sorting,
              para,
              by = "subsampleID")  %>%
    ## why are there so many other shared columns (siteID, collectDate, etc?  and why don't they match!?)
    ## we use `select` to avoid these
    left_join(expert,
      by = "individualID") %>%
    distinct() %>%
     ## Prefer the para table cols over the sorting table cols only for sampleType=="other carabid"
    mutate(taxonRank.x = ifelse(is.na(taxonRank.y) | sampleType != "other carabid", taxonRank.x, taxonRank.y),
           scientificName.x = ifelse(is.na(scientificName.y) | sampleType != "other carabid", scientificName.x, scientificName.y),
           taxonID.x = ifelse(is.na(taxonID.y) | sampleType != "other carabid", taxonID.x, taxonID.y),
           morphospeciesID.x =  ifelse(is.na(morphospeciesID.y) | sampleType != "other carabid", morphospeciesID.x, morphospeciesID.y)) %>%
      ## Prefer expert values where available
    mutate(taxonRank = ifelse(is.na(taxonRank), taxonRank.x, taxonRank),
           scientificName = ifelse(is.na(scientificName), scientificName.x, scientificName),
           taxonID = ifelse(is.na(taxonID), taxonID.x, taxonID),
           morphospeciesID =  ifelse(is.na(morphospeciesID), morphospeciesID.x, morphospeciesID),
           nativeStatusCode = ifelse(is.na(nativeStatusCode.y), nativeStatusCode.x, nativeStatusCode.y),
           sampleCondition = ifelse(is.na(sampleCondition.y), sampleCondition.x, sampleCondition.y)
           ) %>%
    select(-ends_with(".x"), -ends_with(".y"))
     ## WARNING: if the subsample is split into separate taxa by experts, we do not know
     ## how many of the total count should go to each taxon in the the split
     ## since only part of that subsample have been pinned.
     ## We should flag these cases in some manner.


  #### Should we add a "species" column, using morphospecies or the best available?
  ## Use morphospecies if available for higher-rank-only classifications,
  ## Otherwise, binomialize the scientific name:
  taxonomy <- taxonomy %>%
    mutate(morphospecies =
             ifelse(taxonRank %in% c("subgenus", "genus", "family", "order") & !is.na(morphospeciesID),
                    morphospeciesID,
                    clean_names(scientificName)
             )
    )

  ## Beetles must be identified as carabids by both sorting table and the taxonomists (~3 non-Carabidae slip through in sorting)
  beetles <- taxonomy %>%
    filter(grepl("carabid", sampleType)) %>%
    filter(family == "Carabidae" | is.na(family))

  beetles
}

