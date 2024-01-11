#' Prepares tables to gene set list for other functions, e.g. fgsea
#'
#' @param db_table 
#' @param set 
#' @param proteins 
#'
#' @return
#' @export
#'
#' @examples
db_table2sets <- function(db_table, 
                          set = "Name", 
                          proteins = "UniProt") {
  
  sets <- purrr::map(dplyr::pull(db_table, set, set) %>% 
                       unique() %>% 
                       setNames(., .), 
                     ~ db_table %>% 
                       dplyr::filter(!!dplyr::sym(set) == .x) %>% 
                       dplyr::pull(!!proteins))
  
  return(sets)
  
}


#' Prepares a GO gene set from a downloaded UniProt table for other functions, 
#' e.g. fgsea
#'
#' @param db_UniProt 
#' @param ontology 
#'
#' @return
#' @export
#'
#' @examples
db_UniProt2GO <- function(db_UniProt, 
                          ontology = c("all", "BP", "CC", "MF")) {
  
  if (!hasArg(db_UniProt)) stop("Please provide a database derived by UniProt.")
  
  if (!ontology[1] %in% c("all", "BP", "CC", "MF")) 
    stop("Please use one of the given options for <ontology>.")
  
  ontology <- c(all = "Gene Ontology (GO)", 
                BP = "Gene Ontology (biological process)", 
                CC = "Gene Ontology (cellular component)", 
                MF = "Gene Ontology (molecular function)")[ontology]
  
  db_preGO <- db_UniProt %>% 
    dplyr::filter(!is.na(!!dplyr::sym(ontology))) %>% 
    tidyr::separate_longer_delim(!!sym(ontology), "; ")
  
  db_GO <- db_table2sets(db_preGO, 
                         set = ontology, 
                         proteins = "Entry")
  
  return(db_GO)
  
}


#' Translate STRING IDs in downloaded STRING database to UniProt Accessions 
#'
#' @param db_STRING 
#' @param db_UniProt 
#'
#' @return
#' @export
#'
#' @examples
db_STRING2UniProt <- function(db_STRING, 
                              db_UniProt) {
  
  STRING2UniProt <- db_UniProt %>% 
    dplyr::mutate(STRING = gsub(";", "", STRING)) %>% 
    dplyr::pull(Entry, STRING)
  
  db_STRING_UniProt <- db_STRING %>% 
    dplyr::mutate(protein1 = ifelse(!is.na(STRING2UniProt[protein1]), 
                                    STRING2UniProt[protein1], 
                                    protein1)) %>% 
    dplyr::mutate(protein2 = ifelse(!is.na(STRING2UniProt[protein2]), 
                                    STRING2UniProt[protein2], 
                                    protein2))
  
  return(db_STRING_UniProt)
  
}
