#' Title
#' 
#' Instruction by Henrik Hammaren
#' To find names of columns
#' go to
#' https://www.uniprot.org/uniprotkb?facets=reviewed%3Atrue&query=%28proteome%3AUP000005640%29
#' Click "Download"
#' Format "TSV"
#' Compressed "No"
#' Choose columns as you want
#' Bottom of page: "Generate URL for API"
#' Voila.
#'
#' @param url url decribing the anticipated columns from UniProt REST API
#'
#' @return
#' @export
#'
#' @examples
db_download_UniProt <- function(url = "https://rest.uniprot.org/uniprotkb/stream?fields=accession%2Creviewed%2Cid%2Cprotein_name%2Cgene_names%2Corganism_name%2Clength%2Cgo%2Cgo_p%2Cgo_c%2Cgo_f%2Cxref_corum%2Cxref_string&format=tsv&query=%28%28taxonomy_id%3A10090%29+AND+%28reviewed%3Atrue%29%29") {
  
  db <- vroom::vroom(url)
  
  return(db)
  
}


#' Wrapper function to download data from Reactome pathways
#'
#' @param url 
#' @param col_names 
#'
#' @return
#' @export
#'
#' @examples
download_Reactome <- function(
    url = "https://reactome.org/download/current/UniProt2Reactome.txt", 
    col_names = c("UniProt",
                  "Reactome_Pathway_Stable_identifier",
                  "URL",
                  "Name",
                  "Evidence_Code",
                  "Species")) {
  
  db <- vroom::vroom(file = url, col_names = col_names)
  
  return(db)
  
}


#' Wrapper function to download STRING databases
#'
#' @param url 
#'
#' @return
#' @export
#'
#' @examples
db_download_STRING <- function(type = c("physical_links", 
                                        "physical_links_detailed", 
                                        "physical_links_full", 
                                        "links", 
                                        "links_detailed", 
                                        "links_full"), 
                               taxid = 9606) {
  
  urltype <- c(physical_links = "protein.physical.links.v12.0", 
               physical_links_detailed = "protein.physical.links.detailed.v12.0", 
               physical_links_full = "protein.physical.links.full.v12.0", 
               links = "protein.links.v12.0", 
               links_detailed = "protein.links.detailed.v12.0", 
               links_full = "protein.links.full.v12.0")
  
  
  url <- paste0("https://stringdb-downloads.org/download/", 
                urltype[type[1]], "/", 
                taxid, ".", 
                urltype[type[1]], ".txt.gz")
  
  db_STRING <- vroom::vroom(url)
  
  return(db_STRING)
  
}


#' Wrapper function to download 'All levels' data from Reactome pathways
#'
#' @param url 
#' @param col_names 
#'
#' @return
#' @export
#'
#' @examples
download_Reactome_All_Levels <- function(
    url = "https://reactome.org/download/current/UniProt2Reactome_All_Levels.txt", 
    col_names= c("UniProt",
                 "Reactome_Pathway_Stable_identifier",
                 "URL",
                 "Name",
                 "Evidence_Code",
                 "Species")) {
  
  db <- vroom::vroom(file = url, col_names = col_names)
  
  return(db)
  
}


#' Wrapper function to download 'Reactions' data from Reactome pathways
#'
#' @param url 
#' @param col_names 
#'
#' @return
#' @export
#'
#' @examples
download_ReactomeReactions <- function(
    url = "https://reactome.org/download/current/UniProt2ReactomeReactions.txt", 
    col_names = c("UniProt",
                  "Reactome_Pathway_Stable_identifier",
                  "URL",
                  "Name",
                  "Evidence_Code",
                  "Species")) {
  
  db <- vroom::vroom(file = url, col_names = col_names)
  
  return(db)
  
}


db_download_ComplexPortal <- function(taxID) {
  
  url <-  paste0(
    "http://ftp.ebi.ac.uk/pub/databases/intact/complex/current/complextab/", 
    taxid, 
    ".tsv")
  
  db <- vroom::vroom(url)
  
  return(db)
  
}