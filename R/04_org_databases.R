#' Sets up annotaion database for biological Id translation and functional enrichment
#'
#' @param dataset dataset
#' @param OrgDb name of annotation package from Bioconductor
#' @param taxId taxonomy Id
#'
#' @return
#' @export
#'
#'
org_setup_database <- function(OrgDb = "org.Hs.eg.db") {
  
  
  # Check if available to load
  if (!requireNamespace(package = OrgDb, quietly = TRUE)) {
    
    stop(paste0("Trying to install ", OrgDb, " from Bioconductor."), 
         call. = FALSE)
    
    # Install
    # Check BiocManager
    if (!requireNamespace("BiocManager", quietly = TRUE))
      install.packages("BiocManager")
    
    # Install annotation database
    BiocManager::install(OrgDb)
    
  }
  
  
  
  # Check if all worked
  if (requireNamespace(package = OrgDb, quietly = TRUE)) {
    
    # Attach package for clusterProfiler
    suppressPackageStartupMessages(library(OrgDb, quietly = TRUE, character.only = TRUE))
    
    cat(paste0("Annotation database ", OrgDb, " set up.\n"))
    return(invisible(TRUE))
    
  } else {
    
    stop("Annotation database not found. Make sure the Annotation package can be installed or install it manuall from Bioconductor.")
    
  }
  
}


#' Title
#'
#' @param OrgDb 
#'
#' @return
#' @export
#'
#' @examples
#' # Get possible keytypes to query an organism database
#' get_org_keytypes("org.Hs.eg.db")
org_get_keytypes <- function(OrgDb) {
  
  if (!hasArg(OrgDb)) stop("Please specify the name of an organism database <OrgDb>.", call. = FALSE)
  
  keytypes_org <- AnnotationDbi::keytypes(eval(parse(text = OrgDb)))
  
  return(keytypes_org)
  
}


#' Title
#'
#' @param OrgDb 
#'
#' @return
#' @export
#'
#' @examples
#' # Get possible columns to retrieve from an organism database
#' get_org_columns("org.Hs.eg.db")
org_get_columns <- function(OrgDb) {
  
  if (!hasArg(OrgDb)) stop("Please specify the name of an organism database <OrgDb>.", call. = FALSE)
  
  columns_org <- AnnotationDbi::columns(eval(parse(text = OrgDb)))
  
  return(columns_org)
  
}


#' Queries data from Annotation packages
#'
#' @param keys vector of protein identifiers (see keytypes_org())
#' @param columns data columns to return(see columns_org())
#' @param OrgDb string of Annotation package name to use
#' @param keytype type of supplied keys (identified automatically if not
#' provided)
#' @param output format of output ("vector.keep" (default), "vector.na,
#' "vector.rm", "mapping.na", "mapping.rm", "TERM2GENE")
#' @param ... arguments for TERM2GENE function (subtype = "CC", "BP", or "MF")
#'
#' @return
#' @export
#'
#'
org_select <- function(keys,
                       columns,
                       keytype,
                       OrgDb,
                       paste = NULL, 
                       rm.na = F, 
                       fill.na = F, 
                       to.vector = T, 
                       as.T2G = F, 
                       ...) {
  
  # Check keys
  if (!hasArg(keys)) {
    stop("No <keys> given.", call. = F)
  }
  
  # Check OrgDb
  if (!hasArg(OrgDb))
    stop("No <OrgDb> defined.", call. = F)
  
  
  # Test OrgDb
  if (!require(OrgDb, character.only = T))
    stop(paste0("Annotations package ", OrgDb, " could not be installed."))
  
  
  # Check keytype 
  if (!hasArg(keytype)) 
    stop("No <keytype> provided.", call. = F)
  
  # Test keytype
  if (!keytype %in% AnnotationDbi::keytypes(eval(parse(text = OrgDb))))
    stop(paste0("<keytype> ", keytype, " is incompatible with ", OrgDb, "."))
  
  
  # Test keys
  # if (all(!keys %in%
  #         AnnotationDbi::keys(eval(parse(text = OrgDb)), keytype = keytype)))
  #   stop("No keys match the database. Change the keytype argument or remove it.")
  
  
  # Test columns
  if (!hasArg(columns)) 
    stop("No <columns> provided.", call. = F)
  
  # AnnotationDbi::columns(eval(parse(text = OrgDb)))
  if (any(!columns %in%
          AnnotationDbi::columns(eval(parse(text = OrgDb))))) 
    stop("No <columns> were found.", call. = F)
  
  if (all(!columns %in%
          AnnotationDbi::columns(eval(parse(text = OrgDb))))) 
    stop("No <columns> were found.", call. = F)
  
  
  
  # Query annotation database
  mapping <- AnnotationDbi::select(x = eval(parse(text = OrgDb)),
                                       keys = keys,
                                       columns = columns,
                                       keytype = keytype) %>%
    suppressMessages() %>% 
    dplyr::as_tibble()
  
  
  # Format output
  output.data <- .mapping2output(mapping = mapping,
                                 keys = keys,
                                 database = "org",
                                 paste = paste, 
                                 rm.na = rm.na, 
                                 fill.na = fill.na, 
                                 to.vector = to.vector, 
                                 as.T2G = as.T2G,
                                 ...)
  
  
  # Return
  return(output.data)
  
}


#' Formats mapping from select methods
#'
#' @param mapping 
#' @param keys keys
#' @param database database of origin ("org" or "UniProt")
#' @param paste 
#' @param rm.na 
#' @param fill.na 
#' @param to.vector 
#' @param as.T2G 
#' @param ... arguments for TERM2GENE function (subtype = "CC", "BP", or "MF")
#'
#' @return
#' @export
#'
#'
.mapping2output <- function(mapping, 
                            keys, 
                            database, 
                            paste = NULL, 
                            rm.na = F, 
                            fill.na = F, 
                            to.vector = F, 
                            as.T2G = F,
                            ...) {
  
  # Output TERM2GENE database
  if (as.T2G && database == "org") {
    mapping <- .org_mapping_2_TERM2GENE(mapping = mapping,
                                        ...)
    return(mapping)
  } else if (as.T2G && database == "UniProt") {
    mapping <- .UniProt_mapping_2_TERM2GENE(mapping = mapping)
    return(mapping)
  }
  
  
  # Merge multiple entries per key
  if (!is.null(paste)) {
    mapping <- mapping %>% 
      dplyr::group_by(!!dplyr::sym(colnames(mapping)[1])) %>% 
      dplyr::summarise(dplyr::across(.fns = ~paste(.x, collapse = ";"))) 
  }
  
  # Remove NAs
  if (rm.na) {
    mapping <- mapping %>% 
      dplyr::filter(!is.na(!!dplyr::sym(a)))
  }
  
  # Fill NAs
  if (fill.na) {
    mapping[is.na(mapping[, 2]), 2] <- mapping[is.na(mapping[, 2]), 1]
  }
  
  # Pull data to vector
  if (to.vector) {
    if (ncol(mapping) > 2)
      message(paste0("Query resulted in more than one mapped column. Vector contains ", 
                     colnames(mapping)[2], "."))
    mapping <- mapping %>% 
      dplyr::pull(2, 1)
    mapping <- mapping[keys]
  }
  
  return(mapping)
  
}


#' Exports TERM2GENE dataframe from select_org
#'
#' @param mapping output from select_org
#' @param subtype subtype of mapping; e.g. Ontology
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
.org_mapping_2_TERM2GENE <- function(mapping, subtype) {
  
  if ("GO" %in% colnames(mapping)) {
    
    # Filter specific ontology
    if (hasArg(subtype)) {
      
      mapping <- mapping %>%
        dplyr::filter(ONTOLOGY == subtype)
      
    }
        
    # Remove columns
    TERM2GENE <- mapping %>%
      dplyr::select(c(GO, UNIPROT)) %>%
      dplyr::mutate(GO = org_select(keys = GO,
                                    columns = "TERM",
                                    keytype = "GOID",
                                    OrgDb = "GO.db", 
                                    paste = NULL, 
                                    rm.na = F, 
                                    fill.na = F, 
                                    to.vector = T, 
                                    as.T2G = F))
    
    
    
  } else if ("GOALL" %in% colnames(mapping)) {
    
    # Filter specific ontology
    if (hasArg(subtype)) {
      
      mapping <- mapping %>%
        dplyr::filter(ONTOLOGYALL == subtype)
      
    }
    
    # Remove columns
    TERM2GENE <- mapping %>%
      dplyr::select(c(GOALL, UNIPROT)) %>%
      dplyr::filter(!base::duplicated(paste(GOALL, UNIPROT))) %>% 
      dplyr::mutate(GOALL = org_select(keys = GOALL,
                                       columns = "TERM",
                                       keytype = "GOID",
                                       OrgDb = "GO.db", 
                                       paste = NULL, 
                                       rm.na = F, 
                                       fill.na = F, 
                                       to.vector = T, 
                                       as.T2G = F))
    
    
  } else if ("PATHNAME" %in% colnames(mapping)) {
    
    
    # Remove columns
    TERM2GENE <- mapping %>%
      dplyr::mutate(PATHNAME = substring(text = PATHNAME,
                                         first = regexpr(pattern = ": ", text = PATHNAME) + 2)) %>%
      dplyr::mutate(UNIPROT = select_org(keys = ENTREZID,
                                         columns = "UNIPROT",
                                         keytype = "ENTREZID", 
                                         paste = NULL, 
                                         rm.na = F, 
                                         fill.na = F, 
                                         to.vector = T, 
                                         as.T2G = F)) %>%
      dplyr::select(c(PATHNAME, UNIPROT))
    
  }
  
  
  colnames(TERM2GENE) <- c("TERM", "GENE")
  
  
  #Remove redundant entries
  TERM2GENE <- TERM2GENE[!duplicated(paste0(TERM2GENE$TERM, TERM2GENE$GENE)), ]
  
  
  # Return
  return(TERM2GENE)
  
}


#' Exports TERM2GENE dataframe from select_org
#'
#' @param mapping output from select_UniProt
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
.UniProt_mapping_2_TERM2GENE <- function(mapping) {
  
  if ("SUBCELLULAR-LOCATIONS" %in% colnames(mapping) ||
      "KEYWORDS" %in% colnames(mapping)) {
    
    mapping <- dplyr::pull(mapping, 2, 1) %>%
      strsplit(";")
    
    terms <- unlist(mapping)
    
    mapping.length <- lapply(mapping, length)
    
    genes <- c()
    
    for (i in seq_along(mapping.length)) {
      
      genes <- c(genes, rep(names(mapping.length)[i],
                            times = mapping.length[[i]]))
      
    }
    
    TERM2GENE <- dplyr::tibble(TERM = terms,
                               GENE = genes)
    
  } else if ("GO" %in% colnames(mapping)) {
    
    mapping <- pull_data(mapping) %>%
      strsplit("; ")
    
    terms <- unlist(mapping)
    
    mapping.length <- lapply(mapping, length)
    
    genes <- c()
    
    for (i in seq_along(mapping.length)) {
      
      genes <- c(genes, rep(names(mapping.length)[i],
                            times = mapping.length[[i]]))
      
    }
    
    TERM2GENE <- dplyr::tibble(TERM = terms,
                               GENE = genes)
    
  } else if ("GO-ID" %in% colnames(mapping)) {
    
    mapping <- pull_data(mapping) %>%
      strsplit("; ")
    
    terms <- unlist(mapping)
    
    mapping.length <- lapply(mapping, length)
    
    genes <- c()
    
    for (i in seq_along(mapping.length)) {
      
      genes <- c(genes, rep(names(mapping.length)[i],
                            times = mapping.length[[i]]))
      
    }
    
    TERM2GENE <- dplyr::tibble(TERM = terms,
                               GENE = genes)
    
  } else {
    
    TERM2GENE <- mapping
    
  }
  
  
  #Remove redundant entries
  TERM2GENE <- TERM2GENE[!duplicated(paste0(TERM2GENE[[1]], TERM2GENE[[2]])), ]
  
  
  # Return
  return(TERM2GENE)
  
}



#' Translates Uniprot protein identifiers to gene symbols
#'
#' @param proteins UniProt protein Ids
#' @param database dataset
#'
#' @return
#' @export
#'
#'
p2g <- function(proteins, database = "org.Hs.eg.db") {
  
  gene_symbols <- org_select(keys = proteins, 
                              columns = "SYMBOL",
                              keytype = "UNIPROT", 
                              database = database, 
                              paste = ";", 
                              rm.na = F, 
                              fill.na = T, 
                              to.vector = T, 
                              as.T2G = F) %>% 
    pOmics::strsplit_keep_first(split = ";")
  
  return(gene_symbols)
  
}


#' Translates Uniprot protein identifiers to protein names
#'
#' @param proteins UniProt protein Ids
#' @param database dataset
#'
#' @return
#' @export
#'
#'
p2n <- function(proteins, database = "org.Hs.eg.db") {
  
  protein_names <- org_select(keys = proteins, 
                              columns = "GENENAME",
                              keytype = "UNIPROT", 
                              database = database, 
                              paste = ";", 
                              rm.na = F, 
                              fill.na = T, 
                              to.vector = T, 
                              as.T2G = F) %>% 
    pOmics::strsplit_keep_first(split = ";")
  
  return(protein_names)
  
}
