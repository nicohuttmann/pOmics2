# do_cP_enricher
# do_cP_enrichKEGG
# do_RPA_enrichPathway



do_cP_enrichGO <- function() {
  
  
  
  
  
  
  
  
}


#' Title
#'
#' @param proteins 
#' @param background 
#' @param OrgDb 
#' @param ontology 
#' @param keyType 
#' @param pvalueCutoff 
#' @param pAdjustMethod 
#' @param qvalueCutoff 
#' @param minGSSize 
#' @param maxGSSize 
#'
#' @return
#' @export
#'
#' @examples
.cP_enrichGO <- function(proteins,
                         background, 
                         OrgDb = "org.Hs.eg.db", 
                         ontology = "BP",
                         keyType = "UNIPROT", 
                         pvalueCutoff = 0.05,
                         pAdjustMethod = "none",
                         qvalueCutoff = 0.2,
                         minGSSize = 10,
                         maxGSSize = 500) {
  
  # Check arguments
  if (!hasArg(OrgDb)) stop("Please specify an <OrgDb> organism database.", 
                           call. = FALSE)
  
  # has arg proteins
  
  # ahs arg background
  
  # check protein in backgrond
  
  # Performing enrichment
  go_results <- clusterProfiler::enrichGO(gene = proteins,
                                          universe = background,
                                          OrgDb = OrgDb,
                                          ont = ontology,
                                          keyType = keyType,
                                          pvalueCutoff = pvalueCutoff,
                                          pAdjustMethod = pAdjustMethod,
                                          qvalueCutoff = qvalueCutoff,
                                          minGSSize = minGSSize,
                                          maxGSSize = maxGSSize,
                                          readable = T)

  # Return
  return(go_results)
  
}





#' Performs over-representation analysis using KEGG annotations
#'
#' @param proteins numeric/logical vector of proteins indicating group
#' @param pvalueCutoff p-value cutoff for annotations
#' @param pAdjustMethod one of "none", "BH" (Benjamini-Hochberg correction), "hochberg", "bonferroni", "holm", "hommel", "BY", "fdr"
#' @param qvalueCutoff q-value cutoff for annotations
#' @param minGSSize minimum number of annotated proteins to be included
#' @param maxGSSize maximum number of annotated proteins to be included
#'
#' @return
#' @export
#'
#'
.cP_enrichKEGG <- function(proteins,
                           background,
                           organism = "hsa",
                           keyType = "uniprot",
                           pvalueCutoff = 0.05,
                           pAdjustMethod = "none",
                           qvalueCutoff = 0.2,
                           minGSSize = 10,
                           maxGSSize = 500) {
  
  # Check input
  
  
  # Performing enrichment
  kegg_results <- clusterProfiler::enrichKEGG(gene = proteins,
                                              universe = background,
                                              organism = organism,
                                              keyType = keyType,
                                              pvalueCutoff = pvalueCutoff,
                                              pAdjustMethod = pAdjustMethod,
                                              qvalueCutoff = qvalueCutoff,
                                              minGSSize = minGSSize,
                                              maxGSSize = maxGSSize)
  
  # Return
  return(kegg_results)
  
}


#' Performs over-representation analysis using KEGG annotations
#'
#' @param proteins numeric/logical vector of proteins indicating group
#' @param pvalueCutoff p-value cutoff for annotations
#' @param pAdjustMethod one of "none", "BH" (Benjamini-Hochberg correction), "hochberg", "bonferroni", "holm", "hommel", "BY", "fdr"
#' @param qvalueCutoff q-value cutoff for annotations
#' @param minGSSize minimum number of annotated proteins to be included
#' @param maxGSSize maximum number of annotated proteins to be included
#'
#' @return
#' @export
#'
#'
.cP_gseKEGG <- function(geneList,
                        organism = "hsa",
                        keyType = "uniprot",
                        exponent = 1, 
                        minGSSize = 10,
                        maxGSSize = 120, 
                        eps = 1e-10, 
                        pvalueCutoff = 0.05,
                        pAdjustMethod = "none",
                        qvalueCutoff = 0.2,
                        verbose = TRUE, 
                        use_internal_data = FALSE, 
                        seed = FALSE, 
                        by = "fgsea", 
                        ...) {
  
  # Check input
  
  
  
  # Perform enrichment
  kegg_results <- clusterProfiler::gseKEGG(geneList      = geneList,
                                           organism      = organism,
                                           keyType       = keyType,
                                           exponent      = exponent, 
                                           minGSSize     = minGSSize,
                                           maxGSSize     = maxGSSize,
                                           eps           = eps,
                                           pvalueCutoff  = pvalueCutoff,
                                           pAdjustMethod = pAdjustMethod,
                                           verbose       = verbose, 
                                           use_internal_data = use_internal_data, 
                                           seed = seed, 
                                           by = by, 
                                           ...)
  
  
  
  # Return
  return(kegg_results)
  
}
