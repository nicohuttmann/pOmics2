


#' Title
#'
#' @inheritParams fgsea::fgseaMultilevel
#' @param silent (default = T) Should warning messages be supressed? 
#' @param seed Seed to use for reproducible results
#'
#' @return
#' @export
#'
#' @examples
fgsea_fgseaMultilevel <- function(pathways, 
                                  stats, 
                                  sampleSize = 101, 
                                  minSize = 1, 
                                  maxSize = length(stats) - 1, 
                                  eps = 1e-50, 
                                  scoreType = c("std", "pos", "neg"), 
                                  nproc = 0, 
                                  gseaParam = 1, 
                                  BPPARAM = NULL, 
                                  nPermSimple = 1000, 
                                  absEps = NULL, 
                                  silent = T, 
                                  seed = 42) {
  
  # Set seed for reproducible results
  set.seed(seed)
  
  if (silent) {
    fgseaRes <- suppressWarnings(fgsea::fgseaMultilevel(pathways = pathways,
                                                        stats = stats,
                                                        sampleSize = sampleSize, 
                                                        minSize = minSize, 
                                                        maxSize = maxSize,
                                                        eps = eps,
                                                        scoreType = scoreType,
                                                        nproc = nproc, 
                                                        gseaParam = gseaParam, 
                                                        BPPARAM = BPPARAM, 
                                                        nPermSimple = nPermSimple, 
                                                        absEps = absEps)) 
  } else {
    fgseaRes <- fgsea::fgseaMultilevel(pathways = pathways,
                                       stats = stats,
                                       sampleSize = sampleSize, 
                                       minSize = minSize, 
                                       maxSize = maxSize,
                                       eps = eps,
                                       scoreType = scoreType,
                                       nproc = nproc, 
                                       gseaParam = gseaParam, 
                                       BPPARAM = BPPARAM, 
                                       nPermSimple = nPermSimple, 
                                       absEps = absEps) 
  }
  
  return(fgseaRes)
  
}


#' Title
#'
#' @param pathways list of gene sets used for fgsea 
#' @param stats Gene-level stats, as in 'fgsea' function
#' @param id id of pathway to plot
#' @param title title of plot (default = <id> argument)
#' @param gseaParam GSEA parameter.
#' @param ticksSize width of vertical line corresponding to a gene (default: 0.2)


#'
#' @return
#' @export
#'
#' @examples
fgsea_plotEnrichment <- function(pathways = NULL, 
                                 stats = NULL, 
                                 id = "", 
                                 title = id, 
                                 gseaParam = 1, 
                                 ticksSize = 0.2) {
  
  p <- fgsea::plotEnrichment(pathway = pathways[[id]], 
                             stats = stats, 
                             gseaParam = gseaParam, 
                             ticksSize = ticksSize) + 
    ggplot2::labs(title=title)
  
  return(p)
  
}


#' Title
#'
#' @param n_up number of positively enriched pathways to show (default = 10)
#' @param n_down number of negatively enriched pathways to show (default = 10)
#' @param pathways list of gene sets used for fgsea 
#' @param stats Gene-level stats, as in 'fgsea' function
#' @param fgseaRes Table with fgsea results
#' @param gseaParam GSEA-like parameter. Adjusts displayed statistic values, 
#'  values closer to 0 flatten plots. Default = 1, value of 0.5 is a good choice 
#'  too.
#' @param colwidths Vector of five elements corresponding to column width for 
#'  grid.arrange. Can be both units and simple numeric vector, in latter case 
#'  it defines proportions, not actual sizes. If column width is set to zero, 
#'  the column is not drawn.
#' @param pathwayLabelStyle list with style parameter adjustments for pathway 
#'  labels. For example, 'list(size=10, color="red")' set the font size to 10 
#'  and color to red. See 'cowplot::draw_text' for possible options.
#' @param headerLabelStyle similar to 'pathwayLabelStyle' but for the table 
#'  header.
#' @param valueStyle similar to 'pathwayLabelStyle' but for NES and p-value 
#'  columns.
#' @param axisLabelStyle list with style parameter adjustments for stats axis 
#'  labels. See 'ggplot2::element_text' for possible options.
#'
#' @return
#' @export
#'
#' @examples
fgsea_plotGseaTable <- function(n_up = 10, 
                                n_down = 10, 
                                pathways, 
                                stats, 
                                fgseaRes, 
                                gseaParam = 1, 
                                colwidths = c(5, 3, 0.8, 1.2, 1.2), 
                                pathwayLabelStyle = NULL, 
                                headerLabelStyle = NULL, 
                                valueStyle = NULL, 
                                axisLabelStyle = NULL) {
  
  topPathwaysUp <- dplyr::as_tibble(fgseaRes) %>% 
    dplyr::filter(ES > 0) %>% 
    dplyr::arrange(pval) %>% 
    dplyr::pull("pathway") %>% 
    head(n_up)
  
  topPathwaysDown <- dplyr::as_tibble(fgseaRes) %>% 
    dplyr::filter(ES < 0) %>% 
    dplyr::arrange(pval) %>% 
    dplyr::pull("pathway") %>% 
    head(n_down)
  
  topPathways <- c(topPathwaysUp, rev(topPathwaysDown))
  
  p <- fgsea::plotGseaTable(pathways = pathways[topPathways], 
                            stats = stats, 
                            fgseaRes = fgseaRes, 
                            gseaParam = gseaParam, 
                            colwidths = colwidths, 
                            pathwayLabelStyle = pathwayLabelStyle, 
                            headerLabelStyle = headerLabelStyle, 
                            valueStyle = valueStyle, 
                            axisLabelStyle = axisLabelStyle) 
  
  return(p)
  
}


#' Title
#'
#' @param fgseaRes Table with fgsea results
#' @param pathways List of pathways, should contain all the pathways present in 
#'  'fgseaRes'.
#' @param stats Gene-level statistic values used for ranking, the same as in 
#'  'fgsea()'
#' @param padj.threshold threshold for pathways to be considered
#' @param pval.threshold Two pathways are considered dependent when p-value of 
#'  enrichment of one pathways on background of another is greater then 
#'    'pval.threshold'.
#' @param nperm Number of permutations to test for independence, should be 
#'  several times greater than '1/pval.threhold'. Default value: 
#'    '10/pval.threshold'.
#' @param gseaParam GSEA parameter, same as for 'fgsea()'
#'
#' @return
#' @export
#'
#' @examples
fgsea_collapsePathways <- function(fgseaRes, 
                                   pathways, 
                                   stats, 
                                   padj.threshold = 0.01, 
                                   pval.threshold = 0.05, 
                                   nperm = 10/pval.threshold, 
                                   gseaParam = 1) {
  
  fgseaRes_filtered <- fgseaRes %>% 
    dplyr::as_tibble() %>% 
    dplyr::arrange(pval) %>% 
    dplyr::filter(padj < padj.threshold) %>% 
    data.table::as.data.table()
  
  collapsedPathways <- fgsea::collapsePathways(fgseaRes = fgseaRes_filtered, 
                                               pathways = pathways, 
                                               stats = stats, 
                                               pval.threshold = pval.threshold, 
                                               nperm = nperm, 
                                               gseaParam = gseaParam)
  
  fgseaRes_collapsed <- fgseaRes %>% 
    dplyr::as_tibble() %>% 
    dplyr::filter(pathway %in% collapsedPathways[["mainPathways"]]) %>% 
    data.table::as.data.table()
  
  return(fgseaRes_collapsed)
  
}


