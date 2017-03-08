#' @name confusion2network
#' @title Build network
#' @import visNetwork plyr
#' @export
confusion2network = function(cf, true_labels = NULL, pred_labels = NULL, true_label_prefix = NULL, pred_label_prefix = NULL, width = "100%", height = "100%", edge_threshold = 0.1, exportButton = FALSE){
  network_data = confusion2nodesedges(cf, true_labels = true_labels, pred_labels = pred_labels, true_label_prefix = true_label_prefix, pred_label_prefix = pred_label_prefix)
  # filter edges
  network_data$edges = network_data$edges[network_data$edges$label >= edge_threshold,]

  network = nodesedges2network_(network_data$nodes, network_data$edges, width = width, height = height) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, autoResize = TRUE) %>%
    visEdges(arrows = "to", scaling = list(max = 2, label = list(enabled = FALSE)), font = list(size = 12)) %>%
    visLayout(improvedLayout = TRUE) %>%
    visPhysics(barnesHut = list(gravitationalConstant = -3000, springLength = 500,
                                avoidOverlap = 0.5))
    if(exportButton){
      network = network %>% visExport()
    }
  return(network)
}

#' @rdname confusion2network
#' @import visNetwork plyr
#' @export
prediction2network = function(pf, true_col, pred_col, true_labels = NULL, pred_labels = NULL, width = "100%", height = "100%", edge_threshold = 0.1, exportButton = FALSE){
  network_data = prediction2nodesedges(pf, true_col, pred_col, true_labels = true_labels, pred_labels = pred_labels)
  # filter edges
  network_data$edges = network_data$edges[network_data$edges$label >= edge_threshold,]

  network = nodesedges2network_(network_data$nodes, network_data$edges, width = width, height = height) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, autoResize = TRUE) %>%
    visEdges(arrows = "to", scaling = list(max = 2, label = list(enabled = FALSE)), font = list(size = 12)) %>%
    visLayout(improvedLayout = TRUE) %>%
    visPhysics(barnesHut = list(gravitationalConstant = -3000, springLength = 500,
                                avoidOverlap = 0.5))
  if(exportButton){
    network = network %>% visExport()
  }
  return(network)
}

#' @import visNetwork
nodesedges2network_ = function(nodes, edges, height = "100%", width = "100%"){
  edges$label = sprintf("%.2f", edges$label)
  visNetwork::visNetwork(nodes, edges, width = width, height = height)
}
