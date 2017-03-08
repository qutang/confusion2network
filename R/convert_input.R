#' @name confusion2nodesedges
#' @title Convert to graph nodes
#' @import plyr stringr
#' @export
confusion2nodesedges = function(df_or_matrix, true_labels = NULL, pred_labels = NULL, true_label_prefix = NULL, pred_label_prefix = NULL){
  if(is.data.frame(df_or_matrix)){
    true_labels = rownames(df_or_matrix)
    pred_labels = colnames(df_or_matrix)
    input = as.matrix(df_or_matrix)
  }else if(is.matrix(df_or_matrix)){
    if(is.null(true_labels)){
      true_labels = 1:nrow(df_or_matrix)
    }
    if(is.null(pred_labels)){
      pred_labels = 1:ncol(df_or_matrix)
    }
    input = df_or_matrix
  }else{
    stop("Input data should be dataframe or matrix")
  }

  if(!is.null(true_label_prefix)){
    true_labels = stringr::str_replace(true_labels, true_label_prefix, "")
  }
  if(!is.null(pred_label_prefix)){
    pred_labels = stringr::str_replace(pred_labels, pred_label_prefix, "")
  }

  labels = union(true_labels, pred_labels)
  ids = 1:length(labels)

  nodes = data.frame(id = ids,
                     label = labels,
                     title = labels,
                     stringsAsFactors = FALSE)

  edges = plyr::ldply(1:length(true_labels), function(n){
    true_label = true_labels[n]
    from_id = which(labels == true_label)
    edges = plyr::ldply(1:length(pred_labels), function(m){
      pred_label = pred_labels[m]
      to_id = which(labels == pred_label)
      rate = input[n,m] / sum(input[n,])
      if(rate == 0){
        return(NULL)
      }
      if(from_id == to_id){
        value = 1 - rate
        color.opacity = 1 - rate
      }else{
        value = rate
        color.opacity = rate
      }
      return(data.frame(from = from_id,
                        to = to_id,
                        label = rate,
                        value = value,
                        color.opacity = color.opacity,
                        stringsAsFactors = FALSE))
    })
    return(edges)
  })

  node_values = edges[edges$from == edges$to, c("from", "value")]
  names(node_values) = c("id", "value")
  nodes = merge(nodes, node_values, all.x = TRUE)
  return(list(nodes = nodes, edges = edges))
}

#' @rdname confusion2nodesedges
#' @import plyr stringr
#' @export
prediction2nodesedges = function(prediction_df, true_col, pred_col, true_labels = NULL, pred_labels = NULL, group_cols = NULL){
  if(!is.data.frame(prediction_df)){
    stop("input is not a dataframe")
  }
  if(is.null(true_labels)){
    true_labels = unique(prediction_df[true_col])
  }
  if(is.null(pred_labels)){
    pred_labels = unique(prediction_df[pred_col])
  }

  labels = union(true_labels, pred_labels)
  if(is.numeric(prediction_df[[true_col]])){
    ids = sort(unique(c(prediction_df[[true_col]], prediction_df[[pred_col]])))
  }else if(is.character(prediction_df[[true_col]])){
    ids = 1:length(labels)
  }
  if(length(ids) != length(labels)){
    stop("number of ids is not the same as number of labels")
  }
  group = Reduce(function(x, y){paste(x, y, sep = "_")}, prediction_df[group_cols])
  nodes = data.frame(id = ids,
                     label = labels,
                     title = labels,
                     stringsAsFactors = FALSE)
  if(!is.null(group_cols)){
    nodes$group = group
    nodes = cbind(nodes, prediction_df[group_cols])
  }
  edges = plyr::ldply(1:length(true_labels), function(n){
    true_label = true_labels[n]
    from_id = ids[labels == true_label]
    edges = plyr::ldply(1:length(pred_labels), function(m){
      pred_label = pred_labels[m]
      to_id = ids[labels == pred_label]
      if(is.numeric(prediction_df[[true_col]])){
        rate = sum(prediction_df[true_col] == from_id & prediction_df[pred_col] == to_id) / sum(prediction_df[true_col] == from_id)
      }else if(is.character(prediction_df[[true_col]])){
        rate = sum(prediction_df[true_col] == true_label & prediction_df[pred_col] == to_id) / sum(prediction_df[true_col] == pred_label)
      }
      if(rate == 0){
        return(NULL)
      }
      if(from_id == to_id){
        value = 1 - rate
        color.opacity = 1 - rate
      }else{
        value = rate
        color.opacity = rate
      }
      return(data.frame(from = from_id,
                        to = to_id,
                        label = rate,
                        value = value,
                        color.opacity = color.opacity,
                        stringsAsFactors = FALSE))
    })
    return(edges)
  })

  node_values = edges[edges$from == edges$to, c("from", "value")]
  names(node_values) = c("id", "value")
  nodes = merge(nodes, node_values, all.x = TRUE)
  return(list(nodes = nodes, edges = edges))
}
