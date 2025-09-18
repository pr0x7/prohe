#' Dijkstra's Algorithm
#'
#' Computes the shortest paths from an initial node to all other nodes in a weighted graph.
#'
#' @param graph A data.frame with three columns:
#'   - `v1`: starting node of an edge
#'   - `v2`: ending node of an edge
#'   - `w`: weight of the edge (numeric, non-negative)
#' @param init_node A numeric scalar representing the starting node.
#' @return A numeric vector where each entry gives the shortest distance from `init_node`
#' to the corresponding node in the graph.
#' @export
#' @examples
#' wiki_graph <- data.frame(
#'   v1 = c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'   v2 = c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'   w  = c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)
#' )
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 3)
dijkstra <- function(graph, init_node) {
  # --- input checks ---
  stopifnot(is.data.frame(graph))
  stopifnot(all(c("v1", "v2", "w") %in% names(graph)))
  stopifnot(is.numeric(graph$w), all(graph$w >= 0)) # no negative weights
  stopifnot(is.numeric(init_node), length(init_node) == 1)

  nodes <- sort(unique(c(graph$v1, graph$v2)))
  if (!(init_node %in% nodes)) {
    stop("Initial node not found in graph.")
  }

  dist <- rep(Inf, length(nodes))
  names(dist) <- nodes
  dist[as.character(init_node)] <- 0

  visited <- setNames(rep(FALSE, length(nodes)), nodes)

  while (any(!visited)) {
    u <- names(which.min(ifelse(visited, Inf, dist)))
    visited[u] <- TRUE

    neighbors <- graph[graph$v1 == as.integer(u), ]

    for (i in seq_len(nrow(neighbors))) {
      v <- as.character(neighbors$v2[i])
      alt <- dist[u] + neighbors$w[i]
      if (alt < dist[v]) {
        dist[v] <- alt
      }
    }
  }

  return(as.numeric(dist))
}





wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph, 1)
