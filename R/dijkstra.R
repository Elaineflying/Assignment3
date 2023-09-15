#' dijkstra algorithm
#' @references wiki page link https://en.wikipedia.org/wiki/Dijkstra%27s algorithm and https://en.wikipedia.org/wiki/Graph (mathematics)
#' @description dijkstra algorithm is to find the shortest distince in a graph with a given start point
#' @param graph A dataframe with three variables (v1, v2 and w) that contains the edges of the graph (fromv1 to v2) with the weight of the edge (w).
#' @param init_node A numeric number.
#' @returns A numeric vector.
#' @examples
#' wiki_graph <-
#' data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#' v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#' w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 2)

dijkstra <-
function(graph,init_node) {
        if ( !is.data.frame(graph) | !is.numeric(init_node) ) {
            stop("Your arugments should be data.frame graph and a numeric scalar, please check!!")
        }
        if ( !init_node %in% graph$v1  ) {
            stop("your init_node doesn't exist in the graph")
        }
        ###convert the dataframe graph into a matrix graph##
        allnodes<- union(graph$v1,graph$v2)
        
        mat_graph <- matrix(0, nrow=length(allnodes), ncol=length(allnodes),
                            dimnames=list(allnodes, allnodes))
        mat_graph[as.matrix(graph[,c(1,2)])] <- graph[,3]
        mat_graph[as.matrix(graph[,c(2,1)])] <- graph[,3]
        ### get the total number of node###
        n <- dim(mat_graph)[1]
        
        ### initialize distance vector with infinity except the init_node
        ### create a visited vector to keep track if the node has been searched
        for ( v in 1:n ) {
            dist[v] <- Inf
            visited[v] <- FALSE
        }
        dist[init_node] <- 0
        
        for ( v in 1:n ) {
            min_dist <- Inf
            min_node <- -1
            ### search the min distance and record the min node
            for ( i in 1:n ) {
                if ( !visited[i] && min_dist > dist[i] ) {
                    min_dist <- dist[i]
                    min_node <- i
                }
            }
            ### stop when no unvisited node
            if (min_node == -1) {
                break
            }
            ### mark the new min node as visited
            visited[min_node] <- TRUE
            ### update the shortest distance
            for ( i in 1:n ) {
                if ( !visited[i] && mat_graph[min_node,i] > 0 ) {
                    dist[i]=min(dist[i],dist[min_node] + mat_graph[min_node,i])
                }
            }
        }
        return(dist)
    }
