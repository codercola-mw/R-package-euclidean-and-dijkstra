#' Create Dijkstras Alogrithm
#' 
#' @param graph A numberical value
#' @param init_node A numberical value
#' @return The shortest path
#' @examples
#' dijkstra(wiki_graph = data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)), 1)
#' dijkstra(wiki_graph = data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)), 3)
#' 
#' @export
dijkstra<-function(graph,init_node)
{
   #Check if the class of the arguments is correct
  stopifnot(class(graph)=="data.frame" && class(init_node)=="numeric" && any(graph[1:2]==init_node)) #Changed OR to AND, because it didn't throw error for init_node = 0
  stopifnot(colnames(graph) == c("v1", "v2", "w"))
  
  #vertex_set vector == Q
  #dist vector == dist
  
  #First for each doesn't need to be a loop
  vertex_set<-unique(graph[,1]) #vector of nodes(vertex)
  dist<-rep(Inf,length(vertex_set)) #filling up vector dist with Inf as many times as many nodes(vertex) there are
  dist[init_node]=0 #setting dist of init_node to 0
  
  #while not every vertex_set element is set to NA
  while (sum(is.na(vertex_set))<length(vertex_set)) {
    
    #u is the current node, whichever vertex has the minimum distance. In the starting case it will be the init_node
    
    indices_of_non_NA = which(!is.na(vertex_set)) #get the indices of the vertex_set which hasn't been set to NA
    minimum_value_of_dist = min(dist[indices_of_non_NA]) #get the minimum value of dist on the indices, which hasn't been set to NA
    minimum_index_of_dist = which(dist==minimum_value_of_dist) #get the index of the minimum value in dist
    
    u = vertex_set[intersect(indices_of_non_NA,minimum_index_of_dist)][1] #set u to the vertex which hasn't been set to NA AND has the minimum dist. [1] neccessary in case of multiple results.
    
    #set the current node to NA, so it doesn't do the while loop again for it
    vertex_set[u]=NA
    
    #v is all the neighbours of current node(u)
    #loop for all of u's neighbours that are still not set to NA in vertex_set
    for (v in intersect(subset(graph,graph[,1]==u)[,2],vertex_set)) {
      
      #alt is the alternative distance which you get by going from the initial node to v through u
      alt=dist[u] + subset(graph,graph[,1]==u & graph[,2]==v)[,3] #add the weight of the edge (==distance) (3rd column in data.frame)
      
      #if alt is longer than an already existing route to v, we ignore it
      if (alt<dist[v]) {
        dist[v]=alt
      }


    }

  }
  
  
  return(dist) #return distances to each vertex from init_node in a vector (including itself)
}
