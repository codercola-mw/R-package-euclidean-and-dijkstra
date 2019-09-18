#' Create Dijkstras Alogrithm
#' 
#' @param graph A numberical value
#' @param init_node A numberical value
#' @return The shortest path
#' @examples
#' dijkstra(graph = data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)), 1)
#' dijkstra(graph = data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)), 3)
#' 
#' @export
dijkstra<-function(graph,init_node){
  
  #stop if the classes are not the correct type
  if (!is.data.frame(graph)) stop("Graph is not a data frame")
  if (length(graph) != 3) stop("Graph does not have three variables")
  if (!is.numeric(init_node)) stop("Initial node is not a scalar")
  if (!any(graph[1:2]==init_node)) stop("Initial node not in graph")
  if (!(colnames(graph) == c("v1", "v2", "w"))) stop("Wrong names of variables")
  
  vec1<-unique(graph[,1]) #make vector for the vertex nodes
  dist<-rep(Inf,length(vec1)) #filling up vector with Infs for every vertex node
  dist[init_node]<-0 #distance of initial node set to 0
  
  #while node is not vertex set the element to 0
  while (sum(is.na(vec1))<length(vec1)) {
    
    #a is current node, which vertex has minimum distance. Start from initial node 
    
    indices_of_non_NA = which(!is.na(vec1)) #for the indices of the vector which are not already NA
    minimum_value_of_dist = min(dist[indices_of_non_NA]) #calculate the min of the non NA indices 
    minimum_index_of_dist = which(dist==minimum_value_of_dist) 
    
    a = vec1[intersect(indices_of_non_NA,minimum_index_of_dist)][1] 
    
    #set current node to NA to stop while loop
    vec1[a]=NA
    
    #v is all the neighbours of current node(a)
    #forloop for all of a neighbours that are not NA in vec1
    for (b in intersect(subset(graph,graph[,1]==a)[,2],vec1)) {
      
      #alt is the alternative distance which you get by going from the initial node to v through u
      alt=dist[a] + subset(graph,graph[,1]==a & graph[,2]==b)[,3] #add the weight of the edge (==distance) (3rd column in data.frame)
      
      #if alt is longer than an already existing route to v, we ignore it
      if (alt<dist[b]) {
        dist[b]=alt
      }
    }
  }
  return(dist) #return distances to each vertex from init_node in a vector (including itself)
}
