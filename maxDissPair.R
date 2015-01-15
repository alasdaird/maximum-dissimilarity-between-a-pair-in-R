#--------------------------------------------------------------------------
# Pair with maximum dissimilarity between
# 
# 15/1/15
#--------------------------------------------------------------------------

# this function was created to find the pair within a cluster that were used
# to merge it's two daughter clusters as per the "complete linkage" method 
# used in agglomeratice heirarchical clustering. 

# the maximum dissimilarity between any two in the cluster identifies the observations used to join the 
# two daughter cluster as per the complete linkage method. 



# provide the cluster labels, the label of the cluster of interest, the dissimilarity object used in hclust
maxDissPair <- function(hclustLabels, cluster, hclustDiss){
  #testing
  hclustLabels <- bsSamp.hclustLabels
  cluster <- 1
  hclustDiss <- bsSamp.diss
  
  # convert dissimilarity object to a matrix
  dissMat <- as.matrix(hclustDiss)
  
  # get index' of observations in the cluster of interest
  obsInd <- which(hclustLabels==cluster)
  
  # get the subset of the dissimilarity matrix for the cluster observations
  dissMatSub <- dissMat[obsInd, obsInd]
  
  # find the coordinates of the maximum. 
  # Note: may be multiple matches, return the first match
  coordMax <- which(dissMatSub == max(dissMatSub), arr.ind = TRUE, useNames = TRUE)[1,]
  
  #get index's of clusters from result from result. using first matching pair (in case )
  pair <- obsInd[as.vector(coordMax)]
  
  return(pair)
}
