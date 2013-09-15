
# Try to get at mutual information between target and logical variablesz
signal.metrics <- function(data, columns) { 
  ret.list <- list()
  for (x in columns) {
    if (any(data[,x]) > 0) {
      ret.list[[x]] <- list()
      ret.list[[x]][["chi.sq"]] <- chisq.test(data[,x],data$Survived)
      ret.list[[x]][["cor"]] <- cor(data[,x],data$Survived)
      ret.list[[x]][["conf.mat"]] <- matrix(c(nrow(data[which(data[,x]  == T & data$Survived == 1),]),
                                          nrow(data[which(data[,x]  == F & data$Survived == 1),]),
                                          nrow(data[which(data[,x]  == T & data$Survived == 0),]),
                                          nrow(data[which(data[,x]  == F & data$Survived == 0),])
                                          
                                          ),nrow=2,ncol=2)
    }
  }
  return(ret.list)
}

plot.signal <- function(signal) {

  r.scores <- sapply(signal,function(x){ return(x[["chi.sq"]]$statistic)})
  p.values <- sapply(signal,function(x){ return(x[["chi.sq"]]$p.value)})
  chi.sq.results <- scale(data.frame(p.values,r.scores))
  cor.values <- sapply(signal,function(x){ return(x[["cor"]])}) 

  plot(chi.sq.results[,"r.scores"], type="b", xaxt="n",ylab="")
  axis(1, at=1:nrow(chi.sq.results), labels=names(signal))
  lines(chi.sq.results[,"p.values"], type='l', col="red")
  text(chi.sq.results[,"p.values"], as.character(format(p.values,digits=2)), col="purple")
  lines(cor.values, type='l', col="orange")
  text(cor.values, as.character(format(cor.values,digits=2)), col="blue")

  legend(x=1,y=1,c("R Squared Values","P Values","Correlation"),lty=c(1,1,1),col=c("black","red","orange"))

}

within.groups.ss <- function (data) {
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:15) { 
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }

  plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
}

titanic.kmeans <- function (data,variables,n.clusters) {

  data <- data[,variables]
  
  for (x in 1:n.clusters) {
    data[,paste("c",as.character(x),sep="")] <- F
  }

  # Base kmeans clustering on selected attributes only
  data[,variables] <- scale(data[,variables])

  fit <- kmeans(data[,variables],n.clusters)

  clusplot(data[,variables], fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

  plotcluster(data[,variables], fit$cluster)

  # clustering for the lulz
  # storing these categories in the training data in case it comes in handy...

  for (x in 1:nrow(data)) {
    errors <- apply(X=fit$centers, MARGIN=1, FUN=function(r){
      abs(sum(as.numeric(data[x,variables]) - r))
    }) 
    # If we have two cluster with the same min error... just pick one! :P
    # There's still a problem here...
    cluster.idx <- as.integer(which(errors == min(errors)))[1]
    print(cluster.idx)
    data[x,paste("c",as.character(cluster.idx),sep="")] <- T
  }

  return(data)
}

titanic.hierarchical.clustering <- function(data, variables) {

  data <- data[,variables]

  data[,variables] <- scale(data[,variables])
 
  d <- diana(x=data, metric = "euclidean", stand = T) 
  
  plot(d)

  d <- dist(data, method = "euclidean")

  fit <- hclust(d, method="ward") 

  plot(fit) # display dendogram

  groups <- cutree(fit, k=5) # cut tree into 5 clusters

  # draw dendogram with red borders around the 5 clusters 
  rect.hclust(fit, k=5, border="red")
  
}
