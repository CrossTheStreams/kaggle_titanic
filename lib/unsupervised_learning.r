
# Try to get at mutual information between target and logical variablesz
mut.info <- function(data, columns) { 

  ret.list <- list()

  for (x in columns) { 

    ret.list[[x]] <- list()
    ret.list[[x]][["chi.sq"]] <- chisq.test(data[,x],data$Survived)
    ret.list[[x]][["cor"]] <- cor(data[,x],data$Survived)
    ret.list[[x]][["conf.mat"]] <- matrix(c(nrow(data[which(data[,x]  == T & data$Survived == 1),]),
                                        nrow(data[which(data[,x]  == F & data$Survived == 1),]),
                                        nrow(data[which(data[,x]  == T & data$Survived == 0),]),
                                        nrow(data[which(data[,x]  == F & data$Survived == 0),])
                                        
                                        ),nrow=2,ncol=2)
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

}

within.groups.ss <- function (data) {
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:15) { 
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }

  plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
}

titanic.kmeans <- function (data,variables,n.clusters) {

  for (x in 1:n.clusters) {
    data[,paste("c",as.character(x),sep="")] <- F
  }

  # Base kmeans clustering on selected attributes only
  data <- scale(data[,variables])

  fit <- kmeans(data,n.clusters)

  clusplot(data, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

  plotcluster(data, fit$cluster)

  # clustering for the lulz
  # storing these categories in the training data in case it comes in handy...

  for (x in 1:nrow(data)) {
    mean.errors <- apply(X=fit$centers, MARGIN=1, FUN=function(r){
      abs(sum(data[x,] - r))
    }) 
    cluster.idx <- as.integer(which(mean.errors == min(mean.errors)))
    data[x,paste("c",cluster.idx,sep="")] <- T
  }

  return(data)
}
