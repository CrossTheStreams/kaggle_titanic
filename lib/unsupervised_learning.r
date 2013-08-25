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

plot.kmeans <- function (data,variables) {

  data <- scale(data[,variables])

  wss <- (nrow(train.selected)-1)*sum(apply(train.selected,2,var))
  for (i in 2:15) { 
    wss[i] <- sum(kmeans(train.selected, centers=i)$withinss)
  }

  plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

  fit <- kmeans(train.selected,7)

  clusplot(train.selected, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

  plotcluster(train.selected, fit$cluster)
}


assign.cluster.variables <- function ( ) {

  for (x in 1:7) {
    train.selected[,paste("c",as.character(x),sep="")] <- F
  }

  # clustering for the lulz
  # storing these categories in the training data in case it comes in handy...

  for (x in 1:nrow(train.selected)) {
    mean.errors <- apply(X=fit$centers, MARGIN=1, FUN=function(r){
      abs(sum(train.selected[x,] - r))
    }) 
    cluster.idx <- as.integer(which(mean.errors == min(mean.errors)))
    train.selected[x,paste("c",cluster.idx,sep="")] <- T
  }

}

