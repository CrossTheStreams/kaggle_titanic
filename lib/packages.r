# Helper method for fetching packages.
pkgTest <- function(x) {
    if (!require(x,character.only = TRUE)) {
              install.packages(x,dep=TRUE)
  if(!require(x,character.only = TRUE)) stop("Package not found")
     
      }
}

pkgTest('outliers')

pkgTest('e1071')

pkgTest('SparseM')

pkgTest('ggplot2')

pkgTest('ROCR')

pkgTest('cluster') 

pkgTest('fpc')

pkgTest('randomForest')
