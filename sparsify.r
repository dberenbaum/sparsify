sparsify <- function(trainData, testData=trainData) {
  
  #Use for data frame where all columns are comprised of factors.
  #This function creates a data frame with a column for each factor level in each column in the training data.
  #The rows of the data frame correspond to the those in the test data.
  #Each row has a value of one in each column that matches the corresponding test data and a value of zero in all other columns.
  
  if (ncol(trainData) != ncol(testData)) {
    stop('Number of columns in data frames must match.')
  }
  
  library(Matrix)
  
  rowIndices <- c()
  colIndices <- c()
  numCols <- 0
  colNames <- c()
  
  for (i in 1:ncol(trainData)) {
    #Create columns for each unique feature in each column of the original dataset.
    features <- unique(trainData[,i])
    colNames <- append(colNames, paste(colnames(trainData[i]),features))
    for (j in 1:length(features)) {
      #Mark rows for which the new column should be nonzero (equal to 1).
      addRows <- which(testData[,i]==features[j])
      rowIndices <- append(rowIndices,addRows)
      colIndices <- append(colIndices,rep(numCols+j,length(addRows)))
    }
    numCols <- numCols + length(features)
  }
  
  #Create sparse matrix based on the columns and nonzero indices above.
  sparseData <- sparseMatrix(rowIndices,colIndices)
  colnames(sparseData) <- colNames
  sparseData
}
