#' Binary Classifier Interpretation functions: Partial Dependence Plots
#'
#' \code{parDepPlot} creates partial dependence plots for binary (cross-validated) classification models. Currently only binary classification model estimated with the packages \code{randomForest} and \code{ada} are supported.
#'
#' @param x.name the name of the predictor as a character string for which a partial dependence plot has to be created.
#' @param object can be a model or a list of cross- validated models. Currently only binary classification models built using the packages \code{randomForest} and \code{ada} are supported.
#' @param data a data frame containing the predictors for the model or a list of data frames for cross-validation with length equal to the number of models.
#' @param xlab  label for the x-axis.
#' @param ylab	label for the y-axis.
#' @param main main title for the plot.
#' @param ...  other graphical parameters for \code{plot}.
#' @references The code in this function uses part of the code from the \code{partialPlot} function in \code{randomForest}. It is expanded and generalized to support cross-validation and other packages. 
#' @details The response variable in the model is always assumed to take on the values \{0,1\}. Resulting partial dependence plots always refer to class 1.
#' @examples
#' 
#' library(randomForest)
#' #Prepare data
#' data(iris)
#' iris <- iris[1:100,]
#' iris$Species <- as.factor(ifelse(factor(iris$Species)=="setosa",0,1))
#' 
#' #Cross-validated models
#' #Estimate 10 models and create 10 test sets
#' data <- list()
#' rf <- list()
#' for (i in 1:10) {
#'   ind <- sample(nrow(iris),50)
#'   rf[[i]] <- randomForest(Species~., iris[ind,])
#'   data[[i]] <- iris[-ind,]
#' }
#' 
#' 
#' parDepPlot(x.name="Petal.Width", object=rf, data=data)
#' 
#' #Single model
#' #Estimate a single model
#' rf <- randomForest(Species~., iris[ind,])
#' parDepPlot(x.name="Petal.Width", object=rf, data=iris)
#' 
#' @author Authors: Michel Ballings, and Dirk Van den Poel, Maintainer: \email{Michel.Ballings@@GMail.com}
parDepPlot <-
    function (x.name,
              object, 
              data,               
              xlab=x.name, 
              ylab=if (any(class(object) %in% c("randomForest","ada"))) bquote(paste('mean(0.5*logit(P'[1],'))')) else bquote(paste('CV mean(0.5*logit(P'[1],'))')),
              main= if (any(class(object) %in% c("randomForest","ada"))) paste("Partial Dependence on",x.name) else paste("Cross-Validated Partial Dependence on",x.name),
              ...)
{

#solve labeling issue by assigning labels before anyting else  
ylab <- ylab
main <- main

#if object has only one model (=object has a class) then assign object to a list. Same for data.
#If there are multiple models (=object has no class) then do nothing
if (any(class(object) %in% c("randomForest","ada"))) {
    object <- list(object)
    data <- list(data)
}

xy <- vector(mode="list", length=length(object))

#get min and max of predictor across all data sets for numeric predictors
if (!is.factor(data[[1]][,x.name]) ) {
    ranges <- matrix(NA,ncol=2,nrow=length(object))
    for (ii in 1:length(object)) {
      
      ranges[ii,] <- range(data[[ii]][,x.name])
    
    }
x <- seq(min(ranges[,1]), max(ranges[,2]), length = min(length(unique(data[[1]][,x.name])), 51))
}



#start loop through models
for (ii in 1:length(object)) {
              
              predictor <- data[[ii]][,x.name]
              n <- nrow(data[[ii]])
            
              if (is.factor(predictor) ) { #if predictor is a factor
                  x <- levels(predictor)
                  y <- numeric(length(x))
                  for (i in seq(along = x)) {
                      data.after <- data[[ii]]
                      data.after[, x.name] <- factor(rep(x[i], n), levels = x)
          
                          if(any(class(object[[ii]]) %in% "randomForest")) pred <- predict(object[[ii]], data.after, type = "prob")
                          if(any(class(object[[ii]])=="ada")) pred <- predict(object[[ii]], data.after, type="probs")
                      
                          y[i] <- mean(log(ifelse(pred[, 2] > 0,
                                                              pred[, 2], .Machine$double.eps)) -
                                                   rowMeans(log(ifelse(pred > 0, pred, .Machine$double.eps))),
                                                   na.rm=TRUE)
                      
                           
                  }
                
              } else { #if predictor is a numeric
                 
                  #x <- seq(min(predictor), max(predictor), length = min(length(unique(predictor)), 51))
                  y <- numeric(length(x))
                  for (i in seq(along.with = x)) {
                      data.after <- data[[ii]]
                      data.after[, x.name] <- rep(x[i], n)
                      
                      if(any(class(object[[ii]]) %in% "randomForest"))  pred <- predict(object[[ii]], data.after, type = "prob")
                      if(any(class(object[[ii]])=="ada")) pred <- predict(object[[ii]], data.after, type="probs")
                          
                          y[i] <- mean(log(ifelse(pred[, 2] == 0,
                                                              .Machine$double.eps, pred[, 2]))
                                                   - rowMeans(log(ifelse(pred == 0, .Machine$double.eps, pred))),
                                                    na.rm=TRUE)
                      
          
                      
                    }
                  }
              
          #store x and y as a data.frame in one list
          xy[[ii]] <- data.frame(x,y)

#end loop through models
}

#merge all elements of the list by x 
#compute mean of y by x
options(warn=-1)
xy <- data.frame(Reduce(function(x, y) merge(x, y, by='x',all=TRUE),xy, accumulate=FALSE))
x <- xy[,1]

y <- rowMeans(data.frame(xy[,2:ncol(xy)]),na.rm=TRUE)
options(warn=0)
    
plot(as.numeric(x), y, type = "l", xlab=xlab, ylab=ylab, main = main, ...)
}