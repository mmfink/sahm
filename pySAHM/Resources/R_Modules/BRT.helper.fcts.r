###############################################################################
##
## Copyright (C) 2010-2012, USGS Fort Collins Science Center.
## All rights reserved.
## Contact: talbertc@usgs.gov
##
## This file is part of the Software for Assisted Habitat Modeling package
## for VisTrails.
##
## "Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
##
##  - Redistributions of source code must retain the above copyright notice,
##    this list of conditions and the following disclaimer.
##  - Redistributions in binary form must reproduce the above copyright
##    notice, this list of conditions and the following disclaimer in the
##    documentation and/or other materials provided with the distribution.
##  - Neither the name of the University of Utah nor the names of its
##    contributors may be used to endorse or promote products derived from
##    this software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
## THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
## PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
## CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
## EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
## PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
## OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
## WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
## OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
## ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
##
## Although this program has been used by the U.S. Geological Survey (USGS),
## no warranty, expressed or implied, is made by the USGS or the
## U.S. Government as to the accuracy and functioning of the program and
## related program material nor shall the fact of distribution constitute
## any such warranty, and no responsibility is assumed by the USGS
## in connection therewith.
##
## Any use of trade, firm, or product names is for descriptive purposes only
## and does not imply endorsement by the U.S. Government.
###############################################################################

est.lr <- function(out){
    # this function estimates optimal number of trees at a variety of learning rates #
    # the learning rate that produces closest to 1000 trees is selected #
    # in addition, variables with >1% influence are identified by column number in
    # the original dataframe
    # written by AKS early 2009 modified by Marian Talbert
   attach(out$input)
   on.exit(detach(out$input))
    t0 <- unclass(Sys.time())
    # set tree complexity for full-data run #
    a<-2.69; b<-0.0012174

    if(is.null(out$mods$parms$tc.full)) out$mods$parms$tc.full<-min(round(a+b*nrow(out$dat$ma$train$dat)),20) # this gives 3 for n=250
    if(is.null(out$mods$parms$tc.sub)){
        n <- nrow(out$dat$Subset$dat)  # this gives 3 for n=250
       if(is.na(n)) (n=length(out$dat$Subset$weight))
        out$mods$parms$tc.sub <- min(round(a+b*n),20) # don't let there be more than 20 nodes in a tree
    }

    cat("\n");cat("tree complexity set to",out$mods$parms$tc.sub,"\n")

    n.trees <- c(100,200,400,800,900,1000,1100,1200,1500,1800,2400)
    lrs <- c(.1,.05,.02,.01,.005,.0025,.001,.0005,.0001)
    lr.out <- data.frame(lrs=lrs,max.trees=0,cv.dev=0)
    dat <- out$dat$Subset$dat
    gbm.y <- 1
    gbm.x <- 2:ncol(dat)

    max.trees <- 0
    i=1

    while(max.trees<800 & i<=nrow(lr.out)){
      gbm.fit <- gbm.step.fast(dat=dat,
          gbm.x = gbm.x,
          gbm.y = gbm.y,
          family = out$input$model.family,
          n.trees = n.trees,
          n.folds=out$input$n.folds,
          tree.complexity = out$mods$parms$tc.sub,
          learning.rate = lrs[i],
          bag.fraction = out$input$bag.fraction,
          site.weights=out$dat$Subset$weight,
          autostop=T,verbose=F,silent=T,plot.main=F)
          print(gbm.fit$target.trees)
       lr.out$cv.dev[i]<-gbm.fit$cv.statistics$deviance.mean
       lr.out$max.trees[i] <- max.trees <- gbm.fit$target.trees
       assign(paste("gbm.fit",i,sep="_"),gbm.fit)
       cat("lr =",lrs[i],"  optimal n trees =",max.trees,"\n");flush.console()
       i<-i+1 #ifelse(max.trees<=200,i+2,i+1)
       }
    # pick lr that gives closest to 1000 trees #
    lr.out$i <- 1:nrow(lr.out)
    lr.out <- lr.out[lr.out$max.trees!=0,]
    ab<-coef(lm(max.trees~log(lrs),data=lr.out))
    tt <- out$dat$Subset$ratio*800
    lr.full <- round(as.numeric(exp((tt-ab[1])/ab[2])),4)
    if(is.na(lr.full)) lr.full<-lr.out$lrs # sometimes just one lr is calculated in the while loop and we can't fit a line
    lr <- round(as.numeric(exp((1000-ab[1])/ab[2])),6)
    lr.out$abs <- abs(lr.out$max.trees-1000)
    lr.out$d.lr <- abs(lr.out$lrs-lr)
    lr.out <- lr.out[order(lr.out$abs,lr.out$d.lr),]
    lr0 <- lr.out$lrs[1]
    i <- lr.out$i[1]
    gbm.fit <- get(paste("gbm.fit",i,sep="_"))
    gbm.fit$gbm.call$dataframe <- as.character(substitute(dat))
    good.vars <- summary(gbm.fit,plotit=F)
    good.vars <- good.vars$var[good.vars$rel.inf>ifelse(out$input$simp.method!="none",1,0)]
    good.cols <- c(1:ncol(dat))[names(dat)  %in%  good.vars]
    out$mods$simp.mod <- list(good.cols=good.cols,good.vars=good.vars)
    out$mods$lr.mod <- list(tc=out$mods$parms$tc.sub,lr=lr.full,lr0=lr0,lr.out=lr.out,ab=ab,good.cols=good.cols)
    if(!is.null(out$input$learning.rate)) {out$mods$lr.mod$lr0=out$input$learning.rate
       out$mods$lr.mod$lr=out$input$learning.rate
    }

    return(out$mods)
    }

gbm.step.fast <- function(
  dat,                             # the input dataframe
  gbm.x,                                    # the predictors
  gbm.y,                                    # and response
  offset = NULL,                            # allows an offset to be specified
  fold.vector = NULL,                       # allows a fold vector to be read in for CV with offsets,
  tree.complexity = 1,                      # sets the complexity of individual trees
  learning.rate = 0.01,                     # sets the weight applied to inidivudal trees
  bag.fraction = 0.75,                      # sets the proportion of observations used in selecting variables
  site.weights = rep(1, nrow(dat)),        # allows varying weighting for sites
  var.monotone = rep(0, length(gbm.x)),     # restricts responses to individual predictors to monotone
  n.folds = 10,                             # number of folds
  prev.stratify = TRUE,                     # prevalence stratify the folds - only for p/a data
  family = "bernoulli",                     # family - bernoulli (=binomial), poisson, laplace or gaussian
  n.trees = 50,                             # number of initial trees to fit
  step.size = n.trees,                      # numbers of trees to add at each cycle
  max.trees = 10000,                        # max number of trees to fit before stopping
  tolerance.method = "auto",                # method to use in deciding to stop - "fixed" or "auto"
  tolerance = 0.001,                        # tolerance value to use - if method == fixed is absolute,
                                            # if auto is multiplier * total mean deviance
  keep.data = FALSE,                        # keep raw data in final model
  plot.main = TRUE,                         # plot hold-out deviance curve
  plot.folds = FALSE,                       # plot the individual folds as well
  verbose = TRUE,                           # control amount of screen reporting
  silent = FALSE,                           # to allow running with no output for simplifying model)
  keep.fold.models = FALSE,                 # keep the fold models from cross valiation
  keep.fold.vector = FALSE,                 # allows the vector defining fold membership to be kept
  keep.fold.fit = FALSE,                    # allows the predicted values for observations from CV to be kept
  autostop = FALSE,                       # setting autostop to T causes the tree fitting while loop to terminate
                                            #   if the cv.loss value increases twice in a row (AKS).

  superfast=FALSE,                          # controls whether or not to compute final model when optimal n.trees differs from 1000 (AKS)
  ...)                                      # allows for any additional plotting parameters
{
#
# j. leathwick/j. elith - 19th September 2005
#
# version 2.9
#
# function to assess optimal no of boosting trees using k-fold cross validation
#
# implements the cross-validation procedure described on page 215 of
# Hastie T, Tibshirani R, Friedman JH (2001) The Elements of Statistical Learning:
# Data Mining, Inference, and Prediction Springer-Verlag, New York.
#
# divides the data into 10 subsets, with stratification by prevalence if required for pa data
# then fits a gbm model of increasing complexity along the sequence from n.trees to n.trees + (n.steps * step.size)
# calculating the residual deviance at each step along the way
# after each fold processed, calculates the average holdout residual deviance and its standard error
# then identifies the optimal number of trees as that at which the holdout deviance is minimised
# and fits a model with this number of trees, returning it as a gbm model along with additional information
# from the cv selection process
#
# updated 13/6/05 to accommodate weighting of sites
#
# updated 19/8/05 to increment all folds simultaneously, allowing the stopping rule
# for the maxinum number of trees to be fitted to be imposed by the data,
# rather than being fixed in advance
#
# updated 29/8/05 to return cv test statistics, and deviance as mean
# time for analysis also returned via unclass(Sys.time())
#
# updated 5/9/05 to use external function calc.deviance
# and to return cv test stats via predictions formed from fold models
# with n.trees = target.trees
#
# updated 15/5/06 to calculate variance of fitted and predicted values across folds
# these can be expected to approximate the variance of fitted values
# as would be estimated for example by bootstrapping
# as these will underestimate the true variance
# they are corrected by multiplying by (n-1)2/n
# where n is the number of folds
#
# updated 25/3/07 tp allow varying of bag fraction
#
# requires gbm library from Cran
# requires roc and calibration scripts of J Elith
# requires calc.deviance script of J Elith/J Leathwick
#
# updated 1/2/09 by aks:
# now takes a vector arguement for n.trees:  if n.trees is given as a (should be montonically
#   increasing) vector, then models with those # of trees are fit.  if n.trees is a single number,
#   then the program works as default
# there is also a provision for stopping the fitting of successive models IF the cv.loss.value increases
#   over two successive steps.

  require(gbm)

  if (silent) verbose <- FALSE

# initiate timing call

  z1 <- unclass(Sys.time())

# setup input data and assign to position one

  dataframe.name <- deparse(substitute(dat))   # get the dataframe name

  dat <- eval(dat)
  x.data <- eval(dat[, gbm.x])                 #form the temporary datasets
  names(x.data) <- names(dat)[gbm.x]
  y.data <- eval(dat[, gbm.y])
  sp.name <- names(dat)[gbm.y]
  if (family == "bernoulli") prevalence <- mean(y.data)

  assign("x.data", x.data, env = globalenv())               #and assign them for later use
  assign("y.data", y.data, env = globalenv())
  offset.name <- deparse(substitute(offset))   # get the dataframe name
  offset = eval(offset)

  n.cases <- nrow(dat)
  n.preds <- length(gbm.x)

  if (!silent) {
    cat("\n","\n");cat("GBM STEP - version 2.9","\n","\n") #AKS
    cat("Performing cross-validation optimization of a boosted regression tree model \n")#AKS
    cat("for",sp.name,"with dataframe",dataframe.name,"and using a family of",family,"\n\n")
    cat("Using",n.cases,"observations and",n.preds,"predictors \n\n")
    flush.console() #AKS
  }

# set up the selector variable either with or without prevalence stratification

  if (is.null(fold.vector)) {

    if (prev.stratify & family == "bernoulli") {
      presence.mask <- dat[,gbm.y] == 1
      absence.mask <- dat[,gbm.y] == 0
      n.pres <- sum(presence.mask)
      n.abs <- sum(absence.mask)
# create a vector of randomised numbers and feed into presences
      selector <- rep(0,n.cases)
      temp <- rep(seq(1, n.folds, by = 1), length = n.pres)
      temp <- temp[order(runif(n.pres, 1, 100))]
      selector[presence.mask] <- temp
# and then do the same for absences
      temp <- rep(seq(1, n.folds, by = 1), length = n.abs)
      temp <- temp[order(runif(n.abs, 1, 100))]
      selector[absence.mask] <- temp
      }

    else {  #otherwise make them random with respect to presence/absence
      selector <- rep(seq(1, n.folds, by = 1), length = n.cases)
      selector <- selector[order(runif(n.cases, 1, 100))]
      }
    }
  else {
    if (length(fold.vector) != n.cases) stop("supplied fold vector is of wrong length")
    cat("loading user-supplied fold vector \n\n")
    selector <- eval(fold.vector)
    }

# set up the storage space for results

  pred.values <- rep(0, n.cases)
  cv.loss.matrix <- matrix(0, nrow = n.folds, ncol = 1)
  training.loss.matrix <- matrix(0, nrow = n.folds, ncol = 1)
  trees.fitted <- n.trees[1]

  model.list <- list(paste("model",c(1:n.folds),sep=""))     # dummy list for the tree models

# set up the initial call to gbm

  if (is.null(offset)) {  #AKS (n.trees arg)
    gbm.call <- paste("gbm(y.subset~.,data=x.subset,n.trees=n.trees[1],interaction.depth=tree.complexity,shrinkage=learning.rate,bag.fraction=bag.fraction,weights=weight.subset,distribution=as.character(family),var.monotone=var.monotone,verbose=FALSE)",sep="")
    }
  else {       #AKS (n.trees arg)
    gbm.call <- paste("gbm(y.subset~.+offset(offset.subset),data=x.subset,n.trees=n.trees[1],interaction.depth=tree.complexity,shrinkage=learning.rate,bag.fraction=bag.fraction,weights=weight.subset,distribution=as.character(family),var.monotone=var.monotone,verbose=FALSE)",sep="")
    }


  n.fitted <- n.trees[1] # AKS

# calculate the total deviance

  y_i <- y.data
  u_i <- sum(y.data * site.weights) / sum(site.weights)
  u_i <- rep(u_i,length(y_i))
  total.deviance <- calc.deviance(y_i, u_i, weights = site.weights, family = family, calc.mean = FALSE)
  mean.total.deviance <- total.deviance/n.cases
  tolerance.test <- tolerance

  if (tolerance.method == "auto") {
     tolerance.test <- mean.total.deviance * tolerance
  }

# now step through the folds setting up the initial call

  if (!silent){

    cat("creating",n.folds,"initial models of",n.trees,"trees","\n")

    if (prev.stratify & family == "bernoulli") {cat("\n");cat("folds are stratified by prevalence","\n","\n")#AKS
      } else {cat("\n","folds are unstratified","\n","\n")}

    cat ("total mean deviance = ",round(mean.total.deviance,4),"\n","\n")

    cat("tolerance is fixed at ",round(tolerance.test,4),"\n","\n")
    flush.console()#AKS

    if (tolerance.method != "fixed" & tolerance.method != "auto") {
      cat("invalid argument for tolerance method - should be auto or fixed","\n")
      return()}
  }

  if (verbose) cat("fitting initial model...","\n");flush.console()#AKS

  for (i in 1:n.folds) {

    model.mask <- selector != i  #used to fit model on majority of data
    pred.mask <- selector == i   #used to identify the with-held subset

    y.subset <- y.data[model.mask]
    x.subset <- x.data[model.mask,]
    weight.subset <- site.weights[model.mask]

    if (!is.null(offset)) {
      offset.subset <- offset[model.mask]
    }
    else {
      offset.subset <- NULL
    }

    model.list[[i]] <- eval(parse(text = gbm.call))

    fitted.values <- model.list[[i]]$fit  #predict.gbm(model.list[[i]], x.subset, type = "response", n.trees = n.trees)
    if (!is.null(offset)) fitted.values <- fitted.values + offset[model.mask]
    if (family == "bernoulli") fitted.values <- exp(fitted.values)/(1 + exp(fitted.values))
    if (family == "poisson") fitted.values <- exp(fitted.values)

    pred.values[pred.mask] <- predict.gbm(model.list[[i]], x.data[pred.mask, ], n.trees = n.trees[1])
    if (!is.null(offset)) pred.values[pred.mask] <- pred.values[pred.mask] + offset[pred.mask]
    if (family == "bernoulli") pred.values[pred.mask] <- exp(pred.values[pred.mask])/(1 + exp(pred.values[pred.mask]))
    if (family == "poisson") pred.values[pred.mask] <- exp(pred.values[pred.mask])

# calc training deviance

    y_i <- y.subset
    u_i <- fitted.values
    weight.fitted <- site.weights[model.mask]
    training.loss.matrix[i,1] <- calc.deviance(y_i, u_i, weight.fitted, family = family)

# calc holdout deviance

    y_i <- y.data[pred.mask]
    u_i <- pred.values[pred.mask]
    weight.preds <- site.weights[pred.mask]
    cv.loss.matrix[i,1] <- calc.deviance(y_i, u_i, weight.preds, family = family)

  } # end of first loop

# now process until the change in mean deviance is =< tolerance or max.trees is exceeded

  delta.deviance <- 1

  cv.loss.values <- apply(cv.loss.matrix,2,mean)
  if (verbose) cat("ntrees resid. dev.","\n")#AKS
  if (verbose) cat(n.fitted,"  ",round(cv.loss.values,4),"\n","\n")

  if (!silent) cat("")
  if (!silent) cat("now adding trees...","\n");flush.console()#AKS

  j <- 1
  k <- 2 #AKS
  stop.loop <- F #AKS
  if(length(n.trees>1)) max.trees <- n.trees[length(n.trees)] #AKS

  while (delta.deviance > tolerance.test & n.fitted < max.trees & stop.loop==F) {  # beginning of inner loop

        # add a new column to the results matrice..

        training.loss.matrix <- cbind(training.loss.matrix,rep(0,n.folds))
        cv.loss.matrix <- cbind(cv.loss.matrix,rep(0,n.folds))

        if(length(n.trees>1)){ #AKS
          n.fitted <- n.trees[k] #AKS
          step.size <- n.trees[k]-n.trees[k-1]
          k <- k+1 #AKS
              } else {  #AKS
          n.fitted <- n.fitted + step.size
          }  #AKS

        trees.fitted <- c(trees.fitted,n.fitted)

        j <- j + 1

        for (i in 1:n.folds) {

          model.mask <- selector != i  #used to fit model on majority of data
          pred.mask <- selector == i   #used to identify the with-held subset

          y.subset <- y.data[model.mask]
          x.subset <- x.data[model.mask,]
          weight.subset <- site.weights[model.mask]
          if (!is.null(offset)) {
            offset.subset <- offset[model.mask]
          }

          model.list[[i]] <- gbm.more(model.list[[i]], weights = weight.subset, step.size)

          fitted.values <- model.list[[i]]$fit # predict.gbm(model.list[[i]],x.subset, type = "response", n.trees = n.fitted)
          if (!is.null(offset)) fitted.values <- fitted.values + offset[model.mask]
          if (family == "bernoulli") fitted.values <- exp(fitted.values)/(1 + exp(fitted.values))
          if (family == "poisson") fitted.values <- exp(fitted.values)

          pred.values[pred.mask] <- predict.gbm(model.list[[i]], x.data[pred.mask, ], n.trees = n.fitted)
          if (!is.null(offset)) pred.values[pred.mask] <- pred.values[pred.mask] + offset[pred.mask]
          if (family == "bernoulli") pred.values[pred.mask] <- exp(pred.values[pred.mask])/(1 + exp(pred.values[pred.mask]))
          if (family == "poisson") pred.values[pred.mask] <- exp(pred.values[pred.mask])

    # calculate training deviance

          y_i <- y.subset
          u_i <- fitted.values
          weight.fitted <- site.weights[model.mask]
          training.loss.matrix[i,j] <- calc.deviance(y_i, u_i, weight.fitted, family = family)

    # calc holdout deviance

          u_i <- pred.values[pred.mask]
          y_i <- y.data[pred.mask]
          weight.preds <- site.weights[pred.mask]
          cv.loss.matrix[i,j] <- calc.deviance(y_i, u_i, weight.preds, family = family)

        }  # end of inner loop

        cv.loss.values <- apply(cv.loss.matrix,2,mean)

        if (j < 5 & autostop==F) {   #AKS
          if (cv.loss.values[j] > cv.loss.values[j-1]) {
            if (!silent) cat("restart model with a smaller learning rate or smaller step size...")
            return()
          }
        }

          if (j >= 20) {   #calculate stopping rule value
            test1 <- mean(cv.loss.values[(j-9):j])
            test2 <- mean(cv.loss.values[(j-19):(j-9)])
            delta.deviance <- test2 - test1
          }

          if (verbose) cat(n.fitted," ",round(cv.loss.values[j],4),"\n");flush.console()#AKS
          if(autostop==T & j>2){ # this if statement added by AKS
              if((cv.loss.values[j]>cv.loss.values[j-1])){# & (cv.loss.values[j-1]>cv.loss.values[j-2])){ # if deviance increases twice in a row
                  stop.loop=T#n.fitted <- max.trees # by setting n.fitted to max.trees, the while loop will terminate.
                  }
              }



  } # end of while loop

# now begin process of calculating optimal number of trees

  training.loss.values <- apply(training.loss.matrix,2,mean)

  cv.loss.ses <- rep(0,length(cv.loss.values))
  cv.loss.ses <- sqrt(apply(cv.loss.matrix,2,var)) / sqrt(n.folds)

# find the target holdout deviance

  y.bar <- min(cv.loss.values)

# plot out the resulting curve of holdout deviance

  if (plot.main) {

    y.min <- min(cv.loss.values - cv.loss.ses)  #je added multiplier 10/8/05
    y.max <- max(cv.loss.values + cv.loss.ses)  #je added multiplier 10/8/05 }

    if (plot.folds) {
      y.min <- min(cv.loss.matrix)
      y.max <- max(cv.loss.matrix) }

      plot(trees.fitted, cv.loss.values, type = 'l', ylab = "holdout deviance",
          xlab = "no. of trees", ylim = c(y.min,y.max), ...)
      abline(h = y.bar, col = 2)

      lines(trees.fitted, cv.loss.values + cv.loss.ses, lty=2)
      lines(trees.fitted, cv.loss.values - cv.loss.ses, lty=2)

      if (plot.folds) {
        for (i in 1:n.folds) {
          lines(trees.fitted, cv.loss.matrix[i,],lty = 3)
      }
    }
  }

# identify the optimal number of trees

  target.trees <- trees.fitted[match(TRUE,cv.loss.values == y.bar)]
  #cat("\n");cat("optimal n trees =",target.trees,"\n")    #AKS

  if(plot.main) {
    abline(v = target.trees, col=3)
    title(paste(sp.name,", d - ",tree.complexity,", lr - ",learning.rate, sep=""))
  }

# skip additional details on final model unless target.trees is close to 1000 (AKS)#
if(abs(target.trees-1000) >= 600 & superfast==T){         # AKS
    return(list(target.trees=target.trees)) # AKS
    } else {      # AKS


# estimate the cv deviance and test statistics
# includes estimates of the standard error of the fitted values added 2nd may 2005

  cv.deviance.stats <- rep(0, n.folds)
  cv.roc.stats <- rep(0, n.folds)
  cv.cor.stats <- rep(0, n.folds)
  cv.calibration.stats <- matrix(0, ncol=5, nrow = n.folds)
  if (family == "bernoulli") threshold.stats <- rep(0, n.folds)

  fitted.matrix <- matrix(NA, nrow = n.cases, ncol = n.folds)   #used to calculate se's
  fold.fit <- rep(0,n.cases)

  for (i in 1:n.folds) {

    pred.mask <- selector == i   #used to identify the with-held subset
    model.mask <- selector != i  #used to fit model on majority of data

    fits <- predict.gbm(model.list[[i]], x.data[model.mask, ], n.trees = target.trees)
    if (!is.null(offset)) fits <- fits + offset[model.mask]
    if (family == "bernoulli") fits <- exp(fits)/(1 + exp(fits))
    if (family == "poisson") fits <- exp(fits)
    fitted.matrix[model.mask,i] <- fits

    fits <- predict.gbm(model.list[[i]], x.data[pred.mask, ], n.trees = target.trees)
    if (!is.null(offset)) fits <- fits + offset[pred.mask]
    fold.fit[pred.mask] <- fits  # store the linear predictor values
    if (family == "bernoulli") fits <- exp(fits)/(1 + exp(fits))
    if (family == "poisson") fits <- exp(fits)
    fitted.matrix[pred.mask,i] <- fits

    y_i <- y.data[pred.mask]
    u_i <- fitted.matrix[pred.mask,i]  #pred.values[pred.mask]      ####
    weight.preds <- site.weights[pred.mask]

    cv.deviance.stats[i] <- calc.deviance(y_i, u_i, weight.preds, family = family)

    cv.cor.stats[i] <- cor(y_i,u_i)

    if (family == "bernoulli") {
      cv.roc.stats[i] <- roc(y_i,u_i)
      cv.calibration.stats[i,] <- calibration(y_i,u_i,"binomial")
      threshold.stats[i] <- approx(ppoints(u_i), sort(u_i,decreasing = T), prevalence)$y
    }

    if (family == "poisson") {
      cv.calibration.stats[i,] <- calibration(y_i,u_i,"poisson")
    }
  }

  fitted.vars <- apply(fitted.matrix,1, var, na.rm = TRUE)

 #now calculate the mean and se's for the folds

  cv.dev <- mean(cv.deviance.stats, na.rm = TRUE)
  cv.dev.se <- sqrt(var(cv.deviance.stats)) / sqrt(n.folds)

  cv.cor <- mean(cv.cor.stats, na.rm = TRUE)
  cv.cor.se <- sqrt(var(cv.cor.stats, use = "na.or.complete")) / sqrt(n.folds) #MMF - hack instead of error trapping. was "complete.obs"

  cv.roc <- 0.0
  cv.roc.se <- 0.0

  if (family == "bernoulli") {
    cv.roc <- mean(cv.roc.stats,na.rm=TRUE)
    cv.roc.se <- sqrt(var(cv.roc.stats, use = "na.or.complete")) / sqrt(n.folds) #MMF
    cv.threshold <- mean(threshold.stats, na.rm = T)
    cv.threshold.se <- sqrt(var(threshold.stats, use = "na.or.complete")) / sqrt(n.folds) #MMF
  }

  cv.calibration <- 0.0
  cv.calibration.se <- 0.0

  if (family == "poisson" | family == "bernoulli") {
    cv.calibration <- apply(cv.calibration.stats,2,mean)
    cv.calibration.se <- apply(cv.calibration.stats,2,var)
    cv.calibration.se <- sqrt(cv.calibration.se) / sqrt(n.folds) }

 #fit the final model

  if (is.null(offset)) {
    gbm.call <- paste("gbm(y.data~.,data=x.data,n.trees=target.trees,interaction.depth=tree.complexity,shrinkage=learning.rate,bag.fraction=bag.fraction,weights=site.weights,distribution=as.character(family),var.monotone=var.monotone,verbose=FALSE)",sep="")
    }
  else {
    gbm.call <- paste("gbm(y.data~.+ offset(offset),data=x.data,n.trees=target.trees,interaction.depth=tree.complexity,shrinkage=learning.rate,bag.fraction=bag.fraction,weights=site.weights,distribution=as.character(family),var.monotone=var.monotone,verbose=FALSE)",sep="")
    }

  if (!silent) cat("fitting final gbm model with a fixed number of ",target.trees," trees for ",sp.name,"\n")

  gbm.object <- eval(parse(text = gbm.call))  # AKS

  best.trees <- target.trees

#extract fitted values and summary table

  gbm.summary <- summary(gbm.object,n.trees = target.trees, plotit = FALSE)

  fits <- predict.gbm(gbm.object,x.data,n.trees = target.trees)
  if (!is.null(offset)) fits <- fits + offset
  if (family == "bernoulli") fits <- exp(fits)/(1 + exp(fits))
  if (family == "poisson") fits <- exp(fits)
  fitted.values <- fits

  y_i <- y.data
  u_i <- fitted.values
  resid.deviance <- calc.deviance(y_i, u_i, weights = site.weights, family = family, calc.mean = FALSE)

  self.cor <- cor(y_i,u_i)
  self.calibration <- 0.0
  self.roc <- 0.0

  if (family == "bernoulli") {   #do this manually as we need the residuals
    deviance.contribs <- (y_i * log(u_i)) + ((1-y_i) * log(1 - u_i))
    residuals <- sqrt(abs(deviance.contribs * 2))
    residuals <- ifelse((y_i - u_i) < 0, 0 - residuals, residuals)
    self.roc <- roc(y_i,u_i)
    self.calibration <- calibration(y_i,u_i,"binomial")
  }

  if (family == "poisson") {    #do this manually as we need the residuals
    deviance.contribs <- ifelse(y_i == 0, 0, (y_i * log(y_i/u_i))) - (y_i - u_i)
    residuals <- sqrt(abs(deviance.contribs * 2))
    residuals <- ifelse((y_i - u_i) < 0, 0 - residuals, residuals)
    self.calibration <- calibration(y_i,u_i,"poisson")
  }

  if (family == "gaussian" | family == "laplace") {
    residuals <- y_i - u_i
  }

  mean.resid.deviance <- resid.deviance/n.cases

  z2 <- unclass(Sys.time())
  elapsed.time.minutes <- round((z2 - z1)/ 60,2)  # calculate the total elapsed time

  if (verbose) {
    cat("\n")
    cat("mean total deviance =", round(mean.total.deviance,3),"\n")
    cat("mean residual deviance =", round(mean.resid.deviance,3),"\n","\n")
    cat("estimated cv deviance =", round(cv.dev,3),"; se =",
      round(cv.dev.se,3),"\n","\n")
    cat("training data correlation =",round(self.cor,3),"\n")
    cat("cv correlation = ",round(cv.cor,3),"; se =",round(cv.cor.se,3),"\n","\n")
    if (family == "bernoulli") {
      cat("training data ROC score =",round(self.roc,3),"\n")
      cat("cv ROC score =",round(cv.roc,3),"; se =",round(cv.roc.se,3),"\n","\n")
    }
    cat("elapsed time - ",round(elapsed.time.minutes,2),"minutes","\n")
  }

  if (n.fitted == max.trees & !silent) {
    cat("\n"," warning ","\n","\n")
    cat("maximum tree limit reached - results may not be optimal","\n")
    cat("  - refit with faster learning rate or increase maximum number of trees","\n")


  }

 # now assemble data to be returned

  gbm.detail <- list(dataframe = dataframe.name, gbm.x = gbm.x, predictor.names = names(x.data),
    gbm.y = gbm.y, response.name = sp.name, offset = offset.name, family = family, tree.complexity = tree.complexity,
    learning.rate = learning.rate, bag.fraction = bag.fraction, cv.folds = n.folds,
    prev.stratification = prev.stratify, max.fitted = n.fitted, n.trees = target.trees,
    best.trees = target.trees, train.fraction = 1.0, tolerance.method = tolerance.method,
    tolerance = tolerance, var.monotone = var.monotone, date = date(),
    elapsed.time.minutes = elapsed.time.minutes)

  training.stats <- list(null = total.deviance, mean.null = mean.total.deviance,
    resid = resid.deviance, mean.resid = mean.resid.deviance, correlation = self.cor,
    discrimination = self.roc, calibration = self.calibration)

  cv.stats <- list(deviance.mean = cv.dev, deviance.se = cv.dev.se,
    correlation.mean = cv.cor, correlation.se = cv.cor.se,
    discrimination.mean = cv.roc, discrimination.se = cv.roc.se,
    calibration.mean = cv.calibration, calibration.se = cv.calibration.se)

  if (family == "bernoulli") {
    cv.stats$cv.threshold <- cv.threshold
    cv.stats$cv.threshold.se <- cv.threshold.se
  }

  rm(x.data,y.data, envir = globalenv())          # finally, clean up the temporary dataframes

 # and assemble results for return

  gbm.object$gbm.call <- gbm.detail
  gbm.object$fitted <- fitted.values
  gbm.object$fitted.vars <- fitted.vars
  gbm.object$residuals <- residuals
  gbm.object$contributions <- gbm.summary
  gbm.object$self.statistics <- training.stats
  gbm.object$cv.statistics <- cv.stats
  gbm.object$weights <- site.weights
  gbm.object$trees.fitted <- trees.fitted
  gbm.object$training.loss.values <- training.loss.values
  gbm.object$cv.values <- cv.loss.values
  gbm.object$cv.loss.ses <- cv.loss.ses
  gbm.object$cv.loss.matrix <- cv.loss.matrix
  gbm.object$cv.roc.matrix <- cv.roc.stats
  gbm.object$target.trees <- target.trees


  if (keep.fold.models) gbm.object$fold.models <- model.list
  else gbm.object$fold.models <- NULL

  if (keep.fold.vector) gbm.object$fold.vector <- selector
  else gbm.object$fold.vector <- NULL

  if (keep.fold.fit) gbm.object$fold.fit <- fold.fit
  else gbm.object$fold.fit <- NULL

  return(gbm.object)
  } #end if statement
}

"gbm.plot" <-
function(gbm.object,                # a gbm object - could be one from gbm.step
     variable.no = 0,               # the var to plot - if zero then plots all
     nt = gbm.object$n.trees,       # how many trees to use
     smooth = FALSE,                # should we add a smoothed version of the fitted function
     rug = T,                       # plot a rug of deciles
     n.plots = length(pred.names),  # plot the first n most important preds
     common.scale = T,              # use a common scale on the y axis
     write.title = T,               # plot a title above the plot
     y.label = "fitted function",   # the default y-axis label
     x.label = NULL,                # the default x-axis label
     show.contrib = T,              # show the contribution on the x axis
     plot.layout = c(3,4),          # define the default layout for graphs on the page
     rug.side = 3,                  # which axis for rug plot? default (3) is top; bottom=1
     rug.lwd = 1,                   # line width for rug plots
     rug.tick = 0.03,               # tick length for rug plots
     plotit=T,                      # AKS
     prob.scale=T,                       # AKS
     ...                            # other arguments to pass to the plotting
                                    # useful options include cex.axis, cex.lab, etc.
     )
{
# function to plot gbm response variables, with the option
# of adding a smooth representation of the response if requested
# additional options in this version allow for plotting on a common scale
# note too that fitted functions are now centered by subtracting their mean
#
# version 2.9
#
# j. leathwick/j. elith - March 2007
#

require(gbm)

gbm.call <- gbm.object$gbm.call
gbm.x <- gbm.call$gbm.x
pred.names <- gbm.call$predictor.names
out.names <- rep("",length(pred.names))
response.name <- gbm.call$response.name
dataframe.name <- gbm.call$dataframe
data <- try(eval.parent(parse(text = dataframe.name)),silent=TRUE)
if(class(data)=="try-error") data<-eval.parent(dat)

max.plots <- plot.layout[1] * plot.layout[2]
plot.count <- 0
n.pages <- 1

if (length(variable.no) > 1) {stop("only one response variable can be plotted at a time")}

if (variable.no > 0) {   #we are plotting all vars in rank order of contribution
  n.plots <- 1
  }

max.vars <- length(gbm.object$contributions$var)
if (n.plots > max.vars) {
  n.plots <- max.vars
  cat("warning - reducing no of plotted predictors to maximum available (",max.vars,")\n",sep="")
  }

predictors <- list(rep(NA,n.plots)) # matrix(0,ncol=n.plots,nrow=100)
responses <- list(rep(NA,n.plots)) # matrix(0,ncol=n.plots,nrow=100)
for (j in c(1:n.plots)) {  #cycle through the first time and get the range of the functions
  if (n.plots == 1) {
    k <- variable.no
  }
  else k <- match(gbm.object$contributions$var[j],pred.names)

  if (is.null(x.label)) var.name <- gbm.call$predictor.names[k]
    else var.name <- x.label

  pred.data <- data[,gbm.call$gbm.x[k]]
  out.names[j] <- names(data)[gbm.call$gbm.x[k]]

  response.matrix <- plot.gbm(gbm.object, i.var = k, n.trees = nt, return.grid = TRUE,...)

  predictors[[j]] <- response.matrix[,1]
  if (is.factor(data[,gbm.call$gbm.x[k]])) {
    predictors[[j]] <- factor(predictors[[j]],levels = levels(data[,gbm.call$gbm.x[k]]))
    }
  responses[[j]] <- response.matrix[,2] - mean(response.matrix[,2])
  if(prob.scale) responses[[j]] <- logit(responses[[j]])

  if(j == 1) {
    ymin = min(responses[[j]])
    ymax = max(responses[[j]])
    }
  else {
    ymin = min(ymin,min(responses[[j]]))
    ymax = max(ymax,max(responses[[j]]))
    }
  }

# now do the actual plots
  if(plotit==T){
      for (j in c(1:n.plots)) {

       if (plot.count == max.plots) {
         plot.count = 0
         n.pages <- n.pages + 1
       }

       if (plot.count == 0) {
         #windows(width = 11, height = 8)  #aks
         par(mfrow = plot.layout)
       }

        plot.count <- plot.count + 1

        if (n.plots == 1) {
          k <- match(pred.names[variable.no],gbm.object$contributions$var)
          if (show.contrib) {
             x.label <- paste(var.name,"  (",round(gbm.object$contributions[k,2],1),"%)",sep="")
          }
        }
        else {
          k <- match(gbm.object$contributions$var[j],pred.names)
          var.name <- gbm.call$predictor.names[k]
          if (show.contrib) {
             x.label <- paste(var.name,"  (",round(gbm.object$contributions[j,2],1),"%)",sep="")
          }
          else x.label <- var.name
        }

        if (common.scale) {
          plot(predictors[[j]],responses[[j]],ylim=c(ymin,ymax), type='l',
            xlab = x.label, ylab = y.label, ...)
        }
        else {
          plot(predictors[[j]],responses[[j]], type='l',
            xlab = x.label, ylab = y.label, ...)
        }
        if (smooth & is.vector(predictors[[j]])) {
          temp.lo <- loess(responses[[j]] ~ predictors[[j]], span = 0.3)
          lines(predictors[[j]],fitted(temp.lo), lty = 2, col = 2)
        }
        if (plot.count == 1) {
          if (write.title) {
            title(paste(response.name," - page ",n.pages,sep=""))
          }
          if (rug & is.vector(data[,gbm.call$gbm.x[k]])) {
            rug(quantile(data[,gbm.call$gbm.x[k]], probs = seq(0, 1, 0.1), na.rm = TRUE), side = rug.side, lwd = rug.lwd, ticksize = rug.tick)
          }
        }
        else {
          if (write.title & j == 1) {
            title(response.name)
          }
          if (rug & is.vector(data[,gbm.call$gbm.x[k]])) {
            rug(quantile(data[,gbm.call$gbm.x[k]], probs = seq(0, 1, 0.1), na.rm = TRUE), side = rug.side, lwd = rug.lwd, ticksize = rug.tick)
          }
        }
      }
    }
  return(invisible(list(names=out.names,preds=predictors,resp=responses)))
}


"gbm.perspec" <-
function(gbm.object,
     x = 1,                # the first variable to be plotted
     y = 2,                # the second variable to be plotted
     pred.means = NULL,    # allows specification of values for other variables
     x.label = NULL,       # allows manual specification of the x label
     x.range = NULL,       # manual range specification for the x variable
     y.label = NULL,       # and y label
     y.range = NULL,       # and the y
     z.range = NULL,       # allows control of the vertical axis
     ticktype = "detailed",# specifiy detailed types - otherwise "simple"
     theta = 55,           # rotation
     phi=40,               # and elevation
     smooth = "none",      # controls smoothing of the predicted surface
     mask = FALSE,         # controls masking using a sample intensity model
     perspective = TRUE,   # controls whether a contour or perspective plot is drawn
     verbose=F,            # AKS
     pas.data,...)                  # allows the passing of additional arguments to plotting routine
                           # useful options include shade, ltheta, lphi for controlling illumination
                           # and cex for controlling text size - cex.axis and cex.lab have no effect
{
#
# gbm.perspec version 2.9 April 2007
# J Leathwick/J Elith
#
# takes a gbm boosted regression tree object produced by gbm.step and
# plots a perspective plot showing predicted values for two predictors
# as specified by number using x and y
# values for all other variables are set at their mean by default
# but values can be specified by giving a list consisting of the variable name
# and its desired value, e.g., c(name1 = 12.2, name2 = 57.6)

  require(gbm)


#get the boosting model details

  gbm.call <- gbm.object$gbm.call
  gbm.x <- gbm.call$gbm.x
  n.preds <- length(gbm.x)
  gbm.y <- gbm.call$gbm.y
  pred.names <- gbm.call$predictor.names
  family = gbm.call$family

  x.name <- gbm.call$predictor.names[x]

  if (is.null(x.label)) {
    x.label <- gbm.call$predictor.names[x]}

  y.name <- gbm.call$predictor.names[y]

  if (is.null(y.label)) {
    y.label <- gbm.call$predictor.names[y]}

  data <- try(eval.parent(parse(text=gbm.call$dataframe))[,gbm.x])   #AKS
  if(class(data)=="try-error") data<-pas.data$dat[,gbm.x]
  n.trees <- gbm.call$best.trees

  if (is.null(x.range)) {
   if(!is.factor(data[,x]))
        x.var <- seq(min(data[,x],na.rm=T),max(data[,x],na.rm=T),length = 50)
      else x.var<- sort(unique(as.numeric(levels(data[,x]))))
  }
  else {x.var <- seq(x.range[1],x.range[2],length = 50)}

  if (is.null(y.range)) {
      if(!is.factor(data[,y]))
        y.var <- seq(min(data[,y],na.rm=T),max(data[,y],na.rm=T),length = 50)
      else y.var<- sort(unique(as.numeric(levels(data[,y]))))
  }
  else {y.var <- seq(y.range[1],y.range[2],length = 50)}

  pred.frame <- expand.grid(list(x.var,y.var))
  names(pred.frame) <- c(x.name,y.name)

  j <- 3
  for (i in 1:n.preds) {
    if (i != x & i != y) {
      if (is.vector(data[,i])) {
        m <- match(pred.names[i],names(pred.means))
        if (is.na(m)) {
          pred.frame[,j] <- mean(data[,i],na.rm=T)
        }
        else pred.frame[,j] <- pred.means[m]
      }
      if (is.factor(data[,i])) {
        m <- match(pred.names[i],names(pred.means))
        temp.table <- table(data[,i])
        if (is.na(m)) {
          pred.frame[,j] <- rep(names(temp.table)[2],2500)
        }
        else pred.frame[,j] <- pred.means[m]
        pred.frame[,j] <- factor(pred.frame[,j],levels=names(temp.table))
      }
      names(pred.frame)[j] <- pred.names[i]
      j <- j + 1
     }
  }
#
# form the prediction
#
  prediction <- predict.gbm(gbm.object,pred.frame,n.trees = n.trees, type="response")

# model smooth if required

  if (smooth == "model") {
    require(splines)
    pred.glm <- glm(prediction ~ ns(pred.frame[,1], df = 8) * ns(pred.frame[,2], df = 8), data=pred.frame,family=poisson)
    prediction <- fitted(pred.glm)
  }

# report the maximum value and set up realistic ranges for z

  max.pred <- max(prediction)
  min.pred<-min(prediction)
  if(verbose) cat("maximum value = ",round(max.pred,2),"\n")    #AKS

  if (is.null(z.range)) {
    if (family == "bernoulli") {
      z.range <- c(min.pred,max.pred)
    }
    else if (family == "poisson") {
      z.range <- c(0,max.pred * 1.1)
    }
    else {
      z.min <- min(data[,y],na.rm=T)
      z.max <- max(data[,y],na.rm=T)
      z.delta <- z.max - z.min
      z.range <- c(z.min - (1.1 * z.delta), z.max + (1.1 * z.delta))
    }
  }
# form the matrix

  pred.matrix <- matrix(prediction,ncol=length(y.var),nrow=length(x.var))

# kernel smooth if required

  if (smooth == "average") {  #apply a 3 x 3 smoothing average
     pred.matrix.smooth <- pred.matrix
     for (i in 2:(length(x.var)-1)) {
       for (j in 2:(length(y.var)-1)) {
         pred.matrix.smooth[i,j] <- mean(pred.matrix[c((i-1):(i+1)),c((j-1):(j+1))])
       }
     }
  pred.matrix <- pred.matrix.smooth
  }

# mask out values inside hyper-rectangle but outside of sample space

  if (mask) {
    mask.trees <- mask.object$gbm.call$best.trees
    point.prob <- predict.gbm(mask.object[[1]],pred.frame, n.trees = mask.trees, type="response")
    point.prob <- matrix(point.prob,ncol=50,nrow=50)
    pred.matrix[point.prob < 0.5] <- 0.0
  }
#
# and finally plot the result

  if (!perspective) {
    image(x = x.var, y = y.var, z = pred.matrix, zlim = z.range)
  }
  else {
    persp(x=x.var, y=y.var, z=pred.matrix, zlim= z.range,      # input vars
      xlab = x.label, ylab = y.label, zlab = "fitted value",   # labels
      theta=theta, phi=phi, r = sqrt(10), d = 3,               # viewing pars
      ticktype = ticktype, mgp = c(4,1,0), ...) #
  }
}

"calibration" <-
function(obs, preds, family = "binomial")
{
#
# j elith/j leathwick 17th March 2005
# calculates calibration statistics for either binomial or count data
# but the family argument must be specified for the latter
# a conditional test for the latter will catch most failures to specify
# the family
#

if (family == "bernoulli") family <- "binomial"
pred.range <- max(preds) - min(preds)
if(pred.range > 1.2 & family == "binomial") {
print(paste("range of response variable is ", round(pred.range, 2)), sep = "", quote = F)
print("check family specification", quote = F)
return()
}
if(family == "binomial") {
pred <- preds + 1e-005
pred[pred >= 1] <- 0.99999
mod <- glm(obs ~ log((pred)/(1 - (pred))), family = binomial)
lp <- log((pred)/(1 - (pred)))
a0b1 <- glm(obs ~ offset(lp) - 1, family = binomial)
miller1 <- 1 - pchisq(a0b1$deviance - mod$deviance, 2)
ab1 <- glm(obs ~ offset(lp), family = binomial)
miller2 <- 1 - pchisq(a0b1$deviance - ab1$deviance, 1)
miller3 <- 1 - pchisq(ab1$deviance - mod$deviance, 1)
}
if(family == "poisson") {
if(any(preds==0)) preds[preds==0] <- preds[preds==0] + 1e-006 #a little fix so this still works whit predictions equal to zero
mod <- glm(obs ~ log(preds), family = poisson)
lp <- log(preds)
a0b1 <- glm(obs ~ offset(lp) - 1, family = poisson)
miller1 <- 1 - pchisq(a0b1$deviance - mod$deviance, 2)
ab1 <- glm(obs ~ offset(lp), family = poisson)
miller2 <- 1 - pchisq(a0b1$deviance - ab1$deviance, 1)
miller3 <- 1 - pchisq(ab1$deviance - mod$deviance, 1)
}
calibration.result <- c(mod$coef, miller1, miller2, miller3)
names(calibration.result) <- c("intercept", "slope", "testa0b1", "testa0|b1", "testb1|a")
return(calibration.result)
}

"roc" <-
function (obsdat, preddat)
{
# code adapted from Ferrier, Pearce and Watson's code, by J.Elith
#
# see:
# Hanley, J.A. & McNeil, B.J. (1982) The meaning and use of the area
# under a Receiver Operating Characteristic (ROC) curve.
# Radiology, 143, 29-36
#
# Pearce, J. & Ferrier, S. (2000) Evaluating the predictive performance
# of habitat models developed using logistic regression.
# Ecological Modelling, 133, 225-245.
# this is the non-parametric calculation for area under the ROC curve,
# using the fact that a MannWhitney U statistic is closely related to
# the area
#
    if (length(obsdat) != length(preddat))
        stop("obs and preds must be equal lengths")
    n.x <- length(obsdat[obsdat == 0])
    n.y <- length(obsdat[obsdat == 1])
    xy <- c(preddat[obsdat == 0], preddat[obsdat == 1])
    rnk <- rank(xy)
    wilc <- ((n.x * n.y) + ((n.x * (n.x + 1))/2) - sum(rnk[1:n.x]))/(n.x *
        n.y)
    return(round(wilc, 4))
}



"gbm.interactions" <-
function(gbm.object,
   use.weights = FALSE,     # use weights for samples
   mask.object)             # a gbm object describing sample intensity
{
#
# gbm.interactions version 2.9
#
# j. leathwick, j. elith - May 2007
#
# functions assesses the magnitude of 2nd order interaction effects
# in gbm models fitted with interaction depths greater than 1
# this is achieved by:
#   1. forming predictions on the linear scale for each predictor pair;
#   2. fitting a linear model that relates these predictions to the predictor
#        pair, with the the predictors fitted as factors;
#   3. calculating the mean value of the residuals, the magnitude of which
#        increases with the strength of any interaction effect;
#   4. results are stored in an array;
#   5. finally, the n most important interactions are identified,
#        where n is 25% of the number of interaction pairs;

  require(gbm)

  gbm.call <- gbm.object$gbm.call
  n.trees <- gbm.call$best.trees
  depth <- gbm.call$interaction.depth
  gbm.x <- gbm.call$gbm.x
  n.preds <- length(gbm.x)
  pred.names <- gbm.object$gbm.call$predictor.names
  cross.tab <- matrix(0,ncol=n.preds,nrow=n.preds)
  dimnames(cross.tab) <- list(pred.names,pred.names)
  int.p.table <- matrix(NA,ncol=n.preds,nrow=n.preds,dimnames=list(pred.names,pred.names))#AKS

  if (use.weights) mask.trees <- mask.object$gbm.call$best.trees

  #cat("gbm.interactions - version 2.9 \n")  #aks
  #cat("Cross tabulating interactions for gbm model with ",n.preds," predictors","\n",sep="") #aks

  data <- eval.parent(parse(text=gbm.call$dataframe),n=1)[,gbm.x]    #aks

  for (i in 1:(n.preds - 1)) {  # step through the predictor set
    x.fac<-F
    if (is.vector(data[,i])) {  # create a sequence through the range
       x.var <- seq(min(data[,i],na.rm=T),max(data[,i],na.rm=T),length = 20)
       x2 <-  as.vector(tapply(x.var,rep(1:10,rep(2,10)),mean))
       }  else {                      # otherwise set up simple factor variable
       x.var <- factor(names(table(data[,i])),levels = levels(data[,i]))
       x.fac<-T
       }
    x.length <- length(x.var)

    #cat(i,"\n")  #aks

    for (j in (i+1):n.preds) { #create vector or factor data for second variable
      y.fac<-F
      if (is.vector(data[,j])) {
        y.var <- seq(min(data[,j],na.rm=T),max(data[,j],na.rm=T),length = 20)
        y2 <-  as.vector(tapply(y.var,rep(1:10,rep(2,10)),mean))

      }    else {
        y.var <- factor(names(table(data[,j])),levels = levels(data[,j]))
        y.fac<-T
      }
      y.length <- length(y.var)

# and now make a temporary data frame

      pred.frame <- expand.grid(list(x.var,y.var))
      names(pred.frame) <- c(pred.names[i],pred.names[j])

      n <- 3 # and add the balance of the variables to it

      for (k in 1:n.preds) {
        if (k != i & k != j) {
          if (is.vector(data[,k])) {  # either with the mean
            pred.frame[,n] <- mean(data[,k],na.rm=T)
          } else {   # or the most common factor level
            temp.table <- sort(table(data[,k]),decreasing = TRUE)
            pred.frame[,n] <- rep(names(temp.table)[1],x.length * y.length)
            pred.frame[,n] <- as.factor(pred.frame[,n])
          }
          names(pred.frame)[n] <- pred.names[k]
          n <- n + 1
        }
      }
#
# form the prediction
#
      prediction <- predict.gbm(gbm.object,pred.frame,n.trees = n.trees, type="link")

      if (use.weights) {
        point.prob <- predict.gbm(mask.object[[1]],pred.frame, n.trees = mask.trees, type="response")
        interaction.test.model <- lm(prediction ~ as.factor(pred.frame[,1]) + as.factor(pred.frame[,2]),
          weights = point.prob)
      }

      else {

        interaction.test.model <- lm(prediction ~ as.factor(pred.frame[,1]) + as.factor(pred.frame[,2]))
      }
      #cat("\n",i,names(data)[i],x.length,length(unique(data[,i])),"  ",
      #      j,names(data)[j],y.length,length(unique(data[,j])),dim(pred.frame));flush.console()
      #print(summary(data[,i]));print(summary(data[,j]))
      # check significance of interaction term in model that pools every 4 grid points #
      pred.frame<<-pred.frame

      if(x.fac==F | y.fac==F){ # case where at least one cov is not a factor
          if(x.fac==T) {x3 <- pred.frame[,1]
          } else { x3 <- rep(rep(x2,rep(2,10)),y.length) }
          if(y.fac==T) {y3 <- pred.frame[,1]
          } else { y3 <- rep(y2,rep(x.length*2,length(y2)))}
          int.p.table[i,j] <- anova(lm(prediction~as.factor(x3)*as.factor(y3)))[3,5] #aks
          }


      interaction.flag <- round(mean(resid(interaction.test.model)^2)*1000,2)

      cross.tab[i,j] <- interaction.flag
      }   # end of j loop
  }  # end of i loop

# create an index of the values in descending order

  search.index <- ((n.preds^2) + 1) - rank(cross.tab, ties.method = "first")

  n.important <- max(2,round(0.1 * ((n.preds^2)/2),0))
  var1.names <- rep(" ",n.important)
  var1.index <- rep(0,n.important)
  var2.names <- rep(" ",n.important)
  var2.index <- rep(0,n.important)
  int.size <- rep(0,n.important)
  int.p <- rep(NA,n.important) #aks

  for (i in 1:n.important) {

    index.match <- match(i,search.index)

    j <- trunc(index.match/n.preds) + 1
    var1.index[i] <- j
    var1.names[i] <- pred.names[j]

    k <- index.match%%n.preds
    if (k > 0) {   #only do this if k > 0 - otherwise we have all zeros from here on
      var2.index[i] <- k
      var2.names[i] <- pred.names[k]

      int.size[i] <- cross.tab[k,j]
      int.p[i] <- int.p.table[k,j] #aks
    }

  }
  rank.list <- data.frame(var1.index,var1.names,var2.index,var2.names,int.size,p=int.p)
  rank.list <- rank.list[complete.cases(rank.list),]
  return(list(rank.list = rank.list, interactions = cross.tab, gbm.call = gbm.object$gbm.call))
}

"gbm.simplify" <-
function(gbm.object,          # a gbm object describing sample intensity
  n.folds = 10,               # number of times to repeat the analysis
  n.drops = "auto",           # can be automatic or an integer specifying the number of drops to check
  alpha = 1,                  # controls stopping when n.drops = "auto"
  prev.stratify = TRUE,       # use prevalence stratification in selecting evaluation data
  eval.data = NULL,           # an independent evaluation data set - leave here for now
  plot = TRUE,                # plot results
  verbose=F)                  # to suppress text output.
{
# function to simplify a brt model fitted using gbm.step
#
# version 2.9 - J. Leathwick/J. Elith - June 2007
#
# starts with an inital cross-validated model as produced by gbm.step
# and then assesses the potential to remove predictors using k-fold cv
# does this for each fold, removing the lowest contributing predictor,
# and repeating this process for a set number of steps
# after the removal of each predictor, the change in predictive deviance
# is computed relative to that obtained when using all predictors
# it returns a list containing the mean change in deviance and its se
# as a function of the number of variables removed
# having completed the cross validation, it then identifies the sequence
# of variable to remove when using the full data set, testing this
# up to the number of steps used in the cross-validation phase of the analysis
# with results reported to the screen - it then returns
# a table containing the order in which variables are to be removed
# and a list of vectors, each of which specifies the predictor col numbers
# in the original dataframe  - the latter can be used as an argument to gbm.step
# e.g., gbm.step(data = data, gbm.x = simplify.object$pred.list[[4]]...
# would implement a new analysis with the original predictor set, minus its
# four lowest contributing predictors
#

require(gbm)

# first get the original analysis details..

  gbm.call <- gbm.object$gbm.call
  data <- eval.parent(parse(text=gbm.call$dataframe))
  n.cases <- nrow(data)
  gbm.x <- gbm.call$gbm.x
  gbm.y <- gbm.call$gbm.y
  family <- gbm.call$family
  lr <- gbm.call$learning.rate
  tc <- gbm.call$tree.complexity
  start.preds <- length(gbm.x)
  max.drops <- start.preds - 2
  response.name <- gbm.call$response.name
  predictor.names <- gbm.call$predictor.names
  n.trees <- gbm.call$best.trees
  pred.list <- list(initial = gbm.x)
  weights <- gbm.object$weights

  if (n.drops == "auto") auto.stop <- TRUE

  else auto.stop <- FALSE

# take a copy of the original data and starting predictors

  orig.data <- data
  orig.gbm.x <- gbm.x
  verbose=TRUE
#  if (!is.null(eval.data)) independent.test <- TRUE
#    else independent.test <- FALSE

# extract original performance statistics...

  original.deviance <- round(gbm.object$cv.statistics$deviance.mean,4)
  original.deviance.se <- round(gbm.object$cv.statistics$deviance.se,4)

  if(verbose){ #aks
    cat("\n");cat("gbm.simplify - version 2.9","\n\n")#AKS
    cat("simplifying gbm.step model for ",response.name," with ",start.preds," predictors",sep="")
    cat(" and ",n.cases," observations \n",sep="")
    cat("original deviance = ",original.deviance,"(",original.deviance.se,")\n\n",sep="")
    }

# check that n.drops is less than n.preds - 2 and update if required

  if (auto.stop) {
    if(verbose) cat("variable removal will proceed until average change exceeds the original se\n\n") #aks
    n.drops <- 1 } else{
    if (n.drops > start.preds - 2) {
      if(verbose) cat("value of n.drops (",n.drops,") is greater than permitted","\n", #aks
        "resetting value to ",start.preds - 2,"\n\n",sep="")
      n.drops <- start.preds - 2
    } else {
      if(verbose) cat("a fixed number of",n.drops,"drops will be tested\n\n") #aks
    }
  }
  flush.console()#AKS


# set up storage for results

  dev.results <- matrix(0, nrow = n.drops, ncol = n.folds)
  dimnames(dev.results) <- list(paste("drop.",1:n.drops,sep=""),
   paste("rep.",1:n.folds,sep=""))

  drop.count <- matrix(NA, nrow = start.preds, ncol = n.folds)
  dimnames(drop.count) <- list(predictor.names,paste("rep.",1:n.folds,sep=""))

  original.deviances <- rep(0,n.folds)

  model.list <- list(paste("model",c(1:n.folds),sep=""))     # dummy list for the tree models

# create gbm.fixed function call

  gbm.call.string <- paste("gbm.fixed(data=train.data,gbm.x=gbm.new.x,gbm.y=gbm.y,",sep="")
  gbm.call.string <- paste(gbm.call.string,"family=family,learning.rate=lr,tree.complexity=tc,",sep="")
  gbm.call.string <- paste(gbm.call.string,"n.trees = ",n.trees,", site.weights = weights.subset,verbose=FALSE)",sep="")

# now set up the fold structure

  if (prev.stratify & family == "bernoulli") {
    presence.mask <- data[,gbm.y] == 1
    absence.mask <- data[,gbm.y] == 0
    n.pres <- sum(presence.mask)
    n.abs <- sum(absence.mask)

# create a vector of randomised numbers and feed into presences
    selector <- rep(0,n.cases)
    temp <- rep(seq(1, n.folds, by = 1), length = n.pres)
    temp <- temp[order(runif(n.pres, 1, 100))]
    selector[presence.mask] <- temp

# and then do the same for absences
    temp <- rep(seq(1, n.folds, by = 1), length = n.abs)
    temp <- temp[order(runif(n.abs, 1, 100))]
    selector[absence.mask] <- temp
    }

  else {  #otherwise make them random with respect to presence/absence
    selector <- rep(seq(1, n.folds, by = 1), length = n.cases)
    selector <- selector[order(runif(n.cases, 1, 100))]
    }

# now start by creating the intial models for each fold

  if(verbose) cat("creating initial models");flush.console()#AKS

  gbm.new.x <- orig.gbm.x

  for (i in 1:n.folds) {

# create the training and prediction folds

    train.data <- orig.data[selector!=i,]
    weights.subset <- weights[selector != i]
    eval.data <- orig.data[selector==i,]

    model.list[[i]] <- eval(parse(text=gbm.call.string))  # create a fixed size object

# now make predictions to the withheld fold

    u_i <- eval.data[,gbm.y]
    y_i <- predict.gbm(model.list[[i]], eval.data, n.trees, "response")

    original.deviances[i] <- round(calc.deviance(u_i,y_i, family = family, calc.mean = TRUE),4)
    if(verbose) cat(".");flush.console()#AKS
  } # end of creating initial models
  if(verbose) {cat("\n");cat("done \n\n")}  #aks
  n.steps <- 1

  while (n.steps <= n.drops & n.steps <= max.drops) {

    if(verbose) cat("dropping predictor",n.steps,"\n");flush.console()#AKS

    for (i in 1:n.folds) {

# get the right data

    train.data <- orig.data[selector!=i,]
    eval.data <- orig.data[selector==i,]
    weights.subset <- weights[selector != i]

# get the current model details

    gbm.x <- model.list[[i]]$gbm.call$gbm.x
    n.preds <- length(gbm.x)
    these.pred.names <- model.list[[i]]$gbm.call$predictor.names
    contributions <- model.list[[i]]$contributions

# get the index number in pred.names of the last variable in the contribution table

    last.variable <- match(as.character(contributions[n.preds,1]),these.pred.names)
    gbm.new.x <- gbm.x[-last.variable]

# and keep a record of what has been dropped

    last.variable <- match(as.character(contributions[n.preds,1]),predictor.names)
    drop.count[last.variable,i] <- n.steps

    model.list[[i]] <- eval(parse(text=gbm.call.string))  # create a fixed size object

    u_i <- eval.data[,gbm.y]
    y_i <- predict.gbm(model.list[[i]],eval.data,n.trees,"response")

    deviance <- round(calc.deviance(u_i,y_i, family = family, calc.mean = TRUE),4)

# calculate difference between intial and new model by subtracting new from old because we want to minimise deviance

    dev.results[n.steps,i] <- round(deviance - original.deviances[i] ,4)

    }

  if (auto.stop){ # check to see if delta mean is less than original deviance error estimate

    delta.mean <- mean(dev.results[n.steps,])

    if (delta.mean < (alpha * original.deviance.se)) {
      n.drops <- n.drops + 1
      dev.results <- rbind(dev.results, rep(0,n.folds))
      }
    }
  n.steps <- n.steps + 1
  }

# now label the deviance matrix

  dimnames(dev.results) <- list(paste("drop.",1:n.drops,sep=""),
   paste("rep.",1:n.folds,sep=""))

# calculate mean changes in deviance and their se

  mean.delta <- apply(dev.results,1,mean)
  se.delta <- sqrt(apply(dev.results,1,var))/sqrt(n.folds)

###########################

  if (plot) {
    y.max <- 1.5 * max(mean.delta + se.delta)
    y.min <- 1.5 * min(mean.delta - se.delta)
    plot(seq(0,n.drops),c(0,mean.delta),xlab="variables removed",
      ylab = "change in predictive deviance",type='l',ylim=c(y.min,y.max))
    lines(seq(0,n.drops),c(0,mean.delta) + c(0,se.delta),lty = 2)
    lines(seq(0,n.drops),c(0,mean.delta) - c(0,se.delta),lty = 2)
    abline(h = 0 , lty = 2, col = 3)
    min.y <- min(c(0,mean.delta))
    min.pos <- match(min.y,c(0,mean.delta)) - 1 # subtract one because now zero base
    abline(v = min.pos, lty = 3, col = 2)
    abline(h = original.deviance.se, lty = 2, col = 2)
    title(paste("RFE deviance - ",response.name," - folds = ",n.folds,sep=""))
  }

# and do a final backwards drop sequence from the original model

  if(verbose) cat("\nnow processing final dropping of variables with full data \n\n") #aks

  gbm.call.string <- paste("gbm.fixed(data=orig.data,gbm.x=gbm.new.x,gbm.y=gbm.y,",sep="")
  gbm.call.string <- paste(gbm.call.string,"family=family,learning.rate=lr,tree.complexity=tc,",sep="")
  gbm.call.string <- paste(gbm.call.string,"n.trees = ",n.trees,", site.weights = weights,verbose=FALSE)",sep="")

  n.steps <- n.steps - 1 #decrement by one to reverse last increment in prev loop

  final.model <- gbm.object  # restore the original model and data
  train.data <- orig.data

# and set up storage

  final.drops <- matrix(NA, nrow = start.preds, ncol = 1)
  dimnames(final.drops) <- list(predictor.names,"step")

  for (i in 1:n.steps) {

# get the current model details

    gbm.x <- final.model$gbm.call$gbm.x
    n.preds <- length(gbm.x)
    these.pred.names <- final.model$gbm.call$predictor.names
    contributions <- final.model$contributions

    if(verbose) cat(i,"-",as.character(contributions[n.preds,1]),"\n");flush.console()#AKS

# get the index number in pred.names of the last variable in the contribution table

    last.variable <- match(as.character(contributions[n.preds,1]),these.pred.names)
    gbm.new.x <- gbm.x[-last.variable]

# and keep a record of what has been dropped

    last.variable <- match(as.character(contributions[n.preds,1]),predictor.names)
    final.drops[last.variable] <- i

    final.model <- eval(parse(text=gbm.call.string))  # create a fixed size object

  }

#and then the corresponding numbers

  removal.list <- dimnames(final.drops)[[1]]
  removal.list <- removal.list[order(final.drops)]
  removal.list <- removal.list[1:n.drops]

  removal.numbers <- rep(0,n.steps)

# construct predictor lists to faciliate final model fitting

  for (i in 1:n.steps) {
    removal.numbers[i] <- match(removal.list[i],predictor.names)
    pred.list[[i]] <- orig.gbm.x[0-removal.numbers[1:i]]
    names(pred.list)[i] <- paste("preds.",i,sep="")
  }

  deviance.summary <- data.frame(mean = round(mean.delta,4), se = round(se.delta,4))

  final.drops <- data.frame("preds" = dimnames(final.drops)[[1]][order(final.drops)],
     "order" = final.drops[order(final.drops)])

  return(list(deviance.summary = deviance.summary,
    deviance.matrix = dev.results, drop.count = drop.count,
    final.drops = final.drops, pred.list = pred.list,
    gbm.call = gbm.call))

}

"gbm.fixed" <-
function (data,                        # the input dataframe
  gbm.x,                               # indices of the predictors in the input dataframe
  gbm.y,                               # index of the response in the input dataframe
  tree.complexity = 1,                 # the tree depth - sometimes referred to as interaction depth
  site.weights = rep(1, nrow(data)),   # by default set equal
  verbose = TRUE,                      # to control reporting
  learning.rate = 0.001,               # controls speed of the gradient descent
  n.trees = 2000,                      # default number of trees
  train.fraction = 1,
  bag.fraction = 0.5,                  # varies random sample size for each new tree
  family = "bernoulli",                # can be any of bernoulli, poisson, gaussian, laplace - note quotes
  keep.data = FALSE,                   # keep original data
  var.monotone = rep(0, length(gbm.x)) # constrain to positive (1) or negative monontone (-1)
  )
{
#
# j leathwick, j elith - 6th May 2007
#
# version 2.9 - developed in R 2.0
#
# calculates a gradient boosting (gbm)object with a fixed number of trees
# with the number of trees identified using gbm.step or some other procedure
#
# mostly used as a utility function, e.g., when being called by gbm.simplify
#
# takes as input a dataset and args selecting x and y variables, learning rate and tree complexity
#
# updated 13/6/05 to accommodate weighting of sites when calculating total and residual deviance
#
# updated 10/8/05 to correct how site.weights are returned
#
# requires gbm
#
#
  require(gbm)

# setup input data and assign to position one

  dataframe.name <- deparse(substitute(data))   # get the dataframe name

  if(length(gbm.x)<=1) stop("Only one predictor remains in backward selection\nplease select more predictors")
  x.data <- eval(data[, gbm.x])                 #form the temporary datasets
  names(x.data) <- names(data)[gbm.x]
  y.data <- eval(data[, gbm.y])
  sp.name <- names(data)[gbm.y]


  assign("x.data", x.data, pos = 1)             #and assign them for later use
  assign("y.data", y.data, pos = 1)

#fit the gbm model

  z1 <- unclass(Sys.time())

  gbm.call <- paste("gbm(y.data ~ .,n.trees = n.trees, data=x.data, verbose = F, interaction.depth = tree.complexity,
    weights = site.weights, shrinkage = learning.rate, distribution = as.character(family),
    var.monotone = var.monotone, bag.fraction = bag.fraction, keep.data = keep.data)", sep="")

  if (verbose) {
    print(paste("fitting gbm model with a fixed number of ",n.trees," trees for ",sp.name,sep=""),quote=FALSE) }

  gbm.object <- eval(parse(text = gbm.call))

  best.trees <- n.trees

#extract fitted values and summary table

  fitted.values <- predict.gbm(gbm.object,x.data,n.trees = n.trees,type="response")
  gbm.summary <- summary(gbm.object,n.trees = n.trees, plotit = FALSE)

  y_i <- y.data
  u_i <- fitted.values

  if (family == "poisson") {
    deviance.contribs <- ifelse(y_i == 0, 0, (y_i * log(y_i/u_i))) - (y_i - u_i)
    resid.deviance <- 2 * sum(deviance.contribs * site.weights)
    residuals <- sqrt(abs(deviance.contribs * 2))
    residuals <- ifelse((y_i - u_i) < 0, 0 - residuals, residuals)

    u_i <- sum(y.data * site.weights) / sum(site.weights)
    deviance.contribs <- ifelse(y_i == 0, 0, (y_i * log(y_i/u_i))) - (y_i - u_i)
    total.deviance <- 2 * sum(deviance.contribs * site.weights)
  }

  if (family == "bernoulli") {
    deviance.contribs <- (y_i * log(u_i)) + ((1-y_i) * log(1 - u_i))
    resid.deviance <- -2 * sum(deviance.contribs * site.weights)
    residuals <- sqrt(abs(deviance.contribs * 2))
    residuals <- ifelse((y_i - u_i) < 0, 0 - residuals, residuals)

    u_i <- sum(y.data * site.weights) / sum(site.weights)
    deviance.contribs <- (y_i * log(u_i)) + ((1-y_i) * log(1 - u_i))
    total.deviance <- -2 * sum(deviance.contribs * site.weights)
  }

  if (family == "laplace") {
    resid.deviance <- sum(abs(y_i - u_i))
    residuals <- y_i - u_i
    u_i <- mean(y.data)
    total.deviance <- sum(abs(y_i - u_i))
  }

  if (family == "gaussian") {
    resid.deviance <- sum((y_i - u_i) * (y_i - u_i))
    residuals <- y_i - u_i
    u_i <- mean(y.data)
    total.deviance <- sum((y_i - u_i) * (y_i - u_i))
  }

  if (verbose) {
    print(paste("total deviance = ",round(total.deviance,2),sep=""),quote=F)
    print(paste("residual deviance = ",round(resid.deviance,2),sep=""),quote=F)}

# now assemble data to be returned

  z2 <- unclass(Sys.time())
  elapsed.time.minutes <- round((z2 - z1)/ 60,2)  #calculate the total elapsed time

  gbm.detail <- list(dataframe = dataframe.name, gbm.x = gbm.x, predictor.names = names(x.data),
    gbm.y = gbm.y, reponse.name = names(y.data), family = family, tree.complexity = tree.complexity,
    learning.rate = learning.rate, bag.fraction = bag.fraction, cv.folds = 0, n.trees = n.trees, best.trees = best.trees,
    train.fraction = train.fraction, var.monotone = var.monotone, date = date(), elapsed.time.minutes = elapsed.time.minutes)

  gbm.object$gbm.call <- gbm.detail
  gbm.object$fitted <- fitted.values
  gbm.object$residuals <- residuals
  gbm.object$contributions <- gbm.summary
  gbm.object$self.statistics <- list(null.deviance = total.deviance, resid.deviance = resid.deviance)
  gbm.object$weights <- site.weights

  rm(x.data,y.data, pos=1)           #finally, clean up the temporary dataframes

  return(gbm.object)
}
