## ======================================================================
## These are the simulation files for a paired interval-null test
## ======================================================================

# ---------------------------------------------------------------------
# sample.function: return a data frame/ matrix / vector with simulated raw data

# get two samples with a specific standardized mean differences
# ES = dz (standardized difference scores)
sample.i.null.paired <- function(n, ES, options.sample=NULL) {
  rnorm(n, ES, sd=1)
}

# ---------------------------------------------------------------------
# select.function: select a specified number of accumulating data from 
# the data frame/ matrix that was simulated with sample.function

select.i.null.paired <- function(MAXSAMP, n) {
  return(MAXSAMP[1:n])
}


# ---------------------------------------------------------------------
# freq.test.function: return p.value, test statistic, and empirical ES

freq.test.i.null.paired <- function(SAMP, alternative=NULL, options.sample=NULL) {
  
  t1 <- t.test(SAMP, mu=0, alternative=alternative)
  
  # see http://journal.frontiersin.org/article/10.3389/fpsyg.2013.00863/full
  
  # must returns these values
  return(list(
    statistic = t1$statistic,
    p.value = t1$p.value,
    emp.ES = t1$statistic / sqrt(length(SAMP))
  ))
}

# ---------------------------------------------------------------------
# Check definition of prior

prior.check.i.null.paired <- function(prior=NULL){
  
  if(!is.list(prior)){
    if(!is.null(prior) == TRUE) {
      stop("Argument prior needs to be specified as a list.")
    } else {
      prior <- list("Cauchy", list(prior.location = 0, prior.scale = sqrt(2)/2))
    }}
  
  match.arg(prior[[1]], c("Cauchy"))
  
  if(is.null(prior[[2]][["prior.location"]])){
    warning("Prior location not defined. Default specification will be used.")
    prior[[2]][["prior.location"]] <- 0
  } 
  if(is.null(prior[[2]][["prior.scale"]])){
    warning("Prior scale not defined. Default specification will be used.")
    prior[[2]][["prior.scale"]] <- sqrt(2)/2
  }
  
  if(any(!is.element(names(prior[[2]]), c("prior.location", "prior.scale")))){
    warning("Cauchy distribution only takes parameters prior.location and prior.scale.")
  }
  return(prior)
}

# ---------------------------------------------------------------------
# BF.test.function: return log(BF10)


BF.test.i.null.paired <- function(SAMP, alternative = NULL, freq.test = NULL, prior=NULL, null.int=c(-.1,.1)) {
  bfInterval = BayesFactor::ttestBF(SAMP, nullInterval = null.int, rscale = prior[[2]][["prior.scale"]])
  BF = bfInterval[2] / bfInterval[1]
  
  # returns the log(BF10)
  return(BF@bayesFactor$bf)
}