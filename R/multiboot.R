#' Non-parametric bootstrap with multiple sample statistics
#' 
#' Computes arbitrary bootstrap statistics on univariate data. By default, 
#' computes a 95\% confidence interval of the mean of the data.
#' 
#' @param x A vector of data to bootstrap over.
#' @param summary.function A string that is the name of a function to be 
#'   computed over each set of samples. This function needs to take a vector and
#'   return a single number (e.g. \code{mean}, \code{median}).
#' @param statistics A vector of strings that are names of functions to be 
#'   computed over the set of summary values from all samples (e.g. \code{sd},
#'   \code{ci.high}, \code{ci.low}).
#' @param nboot The number of bootstrap samples to take.
#'   
#' @examples
#' ## Mean and 95% Confidence Interval for 1000 Samples from a Standard Normal
#' x <- rnorm(1000, mean = 0, sd = 1)
#' ci.low <- function(x) {quantile(x, 0.025)}
#' ci.high <- function(x) {quantile(x, 0.975)}
#' estimates <- multi.boot(x, statistics = c("ci.low", "mean", "ci.high"))
multi.boot <- function(x, statistics = c("ci.low", "ci.high"),
                       summary.function = "mean", nboot = 1000) {
  
  formulas <- sapply(statistics, function(x) {as.formula(paste0("~", x))})
  
  one.sample <- function() {
    do.call(summary.function, list(sample(x, replace = TRUE)))
  }
  
  all.samples <- data.frame(sample = replicate(nboot, one.sample())) %>%
    summarise_each(funs_(formulas), sample)
  
  if(length(formulas) == 1) {
    all.samples <- all.samples %>%
      rename_(.dots = setNames("sample", statistics))
  }
  
  all.samples
  
}

#' Non-parametric bootstrap with multiple sample statistics on dataframes
#' 
#' A wrapper for \code{multi.boot} for use with dataframes. Calls 
#' \code{multi.boot} for a specified column and returns all statistics as their 
#' own columns, while presevering \code{dplyr} grouping structure.
#' 
#' @param df A data frame containg the data to bootstrap.
#' @param col A string indicating the column to bootstrap.
#' @inheritParams multi.boot
#'   
#' @examples
#' ## Mean and 95% Confidence Interval for 1000 Samples from a Standard Normal
#' library(dplyr)
#' gauss.1 <- data.frame(value = rnorm(1000, mean = 0, sd = 1),
#'                      condition = 1)
#' gauss.2 <- data.frame(value = rnorm(1000, mean = 2, sd = 3),
#'                      condition = 2)
#' df <- bind_rows(gauss.1, gauss.2) %>% group_by(condition)
#' ci.low <- function(x) {quantile(x, 0.025)}
#' ci.high <- function(x) {quantile(x, 0.975)}
#' estimates <- multi.boot.df(df, "value", statistics = c("ci.low", "mean", "ci.high"))
multi.boot.df <- function(df, col, statistics = c("ci.low","ci.high"),
                          summary.function = "mean", nboot = 1000) {
  
  boot.df <- df %>%
    do_(boot = ~multi.boot(.[[col]],
                           statistics = statistics,
                           summary.function = summary.function,
                           nboot = nboot))
  
  for (fun in statistics) {
    dots = list(~boot[[fun]])
    boot.df <- boot.df %>%
      mutate_(.dots = setNames(dots, c(fun)))
  }
  
  boot.df %>%
    select_("-boot")

}


one.sample <- function(df, summary.function, summary.groups, replace) {
  function(k) {
    if (!is.null(summary.groups)) {
      df %<>%
        group_by_(.dots = summary.groups)
    }
    df %>%
      sample_frac(replace = replace) %>%
      summary.function() %>%
      mutate(sample = k)
  }
}


multi.boot <- function(df, summary.function, summary.groups = NULL,
                       statistics, statistics.groups = summary.groups,
                       nboot = 1000, replace = TRUE) {
  all.samples <- map(1:nboot, one.sample(df, summary.function, summary.groups, replace)) %>%
    bind_rows()
  
  if (!is.null(statistics.groups)) {
    all.samples %<>% group_by_(.dots = statistics.groups)
  }
  
    all.samples %>% statistics()
}


gauss.1 <- data.frame(value = rnorm(1000, mean = 0, sd = 1),
                     condition = 1)
gauss.2 <- data.frame(value = rnorm(1000, mean = 2, sd = 3),
                     condition = 2)
estimates <- multi.boot.df(df, "value", statistics = c("ci.low", "mean", "ci.high"))

multi.boot(df = bind_rows(gauss.1, gauss.2),
           summary.function = function(df) summarise(df, mean = mean(value)),
           summary.groups = c("condition"),
           statistics = function(df) summarise_each(df,
                                                    funs("ci.upper", "mean", "ci.lower"), 
                                                    mean),
           #statistics.groups = NULL,
           nboot = 100, replace = T)

multi.boot.wrapper <- function(df, summary.function.name, col, summary.groups = NULL,
                               statistics.names, statistics.groups = NULL, nboot, replace) {
  dots = list(~boot[[fun]])
  boot.df <- boot.df %>%
    mutate_(.dots = setNames(dots, c(fun)))
  
  dots = list(~boot[[fun]])
  summary.function <- function(df) summarise_(df, .dots = setNames(dots, c(fun)))
  
  statistics <- 
  multi.boot(df, summary.function, summary.groups,
             statistics, statistics.groups = summary.groups,
             nboot, replace)
}