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
multi.boot.vector <- function(x, statistics = c("ci.low", "ci.high"),
                       summary.function = "mean", nboot = 1000) {
  
  formulas <- sapply(statistics, function(x) interp(~fun, fun = x))
  
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
# multi.boot.df <- function(df, col, statistics = c("ci.low","ci.high"),
#                           summary.function = "mean", nboot = 1000) {
#   
#   boot.df <- df %>%
#     do_(boot = ~multi.boot(.[[col]],
#                            statistics = statistics,
#                            summary.function = summary.function,
#                            nboot = nboot))
#   
#   for (fun in statistics) {
#     dots = list(~boot[[fun]])
#     boot.df <- boot.df %>%
#       mutate_(.dots = setNames(dots, c(fun)))
#   }
#   
#   boot.df %>%
#     select_("-boot")
# 
# }
# 
# 



# gauss.1 <- data.frame(value = rnorm(1000, mean = 0, sd = 1),
#                      condition = 1)
# gauss.2 <- data.frame(value = rnorm(1000, mean = 2, sd = 3),
#                      condition = 2)

#estimates <- multi.boot.df(df, "value", statistics = c("ci.low", "mean", "ci.high"))

# multi.boot(df = bind_rows(gauss.1, gauss.2),
#            summary.function = function(df) summarise(df, mean = mean(value)),
#            summary.groups = c("condition"),
#            statistics = function(df) summarise_each(df,
#                                                     funs("ci.upper", "mean", "ci.lower"), 
#                                                     mean),
#            #statistics.groups = NULL,
#            nboot = 100, replace = T)


multi_boot.numeric <- function(x, statistics = c("ci.low", "ci.high"),
                               summary.function = "mean", nboot = 1000, replace = TRUE) {
  
  formulas <- sapply(statistics, function(x) interp(~fun, fun = x))
  
  one.sample <- function() {
    do.call(summary.function, list(sample(x, replace = replace)))
  }
  
  all.samples <- data.frame(sample = replicate(nboot, one.sample())) %>%
    summarise_each(funs_(formulas), sample)
  
  if(length(formulas) == 1) {
    all.samples <- all.samples %>%
      rename_(.dots = setNames("sample", statistics))
  }
  
  all.samples
  
}


one_sample <- function(df, summary_function, summary_groups, replace) {
  function(k) {
    if (!is.null(summary_groups)) {
      df %<>%
        group_by_(.dots = summary_groups)
    }
    df %>%
      sample_frac(replace = replace) %>%
      summary_function() %>%
      mutate(sample = k)
  }
}

multi_boot.data.frame <- function(data, summary_function = "mean", column = NULL, summary_groups = NULL,
                                  statistics_functions, statistics_groups = summary_groups, 
                                  nboot = 1000, replace = TRUE) {

  assert_that(typeof(summary_function) %in% c("closure","character"))
  assert_that(typeof(statistics_functions) %in% c("closure","character"))
  assert_that(all(statistics_groups %in% summary_groups))

  if(typeof(summary_function) == "closure"){ # function
    call_summary_function <- summary_function
  } else { # string
    assert_that(!is.null(column))
    summary_dots <- list(interp(~fun(arg), fun = as.name(summary_function), arg = as.name(column)))
    call_summary_function <- function(df) summarise_(df, .dots = setNames(summary_dots, "summary"))
  }
  
  if(typeof(statistics_functions) == "closure"){ # function
    call_statistics_functions <- statistics_functions
  } else { # string
    statistics_formulas <- sapply(statistics_functions, function(x) interp(~fun, fun = x))
    call_statistics_functions <- function(df) summarise_each(df, funs_(statistics_formulas), summary)
  }
  
  all_samples <- sapply(1:nboot, one_sample(df, call_summary_function, summary_groups, replace),
                        simplify = FALSE) %>%
    bind_rows()
  
  if (!is.null(statistics_groups)) {
    all_samples %<>% group_by_(.dots = statistics_groups)
  }
  
  booted_vals <- all_samples %>% call_statistics_functions()

  if(typeof(statistics_functions) == "character" & length(statistics_functions) == 1) {
    booted_vals %<>% rename_(.dots = setNames("summary", statistics_functions))
  }
  
  return(booted_vals)
}

multi_boot <- function(data, summary_function, column, summary_groups,
                       statistics_functions, statistics_groups, nboot, replace) UseMethod("multi_boot")

multi_boot(data = bind_rows(gauss.1, gauss.2),
           summary_function = "mean",
           column = "value",
           summary_groups = c("condition"),
           statistics_functions = c("ci.upper"),
           nboot = 2, replace = T)

multi_boot(data = bind_rows(gauss.1, gauss.2),
           summary_function = function(df) summarise(df, mean = mean(value)),
           summary_groups = c("condition"),
           statistics_functions = function(df) summarise_each(df,
                                                    funs("ci.upper", "mean", "ci.lower"), 
                                                    mean),
           #statistics.groups = NULL,
           nboot = 100, replace = T)