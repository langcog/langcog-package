if (getRversion() >= "2.15.1")
  utils::globalVariables(c(".", "Mean", "Lower", "Upper"))

#' @importFrom dplyr "%>%"
NULL

#' Nonparemetric bootstrap and empirical central tendency for data frames
#' 
#' Computes CI of bootstrapped mean of univariate data. NOTE: Bootstrapping
#' functions will be computed over the grouping variables currently specified
#' for the data frame.
#' 
#' @param data A data frame.
#' @param col A string indicating the column of \code{data} to bootstrap
#' @param empirical_function Deprecated.
#' @param statistics_functions Deprecated.
#' @param na.rm A logical indicating whether NAs should be dropped before 
#'   bootstrapping (defaults to \code{NULL})
#' @param nboot The number of bootstrap samples to take (defaults to
#'   \code{1000}).
#'   
#' @examples
#' ## Mean and 95% confidence interval for 1000 samples from two different normal distributions
#' require(dplyr)
#' gauss1 <- data_frame(value = rnorm(1000, mean = 0, sd = 1), condition = 1)
#' gauss2 <- data_frame(value = rnorm(1000, mean = 2, sd = 3), condition = 2)
#' df <- bind_rows(gauss1, gauss2) %>% group_by(condition)
#' df %>% multi_boot_standard(col = "value")
#' @export
multi_boot_standard <- function(data, col, na.rm = FALSE,
                                empirical_function,
                                statistics_functions,
                                nboot = 1000) {
  
  quo_col <- rlang::new_quosure(lazyeval::as_name(col))
  
  empirical <- data %>%
    dplyr::summarise(mean = mean(!!quo_col, na.rm = na.rm))
  
  data %>%
    dplyr::do(data.frame(rbind(
      Hmisc::smean.cl.boot(.[[col]], B = nboot, na.rm = na.rm)
    ))) %>%
    dplyr::select(-Mean) %>%
    dplyr::rename(ci_lower = Lower, ci_upper = Upper) %>%
    dplyr::left_join(empirical, by = as.character(dplyr::groups(data)))
  
}
