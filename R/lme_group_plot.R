#' Prepare data for plot
#'
#' Prepare data for plotting mean and confidence intervals for different fixed effects groups of a lme4 model. 
#' The function takes into consideration the influence of any random effects structure. 
#' @usage lme_group_plot(outcome, fixed, random, data)
#' @param outcome outcome as specified in a lmer() model
#' @param fixed fixed effects as specified in a lmer() model. Has to be a factor.
#' @param random random effects expression as specified in a lmer() model.
#' @param data a data frame or environment containing the variables for the lmer() model.
#' @return NULL 
#' @author Žiga Puklavec, \email{ziga.puklavec@@univie.ac.at}
#' @author Jerome Olsen, \email{olsen@@coll.mpg.de}
#' @examples
#' plot_data <- lme_group_plot(outcome = "var1", 
#'                             fixed = c("var2", "var3", "var4"), 
#'                             random = "(1 | CASE)", 
#'                             data = DF)
#' @import lmerTest
#' @import tibble
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import magrittr
#' @export lme_group_plot
lme_group_plot <- function(
  outcome = NULL,
  fixed = NULL,
  random = NULL,
  data = NULL
){
  
  if (missing(fixed)) {
    stop("Please specify a factor (or multiple factors) for the argument 'fixed'.", 
         call. = FALSE)
  }
  
  if (missing(random)) {
    stop("Please specify a expression (as in a lmer() formula) for the argument 'random'.", 
         call. = FALSE)
  }
  
  if (missing(outcome) | is.numeric(outcome)) {
    stop("Please specify a numeric vector for the argument 'outcome'.", 
         call. = FALSE)
  }
  
  if (missing(data)) {
    stop("Please specify a data frame or environment containing the variables for the lmer() model.", 
         call. = FALSE)
  }
  
  if (all(!fixed %in% names(data)[sapply(data, is.factor)])) {
    stop("Variables defined under argument 'fixed' have to be factors", 
         call. = FALSE)
  }
  
  fixed_effects <- paste0(fixed, collapse = " * ")
  random_effects <- random
  formula <- as.formula(paste(outcome, "~", fixed_effects, "+", random_effects))
  
  n_levels <- data[paste(fixed)]
  n_levels <- lapply(n_levels, function(x) levels(x))
  n_levels <- prod(lengths(n_levels))

  model <- lmerTest::lmer(formula, data = data)

  output <- rownames_to_column(ls_means(model), var = "condition_name") %>%
  as_tibble() %>% 
  top_n(n_levels, row_number()) %>% 
  mutate(condition_name = str_remove_all(condition_name, paste(fixed, collapse = "|"))) %>% 
  separate(condition_name, paste(fixed), sep = ":") %>% 
  dplyr::select(paste(fixed), Estimate, lower, upper) %>% 
  rename(estimate = Estimate,
         lower_ci = lower,
         upper_ci = upper) %>% 
  mutate_at(paste(fixed), factor)

  return(output)
  
}
