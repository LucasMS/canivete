#' Plot nice LM plots
#'
#' This function plots descriptive, diagnostic and the results of anova on linear model. If a string of formula is provided formula instead of a single variable (i.e., "rain + heat" instead of  "heat"), the last element is plotted, although lm is done with the string formula. This function also return the fitted model and the anova resutls. Elements are in a list.
#' 
#' @keywords
#' ggplot, nice plot, anova, linear model, lm
#' @export
#' @details 
#' At the moment it does not support functions with logs or transformations in the formula. transform the column in the dataframe, instead.
#'@usage
#' It returns a list, where the first element is a ggplot/ggarrange.
#' df <- data.frame
#' formula <- formula of the model. if string, use as.formula(string)
#' 
#' @examples
#' #get results
#' data(mtcars)
#' f <- as.formula("mpg ~ cyl")
#' res <- nice.lm.anova.res(formula = f, data = mtcars)
#' res$plot


nice.lm.anova.res <- function (formula, data){

  require(ggplot2)
  require(ggpubr)
  require(gridExtra)
  require(stringr)
  require(broom)
  require(formula.tools)
  require(dplyr)
  
  # Get elements
  f <- formula
  f.string <- as.character(f)
  f.string <- gsub(" ", "", f.string)
  y = str_split(f.string, "~")[[1]][1]
  x = str_split(f.string, "~")[[1]][2]
  
  # Get the last variable of the formula   
  reg <- "\\+|\\*|\\:"  
  if(str_detect(x, reg)){
    x.plot <- str_split(x, reg)[[1]]
    x.plot <- tail(x.plot, n=1)
  } else {x.plot <- x}  
  
  # plot raw data
  if(is.numeric(data[,x.plot])){
    descr <- ggplot(data, aes_string(x = x.plot , y = y)) + 
      geom_point() +
      geom_smooth(method = "lm")
  }else{
  descr <- ggplot(data, aes_string(x = x.plot , y = y)) + 
    geom_boxplot()
  }
  
  # linear model
  l <- lm(f, data = data)
  
  # plot residuals vs fit
  res.fit.plot <- augment(l) %>% 
    ggplot(aes_string(x = ".fitted", y = ".resid", colour = x.plot)) +
    geom_point(alpha = 0.4, size = 5) + 
    geom_hline(yintercept=0,
               linetype="dashed", 
               color = "red",
               size = 1) +
    theme(legend.position="bottom")
  
  # qq plot  
  qqplot <- data %>% 
    ggplot(aes_string(sample = y)) +
    stat_qq(alpha = 0.8, size = 3) +
    stat_qq_line()
  
  # Anova test 
  tab <- anova(l)
  tab.p <- tab %>% 
    data.frame() %>%
    dplyr::select(Df, F.value, Pr..F.) %>% 
    round(3)
  
  # Abbreviate long names
  rownames(tab.p) <- abbreviate(rownames(tab.p), minlength = 12)
  
  tab.p <- tableGrob(tab.p)
  
  p <- ggarrange(descr, tab.p, qqplot, res.fit.plot)
  
  results <- list(p, f, l, tab)
  names(results) <- c("plot", "formula", "model", "anova")
  return(results)
}