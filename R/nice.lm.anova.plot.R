#' Plot nice LM plots
#'
#' This function plots descriptive, diagnostic and the results of anova on linear model. If a string of formula is provided formula instead of a single variable (i.e., "rain + heat" instead of  "heat"), the last element is plotted, although lm is done with the string formula. This function also return the fitted model and the anova resutls. Elements are in a list.
#' 
#' @keywords
#' ggplot, nice plot, anova, linear model, lm
#' @export
#' 
#'@usage
#' df <- data.frame
#' y <- string with the name of the y element in the formula. Column of data.
#' x <- string with the name of the x element in the formula. Column of data or string with the formula of the linear model.
#' @examples
#' nice.lm.anova.out(alpha, "S", "Intervention.timepoint")


nice.lm.anova.plot <- function (data, y, x){

  
  require(ggplot2)
  require(ggpubr)
  require(gridExtra)
  require(stringr)
  require(dplyr)
  
  # Get the last variable of the formula   
  reg <- "\\+| \\+ |\\*| \\* |\\:| \\: "  
  if(str_detect(x, reg)){
    x.plot <- str_split(x, reg)[[1]]
    x.plot <- tail(x.plot, n=1)
  } else {x.plot <- x}  
  
  # plot raw data
  if(is.numeric(data[,x.plot])){
    descr <- ggplot(data, aes_string(x = x.plot , y = y)) + 
      geom_point() +
      geom_smooth()
  }else{
  descr <- ggplot(data, aes_string(x = x.plot , y = y)) + 
    geom_boxplot()
  }
  
  # linear model
  f <- paste(y, "~", x)
  f <- as.formula (f)
  
  l <- lm(f, data = data)
  
  # plot residuals vs fit
  res.fit.plot<- augment(l) %>% 
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
    select(Df, F.value, Pr..F.) %>% 
    round(3)
  
  # Abbreviate long names
  rownames(tab.p) <- abbreviate(rownames(tab.p), minlength = 12)
  
  tab.p <- tableGrob(tab.p)
  
  p <- ggarrange(descr, tab, qqplot, res.fit.plot)
  return(list(p, f, l, res.fit.plot, tab))
}