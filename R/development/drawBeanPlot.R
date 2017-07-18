drawBeanPlot <- function(data, outcome, rm_out = TRUE){
  
  cohort_group <- c("modality", "proc_primary")
  
  tdt <- data[!is.na(get(outcome)), .(proc_primary, modality, outcome = get(outcome))]
  tdt[, joined_groups := str_c(str_replace_all(proc_primary, " ", "_"), " ", modality)]
  
  # if true, remove outliers from the outcome variable at the cohort level
  if(rm_out){
    data[!is.na(get(outcome)),
         is_outlier := id_outliers(get(outcome), method = "prob", p = .025),
         cohort_group]
    
    data <- data[is_outlier == FALSE]
  }
  
  plot_formula <- new_formula(
    lhs = as_symbol("outcome"),
    rhs = as_symbol("joined_groups")
  )
  
  modals <- str_to_title(tdt[, .N, modality][, modality])
  
  plot.new()
  par(lend = 1, mai = c(0.8, 0.8, 0.5, 0.5))
  names(modals) <- c("x", "y")
  sub_txt <- eval(substitute(paste0("(", y, " vs. ", x, ")")), as.list(modals))
  beanplot(plot_formula,
           data = tdt,
           ll = 0,
           main = bquote(substitute(paste("Outcomes By Procedure Type ", italic(.(sub_txt))))),
           side = "both",
           border = NA,
           col = list("black", "grey"),
           log = "y")
  legend("top",
         x.intersp = 0.5,
         inset = 0,
         xjust = .5,
         horiz = TRUE,
         text.width = .5,
         fill = c("black", "grey"),
         legend = rev(modals))
}
