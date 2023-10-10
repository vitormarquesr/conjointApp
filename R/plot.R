
plot_pworths <- function(data){
    ggplot(data, aes(x=level, y=pw, group=1))+
    geom_point(size=5)+
    geom_line(linetype=4)+
    facet_wrap(~feature,
               scales = "free") + 
    labs(x = "", y = "Part-Worths (PW)") +
    theme_linedraw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.95, hjust = 1))
}

plot_iweights <- function(data){
  ggplot(data, aes(y=iw, x = feature)) +
    geom_col()+
    labs(x = "", y = "Importance-Weights (IW)") +
    ylim(c(0, 1)) +
    geom_text(aes(y = iw, label = paste0(round(iw*100,2), "%")),
              vjust = -0.5) +
    theme_linedraw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.95, hjust = 1))
}

plot_prediction <- function(mdl, X, y_min, y_max){
  tibble(y_fitted = predict(mdl, X),
         rating = "") %>%
    ggplot(aes(x=rating, y=y_fitted))+
    geom_col()+
    coord_cartesian(ylim=c(y_min, y_max))+
    labs(x = "", y = "", title = "Predicted Rating")+
    geom_text(aes(y = y_fitted, label = round(y_fitted,2)),
              vjust = -0.5)+
    theme_linedraw()
  
}