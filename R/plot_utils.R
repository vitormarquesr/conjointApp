
plot_pw <- function(pw){
  
  n_attr <- nlevels(pw$Attribute)
  
  pw %>% 
    ggplot(aes(x=Level, y=PW, group=1))+
      geom_point(size=5)+
      geom_line(linetype=4)+
      facet_wrap(~Attribute, nrow=round(sqrt(n_attr)),
                 scales = "free") + 
      labs(x = "", y = "Part-Worths (PW)") + 
      theme_linedraw() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.95, hjust = 1))
}

plot_iw <- function(iw){
  iw %>%
    ggplot(aes(y=IW, x = Attribute)) +
      geom_col()+
      labs(y = "Importance-Weights (IW)", x = "")+
      theme_linedraw() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.95, hjust = 1))+
      ylim(c(0, 1)) +
      geom_text(aes(y = IW, label = paste0(round(IW*100,2), "%")), vjust = -0.5)
}