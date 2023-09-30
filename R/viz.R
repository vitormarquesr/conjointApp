
viz_pw <- function(data_pw){
  
  n_attr <- nlevels(data_pw$Attribute)
  
  data_pw %>% 
    ggplot(aes(x=Level, y=PW, group=1))+
    geom_point(size=5, color= "lightblue")+
    geom_line(linetype=4, colour="blue")+
    facet_wrap(~Attribute, nrow=round(sqrt(n_attr)),
               scales = "free") + 
    labs(x = "", y = "Part-Worths (PW)") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.95, hjust = 1))
}