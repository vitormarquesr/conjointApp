
model_to_pw <- function(mod){
  attr_labels <- mod$assign
  n <- max(mod$assign)
  
  attr_coeff <- lapply(seq_len(n), 
                       function(k) c(0, unname(mod$coefficients)[attr_labels == k]))
  
  data_pw <- tibble(
    Attribute = factor(rep(names(mod$xlevels), lapply(mod$xlevels, length))),
    Level = unlist(mod$xlevels),
    PW = unlist(attr_coeff)
  )
  
  return (data_pw)
}





