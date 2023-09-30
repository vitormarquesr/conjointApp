
mod_to_pw <- function(mod){
  attr_labels <- mod$assign
  n <- max(mod$assign)
  
  attr_coeff <- lapply(seq_len(n), 
                       function(k) c(0, unname(mod$coefficients)[attr_labels == k]))
  
  pw <- tibble(
    Attribute = factor(rep(names(mod$xlevels), lapply(mod$xlevels, length))),
    Level = unlist(mod$xlevels),
    PW = unlist(attr_coeff)
  )
  
  return (pw)
}

pw_to_iw <- function(pw){
  pw %>%
    group_by(Attribute) %>%
    summarize(IW = abs(max(PW) - min(PW))) %>%
    mutate(IW = IW/sum(IW))
}




