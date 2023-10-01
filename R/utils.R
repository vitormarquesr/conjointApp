
lm_part_worths <- function(m){
  tibble(feature = rep(names(m$xlevels), lapply(m$xlevels, length)),
         level = unname(unlist(m$xlevels)),
         pw = unlist(lapply(seq_len(max(m$assign)), 
                            function(x) c(0, m$coefficients[m$assign == x]))))
}

