regionalize <- function(from, to) {
  keep <- geos::geos_intersects_matrix(from, to) %>% unlist() %>% sort() %>% unique()
  
  to_sub <- to[keep, ]
  
}