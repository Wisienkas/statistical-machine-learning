
# Function given
#   - Training data 
# Returns
#   - Input data after performing the Z-Score normalization

z_score_normalize <- function(data) {
  return (scale(as.data.frame(data)));
}

min_max_normalize <- function(data) {
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }
  
  return (normalize(as.data.frame(data)));
}

#Testing
#min_max_normalize(c(1, 2, 3, 4, 5))
#min_max_normalize(c(10, 20, 30, 40, 50))

#z_score_normalize(c(1, 2, 3, 4, 5))
#z_score_normalize(c(10, 20, 30, 40, 50))