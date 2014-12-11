giveA <- function() {
  yesses <- c("I think this time you probably should.", "yeah.", "oh yeah.", "probably should, yeah.")
  noes <- c("Not a one, sir.", "Zero!", "The big donut", "Narry a one")
  if (runif(1) >= .9) { return( sample(yesses, 1))}
  return(sample(noes, 1))
}

howMany <- function() {
  none <- c("zero","0","Zero!", "not a one.", "narry one to be found.")
  if (runif(1) >= .9) {return ("1") }
  return(sample(none, 1))
}