#' Multiple ways to respond to a thank you.
#'
#' @param post the request post data
thanks <- function(post) {
  welcome <- c("your welcome", "da nada", "Anytime!", "'twas nothing", "My pleasure!",
               "Anything for my good friend, $name.", paste0("I like helping you, ", post$user_name, "."))
  sample(welcome, 1)
}

#' Multiple ways to respond to a "how are you" question.
#'
#' @param post the request post data
howyou <- function(post) {
  howAreYa = c("just great", "peachy", "mas o menos",
               "you know how it is", "eh, ok", "pretty good. how about you");
  sample(howAreYa, 1)
}

#' make me a sammich
#' 
#' @param post the request post data
sammich <- function(post) {
  if (grepl("sudo", post$text)) {
    # should return random google image of a sandwich
    sammich <- c("aw man, okay.", "ask later, I'm busy.",
                 "would you like some whine with your sammich?",
                 "with a pickle on the side?", "Ham or Turkey?")
    response <- paste0(post$name, ": ", sample(sammich, 1))
  } else {
    response <- paste0(post$name, ": I didn't here the magic word.")
  }
  response
}

#' Not sure how to respond
#' 
#' @param post the request post data
dunno <- function(post) {
  dunnos <- c("I think you're mumbling", "Magic 8-ball says, _\"Huh?\"_",
              "I hear ya, but I'm not listening.", "You really should stop mumbling",
              paste(weekdays(as.Date(Sys.Date())), "is my day off."),
              "you should type slower, maybe I'll understand you")
  paste0(post$user_name,": ", sample(dunnos, 1))
}