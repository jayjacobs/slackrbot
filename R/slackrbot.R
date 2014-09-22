#' Main Looping Function, called for every statement in a channel
#' 
#' This is the main input function for anything in the channel.
#' This is a rook app function.
#' 
#' @param env the Rook environment
#' @export
#' @import httpuv
#' @import Rook
#' @import rjson
frontDoor <- function(env) {
  req <- Request$new(env)
  logsession(req)
  response <- NULL
  # check if it's a post, it's the right path, and the user agent is slackbot.
  if (req$post() & req$path()=="/slack/" & grepl("Slackbot", req$user_agent())) {
    post <- req$POST()
    if (all(c("user_id", "text", "user_name") %in% names(post))) { # check if slack object
      if(!(post$user_id=="USLACKBOT" & post$user_name=="slackbot")) { # not our own msg
        response <- parseRequest(post)
      }
    }
  }
  res <- Response$new()
  if(is.null(response)) {
    res$write("")
  } else {
    res$write(toJSON(list("text"=response)))
  }
  res$finish()
}

#' Main request parsing function
#' 
#' This is where you would add your own hooks and things
#' @param post the post object to handle
parseRequest <- function(post) {
  post <- setupPost(post)
  log(post$text)
  response <- NULL
  if (post$targeted) {
    words <- unlist(strsplit(post$text, " "))
    log(paste(words, collapse="-"))
    if (tolower(words[1]) == "insult") {
      response <- insult(paste(words[2:length(words)], collapse=" "), post$user_name)
    } else if (tolower(words[1]) == "compliment") {
      response <- compliment(paste(words[2:length(words)], collapse=" "))
    } else if (grepl("how (the hell )?(are )?(ya|you)( doin\\'?g?)?\\?*$", post$text, perl=T)) {
      response <- howyou(post)
    } else if (grepl("thank|thanx|thx", post$text)) {
      response <- thanks(post)
    } else if (grepl("^make me a [sammich|sandwich|sandwitch]+", post$text, perl=T)) {
      response <- sammich(post)
    } else if (grepl("excuse", post$text)) {
      response <- paste0(post$user_name, ": _",  sample(bofh, 1), "_")
    } else if ("pirate" %in% names(post) && post$pirate) {
      response <- arrr(post$text)
    } else if (grepl("(flip|toss) a[[:space:]]?[^[:space:]]*[[:space:]]?coin", tolower(post$text))) {
      response <- cointoss(post)
    } else {
      response <- dunno(post)
    }
  }
  response
}

#' look for a message to me, or if I spoke recently
#'
#' @param post the post object
setupPost <- function(post) {
  if(grepl("^artie", post$text, ignore.case=T)) {
    post$targeted <- TRUE
    post$text <- sub("^artie[:, ]+", "", post$text, perl=T, ignore.case=T)
    Sys.setenv(SLACK_LASTMSG=as.numeric(Sys.time()))
    post$recent <- TRUE
  } else if(grepl("^pirate artie", post$text, ignore.case=T)) {
    post$targeted <- TRUE
    post$pirate <- TRUE
    post$text <- sub("^pirate artie[:, ]+", "", post$text, perl=T, ignore.case=T)
    Sys.setenv(SLACK_LASTMSG=as.numeric(Sys.time()))
    post$recent <- TRUE
  } else {
    post$targeted <- FALSE
    post$recent <- (as.numeric(Sys.time())-10) < Sys.getenv("SLACK_LASTMSG")
  }
  post
}


#' Records the request information from the session
#' 
#' Internal function
#' @param req the Rook request object
logsession <- function(req) {
  msg <- paste(req$ip(), req$request_method(), req$url(), 
               req$user_agent(), req$path())
  log(msg)
}

#' Internal logging function
#' 
#' @param msg the message to log
log <- function(msg) {
  now <- as.character(Sys.time())
  src <- match.call()[[1]]
  cat(now, src, msg, "\n")
}

#' List of BOFH excuses
#'
#' Full list of BOFH excuses taken from http://pages.cs.wisc.edu/~ballard/bofh/excuses
#'
#' @docType data
#' @keywords datasets
#' @format text list
#' @name bofh
NULL

#' List of compliments
#'
#' Full list of compliments taken from http://peoplearenice.blogspot.com/p/compliment-list.html
#'
#' @docType data
#' @keywords datasets
#' @format text list
#' @name compliments
NULL

.onLoad <- function(libname, pkgname) {
  Sys.setenv(SLACK_LASTMSG=0)
}

.onUnload <- function(libname, pkgname) {
  Sys.unsetenv("SLACK_LASTMSG")
}