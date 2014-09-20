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
  logsession(env)
  body <- NULL
  if (req$post() & req$path()=="/slack") {
    zipit <- req$POST()
    if (!is.null(zipit$user_name)) {
      if (zipit$user_name == "jayjacobs") {
        response <- paste0(zipit$user_name, ": zip it.")
        body <- toJSON(list("text"=response))
      }
    }
  }
  
  # $token <- "d9W84OJWBaTUOutg99701Oal"
  # $team_id <- "T02J411CY"
  # $team_domain <- "cyberlab"
  # $service_id <- "2689161008"
  # $channel_id <- "C02J4F7LL"
  # $channel_name <- "code"
  # $timestamp <- "1411175198.000544"
  # $user_id <- "U02J36J49"
  # $user_name <- "jayjacobs"
  # $text <- "test"
  list(
    status = 200L,
    headers = list(
      'Content-Type' = 'text/html'
    ),
    body = body
  )
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