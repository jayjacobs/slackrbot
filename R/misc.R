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
    response <- paste0(post$user_name, ": ", sample(sammich, 1))
  } else {
    response <- paste0(post$user_name, ": I didn't here the magic word.")
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

#' Convert english text to pirate
#' 
#' taken from the short arrr package at https://github.com/noamross/arrr
#' @import httr utils
#' @param text Yer lubberly words
arrr = function(text) {
  URL = paste0('http://isithackday.com/arrpi.php?text=', URLencode(text))
  out = paste0("_Arrr, ", content(GET(URL), as="text"), "_")
  sub("wench", "lass", out)
}

#' Random compliment
#' 
#' generates and returns a random comment from 
#' http://peoplearenice.blogspot.com/p/compliment-list.html
#' 
#' @param target the target of the compliment
compliment = function(target) {
  paste0(target,": ", sample(compliments, 1))
}

#' Random inspirational quote
#' 
#' generates and returns a random inspirational quote from 
#' http://www.forbes.com/sites/kevinkruse/2013/05/28/inspirational-quotes/
inspirational = function() {
  quote <- sample(nrow(inspire), 1)
  paste0("_", inspire$quote[quote], "_  - ", inspire$who[quote])
}

#' refresh demotivational posters
#' @import rvest
getDemotivate <- function() {
  dem <- html("http://www.despair.com/demotivators.html")
  link <- paste0("http://www.despair.com/", 
                 dem %>% html_nodes(xpath="//div[@class='tilecontents']/a[@class='vaimglink']") %>% 
                   html_attr(name="href"))
  img <- paste0("http://www.despair.com/", 
                dem %>% html_nodes(xpath="//img[@class='tileimg']") %>% html_attr("src"))
  category <- dem %>% html_nodes(xpath="//div/a/h3") %>% html_text()
  saying <- dem %>% html_nodes(xpath="//div[@class='tilecontents']/p") %>% html_text()
  demotivate <- data.frame(category, link, img, saying)
  demotivate <- demotivate[-1, ]
  save(demotivate, file="data/demotivate.rda")
}

#' random demotivational quote
#' @import rjson
demotivated <- function() {
  q <- sample(nrow(demotivate), 1)
  #   out <- list()
  #   out$text <- demotivate$saying[q]
  #   out$fields <- c(list(title=demotivate$category[q], 
  #                        value=demotivate$img[q],
  #                        short=TRUE), NULL)
  #   paste('{ "text" :', quote(demotivate$saying[q])
  #         demotivate$img[q])
  if (runif(1)>0.6) {
    out <- paste0("*<", demotivate$link[q], "|", demotivate$category[q],">*\n",
           "_",demotivate$saying[q],"_")
  } else {
    out <- demotivate$img[q]
  }
  out
}


#' Coin Toss
#' 
#' @param post the request post data
cointoss <- function(post) {
  heads <- runif(1) > 0.5
  cointext <- ifelse(heads, "heads", "tails")
  response <- paste0("Sorry ", post$user_name, ", it came up ", cointext)
  if(grepl("i (call|have|want) (head|tail)", tolower(post$text))) {
    if(grepl("i (call|have|want) head", tolower(post$text)) && heads) {
      response <- paste0("Congrats ", post$user_name, ", it came up ", cointext)
    }
  } else {
    response <- paste0(post$user_name, ": it's ", cointext)
  }
  response
}

