#' Returns a random insult
#' 
#' Given the user to insult (and occasionally the person wanting to insult)
#' This will generate a random insult and return the string.
#' @param user the name of the entity to insult
#' @param from the person asking for the insult
insult <- function(user, from) {
  # https://github.com/akatrevorjay/lucy/blob/master/lib/Acme/Scurvy/Whoreson/BilgeRat/Backend/insultserver.pm
  srcadj <- c("acidic", "antique", "contemptible", "culturally-unsound", "despicable", "evil", "fermented",
              "festering", "foul", "fulminating", "humid", "impure", "inept", "inferior", "industrial",
              "left-over", "low-quality", "malodorous", "off-color", "penguin-molesting",
              "petrified", "pointy-nosed", "salty", "sausage-snorfling", "tastless", "tempestuous",
              "tepid", "tofu-nibbling", "unintelligent", "unoriginal", "uninspiring", "weasel-smelling",
              "wretched", "spam-sucking", "egg-sucking", "decayed", "halfbaked", "infected", "squishy",
              "porous", "pickled", "coughed-up", "thick", "vapid", "hacked-up",
              "unmuzzled", "bawdy", "vain", "lumpish", "churlish", "fobbing", "rank", "craven", "puking",
              "jarring", "fly-bitten", "pox-marked", "fen-sucked", "spongy", "droning", "gleeking", "warped",
              "currish", "milk-livered", "surly", "mammering", "ill-borne", "beef-witted", "tickle-brained",
              "half-faced", "headless", "wayward", "rump-fed", "onion-eyed", "beslubbering", "villainous",
              "lewd-minded", "cockered", "full-gorged", "rude-snouted", "crook-pated", "pribbling",
              "dread-bolted", "fool-born", "puny", "fawning", "sheep-biting", "dankish", "goatish",
              "weather-bitten", "knotty-pated", "malt-wormy", "saucyspleened", "motley-mind",
              "it-fowling", "vassal-willed", "loggerheaded", "clapper-clawed", "frothy", "ruttish",
              "clouted", "common-kissing", "pignutted", "folly-fallen", "plume-plucked", "flap-mouthed",
              "swag-bellied", "dizzy-eyed", "gorbellied", "weedy", "reeky", "measled", "spur-galled", "mangled",
              "impertinent", "bootless", "toad-spotted", "hasty-witted", "horn-beat", "yeasty",
              "imp-bladdereddle-headed", "boil-brained", "tottering", "hedge-born", "hugger-muggered",
              "elf-skinned")
  
  srcamt <- c("accumulation", "bucket", "coagulation", "enema-bucketful", "gob", "half-mouthful",
              "heap", "mass", "mound", "petrification", "pile", "puddle", "stack", "thimbleful", "tongueful",
              "ooze", "quart", "bag", "plate", "trunkfull", "buttload")
  
  srcnoun <- c("bat toenails", "bug spit", "cat hair", "chicken pee", "dog vomit", "dung",
               "stomach-bile", "fish heads", "guano", "gunk", "pond scum", "rat retch",
               "red dye number-9", "git manuals", "waffle-house grits", "yoo-hoo",
               "dog drool", "seagull puke", "cat bladders", "pus", "urine samples",
               "squirrel guts", "snake butts", "snake bait", "buzzard gizzards",
               "cat hairballs", "rat farts", "pods", "armadillo snouts", "entrails",
               "snake snot", "eel ooze", "slurpee backwash", "toxic waste", "Stimpy-drool",
               "craptacular carpet droppings", "cold sores", "warts")
  target <- user
  post <- NULL
  if(runif(1)<0.05) {
    target <- from
    post <- paste0("(", user, " was rubber, you were glue).")
  }
  
  srcsetup <- c(paste(target, "is nothing but"), paste("I heard", target, "is"),
                paste(target, "is"), paste(target, "is the embodiment of"))
  setup <- sample(srcsetup, 1)
  adj <- sample(srcadj, 2)
  amt <- sample(srcamt, 1)
  noun <- sample(srcnoun, 1)
  preposition <- ifelse(grepl("^[aeiou]+", adj[1], perl=T), "an", "a")
  paste(setup, preposition, adj[1], amt, "of", adj[2], noun, post);
}
