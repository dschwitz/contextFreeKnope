### Production Rules ###

# S -> INIT
# INIT -> NAME + , + _ + you + _ + COMPLIMENT 
# INIT -> You + _ + are + _ + a + _ + COMPLIMENT
# INIT -> You + _ + COMPLIMENT
# INIT -> Oh + , + _ + NAME + . + _ + You + _ + COMPLIMENT
# COMPLIMENT -> ADJ + _ + NOUN + .
# COMPLIMENT -> ADJ + _ + NOUN + . + _ + INIT
# COMPLIMENT -> ADJ + , + _ + COMPLIMENT
# ADJ -> [random adjective]
# NOUN -> [random noun]
# NAME -> [predetermined name]

## IMPLEMENTATION ##

adjectives <- readLines('adjs.txt')
nouns <- readLines('nns.txt')
names <- readLines('nms.txt')

generateCompliment <- function(){

complete <- FALSE
compliment <- c('S')
name = names[sample(1:1347, 1)]

while(complete == FALSE) {
  for(i in (1:length(compliment))) {
    rollOneD20 <- sample(1:20, 1)
    rollTwoD20 <- sample(1:20, 1)
    if(compliment[i] == 'S') {
      compliment[i] <- 'INIT'
    } else if (compliment[i] == 'INIT'){
      if (rollOneD20 < 6){
        compliment[i] <- 'You'
        compliment <- append(compliment, 'COMPLIMENT')
      } else if (rollOneD20 > 5 & rollOneD20 < 11) {
        compliment[i] <- 'Oh'
        compliment <- append(compliment, ',')
        compliment <- append(compliment, 'NAME')
        compliment <- append(compliment, '.')
        compliment <- append(compliment, 'You')
        compliment <- append(compliment, 'COMPLIMENT')
      } else if (rollOneD20 > 10 & rollOneD20 < 16) {
        compliment[i] <- 'You are my'
        compliment <- append(compliment, 'COMPLIMENT')
      } else if (rollOneD20 > 15) {
      compliment[i] <- 'NAME'
      compliment <- append(compliment, ',')
      compliment <- append(compliment, 'you')
      compliment <- append(compliment, 'COMPLIMENT')
      }
    } else if (compliment[i] == 'COMPLIMENT'){
      if (rollTwoD20 < 7) {
      compliment[i] <- 'ADJ'
      compliment <- append(compliment, 'NOUN')
      compliment <- append(compliment, '.')
      } else if (rollTwoD20 > 6 & rollTwoD20 < 16) {
        compliment[i] <- 'ADJ'
        compliment <- append(compliment, ',')
        compliment <- append(compliment, 'COMPLIMENT')
      } else if (rollTwoD20 > 15) {
        compliment[i] <- 'ADJ'
        compliment <- append(compliment, 'NOUN')
        compliment <- append(compliment, '.')
        compliment <- append(compliment, 'INIT')
      }
    } else {
      compliment[i] <- switch(compliment[i], 
                              'NAME' = name, 
                              'ADJ' = adjectives[sample(1:1347, 1)], 
                              'NOUN' = nouns[sample(1:1525, 1)],
                              compliment[i])
    }
  } 
  
  if(!('S' %in% compliment) & !('INIT' %in% compliment)
     & !('COMPLIMENT' %in% compliment) & !('ADJ' %in% compliment)
     & !('NOUN' %in% compliment)) {
    complete <- TRUE
  }
}
compliment <- paste(compliment, collapse = ' ')
compliment <- gsub(' ,', ',', compliment, fixed=TRUE)
compliment <- gsub(' .', '.', compliment, fixed=TRUE)
return(compliment)
}

generateCompliment()