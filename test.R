# Demo Hierarchical Decomposition
member <- list("André", "Johannes", "Laura", "Lilian", 44)


formatName <- function(name){
  paste0("Super", str_to_lower(name))
}

formatmyNumber <- function(number){
  paste("Ok-", number)
}

formatMembers <- function(mlist){

  result <- "Super sind: "
  for (i in 1:length(mlist)){
    entry <- ""
    if(mlist[[i]] %>% is.character()) {
      entry <- formatName(mlist[[i]])
    }
    if (mlist[[i]] %>% is.numeric()) {
      entry <- formatmyNumber(mlist[[i]])
    }
    #cat(entry)
    result <- paste0(result, ", ", entry)
  }
  result
}


formatMembers(member)
