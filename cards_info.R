#' Get Cards Information
#'
#' This function allows you to obtain the cards and the date of most recent modification for the cards on a specified board.
#' @param user_token The user token for an individual's Trello account.
#' @param board_id The ID of the board you want to browse.
#' @keywords repello
#' @export
#' @examples
#' cards_info(user_token, board_id)
#' 

cards_info <- function(user_token, board_id){
  token <- user_token
  cards_info <- GET(paste0("https://api.trello.com/1/boards/", board_id, "/cards?key=5b771b8595d9fa76ac8724387d9642b4&token=",token))
  cards_content <- content(cards_info)
  num_cards <- length(cards_content)
  
  cards <- c()
  card_id <- c()
  last_modified <- c()
  for (i in 6:num_cards){
    cards[i] <- cards_content[[i]]$name
    card_id[i] <- cards_content[[i]]$id
    last_modified[i] <- unlist(strsplit(cards_content[[i]]$dateLastActivity, "T"))[1]
  }
  trello_activity <- data.frame(cards[6:num_cards], card_id[6:num_cards], last_modified[6:num_cards])
  colnames(trello_activity) <- c("Card", "ID", "Date last modified")
  trello_activity
}

