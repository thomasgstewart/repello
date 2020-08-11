#' Get Checklist
#'
#' This function allows you to obtain a checklist for a card of interest.
#' @param user_token The user token for an individual's Trello account.
#' @param board_id The ID of the board you want to browse.
#' @param card_name The name of the card you want to view
#' @keywords repello
#' @export
#' @examples
#' get_checklist(user_token, board_id, card_name)

get_checklist <- function(user_token, board_id, card_name){
  activity <- cards_info(user_token, board_id)
  ID <- activity[which(activity$Card==card_name),]$ID
  checklist_url <- paste0("https://api.trello.com/1/cards/", ID, "/checklists?key=5b771b8595d9fa76ac8724387d9642b4&token=", user_token)
  checklist_info <- GET(checklist_url)
  checklist_content <- content(checklist_info)
  if (length(checklist_content)==0){
    print("No checklist for this project")
    #checklist <- data.frame(0,0)
  } else {
    list_length <- length(checklist_content[[1]]$checkItems)
    item <- c()
    status <- c()
    for (i in 1:list_length){
      item[i] <- (checklist_content[[1]]$checkItems)[[i]]$name
      status[i] <- (checklist_content[[1]]$checkItems)[[i]]$state
    }
    checklist <- data.frame(item, status)
    checklist
  }
}

