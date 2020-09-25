#' Get Board ID
#'
#' This function allows you to obtain the ID of the Trello board you want to view.
#' @param board_name The name of the board you want to browse.
#' @param user_token The user token for an individual's Trello account.
#' @keywords repello
#' @export
#' @examples
#' get_board_id(board_name)

get_board_id <- function(board_name, user_token=trello_api_token_08192020){
  token <- user_token
  boards_info <- GET(paste0("https://api.trello.com/1/members/me/boards?fields=name,url&key=5b771b8595d9fa76ac8724387d9642b4&token=", token))
  boards_content <- content(boards_info)
  boardnames <- data.frame()
  for (i in 1:length(boards_content)){
    temp <- data.frame(boards_content[[i]])
    boardnames <- rbind(boardnames, temp)
  }
  target_board <- boardnames[which(boardnames$name==board_name),]$id
  target_board
}
