#' All Checklists
#'
#' This function allows you to obtain checklists and card info for all cards on a chosen board.
#' @param board_name The name of the board you want to browse.
#' @param user_token The user token for an individual's Trello account.
#' @keywords repello
#' @export
#' @examples
#' all_checklists(board_name)

all_checklists <- function(board_name, user_token=trello_api_token_08192020){
  board_id <- get_board_id(board_name, user_token)
  activity <- cards_info(board_id, user_token)
  my_checklist <- list()
  for (i in 1:nrow(activity)){
    my_checklist[[activity$ID[i]]] <- c(name=activity$Card[i], ID=activity$ID[i], date=activity$`Date last modified`[i], list=activity$`Trello List`[i], suppressWarnings(get_checklist(board_id, activity$Card[i], user_token)))
  }
  
  filename <- "checklist_date.rds" %>% gsub("date", Sys.Date(),.)
  saveRDS(my_checklist, filename)
  my_checklist
}

