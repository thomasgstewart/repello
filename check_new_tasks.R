#' Compare Checklists
#'
#' This function allows you to compare checklists for new items and completions.
#' @param list1 A checklist for a Trello card.
#' @param list2 A more recent checklist for the same card.
#' @keywords repello
#' @export
#' @examples
#' check_new_tasks(list1, list2)

check_new_tasks <- function(list1, list2){
  changes <- setdiff(list2, list1)
  if (length(changes$item)==0){
    print("No changes made to the card")
  } else {
    update <- c()
    for (i in 1:nrow(changes)){
      if (changes$item[i] %in% list1$item){
        update[i] <- "Status Change"
      } else {
        update[i] <- "New Item"
      }
    }
    changes <- cbind(changes, update)
    changes
  }
}