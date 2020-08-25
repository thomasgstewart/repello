#' Check New Tasks
#'
#' This function allows you to compare checklists for new items and completions.
#' @param list1 A checklist for a Trello card.
#' @param list2 A more recent checklist for the same card.
#' @keywords repello
#' @export
#' @examples
#' check_new_tasks(list1, list2)

check_new_tasks <- function(list1, list2){
  changes <- dplyr::setdiff(list2, list1)
  update <- c()
  if (list2$item[1] == "No checklist for this project"){
    changes <- "No List Available"
    changes
  } else if (length(changes$item)==0){
    changes <- "No changes made to the card"
    changes
  } else {
    for (i in 1:nrow(changes)){
      if (changes$item[i] %in% list1$item){
        update[i] <- "Status Change"
      } else {
        update[i] <- "New Item"
      }
    }
    changes <- changes$item
    changes <- data.frame(changes, update)
    changes
  }
}
