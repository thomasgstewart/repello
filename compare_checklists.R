#' Compare Checklists
#'
#' This function compares checklists for all items in a list of lists.
#' @param old_list A  prior list of Trello activities.
#' @param new_lists A more recent list of Trello activities.
#' @keywords repello
#' @export
#' @examples
#' compare_checklists(old_list, new_list)

compare_checklists <- function(old_list, new_list){
  old_cards <- c()
  new_cards <- c()
  for (i in 1:length(old_list)){
    old_cards[i] <- old_list[[i]]$ID
  }

  for (j in 1:length(new_list)){
    new_cards[j] <- new_list[[j]]$ID
  }

  new_tasks <- list()
  for (k in 1:length(old_cards)){
    temp <- old_cards[k]
    Match <- match(temp, new_cards)
    if (!is.na(Match)){
      list1 <- data.frame(old_list[[k]][["item"]], old_list[[k]][["status"]])
      colnames(list1) <- c("item", "status")
      list2 <- data.frame(new_list[[Match]][["item"]], new_list[[Match]][["status"]])
      colnames(list2) <- c("item", "status")
      new_tasks[[old_cards[k]]] <- check_new_tasks(list1, list2)
    } else {
      new_tasks[[old_cards[k]]] <- "Card deleted"
    }
  }
  for (m in 1:length(new_cards)){
    temp <- new_cards[m]
    Match <- match(temp, old_cards)
    if (is.na(Match)){
      new_tasks[[new_cards[m]]] <- "New Card"
    }
  }
  new_tasks
}
