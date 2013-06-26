chooseByPriority <- function(x, choice_order){
	# Choose the lowest ocurring member of x according to 
	# choice_order
	choice_order[min(which(choice_order %in% x))]
}