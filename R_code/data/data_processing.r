load('data/student_dinner_swipes.rdata')
### write a function to select a group of student

select_group = clean_student_df$class_year == 2015

original_id = clean_student_df$id[select_group]

### slice this group out of dinner_swipe, put it into list 

all_swipe = dinner_swipes[dinner_swipes$id %in% original_id, ]

all_swipe$new_id = unlist(lapply(all_swipe$id, function(x)which(original_id == x)))

all_days = sort(unique(all_swipe$day))



all_swipe_M = all_swipe[all_swipe$hall=="1", ]



make_swipe_list = function(all_swipe_for_this_hall){
  
  all_days = sort(unique(all_swipe_for_this_hall$day))
  swipe_list = rep(list(1), length(all_days))
  for (i in 1:length(all_days)){
    swipe_for_this_day = all_swipe_for_this_hall[all_swipe_for_this_hall$day == all_days[i], ]
    swipe_list[[i]] = swipe_for_this_day[order(swipe_for_this_day$time), ]$new_id
  }
  
  return(swipe_list)
}

swipe_in_list_M = make_swipe_list(all_swipe_M)

