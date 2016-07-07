#### function to make a list

make_swipe_list_for_this_hall = function(all_swipe_for_this_hall){
  
  all_days = sort(unique(all_swipe_for_this_hall$day))
  swipe_list = rep(list(1), length(all_days))
  for (i in 1:length(all_days)){
    swipe_for_this_day = all_swipe_for_this_hall[all_swipe_for_this_hall$day == all_days[i], ]
    swipe_list[[i]] = swipe_for_this_day[order(swipe_for_this_day$time), ]$new_id
  }
  
  print(cbind(all_days, unlist(lapply(swipe_list, function(x) length(x)))))
  
  return(swipe_list)
}
#### 

load('data/student_dinner_swipes.rdata')

### Change the query here to define original_id group

### Freshmen subset: dorm 15, 17, 23 (53, 58, 67)
select_group = (clean_student_df$class_year == 2015 * clean_student_df$fresh_dorm %in% c(15, 17, 23))
original_id = clean_student_df$id[select_group]

### slice this group out of dinner_swipe, put it into list 
all_swipe = dinner_swipes[dinner_swipes$id %in% original_id, ]
all_swipe$new_id = unlist(lapply(all_swipe$id, function(x)which(original_id == x)-1))
id_map = data.frame(id = all_swipe[!duplicated(all_swipe$id), ]$id, new_id = all_swipe[!duplicated(all_swipe$id), ]$new_id)

save(id_map, file = 'data/fresh_man_dorm_15_17_23_id.rds')

all_swipe_M = all_swipe[all_swipe$hall=="1", ]
swipe_in_list_M = make_swipe_list_for_this_hall(all_swipe_M)
all_swipe_TH = all_swipe[all_swipe$hall=="2", ]
swipe_in_list_TH = make_swipe_list_for_this_hall(all_swipe_TH)

#pick the first 10
final_swipe_list = c(swipe_in_list_M[1:10], swipe_in_list_TH[1:10])
save(final_swipe_list, file = 'data/fresh_man_dorm_15_17_23_data.rds')



