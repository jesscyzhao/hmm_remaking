student = read.csv('data/students.csv', header=T, sep=',')
swipes = read.csv('data/swipes.csv', header=T, sep=',')

all_id = unique(student$Student)
all_dorms = unique(as.character(student$Fall_2011))
first_dorms = unique(as.character(student$Freshman))
first_dorms = first_dorms[-which(first_dorms == 'N/A')]

# No point changing the id --> need to re-map later anyway 
# Convert all dorms 
fall_2011 = as.character(student$Fall_2011)
coded_fall_2011 = fall_2011
for(i in 1:length(all_dorms)){
  this_dorm = all_dorms[i]
  this_dorm_index = which(all_dorms == this_dorm)
  coded_fall_2011[fall_2011 == this_dorm] = this_dorm_index
}
coded_fall_2011 = as.numeric(coded_fall_2011)

freshman = as.character(student$Freshman)
freshman[freshman == 'N/A'] = 0
coded_freshman = freshman
for (j in 1:length(first_dorms)){
  this_dorm = first_dorms[j]
  this_dorm_index = which(all_dorms == this_dorm)
  coded_freshman[freshman == this_dorm] = this_dorm_index
}
coded_freshman = as.numeric(coded_freshman)

clean_student_df = data.frame(id = student$Student, gender = student$GENDER_CD, class_year = student$CLASS_YR, fall_2011 = coded_fall_2011, fresh_dorm = coded_freshman)

meal_plan = substring(as.character(swipes$meal.plan), 1, 2)
meal_plan_clean = meal_plan
meal_plan_clean[meal_plan == '9M'] = 9
meal_plan_clean[meal_plan == 'AT'] = 0
meal_plan_clean[meal_plan == 'BT'] = 0
meal_plan_clean = as.numeric(meal_plan_clean)

meal_plan_df = data.frame(id = swipes$ID, meal_plan=meal_plan_clean)
meal_plan_df = subset(meal_plan_df, !duplicated(meal_plan_df$id))
student_clean = merge(clean_student_df, meal_plan_df, by='id')

meal_date = as.Date(swipes$date, "%m/%d/%Y")
min_date = min(meal_date)
meal_day = meal_date - min_date

get_time_stamp_in_sec = function(this_time){
  first_split = unlist(strsplit(as.character(this_time), split=' '))
  time_part = unlist(strsplit(first_split[1], split=':'))
  am_pm = first_split[2]
  hour = if (am_pm == 'PM') as.numeric(time_part[1]) + 12 else as.numeric(time_part[1])
  min = as.numeric(time_part[2])
  sec = as.numeric(time_part[3])
  return(hour+min/60+sec/3600)
}

clean_time = unlist(lapply(swipes$time, get_time_stamp_in_sec))

hall = as.character(swipes$hall)
clean_hall = hall
clean_hall[hall == 'MUAero01'] = 1
clean_hall[hall == 'THAero1'] = 2

id = swipes$ID
new_swipe = data.frame(id = as.numeric(id), day = as.numeric(meal_day), time = as.numeric(clean_time), hall = clean_hall, meal = swipes$meal)

## filter out meals other than dinner and other dining halls 
m_th_swipes = new_swipe[which(new_swipe$hall %in% c("1", "2")), ]
dinner_swipes = m_th_swipes[which(m_th_swipes$meal == 'Dinner'), 1:4]

save.image('data/student_dinner_swipes.rdata')


