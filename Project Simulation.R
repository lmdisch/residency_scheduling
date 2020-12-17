# imports ======================================================================
library(timeDate)
library(readxl)
library(dplyr)
library(lubridate)

# create increment function ====================================================
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))

# setup initial dataframe and information ======================================
# data integrity of input excel is assumed
# create range of dates to schedule
sim_start <- as.Date("2021/1/1")
sim_end <- as.Date("2021/12/31")
sim_days <- data.frame(date = seq.Date(sim_start, sim_end, by = "day"))
# create a list of holidays in the year, Good Friday isn't included
holidays <- data.frame(date = as.Date(holidayNYSE(year(sim_start))@Data), holiday = TRUE)
holidays <- holidays[-4,]
# points assigned to each day
point_data <- read_excel("hospital_data.xlsx", sheet = "PointSystem")
# resident names and the dates that they need off
resident_data <- read_excel("hospital_data.xlsx", sheet = "Residents")
week_off_col_titles <- c("Week_Off_1", "Week_Off_2", "Week_Off_3", "Week_Off_4")
# maj_holiday_col_title <- "Major_Holiday"
# min_holiday_col_title <- "Minor_Holiday"

# combine date, holiday, and weekday into one dataframe
data <- left_join(sim_days, holidays, by = "date")
data[is.na(data)] <- FALSE
data$weekday <- wday(data$date)
data[data["holiday"] == TRUE, "weekday"] <- 0

# calculate points for each slot of each day
for (i in 1:8){
  column_name <- paste("point_", toString(i), sep = "")
  data <- left_join(data, point_data[point_data["Call #"] == i, c("Week Day", "Points")], by = c("weekday" = "Week Day"))
  names(data)[names(data) == "Points"] = column_name
}

# create 8 on-call slots for each date 
for (i in 1:8){
  column_name <- paste("on_call_", toString(i), sep = "")
  data[column_name] <- NA
}

# settle holidays ==============================================================

# function to calculate number of duplicated dates requested
count_duplicate_requests <- function(data1, date_columns){
  aggregated_list <- pull(data1[date_columns[1]])
  for (i in 2:length(date_columns)){
    aggregated_list <- c(aggregated_list, pull(data1[date_columns[i]]))
  }
  return(table(aggregated_list))
}

# function to swap a date to a closest new date, given a list of date constraints
swap_date_holiday <- function(date, constraints){
  # check forward first
  date_temp = date + days(7)
  while (date_temp %in% constraints && date_temp < sim_end){
    date_temp = date_temp + days(7)
  }
  if (date_temp %in% constraints){
    # check backward second
    date_temp = date - days(7)
    while (date_temp %in% constraints && date_temp > sim_start){
      date_temp = date_temp - days(7)
    }
    if (date_temp %in% constraints){
      # stop the program if all fails - this won't happen with 24 residents
      stop("Too many residents to satisfy the 3 day constraint!")
    } else {
      return(date_temp)
    }
  } else {
    return(date_temp)
  }
}

# function to recursively move requests such that duplicates are reduced to below 3
# during every search, we start at the resident with the highest utility score
# conflicting weeks are moved to the closest non-conflicting week possible
# closest weeks are determined by pushing dates further into the future
# until it is past the year being examined. If so, dates are pushed backwards.
# If pushing backwards also cannot find a solution, then no optimal solutions can be
# found for this resident (this is impossible given current number of residents)
# not being able to take a holiday at the right time has a utility of -1
# in real life application, this can be negotiated with the resident
fix_week_off <- function(data2, week_off_checker){
  # sort residents based on their utility scores
  data2 <- data2[order(data2['day_off_score'], decreasing = TRUE),]
  if (max(week_off_checker) <= 3){
    # base case if no more than 3 duplicated requests for each date
    return(data2)
  } else {
    # otherwise, swap 1 person away from the overloaded day
    # set while loop parameters, loop until a success swap is found
    swap_success <- FALSE
    switch_row <- 1
    total_length <- nrow(data2)
    while (switch_row <= total_length && swap_success == FALSE){
      # identify the first overloaded day to swap
      week_off_checker_1stItem <- as.Date.character(names(week_off_checker[week_off_checker > 3][1]))
      # obtain the weeks off requested for the current resident
      weeks_off <- data2[switch_row, week_off_col_titles]
      # check if current resident requested the overloaded day
      # if so, swap that day with another day for the current resident
      for (i in 1:length(weeks_off)){
        if (week_off_checker_1stItem == pull(weeks_off[i])){
          # days already with 3 or more requests cannot be swapped to
          date_constraints <- as.Date.character(names(week_off_checker[week_off_checker > 2]))
          # days the current resident already requested cannot be swapped to
          for (j in weeks_off){
            date_constraints <- c(date_constraints, j)
          }
          date_constraints <- unique(date_constraints)
          # swap date and update score
          temp <- swap_date_holiday(week_off_checker_1stItem, date_constraints)
          data2[switch_row, week_off_col_titles[i]] <- temp
          data2[switch_row, 'day_off_score'] <- data2[switch_row, 'day_off_score'] - 1
          # update week_off_checker to prepare for next iteration
          item_swapped_from <- as.character(week_off_checker_1stItem)
          week_off_checker[item_swapped_from] = week_off_checker[item_swapped_from] - 1
          item_swapped_to <- as.character(temp)
          if (item_swapped_to %in% names(week_off_checker)){
            week_off_checker[item_swapped_to] = week_off_checker[item_swapped_to] + 1
          }
          swap_success <- TRUE
        }
      }
      # update while loop parameters
      switch_row <- switch_row + 1
    }
    # iterative call
    return(fix_week_off(data2, week_off_checker))
  }
}

# randomize order of rows then set day_off_scores to 0
resident_data["day_off_score"] <- sample.int(1000, nrow(resident_data))
resident_data <- resident_data[order(resident_data['day_off_score'], decreasing = TRUE),]
resident_data["day_off_score"] <- 0

week_off_checker <- count_duplicate_requests(resident_data, week_off_col_titles)
resident_data <- fix_week_off(resident_data, week_off_checker)

# randomly slot 8 residents into each day ======================================
# function to swap on-call teammate if they are on vacation
on_call_team_vacay_check <- function(date, team, residents_not_on_call){
  # remove residents that have vacation on this date
  residents_to_remove <- c()
  for (i in 1:length(residents_not_on_call)){
    vacay_days <- resident_data[resident_data['Lookup'] == residents_not_on_call[i], week_off_col_titles]
    for (j in vacay_days){
      if (date %in% seq.Date(as.Date(j), as.Date(j) + days(6), by = "day")){
        residents_to_remove <- c(residents_to_remove, i)
      }
    }
  }
  if (!is.null(residents_to_remove)){
    residents_not_on_call <- residents_not_on_call[-residents_to_remove]
  }
  # if resident on vacation, swap resident out with someone not on vacation
  for (i in 1:length(team)){
    vacay_days <- resident_data[resident_data['Lookup'] == team[i], week_off_col_titles]
    for (j in vacay_days){
      if (date %in% seq.Date(as.Date(j), as.Date(j) + days(6), by = "day")){
        team[i] <- residents_not_on_call[1]
        residents_not_on_call <- residents_not_on_call[-1]
      }
    }
  }
  return(team)
}

for (i in 1:nrow(data)){
  # pull 8 members at random
  on_call_team <- pull(resident_data[sample(nrow(resident_data), 8), 1])
  # update team to avoid those on vacation
  remaining_team <- setdiff(pull(resident_data['Lookup']), on_call_team)
  on_call_team <- on_call_team_vacay_check(as.Date(data[i, 'date']), on_call_team, remaining_team)
  # only 4 slots needed if weekend or holiday
  if (data[i,"holiday"] == TRUE | data[i,"weekday"] %in% c(7, 1)){
    # constraints for Sunday
    if (data[i, "weekday"] == 1 && i > 1 && data[i-1, "holiday"] == FALSE && data[i, "holiday"] == FALSE){
      # constraint 1: if you are a 4 on a Saturday, you must also be 4 on Sunday
      # set temp as person 4 on Saturday
      temp = data[i-1, "on_call_4"]
      if (temp %in% on_call_team){
        if (temp != on_call_team[4]){
          # if temp is in the Sunday team but not on position 4, swap to position 4
          on_call_team[match(temp, on_call_team)] = on_call_team[4]
          on_call_team[4] = temp
        }
      } else {
        # if temp is not in the Sunday team, set position 4 as temp
        on_call_team[4] = temp
      }
      # constraint 2: if you are a 3 on a Saturday, you will not be 3 on Sunday
      # swap with 5 (arbitrary) if 3 on Sat and Sun are the same
      if (data[i-1, "on_call_3"] == on_call_team[3]){
        temp = on_call_team[3]
        on_call_team[3] = on_call_team[5]
        on_call_team[5] = temp
      }
    }
    data[i,12:15] = on_call_team[1:4]
  } else {
    # constraints for weekdays
    if (data[i, "weekday"] %in% c(3:6) && i > 1 && data[i-1, "holiday"] == FALSE && data[i, "holiday"] == FALSE){
      # constraint 1: if you are number 4, you must be number 1 the next day
      # set temp as number 4 from the previous day
      temp = data[i-1, "on_call_4"]
      if (temp %in% on_call_team){
        if (temp != on_call_team[1]){
          # if temp is in the current day team but not on position 1, swap to 1
          on_call_team[match(temp, on_call_team)] = on_call_team[1]
          on_call_team[1] = temp
        }
      } else {
        # if temp is not in the current day team, set position 1 as temp
        on_call_team[1] = temp
      }
      # constraint 2: if you are number 3, you must be number 2 the next day 
      # set temp as number 3 from the previous day
      temp = data[i-1, "on_call_3"]
      if (temp %in% on_call_team){
        if (temp != on_call_team[2]){
          # if temp is in the current day team but not on position 2, swap to 2
          on_call_team[match(temp, on_call_team)] = on_call_team[2]
          on_call_team[2] = temp
        }
      } else {
        # if temp is not in the current day team, set position 2 as temp
        on_call_team[2] = temp
      }
    }
    data[i,12:19] = on_call_team    
  }
}

data2 <- data # save a copy for future reference

# function to calculate score for each resident ================================
calc_score <- function (data3, resident_data){
  resident_data$score <- 0
  # for each resident, sum their scores from each on-call columns (1 to 8)
  for (i in 1:nrow(resident_data)){
    for (j in 1:8){
      point_column_name <- paste("point_", toString(j), sep = "")
      resident_column_name <- paste("on_call_", toString(j), sep = "")
      # NA will show up as TRUE in filter, use separate var to change NA into FALSE
      row_identifier <- data3[resident_column_name] == pull(resident_data[i, 1])
      row_identifier[is.na(row_identifier)] <- FALSE
      scores <- data3[row_identifier, point_column_name]
      resident_data[i, "score"] %+=% sum(scores)
    }
  }
  return(resident_data)
}

# constraint functions==========================================================
check_constraint <- function(pos, check_today, check_tmr, check_yest, wday, first_day, last_day){
  if (first_day){
    # if first day of scheduling, no need to look at "yesterdays" data
    if (wday == 7){
      if (pos != 3 && pos != 4){
        return(c('today'))
      } else if (pos == 3 && (is.na(check_tmr) | check_tmr != 3)){
        return(c('today'))
      } else if (pos == 4 && is.na(check_tmr)){
        return(c('today', 'tmr', 4))
      }
    } else if (wday %in% c(2:5)){
      if (pos != 3 && pos != 4){
        return(c('today'))
      } else if (pos == 3 && is.na(check_tmr)){
        return(c('today', 'tmr', 2))
      } else if (pos == 4 && is.na(check_tmr)){
        return(c('today', 'tmr', 1))
      }
    } else {
      return(c('today'))
    }
  } else if (last_day){
    # if last day of scheduling, no need to look at "tomorrows" data
    if (wday == 1){
      if (pos != 3 && pos != 4){
        return(c('today'))
      } else if (pos == 3 && (is.na(check_yest) | check_yest != 3)){
        return(c('today'))
      } else if (pos == 4 && is.na(check_yest)){
        return(c('today', 'yest', 4))
      }
    } else if (wday %in% c(3:6)){
      if (pos != 1 && pos != 2){
        return(c('today'))
      } else if (pos == 1 && is.na(check_yest)){
        return(c('today', 'yest', 4))
      } else if (pos == 2 && is.na(check_yest)){
        return(c('today', 'yest', 3))
      }
    } else {
      return(c('today'))
    }
  } else {
    # if not last nor first day
    if (wday == 7){
      if (pos != 3 && pos != 4){
        return(c('today'))
      } else if (pos == 3 && (is.na(check_tmr) | check_tmr != 3)){
        return(c('today'))
      } else if (pos == 4 && is.na(check_tmr)){
        return(c('today', 'tmr', 4))
      }
    } else if (wday == 1){
      if (pos != 3 && pos != 4){
        return(c('today'))
      } else if (pos == 3 && (is.na(check_yest) | check_yest != 3)){
        return(c('today'))
      } else if (pos == 4 && is.na(check_yest)){
        return(c('today', 'yest', 4))
      }
    } else if (wday == 2){
      if (pos != 3 && pos != 4){
        return(c('today'))
      } else if (pos == 3 && is.na(check_tmr)){
        return(c('today', 'tmr', 2))
      } else if (pos == 4 && is.na(check_tmr)){
        return(c('today', 'tmr', 1))
      }
    } else if (wday == 6){
      if (pos != 1 && pos != 2){
        return(c('today'))
      } else if (pos == 1 && is.na(check_yest)){
        return(c('today', 'yest', 2))
      } else if (pos == 4 && is.na(check_yest)){
        return(c('today', 'yest', 1))
      }
    } else {
      if (pos %in% c(5:8)){
        return(c('today'))
      } else if (pos == 3 && is.na(check_tmr)){
        return(c('today', 'tmr', 2))
      } else if (pos == 4 && is.na(check_tmr)){
        return(c('today', 'tmr', 1))
      } else if (pos == 1 && is.na(check_yest)){
        return(c('today', 'yest', 4))
      } else if (pos == 2 && is.na(check_yest)){
        return(c('today', 'yest', 3))
      }
    }
  }
  return(c())
}

# function to swap the lowest score resident with a high score resident=========
swap_residents <- function(schedule, resident_data, old_score){
  # sort residents based on their scores
  resident_data <- resident_data[order(resident_data['score'], decreasing = TRUE),]
  # select resident with lowest score
  resident_low <- pull(resident_data[nrow(resident_data), 1])
  # obtain the list of vacation days for this resident
  # remove these from the possible dates the resident can be swapped to
  resident_low_vacay <- resident_data[nrow(resident_data), week_off_col_titles]
  resident_low_vacay_days <- as.Date(c())
  for (i in resident_low_vacay){
    resident_low_vacay_days <- c(resident_low_vacay_days, seq.Date(as.Date(i), as.Date(i) + days(6), by = "day"))
  }
  dates_to_remove <- data.frame('date'=resident_low_vacay_days)
  resident_low_avail_schedule <- anti_join(schedule, dates_to_remove, by='date')
  # check if swapping is possible against each resident
  # starting from the resident with the highest score
  # terminate once a swap is found
  for (i in 1:(nrow(resident_data) - 1)){
    # print(paste("Checking resident:", i))
    resident_high <- pull(resident_data[i, 1])
    # try to swap from the highest on-call score first
    # terminate once a swap is found
    for (j in 1:8){
      # print(paste("Checking on-call:", j))
      # filter for the days where current high-score resident is on the right call
      column_name <- paste("on_call_", toString(j), sep = "")
      potential_swap_days <- resident_low_avail_schedule[resident_low_avail_schedule[column_name] == resident_high,]
      potential_swap_days <- potential_swap_days[complete.cases(potential_swap_days[column_name]),]
      # if potential days is not empty, loop through each day to try to find a swap
      if (nrow(potential_swap_days) != 0){
        for (k in 1:nrow(potential_swap_days)){
          # print(paste("Checking potential swap:", k))
          # generate information to check constraints before swapping
          on_call_cols <- c('on_call_1', 'on_call_2', 'on_call_3', 'on_call_4', 'on_call_5', 'on_call_6', 'on_call_7', 'on_call_8')
          team_today <- potential_swap_days[k, on_call_cols]
          team_today <- unlist(team_today, use.names=FALSE)
          low_pos_today <- match(resident_low, team_today)
          high_pos_today <- j
          today_date <- potential_swap_days[k, 'date']
          today_wday <- potential_swap_days[k, "weekday"]
          today_holiday <- potential_swap_days[k, "holiday"]
          if (today_date > sim_start){
            team_yest <- schedule[schedule$date == today_date - days(1), on_call_cols]
            team_yest <- unlist(team_yest, use.names=FALSE)
            low_pos_yest <- match(resident_low, team_yest)
            high_pos_yest <- match(resident_high, team_yest)
            yest_holiday <- schedule[schedule$date == today_date - days(1), "holiday"]
          }
          if (today_date < sim_end){
            team_tmr <- schedule[schedule$date == today_date + days(1), on_call_cols]
            team_tmr <- unlist(team_tmr, use.names=FALSE)
            low_pos_tmr <- match(resident_low, team_tmr)
            high_pos_tmr <- match(resident_high, team_tmr)
            tmr_holiday <- schedule[schedule$date == today_date + days(1), "holiday"]
          }
          
          # tracker to record what to update, if updates are possible
          update_low_today = FALSE
          update_low_yest = FALSE
          update_low_tmr = FALSE
          update_high_today = FALSE
          update_high_yest = FALSE
          update_high_tmr = FALSE
          
          if (today_holiday == TRUE){
            # holidays are independent and hence no constraints
            update_low_today = high_pos_today
          } else {
            if (is.na(low_pos_today)){
              # if low-score resident not on today's team, directly replace high-score resident with low if possible
              # check constraints
              swap_low <- check_constraint(high_pos_today, low_pos_today, low_pos_tmr, low_pos_yest, today_wday, today_date == sim_start, today_date == sim_end)
              if (!is.null(swap_low)){
                # if constraints satisfied, update trackers
                for (i in 1:length(swap_low)){
                  if (swap_low[i] == 'today'){
                    update_low_today = high_pos_today
                  } else if (swap_low[i] == 'yest'){
                    update_low_yest = swap_low[i + 1]
                  } else if (swap_low[i] == 'tmr'){
                    update_low_tmr = swap_low[i + 1]
                  }
                }
              }
            } else {
              # if low-score resident is on today's team, swap their positions if possible
              # check constraints
              swap_low <- check_constraint(high_pos_today, low_pos_today, low_pos_tmr, low_pos_yest, today_wday, today_date == sim_start, today_date == sim_end)
              swap_high <- check_constraint(low_pos_today, high_pos_today, high_pos_tmr, high_pos_yest, today_wday, today_date == sim_start, today_date == sim_end)
              if (!is.null(swap_low) && !is.null(swap_high)){
                # if constraints are satisfied, update trackers
                for (i in 1:length(swap_low)){
                  if (swap_low[i] == 'today'){
                    update_low_today = high_pos_today
                  } else if (swap_low[i] == 'yest'){
                    update_low_yest = swap_low[i + 1]
                  } else if (swap_low[i] == 'tmr'){
                    update_low_tmr = swap_low[i + 1]
                  }
                }
                for (i in 1:length(swap_high)){
                  if (swap_high[i] == 'today'){
                    update_high_today = low_pos_today
                  } else if (swap_high[i] == 'yest'){
                    update_high_yest = swap_high[i + 1]
                  } else if (swap_high[i] == 'tmr'){
                    update_high_tmr = swap_high[i + 1]
                  }
                }
              }
            }
          }
          # update and return schedule based on trackers
          
          if(typeof(update_low_today) == "character"){
            update_low_today <- strtoi(update_low_today)  
          }
          if(typeof(update_low_tmr) == "character"){
            update_low_tmr <- strtoi(update_low_tmr)  
          }
          if(typeof(update_low_yest) == "character"){
            update_low_yest <- strtoi(update_low_yest)  
          }
          if(typeof(update_high_today) == "character"){
            update_high_today <- strtoi(update_high_today)  
          }
          if(typeof(update_high_tmr) == "character"){
            update_high_tmr <- strtoi(update_high_tmr)  
          }
          if(typeof(update_high_yest) == "character"){
            update_high_yest <- strtoi(update_high_yest)  
          }
          print("Checking -------------------------------------------------")
          print(today_date)
          print(resident_low)
          print(resident_high)
          # print(update_low_yest)
          # print(update_low_tmr)
          # print(update_high_today)
          # print(update_high_yest)
          # print(update_high_tmr)
          # print(update_low_today)
          schedule_temp <- schedule
          if (update_low_today){
            column_name <- paste("on_call_", toString(update_low_today), sep = "")
            schedule[schedule$date == today_date, column_name] <- resident_low
          }
          
          if (update_low_yest){
            column_name <- paste("on_call_", toString(update_low_yest), sep = "")
            schedule[schedule$date == today_date - days(1), column_name] <- resident_low
          }

          if (update_low_tmr){
            column_name <- paste("on_call_", toString(update_low_tmr), sep = "")
            schedule[schedule$date == today_date + days(1), column_name] <- resident_low
          }

          if (update_high_today){
            column_name <- paste("on_call_", toString(update_high_today), sep = "")
            schedule[schedule$date == today_date, column_name] <- resident_high
          }

          if (update_high_yest){
            column_name <- paste("on_call_", toString(update_high_yest), sep = "")
            schedule[schedule$date == today_date - days(1), column_name] <- resident_high
          }          

          if (update_high_tmr){
            column_name <- paste("on_call_", toString(update_high_tmr), sep = "")
            schedule[schedule$date == today_date + days(1), column_name] <- resident_high
          }
          resident_data_temp_new <- calc_score(schedule, resident_data)
          new_score <- max(resident_data_temp_new['score']) - min(resident_data_temp_new['score'])
          print(paste('old_score: ',old_score))
          print(paste('new_score: ', new_score))
          if (old_score > new_score){
            print("Swap!")
            return(schedule)
          } else {
            schedule <- schedule_temp
          }
        }
      }
    }    
  }
  print("Don't swap!")
  return(FALSE)
}

# data <- data2
# initiate scoring and while loop criteria
resident_data <- calc_score(data, resident_data)
discrepancy <- max(resident_data['score']) - min(resident_data['score'])
result <- "start"

# loop until no swaps are made or no more discrepancy
while (typeof(result) != "logical" && discrepancy > 1){
  result <- swap_residents(data, resident_data, discrepancy)
  if (typeof(result) != "logical"){
    data <- result
  }
  # update criteria
  resident_data <- calc_score(data, resident_data)
  discrepancy <- max(resident_data['score']) - min(resident_data['score'])
  print(paste("Updated discrepancy: ", discrepancy))
}

