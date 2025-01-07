#################
### MBA Teams ###
#################

library(ompr.roi)
library(ROI.plugin.glpk)
library(ROI.plugin.symphony)
library(ompr)
library(dplyr)

mba_data <- read.csv("~/Downloads/MBA Fall 2024 Grow Irish Course Preferences (Master) - August 26, 2_30 PM - Data.csv")
# mba_data_ty <- read.csv("~/Downloads/MBA Placement Master - Fall Grow Irish Week (2023) - For Seth Qualtrics results (TY25+MSA).csv")

row_descriptions <- mba_data[1, ]

mba_data <- mba_data[-1, ]

colnames(mba_data) <- row_descriptions

mba_data <- mba_data[mba_data$`Skill Building or Consulting?` == "MBA Consulting Project", ]

# mba_data_ty <- mba_data_ty[-1, ]

# mba_data <- mba_data[grepl("Courses$", mba_data$Course.type), ]

mba_data$Email <- mba_data$Recipient.Email

mba_data$Email <- dplyr::coalesce(
  mba_data$RecipientEmail,
  mba_data$`Your ND Email address:`
)

first_tier_ratings <- mba_data$`CP: First Tier`

first_tier_split <- strsplit(first_tier_ratings, split = ",")

first_tier_out <- lapply(1:length(first_tier_split), function(x) {
  tmp_df <- data.frame(org = first_tier_split[[x]])
  tmp_df$value <- 1
  tmp_df$id <- x
  tmp_df
  })

first_tier_out <- do.call(rbind, first_tier_out)

second_tier_ratings <- mba_data$`CP: Second Tier`

second_tier_split <- strsplit(second_tier_ratings, split = ",")

second_tier_out <- lapply(1:length(second_tier_split), function(x) {
  tmp_df <- data.frame(org = second_tier_split[[x]])
  tmp_df$value <- 2
  tmp_df$id <- x
  tmp_df
})

second_tier_out <- do.call(rbind, second_tier_out)

# third_tier_ratings <- mba_data$Third.Tier......Indicate.6.projects.in.your.3rd.TIER..You.must.select.exactly.6..Selections.are.not.ranked.within.each.tier.
# 
# third_tier_split <- strsplit(third_tier_ratings, split = ",")
# 
# third_tier_out <- lapply(1:length(third_tier_split), function(x) {
#   tmp_df <- data.frame(org = third_tier_split[[x]])
#   tmp_df$value <- 3
#   tmp_df$id <- x
#   tmp_df
# })
# 
# third_tier_out <- do.call(rbind, third_tier_out)

all_rankings <- rbind(
  first_tier_out, 
  second_tier_out 
  # third_tier_out
  )

all_rankings <- tidyr::pivot_wider(all_rankings, 
                   id_cols = "id", 
                   names_from = "org", 
                   values_from = "value", 
                   values_fill = 99)

all_rankings_matrix <- as.matrix(all_rankings[,-1])

all_rankings_matrix <- all_rankings_matrix[, 
                                           !colnames(all_rankings_matrix) %in% c("American Heart Association", "Bruisers Auto Care", "A Rosie Place")]

n_students <- nrow(all_rankings_matrix)
n_teams <- ncol(all_rankings_matrix)

org_capacity <- c(4,
                  4,
                  4,
                  4,
                  6,
                  4,
                  4,
                  6,
                  5,
                  6)

org_min <- c(rep(0, n_teams-2), 0, 0)
# org_min[c(18:23, 25:26)] <- 6

# travel_eligible <- mba_data$international
# international_project <- rep(0, n_teams)
# international_project[c(18:23, 25:26)] <- 1

mba_data$driver <- mba_data$`U.S. drivers license and willing to drive?`

driver <- ifelse(grepl("No/NA", mba_data$driver), 0, 1)

gender <- ifelse(mba_data$`Gender (1=Male; 2=Female)` == "1", 0, 1)
gender_needs <- c(rep(2, 8), 0, 0)

model <- MIPModel() %>%
  add_variable(x[i, j], i = 1:n_students, j = 1:n_teams, type = "binary") %>%
  set_objective(sum_over(x[i, j] * all_rankings_matrix[i, j], i = 1:n_students, j = 1:n_teams), 
                sense = "min") %>%
  # The following will not allow teams to go over capacity:
  add_constraint(sum_over(x[i, j], i = 1:n_students) <= org_capacity[j], 
                 j = 1:n_teams) %>%
  add_constraint(sum_over(x[i, j], i = 1:n_students) >= org_min[j], 
                 j = 1:n_teams) %>% 
  # Every person needs to be on 1 team:
  add_constraint(sum_over(x[i, j], j = 1:n_teams) == 1, 
                 i = 1:n_students) %>%
  # add_constraint(sum_over(x[i, j] * travel_eligible[i], i = 1:n_student), i = 1:n_students)
  # Need at least 1 woman
  add_constraint(sum_over(gender[i] * x[i, j], i = 1:n_students) >= gender_needs[j],
                 j = 1:n_teams) %>%
  add_constraint(sum_over(driver[i] * x[i, j], i = 1:n_students) >= rep(1, n_teams)[j],
                 j = 1:n_teams)
  # # The following will deal with athletes:
  # add_constraint(sum_over(athlete[i] * x[i, j], i = 1:n_students) == athlete_needs[j], 
  #                j = 1:n_teams) %>% 
  # # The following does not allow for more than 1 slacker:
  # add_constraint(sum_over(low_group[i] * x[i, j], i = 1:n_students) <= low_performance_needs[j], 
  #                j = 1:n_teams) %>%
  # # Need at least one high performer:
  # add_constraint(sum_over(high_group[i] * x[i, j], i = 1:n_students) >= high_performance_needs_low[j], 
  #                j = 1:n_teams) %>%
  # add_constraint(sum_over(high_group[i] * x[i, j], i = 1:n_students) <= high_performance_needs_high[j], 
  #                j = 1:n_teams) %>%
# # Every team needs at least 1 driver
# add_constraint(sum_over(driver[i] * x[i, j], i = 1:n_students) >= driver_needs[j], 
#                j = 1:n_teams) %>% 
# # Need at least 1 man
# add_constraint(sum_over(female[i] * x[i, j], i = 1:n_students) >= female_needs[j], 
#                j = 1:n_teams) %>% 

result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

objective_value(result)

solution <- get_solution(result, x[i,j]) 
solution <- solution[solution$value > 0, ]

mba_data$i <- 1:nrow(mba_data)
org_data <- data.frame(j = 1:n_teams, 
                       org = colnames(all_rankings_matrix)
)

join_data <- mba_data[, c("i",
                          "Recipient First Name", 
                          "Recipient Last Name",
                          "Email", 
                          "driver",
                          "CP: First Tier\n", 
                          "CP: Second Tier")]

output <- left_join(solution, org_data) %>% 
  left_join(., join_data)

output$joiner <- paste(output$i, 
                       output$org, 
                       sep = "_")

melted_rankings <- tidyr::pivot_longer(all_rankings, cols = 2:14)

melted_rankings$joiner <- paste(melted_rankings$id, 
                                melted_rankings$name, 
                                sep = "_")

colnames(melted_rankings)[colnames(melted_rankings) == "value"] <- "og_rank"

test <- left_join(output, melted_rankings)

test <- select(test, -joiner, -id, -name, -variable, -value, -i, -j)

write.csv(test, 
          "~/Downloads/fall_24_mba_grow_irish_consulting.csv", 
          row.names = FALSE)

all_keep_vars <- c("Email", grep("^Ranking", colnames(mba_data), value = TRUE))
all_keep_vars <- all_keep_vars[grepl("Vets", all_keep_vars) == FALSE]
all_keep_vars_ty <- c("Email", grep("^Ranking", colnames(mba_data_ty), value = TRUE))

mba_data_important <- mba_data[, all_keep_vars]
mba_ty_data_important <- mba_data_ty[, all_keep_vars_ty]

mba_ty_data_important$Email <- coalesce(mba_ty_data_important$RecipientEmail, 
                                        mba_ty_data_important$Email)

ranking_data <- rbind(mba_data_important, mba_ty_data_important)

ranking_data$i <- 1:nrow(ranking_data)

ranking_columns <- grep("^Ranking", colnames(ranking_data), value = TRUE)

ranking_columns <- ranking_columns[grepl("Vets", ranking_columns) == FALSE]

ranking_course_numbers_mt <- data.frame(j = 1:4, 
                                     course = ranking_columns[1:4])

ranking_course_numbers_wt <- data.frame(l = 1:4, 
                                     course = ranking_columns[5:8])

ranking_data[, ranking_columns] <- lapply(ranking_columns, function(x) as.numeric(ranking_data[, x]))

ranking_data[, ranking_columns] <- lapply(ranking_columns, function(x) {
  ranking_data[, x] <- ifelse(is.na(ranking_data[, x]), 4, ranking_data[, x])
  
})

ranking_matrix <- as.matrix(ranking_data[, ranking_columns])

n_students <- nrow(ranking_matrix)

number_teams <- length(ranking_columns)

team_capacity <- c(35, 78, 40, 42, 35, 78, 40, 35)

team_minimum <- rep(15, number_teams)

model <- MIPModel() %>%
  add_variable(x[i, j], i = 1:n_students, j = 1:4, type = "binary") %>%
  add_variable(y[i, l], i = 1:n_students, l = 4:8, type = "binary") %>%
  set_objective(sum_over(
    (((x[i, j] * ranking_matrix[i, j]) + (y[i, l] * ranking_matrix[i, l])) / 8), 
    i = 1:n_students, j = 1:4, l = 4:8), 
    sense = "min") %>%
  # The following will not allow teams to go over capacity:
  add_constraint(sum_over(x[i, j], i = 1:n_students) <= team_capacity[j], 
                 j = 1:4) %>%
  add_constraint(sum_over(y[i, l], i = 1:n_students) <= team_capacity[l], 
                 l = 4:8) %>% 
  add_constraint(sum_over(x[i, j], i = 1:n_students) >= team_minimum[j], 
                 j = 1:4) %>%
  add_constraint(sum_over(y[i, l], i = 1:n_students) >= team_minimum[l], 
                 l = 4:8) %>%
  # Every person needs to be on 1 team:
  add_constraint(sum_over(x[i, j], j = 1:4) == 1, 
                 i = 1:n_students) %>% 
  add_constraint(sum_over(y[i, l], l = 4:8) == 1, 
                 i = 1:n_students)

variable_keys(model)

extract_constraints(model)

objective_function(model)

result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

objective_value(result)

solution <- get_solution(result, x[i,j]) 
solution_l <- get_solution(result, x[i, l])

solution <- solution[solution$value > 0, ]
solution_l <- solution_l[solution_l$value > 0, ]

solution <- left_join(solution, solution_l, by = c("i", "variable", "value"))

solution <- solution[, c("i", "j", "l")]

solution <- left_join(solution, ranking_course_numbers_mt, 
                      by = c("j" = "j")) %>% 
  left_join(., ranking_course_numbers_wt, 
            by = "l")

matching <- left_join(solution, ranking_data, by = c("i" = "i")) 

course_matcher <- data.frame(course = unique(c(matching$course.x, 
                                               matching$course.y)), 
                             course_name = c("Strategic Communication", "Case Interview Prep", 
                                             "Corporate Valuation", "Intro to Tableau", 
                                             "Strategic Communication", "Case Interview Prep", 
                                             "Financial Modeling", "Data Storytelling with Tableau"))

matching <- left_join(matching, course_matcher, 
                      by = c("course.x" = "course"))

colnames(matching)[which(colnames(matching) == "course_name")] <- "mt_course"

matching <- left_join(matching, course_matcher, 
                      by = c("course.y" = "course"))

colnames(matching)[which(colnames(matching) == "course_name")] <- "wtr_course"

matching_columns <- c(grep("Email", colnames(matching), value = TRUE), 
                      grep("Ranking", colnames(matching), value = TRUE), 
                      "mt_course", "wtr_course", "course.x", "course.y")

matching_columns <- matching_columns[grepl("Vets", matching_columns) == FALSE]

match_test <- matching[, matching_columns]

match_test$name_course <- paste(match_test$Email, 
                                match_test$course, 
                                sep = "_")

melted_match <- reshape2::melt(matching[, matching_columns[-length(matching_columns)]], 
                               id.vars = "Email")

melted_match$name_course <- paste(melted_match$Email, 
                                  melted_match$variable, 
                                  sep = "_")

output <- left_join(match_test, melted_match, by = "name_course") %>% 
  left_join(., course_matcher) %>% 
  select(Email.x, course_name, variable, value, starts_with("Ranking"))

mba_data_ty_small <- mba_data_ty[, c("Email", "First", "Last")]

colnames(mba_data_ty_small) <- c("Email", "RecipientFirstName", "RecipientLastName")

email_join <- rbind(mba_data[, c("Email", "RecipientFirstName", "RecipientLastName")], 
                    mba_data_ty_small)

output <- left_join(output, email_join, by = c("Email.x" = "Email"))

output <- output[, c(1, 13:14, 2:12)]

write.csv(output, "~/Downloads/skill_building_assignment.csv", 
          row.names = FALSE)
