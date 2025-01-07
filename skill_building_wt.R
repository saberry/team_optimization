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

# mba_data_ty <- mba_data_ty[-1, ]

mba_data <- mba_data[grepl("Skill Building", mba_data$`Skill Building or Consulting?`), ]

mba_data$Email <- dplyr::coalesce(
  mba_data$RecipientEmail,
  mba_data$`Your ND Email address:`
)

mba_data$Email <- mba_data$RecipientEmail

consulting_students <- which(mba_data$Ranking.W.Th_4 == 1)

# mba_data <- mba_data[-consulting_students, ]

all_keep_vars <- c("Email", grep("^Ranking", colnames(mba_data), value = TRUE))
all_keep_vars <- all_keep_vars[grepl("Vets", all_keep_vars) == FALSE]
# all_keep_vars_ty <- c("Email", grep("^Ranking", colnames(mba_data_ty), value = TRUE))

mba_data_important <- mba_data[, all_keep_vars]
# mba_ty_data_important <- mba_data_ty[, all_keep_vars_ty]

# mba_ty_data_important$Email <- coalesce(mba_ty_data_important$RecipientEmail, 
#                                         mba_ty_data_important$Email)

# ranking_data <- rbind(mba_data_important, mba_ty_data_important)

ranking_data <- mba_data_important

ranking_data <- ranking_data[-consulting_students, ]

ranking_data$i <- 1:nrow(ranking_data)

ranking_columns <- grep("^Ranking", colnames(ranking_data), value = TRUE)

ranking_columns <- ranking_columns[grepl("Vets", ranking_columns) == FALSE]

ranking_columns <- grep("W", ranking_columns, value = TRUE)

ranking_course_numbers <- data.frame(j = 1:length(ranking_columns), 
                                     course = ranking_columns)

ranking_data[, ranking_columns] <- lapply(ranking_columns, function(x) as.numeric(ranking_data[, x]))

ranking_data[, ranking_columns] <- lapply(ranking_columns, function(x) {
  ranking_data[, x] <- ifelse(is.na(ranking_data[, x]), 4, ranking_data[, x])
  
})

ranking_data <- ranking_data[, c("Email", ranking_columns, "i")]

ranking_matrix <- as.matrix(ranking_data[, ranking_columns])

ranking_matrix_1 <- ranking_matrix[, 1:4]

n_students <- nrow(ranking_matrix)

number_teams <- length(ranking_columns)

team_capacity_1 <- c(5, 60, 15, 15)

team_capacity_2 <- c(5, 60, 15, 15)

team_minimum <- rep(5, number_teams)

model <- MIPModel() %>%
  add_variable(x[i, j], i = 1:n_students, j = 1:number_teams, type = "binary") %>%
  set_objective(sum_over((x[i, j] * ranking_matrix_1[i, j]), i = 1:n_students, j = 1:number_teams), 
                sense = "min") %>%
  # The following will not allow teams to go over capacity:
  add_constraint(sum_over(x[i, j], i = 1:n_students) <= team_capacity_2[j], 
                 j = 1:number_teams) %>%
  add_constraint(sum_over(x[i, j], i = 1:n_students) >= team_minimum[j], 
                 j = 1:number_teams) %>%
  # Every person needs to be on 1 team:
  add_constraint(sum_over(x[i, j], j = 1:number_teams) == 1, 
                 i = 1:n_students) 

variable_keys(model)

extract_constraints(model)

objective_function(model)

result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

objective_value(result)

solution <- get_solution(result, x[i,j]) 

solution <- solution[solution$value > 0, ]

solution <- left_join(solution, ranking_course_numbers)

solution <- solution[, c("i", "j", "course")]

matching <- left_join(solution, ranking_data, by = c("i" = "i")) 

course_matcher <- data.frame(course = unique(matching$course), 
                             course_name = c(#"Strategic Communication", "Case Interview Prep", 
                               #"Corporate Valuation", "Intro to Tableau", 
                               "Change Management", "Mergers",
                               "SQL", "Data Storytelling"))

matching_columns <- c(grep("Email", colnames(matching), value = TRUE), 
                      grep("Ranking", colnames(matching), value = TRUE), 
                      "course")

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

# mba_data_ty_small <- mba_data_ty[, c("Email", "First", "Last")]

# colnames(mba_data_ty_small) <- c("Email", "RecipientFirstName", "RecipientLastName")

email_join <- mba_data[, c("Email", "RecipientFirstName", "RecipientLastName")]
                    
output <- left_join(output, email_join, by = c("Email.x" = "Email"))

output <- output[, c(1, 9:10, 2:8)]

output <- output[!duplicated(output), ]

change_man_data <- mba_data[
  consulting_students, 
  c("Email", "RecipientFirstName", "RecipientLastName", 
    grep("Ranking.W", colnames(mba_data), value = TRUE))
]

change_man_data[, grep("Ranking.W", colnames(change_man_data))] <- 
  lapply(grep("Ranking.W", colnames(change_man_data)), function(x) {
    as.numeric(change_man_data[, x])
  })

change_man_data$course_name <- "Change Management"

change_man_data$value <- 1

colnames(change_man_data)[1] <- "Email.x"

complete_out <- bind_rows(output, na.omit(change_man_data))

write.csv(complete_out, "~/Downloads/skill_building_assignment_wt.csv", 
          row.names = FALSE)

mt <- read.csv("~/Downloads/skill_building_assignment_mt.csv")
wt <- read.csv("~/Downloads/skill_building_assignment_wt.csv")

final_output <- merge(mt, 
                      wt, 
                      by = c("Email.x", "RecipientFirstName", "RecipientLastName"), 
                      all.x = TRUE, 
                      all.y = TRUE)

final_output <- final_output[, c(1:3, 4, 11, 6, 13)]

colnames(final_output) <- c("email", "first", "last", 
                            "mt_course", "wtr_course", 
                            "mt_value", "wtr_value")

write.csv(final_output, "~/Downloads/skill_building_assignments_spring_24.csv", 
          row.names = FALSE)

View(read.csv("~/Downloads/skill_building_assignments_all.csv"))
