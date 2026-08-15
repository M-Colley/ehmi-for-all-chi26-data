if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
} else {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    setwd(dirname(normalizePath(sub("^--file=", "", file_arg))))
  }
}

library(colleyRstats)
colleyRstats::colleyRstats_setup()

library(dplyr)
library(easystats)
library(ARTool)
library(FSA)
library(ggplot2)

library(data.table)
library(dplyr)
library(tidyr)

# Directory path
dir_path <- "./data"

files <- list.files(
  path = dir_path,
  recursive = TRUE,
  pattern = ".*_[A-Z].csv$",
  full.names = TRUE
)

filesTime <- list.files(
  path = dir_path,
  recursive = TRUE,
  pattern = ".*_[A-Z]_time.csv$",
  full.names = TRUE
)


# Read and combine all CSV files into a single data frame
combined_df <- do.call("rbind", sapply(files, function(x) read.csv(x, sep = ";"), simplify = FALSE))

combined_df <- combined_df |>
  mutate(scenario = if_else(grepl(" Pedestrian", scenario), "Pedestrian", scenario))

combined_df <- combined_df |>
  mutate(scenario = if_else(grepl(" Bicycle", scenario), "Cyclist", scenario))

combined_df <- combined_df |>
  mutate(scenario = if_else(grepl(" Driver", scenario), "Driver", scenario))

combined_df$ehmi <- factor(ifelse(combined_df$ehmi %in% c(" None"), "No eHMI", "Intention-Based eHMI"),
                           levels = c("No eHMI", "Intention-Based eHMI"))

                       
combined_df$user_id <- as.factor(combined_df$user_id)
combined_df$scenario <- as.factor(combined_df$scenario)
combined_df$distraction <- as.factor(combined_df$distraction)
combined_df$ehmi <- as.factor(combined_df$ehmi)


names(combined_df)


# Modify the focused.object column
# there are multiple Noise objects (2,3,4,5,...)


# Attention, the Noise has an empty space at the beginning
# combined_df <- combined_df |>
# mutate(focused.object = if_else(grepl("^ Noise", focused.object), "Noise", focused.object))

combined_df <- combined_df |>
  mutate(focused.object = if_else(grepl(" Checking Box Human Car", focused.object), "MV", focused.object))

# combined_df$focused.object

combined_df <- combined_df |>
  mutate(focused.object = if_else(grepl("^ Noise|^ Man| Checking Box Human", focused.object), "Noise", focused.object))

combined_df <- combined_df |>
  mutate(focused.object = if_else(grepl("^ Pedestrian|^ Bicycle", focused.object), "Interference", focused.object))

combined_df <- combined_df |>
  mutate(focused.object = if_else(grepl(" Box_Collider| Checking Box AICar", focused.object), "AV", focused.object))


combined_df$focused.object <- as.factor(combined_df$focused.object)


# Read and combine all CSV files into a single data frame
combined_df_time <- do.call("rbind", sapply(filesTime, function(x) read.csv(x, sep = ";"), simplify = FALSE))


combined_df_time <- combined_df_time |>
  mutate(scenario = if_else(grepl(" Pedestrian", scenario), "Pedestrian", scenario))

combined_df_time <- combined_df_time |>
  mutate(scenario = if_else(grepl(" Bicycle", scenario), "Cyclist", scenario))

combined_df_time <- combined_df_time |>
  mutate(scenario = if_else(grepl(" Driver", scenario), "Driver", scenario))

combined_df_time$ehmi <- factor(ifelse(combined_df_time$ehmi %in% c(" None"), "No eHMI", "Intention-Based eHMI"),
                           levels = c("No eHMI", "Intention-Based eHMI"))


combined_df_time$user_id <- as.factor(combined_df_time$user_id)
combined_df_time$scenario <- as.factor(combined_df_time$scenario)
combined_df_time$distraction <- as.factor(combined_df_time$distraction)
combined_df_time$ehmi <- as.factor(combined_df_time$ehmi)


combined_df_time |>
  group_by(user_id, scenario) |>
  dplyr::count() |>
  print()


# total time
combined_df_time$total.time <- as.numeric(combined_df_time$total.time)
combined_df_time$time.before <- as.numeric(combined_df_time$time.before)
combined_df_time$time.on.intersection <- as.numeric(combined_df_time$time.on.intersection)
combined_df_time$time.after <- as.numeric(combined_df_time$time.after)


levels(combined_df_time$ehmi)

### Do analysis over all scenarios
combined_df_time |> dplyr::group_by(scenario) |> do(art(data = ., total.time ~ distraction * ehmi + Error(user_id / (distraction * ehmi))) |> anova())


combined_df_time_driver <- subset(combined_df_time, scenario == "Driver")
combined_df_time_pedestrian <- subset(combined_df_time, scenario == "Pedestrian")
combined_df_time_cyclist <- subset(combined_df_time, scenario == "Cyclist")


reportMeanAndSD(data = combined_df_time,iv = "scenario", dv = "total.time")



modelArt <- art(data = combined_df_time_driver, total.time ~ distraction * ehmi + Error(user_id / (distraction * ehmi))) |> anova()
modelArt
reportART(modelArt, dv = "total duration")

reportMeanAndSD(data = combined_df_time_driver, iv = "ehmi", dv = "total.time")
effectsize::rank_biserial(as.formula(paste("total.time", "~", "ehmi")), data = combined_df_time_driver)



modelArt <- art(data = combined_df_time_pedestrian, total.time ~ distraction * ehmi + Error(user_id / (distraction * ehmi))) |> anova()
modelArt
reportART(modelArt, dv = "total duration")

modelArt <- art(data = combined_df_time_cyclist, total.time ~ distraction * ehmi + Error(user_id / (distraction * ehmi))) |> anova()
modelArt
reportART(modelArt, dv = "total duration")





modelArt <- art(data = combined_df_time_driver, time.before ~ distraction * ehmi + Error(user_id / (distraction * ehmi))) |> anova()
modelArt
reportART(modelArt, dv = "time before intersection")

reportMeanAndSD(data = combined_df_time_driver, iv = "ehmi", dv = "time.before")
#effectsize::rank_biserial(as.formula(paste("time.before", "~", "ehmi")), data = combined_df_time_driver)


modelArt <- art(data = combined_df_time_pedestrian, time.before ~ distraction * ehmi + Error(user_id / (distraction * ehmi))) |> anova()
modelArt
reportART(modelArt, dv = "time before intersection")

FSA::dunnTest(time.before ~ distraction, data = combined_df_time_pedestrian, method = "holm") |> reportDunnTest(data = combined_df_time_pedestrian, iv = "distraction", dv = "time.before")
reportMeanAndSD(data = combined_df_time_pedestrian, iv = "distraction", dv = "time.before")
#effectsize::rank_biserial(as.formula(paste("time.before", "~", "distraction")), data = combined_df_time_pedestrian)



modelArt <- art(data = combined_df_time_cyclist, time.before ~ distraction * ehmi + Error(user_id / (distraction * ehmi))) |> anova()
modelArt
reportART(modelArt, dv = "time before intersection")


reportMeanAndSD(data = combined_df_time_cyclist, iv = "ehmi", dv = "time.before")
effectsize::rank_biserial(as.formula(paste("time.before", "~", "ehmi")), data = combined_df_time_cyclist)




modelArt <- art(data = combined_df_time_driver, time.on.intersection ~ distraction * ehmi + Error(user_id / (distraction * ehmi))) |> anova()
modelArt
reportART(modelArt, dv = "time on intersection")

modelArt <- art(data = combined_df_time_pedestrian, time.on.intersection ~ distraction * ehmi + Error(user_id / (distraction * ehmi))) |> anova()
modelArt
reportART(modelArt, dv = "time on intersection")

modelArt <- art(data = combined_df_time_cyclist, time.on.intersection ~ distraction * ehmi + Error(user_id / (distraction * ehmi))) |> anova()
modelArt
reportART(modelArt, dv = "time on intersection")


combined_df_time_cyclist |> ggplot() +
  aes(x = distraction, y = time.on.intersection, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("TIme on Intersection (s)") +
  # ylim(1,5) +
  theme(legend.position.inside = c(0.65, 0.85)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.05))
ggplot2::ggsave("plots/cyclist_time_on_intersection_2WAY.pdf", width = 9, height = 6, device = cairo_pdf)






modelArt <- art(data = combined_df_time_driver, time.after ~ distraction * ehmi + Error(user_id / (distraction * ehmi))) |> anova()
modelArt
reportART(modelArt, dv = "time after intersection")

modelArt <- art(data = combined_df_time_pedestrian, time.after ~ distraction * ehmi + Error(user_id / (distraction * ehmi))) |> anova()
modelArt
reportART(modelArt, dv = "time after intersection")

modelArt <- art(data = combined_df_time_cyclist, time.after ~ distraction * ehmi + Error(user_id / (distraction * ehmi))) |> anova()
modelArt
reportART(modelArt, dv = "time after intersection")




modelArt <- art(data = combined_df_time_driver, collision.with.cars ~ distraction * ehmi + Error(user_id / (distraction * ehmi))) |> anova()
modelArt
reportART(modelArt, dv = "collision with cars")

modelArt <- art(data = combined_df_time_pedestrian, collision.with.cars ~ distraction * ehmi + Error(user_id / (distraction * ehmi))) |> anova()
modelArt
reportART(modelArt, dv = "collision with cars")

modelArt <- art(data = combined_df_time_cyclist, collision.with.cars ~ distraction * ehmi + Error(user_id / (distraction * ehmi))) |> anova()
modelArt
reportART(modelArt, dv = "collision with cars")









checkAssumptionsForAnova(data = combined_df_time, y = "total.time", factors = c("scenario", "distraction", "ehmi"))

modelArt <- art(total.time ~ scenario * distraction * ehmi + Error(user_id / (scenario * distraction * ehmi)), data = combined_df_time) |> anova()
modelArt
reportART(modelArt, dv = "total time")

combined_df_time |> ggplot() +
  aes(x = scenario, y = total.time, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Total Time (s)") +
  # ylim(1,5) +
  theme(legend.position.inside = c(0.65, 0.85)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.05))
ggsave("plots/total_time_2WAY.pdf", width = 9, height = 6, device = cairo_pdf)


p <- combined_df_time |> ggplot() +
  aes(x = scenario, y = total.time, fill = distraction, colour = distraction, group = distraction) +
  scale_color_see() +
  ylab("Total Time (s)") +
  theme(legend.position.inside = c(0.45, 0.25)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.8) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.05))
p + facet_grid(~ehmi)
ggsave("plots/total_time_3WAY.pdf", width = 9, height = 6, device = cairo_pdf)



# time before
checkAssumptionsForAnova(data = combined_df_time, y = "time.before", factors = c("scenario", "distraction", "ehmi"))

modelArt <- art(time.before ~ scenario * distraction * ehmi + Error(user_id / (scenario * distraction * ehmi)), data = combined_df_time) |> anova()
modelArt
reportART(modelArt, dv = "time before intersection")


combined_df_time |> ggplot() +
  aes(x = scenario, y = time.before, fill = distraction, colour = distraction, group = distraction) +
  scale_color_see() +
  ylab("Time Before Intersection (s)") +
  # ylim(1,5) +
  theme(legend.position.inside = c(0.65, 0.85)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.05))
ggsave("plots/time_before_2WAY_distraction.pdf", width = 9, height = 6, device = cairo_pdf)


combined_df_time |> ggplot() +
  aes(x = scenario, y = time.before, fill = ehmi, colour = ehmi, group = ehmi) +
  scale_color_see() +
  ylab("Time Before Intersection (s)") +
  # ylim(1,5) +
  theme(legend.position.inside = c(0.65, 0.85)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.05))
ggsave("plots/time_before_2WAY_ehmi.pdf", width = 9, height = 6, device = cairo_pdf)


# time on intersection
checkAssumptionsForAnova(data = combined_df_time, y = "time.on.intersection", factors = c("scenario", "distraction", "ehmi"))

modelArt <- art(time.on.intersection ~ scenario * distraction * ehmi + Error(user_id / (scenario * distraction * ehmi)), data = combined_df_time) |> anova()
modelArt
reportART(modelArt, dv = "time on intersection")

p <- combined_df_time |> ggplot() +
  aes(x = scenario, y = time.on.intersection, fill = distraction, colour = distraction, group = distraction) +
  scale_color_see() +
  ylab("Time on Intersection (s)") +
  theme(legend.position.inside = c(0.65, 0.75)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.8) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.05))
p + facet_grid(~ehmi)
ggsave("plots/time_on_intersection_3WAY.pdf", width = 9, height = 6, device = cairo_pdf)




# time after

checkAssumptionsForAnova(data = combined_df_time, y = "time.after", factors = c("scenario", "distraction", "ehmi"))

modelArt <- art(time.after ~ scenario * distraction * ehmi + Error(user_id / (scenario * distraction * ehmi)), data = combined_df_time) |> anova()
modelArt
reportART(modelArt, dv = "time after intersection")


p <- combined_df_time |> ggplot() +
  aes(x = scenario, y = time.after, fill = distraction, colour = distraction, group = distraction) +
  scale_color_see() +
  ylab("Time after Intersection (s)") +
  theme(legend.position.inside = c(0.65, 0.25)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.8) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.05))
p + facet_grid(~ehmi)
ggsave("plots/time_after_intersection_3WAY.pdf", width = 9, height = 6, device = cairo_pdf)





# all zero
# checkAssumptionsForAnova(data = combined_df_time, y = "collision.with.cars", factors = c("scenario", "distraction", "ehmi"))
#
#
# modelArt <- art(collision.with.cars ~ scenario * distraction * ehmi + Error(user_id / (scenario * distraction * ehmi )), data = combined_df_time) |> anova()
# modelArt
# reportART(modelArt, dv = "collision.with.cars")
















#####################################################################
#### EYE GAZE
#####################################################################
#####################################################################
#####################################################################

# List of relevant strings
aoi_list <- levels(combined_df$focused.object)
aoi_list


# Remove empty strings
aoi_list <- aoi_list[nzchar(aoi_list)]


# Grouping columns
group_cols <- c("user_id", "scenario", "distraction", "ehmi")

library(data.table)

combined_df <- as.data.table(combined_df)

#n is true but does not work currently
is.data.table(combined_df)

# Create a list of data frames, one for each AOI
df_list <- lapply(aoi_list, function(aoi) {
  df <- combined_df |>
    group_by(across(all_of(group_cols))) |>
    dplyr::summarise(!!paste0("sumFixations", aoi) := sum(focused.object == aoi))
  return(df)
})

  # Merge all data frames in the list
test <- Reduce(function(x, y) {
  merge(x, y, by = group_cols, all = TRUE)
}, df_list)

# combine fixations
# Create column names to be summed based on aoi_list
sum_cols <- paste0("sumFixations", aoi_list)

# Calculate totalFixations by summing the relevant columns
test$totalFixations <- rowSums(select(test, all_of(sum_cols)), na.rm = TRUE)

# Dynamically create percentage columns based on aoi_list
for (aoi in aoi_list) {
  new_col_name <- paste0("sumFixations", aoi, "_Percentage")
  sum_col_name <- paste0("sumFixations", aoi)
  test[[new_col_name]] <- test[[sum_col_name]] / test$totalFixations
}




test2 <- dplyr::select(test, !(`sumFixations null`:totalFixations))

data_long <- gather(test2, aoi, measurement, `sumFixations null_Percentage`:`sumFixationsNoise_Percentage`, factor_key = TRUE)
data_long

# remove unnecessary strings
data_long$aoi <- gsub("sumFixations", "", data_long$aoi)
data_long$aoi <- gsub("_Percentage", "", data_long$aoi)

data_long$aoi <- as.factor(data_long$aoi)



# Function to streamline the data transformation process
process_data <- function(df_list, columns_to_merge) {
  # Merge multiple data frames by specified columns
  merged_data <- Reduce(function(x, y) merge(x, y, by = columns_to_merge), df_list)

  # Calculate total fixations
  sum_columns <- grep("sumFixations", names(merged_data), value = TRUE)
  merged_data$totalFixations <- rowSums(merged_data[, sum_columns])

  # Calculate percentages for each sumFixations column
  for (col in sum_columns) {
    new_col_name <- paste0(col, "_Percentage")
    merged_data[, new_col_name] <- merged_data[, col] / merged_data$totalFixations
  }

  # Transform data into long format
  percentage_columns <- grep("Percentage", names(merged_data), value = TRUE)
  data_long <- tidyr::gather(merged_data, aoi, measurement, percentage_columns, factor_key = TRUE)

  # Remove unnecessary strings from 'aoi' column
  data_long$aoi <- gsub("sumFixations|_Percentage", "", data_long$aoi)
  data_long$aoi <- as.factor(data_long$aoi)

  return(data_long)
}




data_long |> ggplot() +
  aes(x = scenario, y = measurement, fill = aoi, colour = aoi, group = aoi) +
  ylab("Percentage Fixation") +
  scale_color_see() +
  theme(legend.position.inside = c(0.89, 0.78)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1, position = position_dodge(width = .05), alpha = 0.5) # 95 % mean_cl_boot is 95% confidence intervals




data_long_1 <- subset(data_long, aoi != "Null")
data_long_1 <- subset(data_long_1, aoi != "NULL")
data_long_1 <- subset(data_long_1, aoi != "null")
data_long_1 <- subset(data_long_1, aoi != " null")




data_long_1 |> ggplot() +
  aes(x = scenario, y = measurement * 100, fill = aoi, colour = aoi, group = aoi) +
  scale_color_see() +
  ylab("Percentage Fixation - Excluded Null") +
  theme(legend.position.inside = c(0.15, 0.8)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi), alpha = 0.9) +
  scale_shape_manual(values = 1:nlevels(data_long_1$aoi)) +
  # stat_summary(fun = mean, geom = "line", linewidth = 1, aes(linetype= aoi)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1, alpha = 0.5) +
  guides(colour = guide_legend(override.aes = list(linetype = 0)))
ggsave("plots/Fixation_without_null.pdf", width = 9, height = 6, device = cairo_pdf)


data_long_1 |> ggplot() +
  aes(x = ehmi, y = measurement * 100, fill = aoi, colour = aoi, group = aoi) +
  scale_color_see() +
  ylab("Percentage Fixation - Excluded Null") +
  theme(legend.position.inside = c(0.9, 0.8)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi), alpha = 0.9) +
  scale_shape_manual(values = 1:nlevels(data_long_1$aoi)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1, alpha = 0.5) +
  guides(colour = guide_legend(override.aes = list(linetype = 0)))
ggsave("plots/Fixation_without_null_no_zero_visualization_study1.pdf", width = 9, height = 6, device = cairo_pdf)



data_long_1 |> ggplot() +
  aes(x = distraction, y = measurement * 100, fill = aoi, colour = aoi, group = aoi) +
  scale_color_see() +
  ylab("Percentage Fixation - Excluded Null") +
  theme(legend.position.inside = c(0.65, 0.8)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi), alpha = 0.9) +
  scale_shape_manual(values = 1:nlevels(data_long_1$aoi)) +
  # stat_summary(fun = mean, geom = "line", linewidth = 1, aes(linetype= aoi)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1, alpha = 0.5) +
  guides(colour = guide_legend(override.aes = list(linetype = 0)))
ggsave("plots/Fixation_without_null_no_zero_scenario_study1.pdf", width = 9, height = 6, device = cairo_pdf)







data_only_pedestrian <- subset(data_long_1, scenario == "Pedestrian")

data_only_pedestrian |> ggplot() +
  aes(x = ehmi, y = measurement * 100, fill = aoi, colour = aoi, group = aoi) +
  scale_color_see() +
  ylab("Percentage Fixation - Excluded Null\n Only Pedestrians") +
  theme(legend.position.inside = c(0.9, 0.78)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi), alpha = 0.9) +
  scale_shape_manual(values = 1:nlevels(data_only_pedestrian$aoi)) +
  # stat_summary(fun = mean, geom = "line", linewidth = 1, aes(linetype= aoi)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1, alpha = 0.5) +
  guides(colour = guide_legend(override.aes = list(linetype = 0)))
ggsave("plots/Fixation_without_null_no_zero_ehmi_only_pedestrian_study1.pdf", width = 9, height = 6, device = cairo_pdf)


data_only_pedestrian |> ggplot() +
  aes(x = distraction, y = measurement * 100, fill = aoi, colour = aoi, group = aoi) +
  scale_color_see() +
  ylab("Percentage Fixation - Excluded Null\n Only Pedestrians") +
  theme(legend.position.inside = c(0.65, 0.8)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi), alpha = 0.9) +
  scale_shape_manual(values = 1:nlevels(data_only_pedestrian$aoi)) +
  # stat_summary(fun = mean, geom = "line", linewidth = 1, aes(linetype= aoi)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1, alpha = 0.5) +
  guides(colour = guide_legend(override.aes = list(linetype = 0)))
ggsave("plots/Fixation_without_null_no_zero_distraction_only_pedestrian_study1.pdf", width = 9, height = 6, device = cairo_pdf)


p <- data_only_pedestrian |> ggplot() +
  aes(x = distraction, y = measurement * 100, fill = aoi, colour = aoi, group = aoi) +
  scale_color_see() +
  ylab("Percentage Fixation - Excluded Null\n Only Pedestrians") +
  theme(legend.position.inside = c(0.4, 0.3)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi), alpha = 0.9) +
  scale_shape_manual(values = 1:nlevels(data_only_pedestrian$aoi)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1, alpha = 0.5) +
  guides(colour = guide_legend(override.aes = list(linetype = 0)))
p + facet_grid(~ehmi, ) +   theme(panel.spacing = unit(.1, "lines"),
                                  panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                  strip.background = element_rect(color = "black", size = 0.5))
ggsave("plots/Fixation_without_null_no_zero_distraction_only_pedestrian_all.pdf", width = 9, height = 6, device = cairo_pdf)




data_only_bicycle <- subset(data_long_1, scenario == "Cyclist")

data_only_bicycle |> ggplot() +
  aes(x = ehmi, y = measurement * 100, fill = aoi, colour = aoi, group = aoi) +
  scale_color_see() +
  ylab("Percentage Fixation - Excluded Null\n Only Cyclist") +
  theme(legend.position.inside = c(0.9, 0.78)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi), alpha = 0.9) +
  scale_shape_manual(values = 1:nlevels(data_only_bicycle$aoi)) +
  # stat_summary(fun = mean, geom = "line", linewidth = 1, aes(linetype= aoi)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1, alpha = 0.5) +
  guides(colour = guide_legend(override.aes = list(linetype = 0)))
ggsave("plots/Fixation_without_null_no_zero_ehmi_only_bicycle_study1.pdf", width = 9, height = 6, device = cairo_pdf)


data_only_bicycle |> ggplot() +
  aes(x = distraction, y = measurement * 100, fill = aoi, colour = aoi, group = aoi) +
  scale_color_see() +
  ylab("Percentage Fixation - Excluded Null\n Only Cyclist") +
  theme(legend.position.inside = c(0.65, 0.8)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi), alpha = 0.9) +
  scale_shape_manual(values = 1:nlevels(data_only_bicycle$aoi)) +
  # stat_summary(fun = mean, geom = "line", linewidth = 1, aes(linetype= aoi)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1, alpha = 0.5) +
  guides(colour = guide_legend(override.aes = list(linetype = 0)))
ggsave("plots/Fixation_without_null_no_zero_distraction_only_bicycle_study1.pdf", width = 9, height = 6, device = cairo_pdf)



p <- data_only_bicycle |> ggplot() +
  aes(x = distraction, y = measurement * 100, fill = aoi, colour = aoi, group = aoi) +
  scale_color_see() +
  ylab("Percentage Fixation - Excluded Null\n Only Cyclists") +
  theme(legend.position.inside = c(0.4, 0.3)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi), alpha = 0.9) +
  scale_shape_manual(values = 1:nlevels(data_only_bicycle$aoi)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1, alpha = 0.5) +
  guides(colour = guide_legend(override.aes = list(linetype = 0)))
p + facet_grid(~ehmi, ) +   theme(panel.spacing = unit(.1, "lines"),
                                  panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                  strip.background = element_rect(color = "black", size = 0.5))
ggsave("plots/Fixation_without_null_no_zero_distraction_only_cyclist_all.pdf", width = 9, height = 6, device = cairo_pdf)




data_only_driver <- subset(data_long_1, scenario == "Driver")

data_only_driver |> ggplot() +
  aes(x = ehmi, y = measurement * 100, fill = aoi, colour = aoi, group = aoi) +
  scale_color_see() +
  ylab("Percentage Fixation - Excluded Null\n Only Driver") +
  theme(legend.position.inside = c(0.9, 0.78)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi), alpha = 0.9) +
  scale_shape_manual(values = 1:nlevels(data_only_driver$aoi)) +
  # stat_summary(fun = mean, geom = "line", linewidth = 1, aes(linetype= aoi)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1, alpha = 0.5) +
  guides(colour = guide_legend(override.aes = list(linetype = 0)))
ggsave("plots/Fixation_without_null_no_zero_ehmi_only_driver_study1.pdf", width = 9, height = 6, device = cairo_pdf)


data_only_driver |> ggplot() +
  aes(x = distraction, y = measurement * 100, fill = aoi, colour = aoi, group = aoi) +
  scale_color_see() +
  ylab("Percentage Fixation - Excluded Null\n Only Driver") +
  theme(legend.position.inside = c(0.65, 0.8)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi), alpha = 0.9) +
  scale_shape_manual(values = 1:nlevels(data_only_driver$aoi)) +
  # stat_summary(fun = mean, geom = "line", linewidth = 1, aes(linetype= aoi)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1, alpha = 0.5) +
  guides(colour = guide_legend(override.aes = list(linetype = 0)))
ggsave("plots/Fixation_without_null_no_zero_distraction_only_driver_study1.pdf", width = 9, height = 6, device = cairo_pdf)


p <- data_only_driver |> ggplot() +
  aes(x = distraction, y = measurement * 100, fill = aoi, colour = aoi, group = aoi) +
  scale_color_see() +
  ylab("Percentage Fixation - Excluded Null\n Only Driver") +
  theme(legend.position.inside = c(0.25, 0.9)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, mapping = aes(shape = aoi), alpha = 0.9) +
  scale_shape_manual(values = 1:nlevels(data_only_driver$aoi)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1, alpha = 0.5) +
  guides(colour = guide_legend(override.aes = list(linetype = 0)))
p + facet_grid(~ehmi, ) +   theme(panel.spacing = unit(.1, "lines"),
                                  panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
                                  strip.background = element_rect(color = "black", size = 0.5))
ggsave("plots/Fixation_without_null_no_zero_distraction_only_driver_all.pdf", width = 9, height = 6, device = cairo_pdf)




# collision with cars
mean(combined_df_time$collision.with.cars)
sd(combined_df_time$collision.with.cars)
table(combined_df_time$collision.with.cars)



# checkAssumptionsForAnova(data = data_long_1, y = "measurement", factors = c("scenario", "distraction", "ehmi"))
# 
# modelArt <- art(measurement ~ scenario * visualization * aoi + Error(pid / (scenario * visualization * aoi)), data = data_long_1) |> anova()
# modelArt
# reportART(modelArt, dv = "AOI fixation")






levels(combined_df$focused.object)





#### CHI'26 review requests:
# Eye-tracking results are reported as AOI percentages, but interpretation is limited. 
# Present more detailed gaze metrics (time to first fixation, fixation duration on AV eHMI, saccade ‎patterns) if available, 
# and relate them to subjective measures (e.g., higher fixation on eHMI ‎correlates with higher understandability).‎



# Ensure data is sorted chronologically per user/trial
combined_df <- combined_df |>
  arrange(user_id, scenario, distraction, ehmi, time)

# Convert to data.table for efficient calculation
dt_combined <- as.data.table(combined_df)

dt_combined$time <- as.numeric(dt_combined$time)

dt_combined <- dt_combined[time <= 50 & time >= 0]

# 1. Calculate the duration of each sample (row)
# Assuming 'time' is in seconds. If it's a timestamp, ensure it is numeric.
dt_combined[, dt := shift(time, type = "lead") - time, by = .(user_id, scenario, distraction, ehmi)]

# catch outliers
dt_combined <- dt_combined[dt >= 0]

# Handle the last row of each group (which will be NA) by replacing with median or 0
dt_combined[is.na(dt), dt := 0] 

# 2. Identify discrete "Fixation Events"
# rleid increments every time the focused.object changes within the group
dt_combined[, fixation_id := rleid(focused.object), by = .(user_id, scenario, distraction, ehmi)]

# 3. Aggregate into a Fixations Table (One row per fixation event, not per frame)
fixation_events <- dt_combined[, .(
  start_time = min(time),
  end_time = max(time),
  duration = sum(dt, na.rm = TRUE),
  aoi = first(focused.object)
), by = .(user_id, scenario, distraction, ehmi, fixation_id)]

# Remove "Noise" or "Null" fixations if desired (optional)
fixation_events <- fixation_events[!aoi %in% c("Noise", "null", "Null", " ")]






# Step 2: Calculate "Time to First Fixation" (TTFF) & "Average Fixation Duration"


# Calculate Trial Start Times (to normalize TTFF)
trial_starts <- dt_combined[, .(trial_start = min(time, na.rm = TRUE)), by = .(user_id, scenario, distraction, ehmi)]

# Merge trial starts into fixation events
fixation_events <- merge(fixation_events, trial_starts, by = c("user_id", "scenario", "distraction", "ehmi"))

# --- Metric 1: Time to First Fixation (TTFF) ---
# Find the first time an AOI was looked at per trial
ttff_df <- fixation_events[, .(
  first_fixation_time = min(start_time),
  ttff = min(start_time) - first(trial_start)
), by = .(user_id, scenario, distraction, ehmi, aoi)]

# --- Metric 2: Average Fixation Duration ---
# How long, on average, does a participant look at the AOI once they fixate on it?
avg_dur_df <- fixation_events[, .(
  avg_fixation_duration = mean(duration),
  total_fixation_duration = sum(duration),
  fixation_count = .N
), by = .(user_id, scenario, distraction, ehmi, aoi)]

# Combine metrics into one dataframe
gaze_metrics_df <- merge(ttff_df, avg_dur_df, by = c("user_id", "scenario", "distraction", "ehmi", "aoi"))

# View result
head(gaze_metrics_df)



# Step 3: Visualize and Analyze the New Metrics

# Time to First Fixation (TTFF)

# Filter for just the AV or eHMI object
av_ttff <- subset(gaze_metrics_df, aoi == "AV") 

# Plot TTFF
ggplot(av_ttff, aes(x = distraction, y = ttff, fill = ehmi)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_point(position = position_jitterdodge(), alpha = 0.3) +
  scale_fill_see() +
  theme(legend.position.inside = c(0.75, 0.8), axis.text.x = element_text(size = 12)) +
  ylab("Time to First Fixation on AV (s)") +
  xlab("Distraction") +
  facet_wrap(~scenario)
ggsave("plots/TTFF_AV.pdf", width = 8, height = 6)

# Stats for TTFF - not adequate
modelArt <- art(ttff ~ distraction * ehmi * scenario + Error(user_id / (distraction * ehmi * scenario)), data = av_ttff)|> anova()
modelArt
reportART(modelArt, dv = "ttff")

levels(av_ttff$scenario)


model_lmm <- lmer(ttff ~ distraction * ehmi * scenario + (1 | user_id), 
                  data = av_ttff)

# 3. View Results
anova(model_lmm)

report::report(model_lmm) |> latexify_report(only_sig = TRUE)

plot_model(model_lmm, 
             type = "pred", 
             terms = c("distraction", "ehmi", "scenario"),
             show.data = TRUE,        # This adds the points automatically
             jitter = 0.2,            # Controls the jitter width
             dot.alpha = 0.2,         # Controls transparency
             title = "Predicted Time to First Fixation (TTFF)",
             axis.title = c("Distraction Level", "TTFF")) + 
    theme_sjplot() +
    scale_color_see()
ggsave("plots/predicted_TTFF_AV.pdf", width = 8, height = 6)






















#### MERGING (only works if main_df is present) ####

# --- CALCULATE METRICS ---
gaze_metrics <- fixation_events[, .(
  TTFF = min(start_time) - first(trial_start),    # Time to First Fixation
  Avg_Fixation_Duration = mean(duration),         # Average Duration
  Total_Fixation_Duration = sum(duration)         # Total Duration
), by = .(user_id, scenario, distraction, ehmi, aoi)]

# Filter for the "AV" object only (since we want to correlate AV attention with AV Trust)
# Ensure "AV" matches your actual object name (e.g., "AV", "Intention-Based eHMI", "Box_Collider")
gaze_metrics_av <- gaze_metrics[aoi == "AV"]


# --- 1. Harmonize Distraction Names ---
# Convert to character to allow editing
gaze_metrics_av$distraction <- as.character(gaze_metrics_av$distraction)
main_df$Distraction <- as.character(main_df$Distraction)

# Remove any leading/trailing whitespace (often the culprit in CSVs)
gaze_metrics_av$distraction <- trimws(gaze_metrics_av$distraction)
main_df$Distraction <- trimws(main_df$Distraction)

# RENAME "None" to "No Distraction" in the eye-tracking data
gaze_metrics_av$distraction[gaze_metrics_av$distraction == "None"] <- "No Distraction"



# 1. Clean IDs to ensure they match
# Assuming main_df$UserID is "1", "2" and gaze_metrics$user_id is "1", "2"
# If one is numeric and one is factor, force them to character to be safe
main_df$UserID_join <- as.character(main_df$UserID)
gaze_metrics_av$user_id_join <- as.character(gaze_metrics_av$user_id)

# 2. Clean Factor Levels to ensure they match
# Check if Distraction levels match exactly
# main_df uses: "No Distraction", "Noise", "Interference"
# gaze_metrics usually uses: "Noise", "Interference" (and maybe blank for None)
# You might need to harmonize them here:
levels(main_df$Distraction)
unique(gaze_metrics_av$distraction)

# 3. Perform the Merge
# We use inner_join to keep only data present in both sets
combined_analysis <- main_df |>
  mutate(UserID = as.character(UserID),
         Scenario = as.character(Scenario),
         Distraction = as.character(Distraction),
         EHMI = as.character(EHMI)) |>
  inner_join(gaze_metrics_av, 
             by = c("UserID" = "user_id", 
                    "Scenario" = "scenario", 
                    "Distraction" = "distraction", 
                    "EHMI" = "ehmi"))

# Check how many rows we have (should roughly match main_df size)
print(paste("Rows for correlation analysis:", nrow(combined_analysis)))





# Function to report correlation
report_correlation <- function(data, x_col, y_col, method = "spearman") {
  res <- cor.test( data[[x_col]], data[[y_col]], method = method)
    print(paste0("--- Correlation: ", x_col, " vs ", y_col, " ---"))
    print(paste("rho =", round(res$estimate, 3), ", p =", round(res$p.value, 4)))
  return(res)
}

# 1. Does looking longer at the AV correlate with better Understanding?
report_correlation(combined_analysis, "Total_Fixation_Duration", "TrustUnderstanding")
report(cor.test(combined_analysis$Total_Fixation_Duration, combined_analysis$TrustUnderstanding, method = "spearman"))

# 2. Does noticing the AV faster (lower TTFF) correlate with higher Perceived Safety?
report_correlation(combined_analysis, "TTFF", "psScore")
report(cor.test(combined_analysis$TTFF, combined_analysis$psScore, method = "spearman"))

# 3. Does Average Fixation Duration correlate with Trust?
report_correlation(combined_analysis, "Avg_Fixation_Duration", "Trust")
report(cor.test(combined_analysis$Avg_Fixation_Duration, combined_analysis$Trust, method = "spearman"))

# 4. Does Total Fixation Duration correlate with Mental Demand (TLX)?
report_correlation(combined_analysis, "Total_Fixation_Duration", "TLX1")
report(cor.test(combined_analysis$Total_Fixation_Duration, combined_analysis$TLX1, method = "spearman"))







# Plot 1: Total Fixation Duration vs. Understanding
ggplot(combined_analysis, aes(x = Total_Fixation_Duration, y = TrustUnderstanding)) +
  geom_point(aes(color = Scenario), alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", color = "black", se = TRUE) + # Add trend line
  scale_color_see() +
  labs(
    title = "Correlation: Gaze Duration vs. Understanding",
    x = "Total Time Looking at AV (s)",
    y = "Subjective Understanding Score"
  ) +
  theme(legend.position = "bottom")

ggsave("plots/Corr_Fixation_Understanding.pdf", width = 7, height = 6)


# Plot 2: Time to First Fixation (TTFF) vs. Perceived Safety
ggplot(combined_analysis, aes(x = TTFF, y = psScore)) +
  geom_point(aes(color = Scenario), alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  scale_color_see() +
  labs(
    title = "Correlation: Attentional Capture vs. Safety",
    x = "Time to First Fixation on AV (s)",
    y = "Perceived Safety Score"
  ) +
  theme(legend.position = "bottom")

ggsave("plots/Corr_TTFF_Safety.pdf", width = 7, height = 6)











