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
library(tidyr)

main_df <- readxl::read_xlsx(path = "results-survey-main.xlsx")
main_df <- as.data.frame(main_df)
names(main_df)




second_df <- readxl::read_xlsx(path = "results-survey-final.xlsx")
second_df <- as.data.frame(second_df)
names(second_df)


# replace all negative values with the actual values
# sometimes use either
main_df <- colleyRstats::replace_values(main_df, c("neg3", "neg2", "neg1"), c("-3", "-2", "-1"))
main_df <- colleyRstats::replace_values(main_df, c("3neg", "2neg", "1neg"), c("-3", "-2", "-1"))


main_df[, 8:38] <- sapply(main_df[, 8:38], as.numeric)

second_df[, 13:14] <- sapply(second_df[, 13:14], as.numeric)
second_df[, 17:19] <- sapply(second_df[, 17:19], as.numeric)


# main_df$ConditionID <- as.factor(main_df$ConditionID)
main_df$UserID <- as.factor(main_df$UserID)
second_df$UserID <- as.factor(second_df$UserID)


# Scenario:
main_df$Scenario <- factor(
  ifelse(main_df$ConditionID %in% c("A", "B", "C", "D", "E", "F"), "Pedestrian",
    ifelse(main_df$ConditionID %in% c("G", "H", "I", "J", "K", "L"), "Driver", "Cyclist")
  ),
  levels = c("Pedestrian", "Driver", "Cyclist")
)
# EHMI:
main_df$EHMI <- factor(ifelse(main_df$ConditionID %in% c("A", "C", "E", "G", "I", "K", "M", "O", "Q"), "No eHMI", "Intention-Based eHMI"),
  levels = c("No eHMI", "Intention-Based eHMI")
)
# Distraction:
main_df$Distraction <- factor(
  ifelse(main_df$ConditionID %in% c("A", "B", "G", "H", "M", "N"), "No Distraction",
    ifelse(main_df$ConditionID %in% c("C", "D", "I", "J", "O", "P"), "Noise", "Interference")
  ),
  levels = c("No Distraction", "Noise", "Interference")
)


# SUS
main_df$SUSScore <- ((main_df$SUS1 - 1) + (main_df$SUS3 - 1) + (main_df$SUS5 - 1) + (main_df$SUS7 - 1) + (main_df$SUS9 - 1) + (5 - main_df$SUS2) + (5 - main_df$SUS4) + (5 - main_df$SUS6) + (5 - main_df$SUS8) + (5 - main_df$SUS10)) * 2.5


# Perceived Safety
main_df$psScore <- (main_df$PerSafe01 + main_df$PerSafe02 + main_df$PerSafe03 + main_df$PerSafe04) / 4.0

# Assessment of Acceptance - van der Laan
main_df$AOAUsefulness <- (main_df$aoa1 - main_df$aoa3 + main_df$aoa5 + main_df$aoa7 + main_df$aoa9) / 5.0
main_df$AOASatisfying <- (main_df$aoa2 + main_df$aoa4 - main_df$aoa6 - main_df$aoa8) / 4.0

# Trust
main_df$Trust <- rowSums(main_df[, c("TiA01", "TiA02")]) / 2.0

# Calculate relevant trust scores:
# tiau 2 und 4 inverse
main_df$TiA04 <- 6 - main_df$TiA04
main_df$TiA06 <- 6 - main_df$TiA06
main_df$TrustUnderstanding <- rowSums(main_df[, c("TiA03", "TiA04", "TiA05", "TiA06")]) / 4.0


# TLX1 and own
# nothing to do


levels(main_df$EHMI)
levels(main_df$Scenario)
levels(main_df$Distraction)
levels(main_df$UserID)



main_df <- main_df |>
  dplyr::group_by(UserID) |>
  dplyr::arrange(UserID, .by_group = TRUE) |>
  dplyr::mutate(
    trial = dplyr::row_number()
  ) |>
  dplyr::ungroup()

# treat order as factor for ART
main_df$trial_f <- factor(main_df$trial)


dv_list <- c(
  "TLX1",
  "psScore",
  "SUSScore",
  "AOAUsefulness",
  "AOASatisfying",
  "Trust",
  "TrustUnderstanding",
  "own"
)

order_effect_results <- lapply(dv_list, function(dv) {
  f <- stats::as.formula(paste0(dv, " ~ trial_f + Error(UserID/trial_f)"))
  model_art <- ARTool::art(f, data = main_df) |> anova()
})
names(order_effect_results) <- dv_list

order_effect_results


order_long <- main_df |>
  dplyr::select(UserID, trial, dplyr::all_of(dv_list)) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(dv_list),
    names_to = "DV",
    values_to = "value"
  )

p_order <- ggplot2::ggplot(order_long,
                           ggplot2::aes(x = trial, y = value, group = 1)) +
  ggplot2::stat_summary(fun = mean, geom = "line") +
  ggplot2::stat_summary(fun = mean, geom = "point") +
  ggplot2::stat_summary(
    fun.data = "mean_cl_boot",
    geom = "errorbar",
    width = 0.2
  ) +
  ggplot2::facet_wrap(~ DV, scales = "free_y") +
  ggplot2::xlab("Trial") +
  ggplot2::ylab("Score")

p_order

ggplot2::ggsave(
  filename = "plots/order_effects_by_trial.pdf",
  plot = p_order,
  width = 11,
  height = 6.5,
  device = cairo_pdf
)




# 1. Prepare the Data: Rename variables to match Paper Terminology
# We create a new column 'DV_Label' with nice names
order_long_clean <- order_long |>
  mutate(DV_Label = factor(DV, 
                           levels = c("TLX1", "psScore", "SUSScore", "AOAUsefulness", 
                                      "AOASatisfying", "Trust", "TrustUnderstanding", "own"),
                           labels = c("Mental Demand", 
                                      "Perceived Safety", 
                                      "Usability (SUS)", 
                                      "Usefulness", 
                                      "Satisfaction", 
                                      "Trust", 
                                      "Understandability", 
                                      "Env. Interference"))) # "Interference of Environment"

# 2. Create the Plot
p_order <- ggplot(order_long_clean, aes(x = trial, y = value)) +
  # Add a faint trend line (optional, helps visualize trajectory)
  stat_summary(fun = mean, geom = "line", color = "#2E86C1", linewidth = 0.8, alpha = 0.8) +
  
  # Add Error bars (95% CI via bootstrapping)
  stat_summary(
    fun.data = "mean_cl_boot",
    geom = "errorbar",
    width = 0.3,
    color = "gray40",
    linewidth = 0.4
  ) +
  
  # Add Mean Points on top
  stat_summary(fun = mean, geom = "point", shape = 21, fill = "white", color = "#2E86C1", size = 2, stroke = 1) +
  
  # Facet by the renamed variable
  facet_wrap(~ DV_Label, scales = "free_y", ncol = 4) +
  
  # Axis and Labels
  scale_x_continuous(breaks = seq(1, 18, by = 4)) + # Assuming 18 trials, show every 4th tick
  labs(
    x = "Trial Number (Chronological Order)",
    y = "Mean Score (with 95% CI)",
    caption = "Error bars represent 95% Confidence Intervals"
  ) +
  
  # Apply a clean theme
  theme(
    panel.grid.minor = element_blank(),     # Remove minor grid lines
    strip.background = element_rect(fill = "gray95"), # Light gray background for titles
    strip.text = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold"),
    plot.caption = element_text(size = 8, color = "gray50")
  )

# Display
print(p_order)

# 3. Save
ggsave(
  filename = "plots/order_effects_by_trial_clean.pdf",
  plot = p_order,
  width = 12,
  height = 7,
  device = cairo_pdf
)






# TLX1
checkAssumptionsForAnova(data = main_df, y = "TLX1", factors = c("Scenario", "Distraction", "EHMI"))

modelArt <- art(TLX1 ~ Scenario * Distraction * EHMI + Error(UserID / (Scenario * Distraction * EHMI)), data = main_df) |> anova()
modelArt


# TODO: this is the proposed way in the CHI25 review - is more sensitive (i.e., faster significant)
# see https://cran.r-project.org/web/packages/ARTool/ARTool.pdf#page=6.08
# "ART-C procedure"
model <- art(TLX1 ~ Scenario * Distraction * EHMI + Error(UserID / (Scenario * Distraction * EHMI)), data = main_df)
model
art.con(model, "Scenario")
art.con(model, "Distraction")

modelArt
reportART(modelArt, dv = "mental demand")

eta_squared(modelArt)



dunnTest(TLX1 ~ Scenario, data = main_df, method = "holm") |> reportDunnTest(data = main_df, iv = "Scenario", dv = "TLX1")
dunnTest(TLX1 ~ Distraction, data = main_df, method = "holm") |> reportDunnTest(data = main_df, iv = "Distraction", dv = "TLX1")
#dunnTest(TLX1 ~ EHMI, data = main_df, method = "holm") |> reportDunnTest(data = main_df, iv = "EHMI", dv = "TLX1")
reportMeanAndSD(data = main_df, iv = "EHMI", dv = "TLX1")
effectsize::rank_biserial(as.formula(paste("TLX1", "~", "EHMI")), data = main_df)





# Perceived safety
checkAssumptionsForAnova(data = main_df, y = "psScore", factors = c("Scenario", "Distraction", "EHMI"))

modelArt <- art(psScore ~ Scenario * Distraction * EHMI + Error(UserID / (Scenario * Distraction * EHMI)), data = main_df) |> anova()
modelArt
reportART(modelArt, dv = "perceived safety")


dunnTest(psScore ~ Scenario, data = main_df, method = "holm") |> reportDunnTest(data = main_df, iv = "Scenario", dv = "psScore")
# dunnTest(psScore ~ EHMI, data = main_df, method = "holm") |> reportDunnTest(data = main_df, iv = "EHMI", dv = "psScore")
reportMeanAndSD(data = main_df, iv = "EHMI", dv = "psScore")
effectsize::rank_biserial(as.formula(paste("psScore", "~", "EHMI")), data = main_df)



# SUS
checkAssumptionsForAnova(data = main_df, y = "SUSScore", factors = c("Scenario", "Distraction", "EHMI"))

modelArt <- art(SUSScore ~ Scenario * Distraction * EHMI + Error(UserID / (Scenario * Distraction * EHMI)), data = main_df) |> anova()
modelArt
reportART(modelArt, dv = "SUS")

es <- omega_squared(modelArt, partial = TRUE)
es
# The output order matches your ANOVA table
# Row 7 is your three-way interaction: ω² = 0.04

# To make it clearer, you can manually add names:
effect_names <- c(
  "Scenario",
  "Distraction", 
  "EHMI",
  "Scenario:Distraction",
  "Scenario:EHMI",
  "Distraction:EHMI",
  "Scenario:Distraction:EHMI"
)

es_df <- as.data.frame(es)
es_df$Effect <- effect_names
print(es_df)



# three way interaction
p <- main_df |> ggplot() +
  aes(x = Scenario, y = SUSScore, fill = Distraction, colour = Distraction, group = Distraction) +
  scale_color_see() +
  ylab("SUS") +
  theme(legend.position.inside = c(0.65, 0.35)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.05))
p + facet_grid(~EHMI)
ggsave("plots/SUS_3WAY.pdf", width = 9, height = 6.5, device = cairo_pdf)




# AOAUsefulness
checkAssumptionsForAnova(data = main_df, y = "AOAUsefulness", factors = c("Scenario", "Distraction", "EHMI"))

modelArt <- art(AOAUsefulness ~ Scenario * Distraction * EHMI + Error(UserID / (Scenario * Distraction * EHMI)), data = main_df) |> anova()
modelArt
reportART(modelArt, dv = "AOAUsefulness")

dunnTest(AOAUsefulness ~ Scenario, data = main_df, method = "holm") |> reportDunnTest(data = main_df, iv = "Scenario", dv = "AOAUsefulness")
# dunnTest(AOAUsefulness ~ EHMI, data = main_df, method = "holm") |> reportDunnTest(data = main_df, iv = "EHMI", dv = "AOAUsefulness")
reportMeanAndSD(data = main_df, iv = "EHMI", dv = "AOAUsefulness")
effectsize::rank_biserial(as.formula(paste("AOAUsefulness", "~", "EHMI")), data = main_df)






# AOASatisfying
checkAssumptionsForAnova(data = main_df, y = "AOASatisfying", factors = c("Scenario", "Distraction", "EHMI"))


modelArt <- art(AOASatisfying ~ Scenario * Distraction * EHMI + Error(UserID / (Scenario * Distraction * EHMI)), data = main_df) |> anova()
modelArt
reportART(modelArt, dv = "AOASatisfying")



dunnTest(AOASatisfying ~ Scenario, data = main_df, method = "holm") |> reportDunnTest(data = main_df, iv = "Scenario", dv = "AOASatisfying")
#dunnTest(AOASatisfying ~ EHMI, data = main_df, method = "holm") |> reportDunnTest(data = main_df, iv = "Scenario", dv = "AOASatisfying")
# dunnTest(AOASatisfying ~ EHMI, data = main_df, method = "holm") |> reportDunnTest(data = main_df, iv = "EHMI", dv = "AOASatisfying")
reportMeanAndSD(data = main_df, iv = "EHMI", dv = "AOASatisfying")
effectsize::rank_biserial(as.formula(paste("AOASatisfying", "~", "EHMI")), data = main_df)






# Trust
checkAssumptionsForAnova(data = main_df, y = "Trust", factors = c("Scenario", "Distraction", "EHMI"))


modelArt <- art(Trust ~ Scenario * Distraction * EHMI + Error(UserID / (Scenario * Distraction * EHMI)), data = main_df) |> anova()
modelArt
reportART(modelArt, dv = "Trust")

dunnTest(Trust ~ Scenario, data = main_df, method = "holm") |> reportDunnTest(data = main_df, iv = "Scenario", dv = "Trust")
dunnTest(Trust ~ Distraction, data = main_df, method = "holm") |> reportDunnTest(data = main_df, iv = "Distraction", dv = "Trust")
reportMeanAndSD(data = main_df, iv = "EHMI", dv = "Trust")
effectsize::rank_biserial(as.formula(paste("Trust", "~", "EHMI")), data = main_df)





main_df |> ggplot() +
  aes(x = Scenario, y = Trust, fill = Distraction, colour = Distraction, group = Distraction) +
  scale_color_see() +
  ylab("Trust") +
  # ylim(1,5) +
  theme(legend.position.inside = c(0.65, 0.85)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.05))
ggsave("plots/Trust_2WAY.pdf", width = 9, height = 6.5, device = cairo_pdf)


main_df |> ggplot() +
  aes(x = Scenario, y = Trust, fill = Distraction, colour = Distraction, group = Distraction) +
  scale_color_see() +
  ylab("Trust") +
  ylim(1, 5) +
  theme(legend.position.inside = c(0.65, 0.85)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.05))
ggsave("plots/Trust_2WAY_Big.pdf", width = 9, height = 6.5, device = cairo_pdf)







# TrustUnderstanding
checkAssumptionsForAnova(data = main_df, y = "TrustUnderstanding", factors = c("Scenario", "Distraction", "EHMI"))


modelArt <- art(TrustUnderstanding ~ Scenario * Distraction * EHMI + Error(UserID / (Scenario * Distraction * EHMI)), data = main_df) |> anova()
modelArt
reportART(modelArt, dv = "Understanding")

dunnTest(TrustUnderstanding ~ Scenario, data = main_df, method = "holm") |> reportDunnTest(data = main_df, iv = "Scenario", dv = "TrustUnderstanding")
dunnTest(TrustUnderstanding ~ Distraction, data = main_df, method = "holm") |> reportDunnTest(data = main_df, iv = "Distraction", dv = "TrustUnderstanding")
reportMeanAndSD(data = main_df, iv = "EHMI", dv = "TrustUnderstanding")
effectsize::rank_biserial(as.formula(paste("TrustUnderstanding", "~", "EHMI")), data = main_df)



main_df |> ggplot() +
  aes(x = Scenario, y = TrustUnderstanding, fill = Distraction, colour = Distraction, group = Distraction) +
  scale_color_see() +
  ylab("Understanding") +
  # ylim(1,5) +
  theme(legend.position.inside = c(0.35, 0.25)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.05))
ggsave("plots/TrustUnderstanding_2WAY.pdf", width = 9, height = 6.5, device = cairo_pdf)


main_df |> ggplot() +
  aes(x = Scenario, y = TrustUnderstanding, fill = Distraction, colour = Distraction, group = Distraction) +
  scale_color_see() +
  ylab("Understanding") +
  ylim(1, 5) +
  theme(legend.position.inside = c(0.65, 0.85)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.05))
ggsave("plots/TrustUnderstanding_2WAY_Big.pdf", width = 9, height = 6.5, device = cairo_pdf)



# own
# "The environment interfered in the communication with the automated vehicle"
checkAssumptionsForAnova(data = main_df, y = "own", factors = c("Scenario", "Distraction", "EHMI"))


modelArt <- art(own ~ Scenario * Distraction * EHMI + Error(UserID / (Scenario * Distraction * EHMI)), data = main_df) |> anova()
modelArt
reportART(modelArt, dv = "own")

dunnTest(own ~ Distraction, data = main_df, method = "holm") |> reportDunnTest(data = main_df, iv = "Distraction", dv = "own")



main_df |> ggplot() +
  aes(x = Distraction, y = own, fill = EHMI, colour = EHMI, group = EHMI) +
  scale_color_see() +
  ylab("Interference") +
  theme(legend.position.inside = c(0.65, 0.85)) +
  xlab("") +
  stat_summary(fun = mean, geom = "point", size = 4.0, alpha = 0.9) +
  stat_summary(fun = mean, geom = "line", linewidth = 2, alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .5, position = position_dodge(width = 0.05), alpha = 0.5)
ggsave("plots/interference_ie.pdf", width = 9, height = 6.5, device = cairo_pdf)



if (!require(plyr)) {
  install.packages("plyr")
  library(plyr)
}
# final results data set
# age
mean(second_df$age)
sd(second_df$age)
# table(second_df$age) -- irrelevant


# gender
# mean(second_df$gender) -- irrelevant
# sd(second_df$gender) -- irrelevant
second_df$gender <- as.character(second_df$gender)
gender_map <- c("1" = "Female", "2" = "Male", "3" = "Non-binary")
second_df$gender <- revalue(second_df$gender, gender_map)
table(second_df$gender)


# education
# Erstellen ein benanntes Vektor-Mapping
education_map <- c("A3" = "High School", "A4" = "College", "A5" = "Vocational Training")
# Ändere die Werte in der Spalte 'education'
second_df$education <- revalue(second_df$education, education_map)
table(second_df$education)

# job
job_map <- c("A2" = "Student (college)", "A3" = "Employee")
second_df$job <- revalue(second_df$job, job_map)
table(second_df$job)

# #ranking1
# table(second_df$`ranking[1]`)
# levels(second_df$`ranking[1]`)[1] <- "Pedestrian"
# levels(second_df$`ranking[1]`)[2] <- "Car"
# levels(second_df$`ranking[1]`)[3] <- "Bicycle"
#
# pct <- round(prop.table(table(second_df$`ranking[1]`))*100)   # proz. Anteile
# lbls <- paste(levels(second_df$`ranking[1]`), " (", pct,"%)",sep="")  # Labels
#
# par(mfrow = c(1, 2), cex = 0.8, cex.axis = 0.7)
# barplot(table(second_df$`ranking[1]`), main = "Ranking 1")
#
# #ranking2
# table(second_df$`ranking[2]`)
# levels(second_df$`ranking[2]`)[1] <- "Pedestrian"
# levels(second_df$`ranking[2]`)[2] <- "Car"
# levels(second_df$`ranking[2]`)[3] <- "Bicycle"
#
# pct <- round(prop.table(table(second_df$`ranking[2]`))*100)   # proz. Anteile
# lbls <- paste(levels(second_df$`ranking[2]`), " (", pct,"%)",sep="")  # Labels
#
# par(mfrow = c(1, 2), cex = 0.8, cex.axis = 0.7)
# barplot(table(second_df$`ranking[2]`), main = "Ranking 2")
#
# #ranking3
# table(second_df$`ranking[3]`)
# levels(second_df$`ranking[3]`)[1] <- "Pedestrian"
# levels(second_df$`ranking[3]`)[2] <- "Car"
# levels(second_df$`ranking[3]`)[3] <- "Bicycle"
#
# pct <- round(prop.table(table(second_df$`ranking[3]`))*100)   # proz. Anteile
# lbls <- paste(levels(second_df$`ranking[3]`), " (", pct,"%)",sep="")  # Labels
#
# par(mfrow = c(1, 2), cex = 0.8, cex.axis = 0.7)
# barplot(table(second_df$`ranking[3]`), main = "Ranking 3")


# interest[interest]
# table(second_df$`interest[interest]`) -- irrelevant
mean(second_df$`interest[interest]`)
sd(second_df$`interest[interest]`)

# interest[ease]
# table(second_df$`interest[ease]`) -- irrelevant
mean(second_df$`interest[ease]`)
sd(second_df$`interest[ease]`)

# interest[reality]
# table(second_df$`interest[reality]`) -- irrelevant
mean(second_df$`interest[reality]`)
sd(second_df$`interest[reality]`)



library(report)
report::report_participants(second_df)







ranking <- NULL
ranking <- second_df[, 7:9] # only the rankings



ranking$cycle <- which(apply(ranking[, c(1:3)], 1, function(x) grepl("cycle", x)), arr.ind = TRUE)[, 1]
ranking$drive <- which(apply(ranking[, c(1:3)], 1, function(x) grepl("drive", x)), arr.ind = TRUE)[, 1]
ranking$ped <- which(apply(ranking[, c(1:3)], 1, function(x) grepl("ped", x)), arr.ind = TRUE)[, 1]


ranking_number <- ranking[, c(4:6)]
names(ranking_number)

data_long <- gather(ranking_number, key = ConditionID, value = rank, factor_key = TRUE)
data_long

rank_x_lab <- c("cycle" = "Bicyclist", "ped" = "Pedestrian", "drive" = "Driver")



data_long$ConditionID <- as.factor(data_long$ConditionID)
data_long$ConditionID <- relevel(data_long$ConditionID, "ped")


theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

ggwithinstatsWithPriorNormalityCheck(data = data_long, x = "ConditionID", y = "rank", ylab = "Rank (lower is better)", xlabels = rank_x_lab)
ggsave("plots/rank_user.pdf", width = 9 + 5, height = 6.5, device = cairo_pdf)
