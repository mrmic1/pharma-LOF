#Check select fits
#Seperate fits into seperate files if needed

muddyfoot_perch_select_fits <- readRDS("~/Desktop/Git_Repo/pharma-LOF-mac/data/ctmm_fits/muddyfoot_perch_fits/muddyfoot_perch_select_fits.rds")

#Check model outputs
for (i in 1:length(muddyfoot_perch_select_fits)) {
  element_name <- names(muddyfoot_perch_select_fits)[i]
  cat("Summary for", element_name, ":\n")
  print(summary(muddyfoot_perch_select_fits[[i]]))
  cat("\n")
}

# Assuming muddyfoot_perch_select_fits is a list of lists, where each sublist contains fits
# We will use lapply to iterate over each element and generate a summary

summaries <- lapply(muddyfoot_perch_select_fits, function(fit) {
  summary(fit)$name
})

# Display summaries
summaries

#14/30 = "OU ansiotropic error"

save_ctmm_path = "./data/ctmm_fits/muddyfoot_perch_fits/"

# Function to save each list as an .rds file
save_lists_as_rds <- function(fit_list, fit_name) {
  saveRDS(fit_list, file = paste0(save_ctmm_path, fit_name, ".rds"))
}

# Iterate over each element in muddyfoot_perch_select_fits
for (fit_name in names(muddyfoot_perch_select_fits)) {
  fit_list <- muddyfoot_perch_select_fits[[fit_name]]
  save_lists_as_rds(fit_list, fit_name)
}
