
data <- read.csv("data/aws-open-data/03-prep-checklist/image_metadata_out.csv", check.names = FALSE)

colnames(data)

# generate report of missingness (na or empty string), per column, as total count and percentage of total rows
missing_report <- data.frame(
  Column = colnames(data),
  Missing_Count = sapply(data, function(x) sum(is.na(x) | x == "")),
  Total_Rows = nrow(data),
  Missing_Percentage = sapply(data, function(x) sum(is.na(x) | x == "")) / nrow(data) * 100
)

write.csv(missing_report, "data/aws-open-data/03-prep-checklist/image_metadata_out.report_missingness.csv", row.names = FALSE)


# Now, add a column to that report above, and include all unique values (followed by a "(count)"), but only if there are 10 unique values or fewer.
# If there are more than 5 unique values, just put "Too many unique values" in that column.
missing_report$Unique_Values <- sapply(data, function(x) {
  unique_vals <- unique(x[!is.na(x) & x != ""])
  if (length(unique_vals) <= 10) {
    paste0(unique_vals, " (", sapply(unique_vals, function(val) sum(x == val, na.rm = TRUE)), ")", collapse = "; ")
  } else {
    "Too many unique values"
  }
})

write.csv(missing_report, "data/aws-open-data/03-prep-checklist/image_metadata_out.report_missingness_with_uniques.csv", row.names = FALSE)

# write as xlsx, with top row freezed, and first header row in bold, and format the percentage as number with 1 decimal place
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "Missingness Report")
writeData(wb, "Missingness Report", missing_report)
# Format the header row
headerStyle <- createStyle(textDecoration = "bold")
addStyle(wb, "Missingness Report", headerStyle, rows = 1, cols = 1:ncol(missing_report), gridExpand = TRUE)
# Format the percentage column
percentageStyle <- createStyle(numFmt = "0.0")
addStyle(wb, "Missingness Report", percentageStyle, rows = 2:(nrow(missing_report) + 1), cols = which(colnames(missing_report) == "Missing_Percentage"), gridExpand = TRUE)
# Freeze the top row
freezePane(wb, "Missingness Report", firstRow = TRUE)
# autoscale all columns
setColWidths(wb, "Missingness Report", cols = 1:ncol(missing_report), widths = "auto")

saveWorkbook(wb, "data/aws-open-data/03-prep-checklist/image_metadata_out.report_missingness_with_uniques.xlsx", overwrite = TRUE)
