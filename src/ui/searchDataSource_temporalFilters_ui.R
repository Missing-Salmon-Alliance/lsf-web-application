shinyBS::bsCollapsePanel(
  title = "Temporal Filters",
  style = 'primary',
  sliderInput('temporalSlider', "Select the Date Range", min = min(LSFMetadataTibble$metadataCoverageStartYear), max = max(LSFMetadataTibble$metadataCoverageEndYear), 
              value = c(1970,2010), step = 1, sep = ""),
  selectInput('monthsSelect', "Select the Relevant Months", choices = c("All",uniqueMonths), multiple = TRUE, selected = "All")
)

