############################
# metdataCompleteness_server.R START
############################

#source("./src/server/metdataCompleteness_server.R", local = TRUE)$value

# query graph to get metadata nodes plus counts of HAS_ESV relationships (tells us how many ESV's the data is related to)
result <- neo4r::call_neo4j("MATCH (m:Metadata)-[r:HAS_ESV]-(esv:EssentialSalmonVariable) RETURN m,COUNT(DISTINCT(esv)) as esvCount;",neo_con,type = 'row')
# quick rename of column to something more descriptive than 'value'
names(result$esvCount) <- "esvCount"
#combined neo4r result tibbles into a single tibble
combinedResult <- dplyr::bind_cols(result$m,result$esvCount)
# create report tibble
result_report <- combinedResult[,c("metadataTitle","metadataKNBURI","esvCount")] #extract relevant for info columns
names(result_report) <- c("Title","KNB_URI","ESV_Count")# rename columns for nicer DT display
result_report$Months <- combinedResult$metadataCoverageMonthsOfYear != "" # create TRUE/FALSE column indicating if month values are present
result_report$startYear <- combinedResult$metadataCoverageStartYear != "" # create TRUE/FALSE column indicating if startYear values are present
result_report$endYear <- combinedResult$metadataCoverageEndYear != "" # create TRUE/FALSE column indicating if endYear values are present

# # # # # # 

sd_meta <- crosstalk::SharedData$new(result_report)

month_plot <- plotly::plot_ly(sd_meta, labels = ~Months, type = "pie", textposition = "inside", textinfo = "percent",insidetextfont = list(color = '#FFFFFF'))
month_plot <- month_plot %>% plotly::layout(title = "Proportion of Meta Data not being assigned to Months")
output$monthsOfYearStats <- plotly::renderPlotly(month_plot)
# 
start_plot <- plotly::plot_ly(sd_meta, labels = ~startYear, type = "pie", textposition = "inside", textinfo = "percent",insidetextfont = list(color = '#FFFFFF'))
start_plot <- start_plot %>% plotly::layout(title = "Proportion of Meta Data not being assigned a Start Date")
output$startYearStats <- plotly::renderPlotly(start_plot)
# 
end_plot <- plotly::plot_ly(sd_meta, labels = ~endYear, type = "pie", textposition = "inside", textinfo = "percent",insidetextfont = list(color = '#FFFFFF'))
end_plot <- end_plot %>% plotly::layout(title = "Proportion of Meta Data not being assigned an End Date")
output$endYearStats <- plotly::renderPlotly(end_plot)

output$esv_plot <- plotly::renderPlotly(esv_hist_plot)
esv_hist_plot <- plotly::plot_ly(sd_meta, x = ~ESV_Count, type = "histogram")



# month_plot <- plot_ly(result_report, labels = ~Months, type = "pie")
# month_plot <- month_plot %>% layout(title = "Proportion of Meta Data not being assigned to Months")
# 
# output$monthsOfYearStats <- renderPlotly(month_plot)
# 
# start_plot <- plot_ly(result_report, labels = ~startYear, type = "pie")
# start_plot <- start_plot %>% layout(title = "Proportion of Meta Data not being assigned a Start Date")
# 
# output$startYearStats <- renderPlotly(start_plot)
# 
# end_plot <- plot_ly(result_report, labels = ~endYear, type = "pie")
# end_plot <- end_plot %>% layout(title = "Proportion of Meta Data not being assigned an End Date")
# 
# output$endYearStats <- renderPlotly(end_plot)


#output$startYearStats <- renderPlot(pie(count(result_report,startYear)$n,labels = c("Present","Missing"),main = "Start Year", col = c("darkseagreen2","coral2"), radius = 1.25))
#output$endYearStats <- renderPlot(pie(count(result_report,endYear)$n,labels = c("Present","Missing"),main = "End Year", col = c("darkseagreen2","coral2"), radius = 1.25))



# Result Table

#output$metadataTable <- DT::renderDT(result_report,options = list(scrollX = TRUE))
 output$metadataTable <- DT::renderDT(sd_meta,options = list(scrollX = TRUE), server = FALSE)


############################
# metdataCompleteness_server.R END
############################