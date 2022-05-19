# Scripts to report metrics on the metadata
# Written for WKSALMON 2 but should be useful for the site
library(magrittr)
export <- neo4r::call_neo4j("MATCH (m:Metadata) RETURN m;",neo_con,type = 'graph')
#Transform
export <- neo4r::unnest_nodes(export$nodes)
# change NA to ""
export <- export %>% replace(is.na(.),"")

##################################################################
# get a timeseries of metadata counts and accumulation over time
metadataAccumulation <- export[,c("metadataTimestamp","metadataKeywords")]
metadataAccumulation$count <- 1
metadataAccumulation$wksalmon <- FALSE
metadataAccumulation[stringr::str_detect(metadataAccumulation$metadataKeywords,'WKSALMON'),]$wksalmon <- TRUE
metadataAccumulation$metadataTimestamp <- as.Date(metadataAccumulation$metadataTimestamp)
# split out WKSALMON for separate line
metadataAccumulationWKSALMON <- metadataAccumulation[metadataAccumulation$wksalmon,]
metadataAccumulationWKSALMON <- metadataAccumulationWKSALMON %>% dplyr::arrange(metadataTimestamp)
metadataAccumulationWKSALMON <- aggregate(metadataAccumulationWKSALMON["count"], by=metadataAccumulationWKSALMON["metadataTimestamp"], sum)
metadataAccumulationWKSALMON <- metadataAccumulationWKSALMON %>% dplyr::mutate(cumulative = cumsum(count))
##
metadataAccumulation <- metadataAccumulation %>% dplyr::arrange(metadataTimestamp)
metadataAccumulation <- aggregate(metadataAccumulation["count"], by=metadataAccumulation["metadataTimestamp"], sum)
metadataAccumulation <- metadataAccumulation %>% dplyr::mutate(cumulative = cumsum(count))
##
ggplot2::ggplot(data = metadataAccumulation,
                mapping = ggplot2::aes(x = metadataTimestamp,
                              y = cumulative))+
  ggplot2::geom_line(colour = 'black')+
  ggplot2::geom_line(data = metadataAccumulationWKSALMON, ggplot2::aes(x = metadataTimestamp,
                                                              y = cumulative), colour = 'red')
################################################################
# report on open data
withURI <- export[export$metadataAltURI != "",]
availableOnline <- export[export$metadataAvailableOnline,]
someDataOnCDR <- export[export$metadataFilename != "",]
