####################
# Information pop-over messages found throughout the site
####################

# Search Page Pop overs

shinyBS::addPopover(session, id = 'searchRefreshUI',title = "Refresh The Map",
           content = "Refresh the Map",
           placement = 'top',
           options = list(container = "body")
)


shinyBS::addPopover(session, id = 'icesStockUnitFilter',title = "Stock Unit Filter",
           content = "The Stock Unit Filter is not yet active. Work is ongoing to link stock units to data sources",
           placement = 'top',
           options = list(container = "body")
)

shinyBS::addPopover(session, id = 'icesIndexRiverFilter',title = "Index River Filter",
           content = "The Index River Filter is not yet active. Work is ongoing to link index rivers to data sources",
           placement = 'top',
           options = list(container = "body")
)


# Submit page pop overs

shinyBS::addPopover(session, id = 'clearSubmitFormUI',title = "Reset Form Data",
           content = "Clear all of the fields/reset to defaults",
           placement = 'top',
           options = list(container = "body")
)


# shinyBS::addPopover(session, id = 'domainNodeList',title = "Life-Stage Domains",
#                     content = HTML(paste0(strong("Life-Stage Domains"),em(paste0(" - ",domainsDescriptionCopy)))),
#                     placement = 'top',
#                     options = list(container = "body")
# )
# 
# shinyBS::addPopover(session, id = 'esvCategory',title = "Variable Classes",
#            content = HTML(paste0(strong("Variable Classes"),em(paste0(" - ",esvDescriptionCopy)))),
#            placement = 'top',
#            options = list(container = "body")
# )

shinyBS::addPopover(session, id = 'metadataFields', title = "Help Topic",
                    content = "Click here to get more information on Data Source Details",
                    placement = 'top',
                    options = list(container = "body")
                    
)

shinyBS::addPopover(session, id = 'geogTimeFields', title = "Help Topic",
                    content = "Click here to get more information on Geographic Coverage and Temporal Coverage",
                    placement = 'top',
                    options = list(container = "body")
                    
)

shinyBS::addPopover(session, id = 'esvFields', title = "Help Topic",
                    content = "Click here to get more information on Life-Stage Domains and Variable Classes",
                    placement = 'top',
                    options = list(container = "body")
                    
)

shinyBS::addPopover(session, id = 'sourceMaintenance',title = "Update Frequency",
           content = "Indicate how often the dataset is expected to be updated.",
           placement = 'top',
           options = list(container = "body")
           
)

shinyBS::addPopover(session, id = 'sourceCreatorORCID',title = "ORCID URL",
           content = HTML(paste0(p("ORCID is a way to create unique dereferenceable identifiers for individuals. This is an optional field, however ORCID ID's can be easily created at ",
                                   a(href="https://orcid.org/","https://orcid.org/")))),
           placement = 'top',
           options = list(container = "body")
           
)

shinyBS::addPopover(session, id = 'embargoEnd',title = "Embargo End Date",
           content = "If the primary data being described is under an embargo please enter the expected end date or more information here.",
           placement = 'top'
           
)

shinyBS::addPopover(session, id = 'uploadDataFileUI',title = "File Drop",
                    content = "Single file only and Maximum Size 25Mb. You may place multiple files into a zipped archive to upload, alternatively or for larger files please contact the Data Manager for assistance: data.admin@missingsalmonalliance.org",
                    placement = 'top',
                    options = list(container = "body")
                    
)




shinyBS::addPopover(session, id = "researchProjectTheme",title = "Theme Detail",
           content = HTML(paste0(strong("Method development"),em(" - Developing and enhancing models in support of modelling ecosystem impacts and hypotheses testing"),p(),
                                 strong("Carryover effects"),em(" - Influences during freshwater phase that have carry-over penalty (mortality loading) in the marine phase"),p(),
                                 strong("Local/regional mortality drivers"),em(" - Domains transited early in the post smolt phase, approx first 3 months, including transitional waters. Include return phase to homewaters/river"),p(),
                                 strong("Ocean basin scale drivers of marine mortality"),em(" - Common domains occupied by salmon in later phase at sea, associated with feeding areas common to many stocks (e.g. Norwegian Sea, Labrador Sea, W. Greenland)"),p(),
                                 strong("Demographic mechanisms"),em(" - Drivers of key life history traits, such as maturation schedule"),p(),
                                 strong("Eco-evolutionary processes"),em(" - Evolutionary and demographic consequences of multiple stressors, using e.g. IBASM. The LSF virtual domain"),p(),
                                 strong("Getting the data together"),em(" - Availability and structuring of data supporting the above topics. e.g. ICES Index rivers, tracking studies, environmental and ecosystem data. Essential for hypotheses testing"),p(),
                                 strong("Guidance to managers"),em(" - Development of guidance to managers, such as Decision Support Tools (complementary to quantitative catch advice)")
           )
           
           ),
           placement = 'top',
           options = list(container = "body")
)
