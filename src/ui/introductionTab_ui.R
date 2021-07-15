############################
#introductionTab_ui START
############################

tabItem('introduction',
        fluidPage(
          fluidRow(
            h1("Welcome to the Central Data Resource")
          ),
          fluidRow(
            column(
              width = 6,
              box(
                width = 12,
                title = "Introduction",
                status = 'primary',
                solidHeader = FALSE,
                h4("In response to the dramatic declines of Atlantic salmon populations, the Missing Salmon Alliance has developed the Likely Suspects Framework. This programme hopes to improve our understanding of the factors driving salmon population trends to help provision salmon managers with new tools to support their future decisions to help stabilise or reverse these declines in salmon."),
                p(),
                h4("Salmon management is frequently impeded by poor access to data that may support changing their approaches. The Likely Suspects Framework is working to mobilise biological, physical and salmon specific information from freshwater and marine environments located around the North Atlantic.")
              )
            ),
            column(
              width = 6,
              #box(
                #width = 12,
                #img(src = 'images/screenshot_searchmap.PNG')
              #)
            )
          ),
          fluidRow(
            
            column(
              width = 4,
              #box(
              #  width = 12,
                #img(src = 'images/screenshot_searchmap.PNG')
              #)
            ),
            column(
              width = 8,
              carousel(
                id = "mycarousel",
                width = 12,
                carouselItem(
                  box(
                    width = 12,
                    title = "Search The Central Data Resource",
                    status = 'warning',
                    solidHeader = FALSE,
                    h4(searchIntroCopyPara1),
                    br(),
                    h4(searchIntroCopyPara2),
                    br(),
                    br(),
                    br(),
                    br()
                  )
                ), # carousel item end
                carouselItem(
                  box(
                    width = 12,
                    title = "Submit Data to The Central Data Resource",
                    status = 'primary',
                    solidHeader = FALSE,
                    h4(submitIntroCopyPara1),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br()
                  )
                ), # carousel item end
                carouselItem(
                  box(
                    width = 12,
                    title = "The Central Data Resource",
                    status = 'success',
                    solidHeader = FALSE,
                    h4("The Central Data Resource is designed to collate salmon knowledge and index it in a way that will guide researchers to the data they need to test hypotheses. All knowledge in the resource will have been included because it is relevant to Atlantic salmon ecology, including but not restricted to:"),
                    br(),
                    column(
                      2
                    ),
                    column(
                      6,
                      tags$ul(
                        tags$li(h4("Primary Observation")),
                        tags$li(h4("Derived Output")),
                        tags$li(h4("Model Output")),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br()
                      )
                    )
                  )
                ), # carousel item end
                carouselItem(
                  box(
                    width = 12,
                    title = "The Metadata",
                    status = 'danger',
                    solidHeader = FALSE,
                    h4("Metadata (the data source descriptions) are grounded in a common language (Ecological Metadata Language) https://www.dcc.ac.uk/resources/metadata-standards/eml-ecological-metadata-language, all effort put in to describing the data thoroughly via this interface is interoperable with popular online data portals such as:"),
                    br(),
                    column(
                      2
                    ),
                    column(
                      6,
                      tags$ul(
                        tags$li(h4("Global Biodiversity Information Facility")),
                        tags$li(h4("Long Term Ecological Research Network")),
                        tags$li(h4("Knowledge Network for Biocomplexity")),
                        br(),
                        br(),
                        br(),
                        br(),
                        br()
                      )
                    )
                  )
                ) # carousel item end
              ) # carousel end
            )
          ), # fluid row end
          fluidRow(
            box(
              width = 12,
              status = 'warning',
              solidHeader = FALSE,
              title = "Variable Classes",
              h4("Variable classes are used to index data sources within the Central Data Resource. You may like to explore the classes here prior to searching for or submitting data sources"),
              column(
                width = 6,
                selectInput('varClassesDomain',"Domain",choices = LSFDomainTibble$domainTitle)
              ),
              column(
                width = 6,
                selectInput('varClassesCats',"Category",choices = unique(LSFEssentialSalmonVariableTibble$esvCategory))
              ),
              DT::DTOutput('varClassesFull'),
              br(),
              br(),
              br()
            )
          )
        )
)

############################
#introductionTab_ui END
############################