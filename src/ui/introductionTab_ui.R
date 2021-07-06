############################
#introductionTab_ui START
############################

tabItem('introduction',
        fluidPage(
          h1("Welcome to the Missing Salmon Alliance Data Portal",align = 'center'),
          br(),
          h2("The Likely Suspects Framework",align = 'center'),
          br(),
          em("In response to the dramatic declines of Atlantic salmon populations, the Missing Salmon Alliance has developed the Likely Suspects Framework. This programme hopes to improve our understanding of the factors driving salmon population trends to help provision salmon managers with new tools to support their future decisions to help stabilise or reverse these declines in salmon."),
          p(),
          em("Salmon management is frequently impeded by poor access to data that may support changing their approaches. The Likely Suspects Framework is working to mobilise biological, physical and salmon specific information from freshwater and marine environments located around the North Atlantic."),
          br(),
          br(),
          carousel(
            id = "mycarousel",
            width = 12,
            carouselItem(
              column(
                2
              ),
              box(
                width = 8,
                title = "Search The Framework",
                status = 'primary',
                solidHeader = TRUE,
                h4(searchIntroCopyPara1),
                br(),
                h4(searchIntroCopyPara2),
                br(),
                br(),
                br(),
                br()
              ),
              column(
                2
              )
            ),
            carouselItem(
              column(
                2
              ),
              box(
                width = 8,
                title = "Submit Data to The Framework",
                status = 'primary',
                solidHeader = TRUE,
                h4(submitIntroCopyPara1),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                br()
              ),
              column(
                2
              )
            ),
            carouselItem(
              column(
                2
              ),
              box(
                width = 8,
                title = "The Framework",
                status = 'primary',
                solidHeader = TRUE,
                h4("The framework is designed to collate salmon knowledge and index it in a way that will guide researchers to the data they need to test hypotheses. All knowledge in the framework will have been included because it fits in to Atlantic salmon ecology at some point, including but not restricted to:"),
                br(),
                column(
                  2
                ),
                column(
                  6,
                  tags$ul(
                    tags$li(h4("Primary Observation")),
                    tags$li(h4("Derived Output")),
                    tags$li(h4("Model Output"))
                  )
                ),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                br()
              ),
              column(
                2
              )
            ),
            carouselItem(
              column(
                2
              ),
              box(
                width = 8,
                title = "The Metadata",
                status = 'primary',
                solidHeader = TRUE,
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
                  )
                ),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                br()
              ),
              column(
                2
              )
            ),
            carouselItem(
              column(
                2
              ),
              box(
                width = 8,
                status = 'primary',
                solidHeader = TRUE,
                title = "Variable Classes",
                h4("Variable classes are used to index data sources within the framework. You may like to explore the classes here prior to searching for or submitting data sources"),
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
            ),
            column(
              2
            )
          )
        )
)

############################
#introductionTab_ui END
############################