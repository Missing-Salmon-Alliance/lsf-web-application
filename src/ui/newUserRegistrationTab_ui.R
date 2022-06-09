tabItem(tabName = 'newMemberRegistration',
        h2("Data Sharing Group New Member Registration"),
        h4("By registering you are accepting the terms in the Data Sharing Memorandum of Agreement."),
        br(),
        box(
          width = 4,
          status = 'primary',
          uiOutput('registerFieldBlank'),
          textInput('newUserName',"Name:"),
          textInput('newUserEmail',"Email:"),
          textInput('newUserOrganisation',"Affiliation, Organisation or Institute:"),
          hr(),
          checkboxInput('dataSharingAgreementAccept',
                               "I have read and agree to abide by the terms and guiding principles set out in the Data Sharing Group Memorandum of Agreement",
                               value = FALSE),
          checkboxInput('dataSharingAgreementAdvertiseOrganisation',
                        "Organisations that are part of the Data Sharing Group can appear on the Salmon Ecosystem Data Hub as contributors. If you would like your organisation to be shown, please tick here.",
                        value = FALSE),
          hr(),
          actionButton('newUserRegisterSubmit',"Submit")
        ),
        box(
          width = 8,
          title = "Please review this agreement before proceeding.",
          status = 'warning',
          includeMarkdown("src/DataSharingMoA.Rmd")
        ))