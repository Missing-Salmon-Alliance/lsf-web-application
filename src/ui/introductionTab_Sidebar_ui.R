############################
#introductionTab_Sidebar_ui.R START
############################
conditionalPanel(
  condition = "input.menu1 == 'introduction' && !output.logonTrue",
  column(
    width = 12,
    hr(),
    p("Access to the Salmon Ecosystem Data Hub is currently limited to members of a Data Sharing Group. To sign up to this group click here:"),
    actionButton('introRegisterLink',"Register"),
    br(),
    p("You will be asked to abide by terms and guiding principles. To view and download a copy of these terms click here:"),
    a(href='MemorandumofAgreement.pdf',"Memorandum of Agreement",target="_blank"),
    hr(),
    actionButton('introSideLogonButton',"Login",icon = icon("sign-in-alt")),
    hr(),
    tags$a(href = "https://missingsalmonalliance.org/",
           tags$img(src = 'msa-logo.png', width = '80%'),
           target = "_blank"
           )
    
  )
)

############################
#introductionTab_Sidebar_ui.R END
############################