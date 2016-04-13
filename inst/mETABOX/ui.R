dashboardPage(skin = "black",
  dashboardHeader(title=img(src="logo.png", width = 200)),
  dashboardSidebar(
    sidebarMenu(
      menuItem("", tabName = "", badgeLabel = "STATISTICAL ANALYSIS", badgeColor = "orange"),
      menuItem("Data Uploading", tabName = "uploaddata", icon = icon("upload"),selected = TRUE),
      menuItem("Data Exploratory", tabName = "exploredata", icon = icon("eye")),
      menuItem("Statistical Analysis Report", tabName = "Statistical Analysis Report", icon = icon("file-text"),
               menuItem("Overview", tabName = "report", icon = icon("angle-double-right")),
               menuItem("Title", tabName = "title", icon = icon("angle-double-right"))),
      menuItem("", tabName = "", badgeLabel = "INTEGRATIVE ANALYSIS", badgeColor = "blue"),
      menuItem("QueryNetwork", tabName = "bionetwork", icon = icon("code-fork")),
      menuItem("Correlations",  tabName = "correlation", icon = icon("line-chart"),
        menuItem("Correlation",  tabName = "paircorrelation", icon = icon("angle-double-right")),
        menuItem("PartialCorrelation",  tabName = "partcorrelation", icon = icon("angle-double-right"))),
      menuItem("Similarity",  tabName = "similarity", icon = icon("flask")),
      menuItem("Subnetwork",  tabName = "subnetwork", icon = icon("sitemap"), badgeLabel = "*", badgeColor = "red"),
      menuItem("Enrichment",  tabName = "enrichment", icon = icon("pie-chart"), badgeLabel = "*", badgeColor = "red"),
      menuItem("Pre-setting", tabName = "setting", icon = icon("cogs"),
        menuItem("ConvertID",  tabName = "convertid", icon = icon("angle-double-right")),
        menuItem("SetDatabase",  tabName = "setdb", icon = icon("angle-double-right"))),
      menuItem("", tabName = "", badgeLabel = "HELP", badgeColor = "olive"),
      menuItem("InputFormat", tabName = "inputformat", icon = icon("table")),
      menuItem("Tutorial", tabName = "tutorial", icon = icon("graduation-cap")),
      menuItem("About", tabName = "about", icon = icon("info"))
    )
  ),
  dashboardBody(
    fluidRow(
      column(width = 12,
        absolutePanel(
          tags$head(
            tags$script(src = "cytoscape.js-2.5.3/cytoscape.js"), #for cytoscapeJS
            tags$script(src = "http://cdnjs.cloudflare.com/ajax/libs/qtip2/2.2.0/jquery.qtip.min.js"), #for cytoscapeJS
            tags$script(src = "https://cdn.rawgit.com/cytoscape/cytoscape.js-qtip/2.2.5/cytoscape-qtip.js"), #for cytoscapeJS
            tags$link(href="http://cdnjs.cloudflare.com/ajax/libs/qtip2/2.2.0/jquery.qtip.min.css", rel="stylesheet"),
            tags$link(rel = "stylesheet", type = "text/css", href = "mETABOX.css")
          )
        )
      )
    ),
    tabItems(
      tabItem(tabName = "uploaddata",
              h3("Data uploading and Data Modification")
              ,p("Users could upload their own data file and modify data set according to needs or simply use the defaul dataset.")
              ,radioButtons("uploadtype", "Upload your data or use example data",
                            choices = c("Load example dataset","Upload aggregated dataset","Upload expression, feature, phenotype datasets seperately")
                            )
              ,conditionalPanel(condition = "input.uploadtype == 'Upload aggregated dataset'"
                                ,div(style="display:inline-block",p("Please read detailed explanation about the "))
                                ,div(style="display:inline-block",a("upload data format",
                                                                    href = "https://github.com/kwanjeeraw/mETABOX"))
                                ,div(style="display:inline-block",p("."))
                                ,div(style="display:inline-block",p("An example dataset is also "))
                                ,div(style="display:inline-block",a("provided",href = "https://github.com/kwanjeeraw/mETABOX"))
                                ,div(style="display:inline-block",p("."))
                                ,fileInput("InputData", "Upload Aggregated Data"))
              ,conditionalPanel(condition = "input.uploadtype == 'Upload expression, feature, phenotype datasets seperately'"
                                ,div(style="display:inline-block",p("Please read detailed explanation about the "))
                                ,div(style="display:inline-block",a("upload data format",
                                                                    href = "https://github.com/kwanjeeraw/mETABOX"))
                                ,div(style="display:inline-block",p(". If the orders of features or subjects do not match,
                                                                     don't worry we can handle that but please read carefully with the "))
                                ,div(style="display:inline-block",a("upload data format",
                                                                    href = "https://github.com/kwanjeeraw/mETABOX"))
                                ,div(style="display:inline-block",p("."))
                                ,div(
                                      div(style="display:inline-block",p("Example datasets are also "))
                                      ,div(style="display:inline-block",a("provided",href = "https://github.com/kwanjeeraw/mETABOX"))
                                      ,div(style="display:inline-block",p(" for expression, feature, phenotype seperately."))
                                )
                                ,div(
                                  div(style ="display:inline-block",fileInput("InputDataExp", "Upload Expression Data"))
                                  ,div(style ="display:inline-block",fileInput("InputDataF", "Upload Feature Data"))
                                  ,div(style ="display:inline-block",fileInput("InputDataP", "Upload Phenotype Data"))
                                  )
                                )
              ,tabsetPanel(
                tabPanel("Data Summary",
                         verbatimTextOutput("summary.data")
                         )
                ,tabPanel("Feature Data Table",
                          DT::dataTableOutput("View.fData"))
                ,tabPanel("Phenotype Data Table",
                          div(
                            div(style = "display:inline-block",checkboxInput("editViewpData", "", value = FALSE, width = "18px"))
                            ,div(style = "display:inline-block",em("By clicking Edit, you can add, delete, change order of subjects,etc."))
                            )
                          ,conditionalPanel(condition = "input.editViewpData",
                                                actionButton("SubmitModificationpData", "Submit Modification")
                                            )
                          ,DT::dataTableOutput("View.pData")
                          )
                ,tabPanel("Expression Data Table",
                          DT::dataTableOutput("View.eData"))
              )

              )
      ,tabItem(tabName = "exploredata",
               h3("Data Exploratory Analysis"))
      ,tabItem(tabName = "report",
               h3("Automatic report generator")
              ,p("The report generated is based on default settings, which shows in \"Tutorial\" section. You could
                 also change settings according to your needs in the \"Statistics\" section.")

              ,fluidRow(
                box(width = 12, title = "Title", status = "primary", collapsible = TRUE,solidHeader = T,
                     strong(h1("Statistical Analysis", align = "center"))
                     ,h4(textOutput("authorANDdate"), align = "center")
                     ,h5(textOutput("dataname"),align = 'center')
               )
               ,box(width = 12, title = "Data Description", status = "primary", collapsible = TRUE, solidHeader = T,
                    "!!!")

      ))
      ,tabItem(tabName = "title"
               ,h3("Title")
               ,p("Author name, date and subtitle could be modified here.")
               ,fluidRow(
                 box(width = 4, title = "Edit Title", status = "warning", collapsible = TRUE, solidHeader = T,
                     textInput("author.input", "Change Author", value = "Sili Fan"),
                     dateInput("date.input", "Change Date"),
                     textInput("subtitle.input", "Change Subtitle"))
                 ,box(width = 8, title = "Title", status = "primary", collapsible = TRUE,solidHeader = T,
                     strong(h1("Statistical Analysis", align = "center"))
                     ,h4(textOutput("authorANDdate2"), align = "center")
                     ,h5(textOutput("dataname2"),align = 'center')
               )
      ))

      ,tabItem(tabName = "bionetwork",
              h3("Query biological network"),
              p("Query the biological network from the graph database, see ", a(href='http://kwanjeeraw.github.io/grinn/fetchgrinn.html',target='_blank','here'),' for argument details.'),
              fluidRow(
                box(width = 12, title = "Inputs", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                  column(12, textInput(inputId = "fromtxt", label = "from node identifiers: [separate each keyword by space e.g. 27432 36361]", value = "")),
                  column(7, fileInput(inputId='fromfl', label='or upload a file: [one column, separate each keyword by a new line]', accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))),
                  column(5, mainPanel(tableOutput('fromtable'))),
                  column(12, textInput(inputId = "totxt", label = "to node identifiers: [separate each keyword by space e.g. 218678 218994]", value = "")),
                  column(7, fileInput(inputId='tofl', label='or upload a file: [one column, separate each keyword by a new line]', accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))),
                  column(5, mainPanel(tableOutput('totable'))),
                  column(12, radioButtons(inputId = "relationship", label = "select a relationship:", inline = FALSE,
                    choices = list("(from:Compound) -[:BIOCHEMICAL_REACTION]-> (to:Compound)"="cbc",
                                    "(from:Gene) -[:CONVERSION]-> (to:Protein)"="gcp",
                                    "(from:Gene) -[:GENETIC_ASSOCIATION]-> (to:Gene)"="ggg",
                                    "(from:Pathway) -[:ANNOTATION]-> (to:Compound)"="pac",
                                    "(from:Pathway) -[:ANNOTATION]-> (to:Gene)"="pag",
                                    "(from:Pathway) -[:ANNOTATION]-> (to:Protein)"="pap",
                                    "(from:Protein) -[:CATALYSIS]-> (to:Compound)"="pcc",
                                    "(from:Protein) -[:CONTROL]-> (to:Gene)"="pcg",
                                    "(from:Protein) -[:MOLECULAR_BINDING]-> (to:Protein)"="pmp",
                                    "(from:Rna) -[:CONTROL]-> (to:Gene)"="rcg"),selected = "pcc")),
                  textInput(inputId = "pattern", label = "or enter a relationship pattern: [e.g. (:Gene)-[:CONVERSION]->(from:Protein) -[:CATALYSIS]-> (to:Compound)]", value = ""),
                  hr(), actionButton("query","Query")
                )
              ),
              fluidRow(
                tabBox(width = 12, side = "left", selected = "Summary", title = "Outputs",
                       tabPanel("Summary", "Edgelist summary", verbatimTextOutput("consolemsgbio")),
                       tabPanel("Node", dataTableOutput('bioNtable')),
                       tabPanel("Edge", dataTableOutput('bioEtable')),
                       tabPanel("Network", plotlyOutput("bioPlot"))
                )
              ),
              fluidRow(
                column(12, actionButton("saveBionw","Save network"), downloadButton('downloadBionw', 'Download network'))
              ),br(),
              fluidRow(
                box(width = 12, title = "Network legend", status = "info", solidHeader = TRUE, img(src="legend.png"))
              )
      ),
      tabItem(tabName = "paircorrelation",
              h3("Compute weighted correlation network"),
              p("Compute the correlation network for one or two omic data sets, see ", a(href='http://kwanjeeraw.github.io/grinn/fetchgrinn.html',target='_blank','here'),' for argument details.'),
              fluidRow(
                box(width = 12, title = "Inputs", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    column(4, fileInput(inputId='datxfl', label='upload a data set (x):', accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))),
                    column(2, radioButtons('xsep', 'choose delimiter',c(comma=',',tab='\t'),'\t',inline=TRUE)),
                    column(6, radioButtons(inputId = "datxtype", label = "select node type x:",inline = TRUE,
                      choices = list("compound"="Compound","gene"="Gene","protein"="Protein","rna"="Rna"))),
                    tabBox(width = 12, side = "left", selected = "File overview", height = "310px",
                      tabPanel("File overview", dataTableOutput('datXExTable'))),
                    column(4, fileInput(inputId='datyfl', label='or upload another data set (y):', accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))),
                    column(2, radioButtons('ysep', 'choose delimiter',c(comma=',',tab='\t'),'\t',inline=TRUE)),
                    column(6, radioButtons(inputId = "datytype", label = "select node type y:",inline = TRUE,
                      choices = list("compound"="Compound","gene"="Gene","protein"="Protein","rna"="Rna"))),
                    tabBox(width = 12, side = "left", selected = "File overview", height = "318px",
                      tabPanel("File overview", dataTableOutput('datYExTable'))),
                    column(4, numericInput(inputId = "paircoef", label = "minimum correlation coefficient:", value = 0.7, min = 0.1, max = 1.0, step = 0.05)),
                    column(4, numericInput(inputId = "pairpval", label = "maximum p-value:", value = 0.05, min = 0, max = 1.0, step = 0.01)),
                    column(4, selectInput(inputId = "pairmethod", label = "select method:", choices = c("spearman" = "spearman","pearson" = "pearson","kendall" = "kendall"), selected = "spearman")),
                    hr(), column(6,actionButton("computePair","Compute"))
                )
              ),
              fluidRow(
                tabBox(width = 12, side = "left", selected = "Summary", title = "Outputs",
                       tabPanel("Summary", "Edgelist summary", verbatimTextOutput("consolemsgpaircor")),
                       tabPanel("Node", dataTableOutput('pairNtable')),
                       tabPanel("Edge", dataTableOutput('pairEtable')),
                       tabPanel("Network", plotlyOutput("corPlot"))
                )
              ),
              fluidRow(
                column(12, actionButton("savePairnw","Save network"), downloadButton('downloadPairnw', 'Download network'))
              ),br(),
              fluidRow(
                box(width = 12, title = "Network legend", status = "info", solidHeader = TRUE, img(src="legend.png"))
              )
      ),
      tabItem(tabName = "partcorrelation",
              h3("Compute partial correlation network"),
              p("Compute the partial correlation network for omic data, see ", a(href='http://kwanjeeraw.github.io/grinn/fetchgrinn.html',target='_blank','here'),' for argument details.'),
              fluidRow(
                box(width = 12, title = "Inputs", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    column(4, fileInput(inputId='datfl', label='upload a data set (x):', accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))),
                    column(2, radioButtons('datsep', 'choose delimiter',c(comma=',',tab='\t'),'\t',inline=TRUE)),
                    column(6, radioButtons(inputId = "dattype", label = "select node type:",inline = TRUE,
                      choices = list("compound"="Compound","gene"="Gene","protein"="Protein","rna"="Rna"))),
                    tabBox(width = 12, side = "left", selected = "File overview", height = "318px",
                      tabPanel("File overview", dataTableOutput('datExTable'))),
                    column(3, numericInput(inputId = "parcoef", label = "minimum correlation coefficient:", value = 0.7, min = 0.1, max = 1.0, step = 0.05)),
                    column(3, numericInput(inputId = "parpval", label = "maximum p-value:", value = 0.05, min = 0, max = 1.0, step = 0.01)),
                    column(3, numericInput(inputId = "alpha", label = "alpha:", value = 0.05, min = 0, max = 1.0, step = 0.01)),
                    column(3, numericInput(inputId = "epsilon", label = "epsilon:", value = 0.5, min = 0.1, max = 1.0, step = 0.05)),
                    hr(), column(6,actionButton("computePar","Compute"))
                )
              ),
              fluidRow(
                tabBox(width = 12, side = "left", selected = "Summary", title = "Outputs",
                       tabPanel("Summary", "Edgelist summary", verbatimTextOutput("consolemsgparcor")),
                       tabPanel("Node", dataTableOutput('parNtable')),
                       tabPanel("Edge", dataTableOutput('parEtable')),
                       tabPanel("Network", plotlyOutput("parPlot"))
                )
              ),
              fluidRow(
                column(12, actionButton("saveParnw","Save network"), downloadButton('downloadParnw', 'Download network'))
              ),br(),
              fluidRow(
                box(width = 12, title = "Network legend", status = "info", solidHeader = TRUE, img(src="legend.png"))
              )
      ),
      tabItem(tabName = "similarity",
              h3("Compute chemical-structure similarity network"),
              p("Compute Tanimoto similarity network, see ", a(href='http://kwanjeeraw.github.io/grinn/fetchgrinn.html',target='_blank','here'),' for argument details.'),
              fluidRow(
                box(width = 12, title = "Inputs", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    column(12, textInput(inputId = "cmptxt", label = "input compound identifiers: [separate each pubchemID by space e.g. 1110 10413 196 51 311 43 27810 43]", value = "")),
                    column(7, fileInput(inputId='cmptxtfl', width = "70%", label='or upload a file: [one column, separate each pubchemID by a new line]', accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))),
                    column(5, mainPanel(tableOutput('cmptable'))),
                    column(6, numericInput(inputId = "tancoef", label = "minimum Tanimoto similarity correlation coefficient:", value = 0.7, min = 0.1, max = 1.0, step = 0.05)),
                    column(12, hr(), actionButton("computeSim","Compute"))
                )
              ),
              fluidRow(
                tabBox(width = 12, side = "left", selected = "Summary", title = "Outputs",
                       tabPanel("Summary", "Edgelist summary", verbatimTextOutput("consolemsgsim")),
                       tabPanel("Node", dataTableOutput('simNtable')),
                       tabPanel("Edge", dataTableOutput('simEtable')),
                       tabPanel("Network", plotlyOutput("simPlot"))
                )
              ),
              fluidRow(
                column(12, actionButton("saveSimnw","Save network"), downloadButton('downloadSimnw', 'Download network'))
              ),br(),
              fluidRow(
                box(width = 12, title = "Network legend", status = "info", solidHeader = TRUE, img(src="legend.png"))
              )
      ),
      tabItem(tabName = "subnetwork",
              h3("Compute subnetwork"),
              p("Compute an active subnetwork or module by integrating omic data on the biological network, see ", a(href='http://kwanjeeraw.github.io/grinn/fetchgrinn.html',target='_blank','here'),' for argument details.'),
              fluidRow(
                box(width = 12, title = "Inputs", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    column(12, selectizeInput(inputId='networkls_sub', label = "select network(s):", choices = NULL, options = list(placeholder = 'Select a saved network from previous calculation', maxOptions = 5))),
                    column(5, fileInput(inputId='statfl', label='upload data', accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))),
                    column(7, radioButtons('subsep', 'choose delimiter',c(comma=',',tab='\t'),'\t',inline=TRUE)),
                    tabBox(width = 12, side = "left", selected = "File overview", height = "310px",
                      tabPanel("File overview", dataTableOutput('statTable'))),
                    column(4, numericInput(inputId = "subpval", label = "input false discovery rate (FDR):", value = 0.05, min = 0, max = 1.0, step = 0.01)),
                    column(4, numericInput(inputId = "pcol", label = "input column number of p-value:", value = 1, min = 1, max = 5, step = 1)),
                    column(4, numericInput(inputId = "fccol", label = "input column number of fold-change:", value = 0, min = 1, max = 5, step = 1)),
                    column(12, hr(), actionButton("computeSub","Compute"))
                )
              ),
              fluidRow(
                tabBox(width = 12, side = "left", selected = "Summary", title = "Outputs",
                       tabPanel("Summary", "Edgelist summary", verbatimTextOutput("consolemsgsub")),
                       tabPanel("Node", dataTableOutput('subNtable')),
                       tabPanel("Edge", dataTableOutput('subEtable')),
                       tabPanel("Network", plotlyOutput("subPlot"))
                )
              ),
              fluidRow(
                column(12, actionButton("saveSubnw","Save network"), downloadButton('downloadSubnw', 'Download network'))
              ),br(),
              fluidRow(
                box(width = 12, title = "Network legend", status = "info", solidHeader = TRUE, img(src="legend.png"))
              )
      ),
      tabItem(tabName = "enrichment",
              h3("Enrichment Analysis"),
              p("Perform enrichment analysis on the annotation network, see ", a(href='http://kwanjeeraw.github.io/grinn/fetchgrinn.html',target='_blank','here'),' for argument details.'),
              fluidRow(
                box(width = 12, title = "Inputs", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    column(12, selectizeInput(inputId='networkls_enr', label = "select network(s):", choices = NULL, options = list(placeholder = 'Select a saved network from previous calculation', maxOptions = 5))),
                    column(5, fileInput(inputId='statdat', label='upload data:', accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))),
                    column(7, radioButtons('erasep', 'choose delimiter',c(comma=',',tab='\t'),'\t', inline=TRUE)),
                    tabBox(width = 12, side = "left", selected = "File overview", height = "310px",
                      tabPanel("File overview", dataTableOutput('statdatTable'))),
                    column(4, selectInput(inputId = "eramethod", label = "select method:", choices = list("reporter"="reporter","fisher"="fisher","median"="median","mean"="mean","stouffer"="stouffer"))),
                    column(4, numericInput(inputId = "pcol2", label = "input column number of p-value:", value = 1, min = 1, max = 5, step = 1)),
                    column(4, numericInput(inputId = "fccol2", label = "input column number of fold-change:", value = 0, min = 1, max = 5, step = 1)),
                    column(12, hr(), actionButton("computeEra","Compute"))
                )
              ),
              fluidRow(htmlOutput("cyEra")),
              fluidRow(
                tabBox(width = 12, side = "left", height = "800px", selected = "Summary", title = "Outputs",
                       tabPanel("Summary", "Output summary", verbatimTextOutput("consolemsgera")),
                       tabPanel("Enrichment", dataTableOutput('eratable')),
                       tabPanel("Attribute", dataTableOutput('eraattbtable')),
                       tabPanel("Network", htmlOutput("frame"))
                ),
                column(12, downloadButton('downloadEra', 'Download'))
              ),br(),
              fluidRow(
                box(width = 12, title = "Network legend", status = "info", solidHeader = TRUE, img(src="legend.png"))
              )
      ),
      tabItem(tabName = "convertid",
              h3("Convert ID"),
              p("Convert to internal neo4j id(s) and Grinn id(s), see ", a(href='http://kwanjeeraw.github.io/grinn/fetchgrinn.html',target='_blank','here'),' for argument details.'),
              fluidRow(
                box(width = 12, title = "Inputs", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    column(12, textInput(inputId = "txt", label = "input external identifiers: [e.g. pubchemID 6132 6267 138]", value = "")),
                    column(5, fileInput(inputId='txtfl', width = "100%", label='or upload a file: [separate each keyword by a new line]', accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))),
                    column(7, radioButtons('cvsep', 'choose delimiter',c(comma=',',tab='\t'),'\t',inline=TRUE)),
                    tabBox(width = 12, side = "left", selected = "File overview", height = "310px",
                      tabPanel("File overview", dataTableOutput('idstable'))),
                    column(12,
                      column(6, radioButtons(inputId = "nodetype", label = "select a node type",inline = FALSE,
                        choices = list("compound"="compound", "gene"="gene", "pathway"="pathway", "protein"="protein", "rna"="rna"))),
                      column(6, radioButtons(inputId = "searchby", label = "select an identifier type",inline = FALSE,
                        choices = list("name"="name", "synonyms"="synonyms", "xref"="xref"), selected = "xref"),
                        checkboxInput(inputId = "exactmatch", label = "exact match", value = TRUE))),
                    column(12, hr(), actionButton("convert","Convert")))
              ),
              fluidRow(
                tabBox(width = 12, side = "left", selected = "Summary", title = "Outputs",
                       tabPanel("Summary", "Output summary", verbatimTextOutput("consolemsgconvert")),
                       tabPanel("ID", dataTableOutput('converttable'))
                ),
                column(12, downloadButton('downloadid', 'Download'))
              )
      ),
      tabItem(tabName = "setdb",
              h3("Set database"),
              p("Set the database location for the currently working environment, see ", a(href='http://kwanjeeraw.github.io/grinn/fetchgrinn.html',target='_blank','here'),' for argument details.'),
              fluidRow(
                box(width = 12, title = "Inputs", status = "primary", solidHeader = TRUE,
                    column(12, h4("Current database location:"), verbatimTextOutput("currentdb")),
                    column(12, textInput(inputId = "dburl", label = "input database url", value = "")),
                    column(12, hr(), actionButton("settingdb","Submit"), actionButton("resettingdb","Reset"))
                )
              )
      ),
      tabItem(tabName = "inputformat",
              fluidRow(
                box(width = 12, title = "Input file format", status="success", solidHeader = TRUE, collapsible = TRUE,
                    p("This page shows screenshot of different input formats used in mETABOX."),
                    h4("1. List of keywords"), img(src="list.png"),
                    h4("2. Statistical values"), img(src="stat.png"),
                    h4("3. Quantified values"), img(src="quantify.png")
              ))
      ),
      tabItem(tabName = "tutorial",
              h3("Tutorial"),
              p("Details ... ")
      ),
      tabItem(tabName = "about",
              h3("About"),
              p("Details ... ")
      )
#       ,tabItem(tabName = "cytoscape",
#               fluidRow(
#                 column(4, wellPanel(
#                   numericInput("n", "N:", min = 1, max = 2, value = 1),
#                   br(),
#                   actionButton("goButton", "Go!"),
#                   p("Click the button to update the value displayed in the main panel.")
#                 ))
#               ),
#               fluidRow(
#                 htmlOutput("cy1")
#               ),
#               fluidRow(
#                 tabBox(width = 12, side = "left", height = "800px",
#                        selected = "NW",
#                        tabPanel("NW", htmlOutput("frame2")),
#                        tabPanel("Summary", "Output summary", verbatimTextOutput("consolemsgera1")),
#                        tabPanel("Enrichment", dataTableOutput('eratable1')),
#                        tabPanel("Attribute", dataTableOutput('eraattbtable1'))
#                 )
#               ), br(),
#               fluidRow(
#                 box(width = 12,
#                     title = "Network legend", status = "info", solidHeader = TRUE,
#                     img(src="legend.png")
#                 )
#               )
#       )
    )# end tabItems
  )# end dashboardBody
)# end dashboardPage
