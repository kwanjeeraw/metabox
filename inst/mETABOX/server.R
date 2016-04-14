options(shiny.maxRequestSize = 30*1024^2) #the upload file size limit is 30MB
#options(shiny.trace=TRUE)
#load("mynw.RData")
function(input, output, session) {
  #source("cytoscapePlot.R", local = TRUE)
  #source("cytoscapeJsNetwork.R")
  source("cytoscapePie.R", local = TRUE)
  source("plotlyNetwork.R", local = TRUE)
  source("enrichmentAnalyzer.R", local = TRUE)
  source("formatOutTable.R", local = TRUE)

  #reactiveValues
  nwval <- reactiveValues() #networks
  conval <- reactiveValues(dat = NULL) #convertID
  eraval <- reactiveValues(dat = NULL) #enrichment
  dbval <- reactiveValues(dat = NULL) #databaseURL

  ###!--BioNetwork--!###
  #inputs
  frominput <- reactive({
    fromfile = input$fromfl
    if (is.null(fromfile) && input$fromtxt==""){
      return(NULL)
    }else if(input$fromtxt!=""){#txtinput >important fileinput
      unlist(strsplit(input$fromtxt, " "))
    }else{
      read.csv(fromfile$datapath, header=TRUE, stringsAsFactors=FALSE)
    }
  })
  toinput <- reactive({
    tofile = input$tofl
    if (is.null(tofile) && input$totxt==""){
      return(NULL)
    }else if(input$totxt!=""){#txtinput >important fileinput
      unlist(strsplit(input$totxt, " "))
    }else{
      read.csv(tofile$datapath, header=TRUE, stringsAsFactors=FALSE)
    }
  })
  #action
  observeEvent(input$query, {
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Querying network", value = 0)
      for (i in 1:10) {
        progress$inc(1/10, detail = "...")
        Sys.sleep(0.2)
      }
    })
    if(input$pattern==""){
      nwval$bioNetwork = switch(input$relationship,
                                "cbc" = fetchNetwork(from=frominput(), to=toinput(), fromtype="compound", totype="compound", reltype="biochemical_reaction", returnas="dataframe"),
                                "gcp" = fetchNetwork(from=frominput(), to=toinput(), fromtype="gene", totype="protein", reltype="conversion", returnas="dataframe"),
                                "ggg" = fetchNetwork(from=frominput(), to=toinput(), fromtype="gene", totype="gene", reltype="genetic_association", returnas="dataframe"),
                                "pac" = fetchNetwork(from=frominput(), to=toinput(), fromtype="pathway", totype="compound", reltype="annotation", returnas="dataframe"),
                                "pag" = fetchNetwork(from=frominput(), to=toinput(), fromtype="pathway", totype="gene", reltype="annotation", returnas="dataframe"),
                                "pap" = fetchNetwork(from=frominput(), to=toinput(), fromtype="pathway", totype="protein", reltype="annotation", returnas="dataframe"),
                                "pcc" = fetchNetwork(from=frominput(), to=toinput(), fromtype="protein", totype="compound", reltype="catalysis", returnas="dataframe"),
                                "pcg" = fetchNetwork(from=frominput(), to=toinput(), fromtype="protein", totype="gene", reltype="control", returnas="dataframe"),
                                "pmp" = fetchNetwork(from=frominput(), to=toinput(), fromtype="protein", totype="protein", reltype="molecular_binding", returnas="dataframe"),
                                "rcg" = fetchNetwork(from=frominput(), to=toinput(), fromtype="rna", totype="gene", reltype="control", returnas="dataframe")
      )
    }else{
      nwval$bioNetwork = fetchHetNetwork(from=frominput(), to=toinput(), pattern=input$pattern, returnas="dataframe")
    }
  })
  #outputs
  output$fromtable <- renderTable({
    if (!is.null(dim(frominput()))) head(frominput(), n = 5)
  })
  output$totable <- renderTable({
    if (!is.null(dim(toinput()))) head(frominput(), n = 5)
  })
  output$consolemsgbio <- renderPrint({
    if (input$query == 0) return()
    summary(nwval$bioNetwork$edges)
  })
  output$bioNtable <- renderDataTable(datatable(
    nwval$bioNetwork$nodes, rownames = FALSE, extensions = 'Scroller', options = list(pageLength = 25, scrollY = 600, scrollX = TRUE, scrollCollapse = TRUE, columnDefs = list(list(
      targets = ncol(nwval$bioNetwork$nodes)-1,
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 60 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 60) + ' ...</span>' : data;",
        "}"))))
  ))
  output$bioEtable <- renderDataTable(datatable(
    nwval$bioNetwork$edges, rownames = FALSE, extensions = 'Scroller', options = list(pageLength = 25, scrollY = 600, scrollX = TRUE, scrollCollapse = TRUE)
  ))
  output$bioPlot <- renderPlotly({
    if (input$query == 0) return()
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Drawing network", value = 0)
      for (i in 1:10) {
        progress$inc(1/10, detail = "...")
        Sys.sleep(0.2)
      }
    })
    plotlyNetwork(nodeList = nwval$bioNetwork$nodes, edgeList = nwval$bioNetwork$edges)
  })
  output$downloadBionw <- downloadHandler(
    filename = function() {
      paste('outBioNetwork.zip')
    },
    content = function(fname) {
      fs <- c("outBioNetworkNodes.txt", "outBioNetworkEdges.txt")
      write.table(nwval$bioNetwork$nodes, file = 'outBioNetworkNodes.txt', sep='\t', row.names = F, quote = FALSE)
      write.table(nwval$bioNetwork$edges, file = 'outBioNetworkEdges.txt', sep='\t', row.names = F, quote = FALSE)
      zip(zipfile = fname, files = fs)
    },
    contentType = "application/zip"
  )
  #reset
  observeEvent(input$relationship, {
    #reset textinput onclick radio button
    updateTextInput(session, "pattern", value = "")
  })
  observeEvent(input$fromfl, {
    #reset textinput on file upload
    updateTextInput(session, "fromtxt", value = "")
  })
  observeEvent(input$tofl, {
    #reset textinput on file upload
    updateTextInput(session, "totxt", value = "")
  })
  #save
  observeEvent(input$saveBionw, {
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Saving network", value = 0)
      for (i in 1:10) {
        progress$inc(1/5, detail = "...")
        Sys.sleep(0.05)
      }
    })
    updateSelectizeInput(session, 'networkls_enr', choices = names(nwval), server = TRUE)
    updateSelectizeInput(session, 'networkls_sub', choices = names(nwval), server = TRUE)
  })
  ###!--END BioNetwork--!###

  ###!--Correlation--!###
  #inputs
  datxinput <- reactive({
    datxfile <- input$datxfl
    if (is.null(datxfile))
      return(NULL)
    read.csv(datxfile$datapath, header=TRUE, stringsAsFactors=FALSE, sep = input$xsep, row.names = 1)
  })
  datyinput <- reactive({
    datyfile <- input$datyfl
    if (is.null(datyfile))
      return(NULL)
    read.csv(datyfile$datapath, header=TRUE, stringsAsFactors=FALSE, sep = input$ysep, row.names = 1)
  })
  datytyp <- reactive({
    datyfile <- input$datyfl
    if (is.null(datyfile))
      return(NULL)
    input$datytype
  })
  #action
  observeEvent(input$computePair, {
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Computing network", value = 0)
      for (i in 1:10) {
        progress$inc(1/10, detail = "...")
        Sys.sleep(0.2)
      }
    })
    nwval$correlationNetwork = computeCorrelation(x=datxinput(), y=datyinput(), xtype=input$datxtype, ytype=datytyp(), coef=input$paircoef, pval=input$pairpval, method=input$pairmethod, returnas="dataframe")
  })
  #outputs
  output$datXExTable <- renderDataTable({
    if (!is.null(dim(datxinput()))) datatable(datxinput(), extensions = 'Scroller', class = 'cell-border hover stripe', options = list(dom = 'tp', pageLength = 5,
      columnDefs = list(list(className = 'dt-left', targets = 0)), scrollY = 190, scrollX = TRUE, scrollCollapse = TRUE))
  })
  output$datYExTable <- renderDataTable({
    if (!is.null(dim(datyinput()))) datatable(datyinput(), extensions = 'Scroller', class = 'cell-border hover stripe', options = list(dom = 'tp', pageLength = 5,
      columnDefs = list(list(className = 'dt-left', targets = 0)), scrollY = 190, scrollX = TRUE, scrollCollapse = TRUE))
  })
  output$consolemsgpaircor <- renderPrint({
    if (input$computePair == 0) return()
    summary(nwval$correlationNetwork$edges)
  })
  output$pairNtable <- renderDataTable(datatable(
    nwval$correlationNetwork$nodes, rownames = FALSE, extensions = 'Scroller', options = list(pageLength = 25, scrollY = 600, scrollX = TRUE, scrollCollapse = TRUE, columnDefs = list(list(
      targets = ncol(nwval$correlationNetwork$nodes)-1,
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 60 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 60) + ' ...</span>' : data;",
        "}"))))
  ))
  output$pairEtable <- renderDataTable(datatable(
    nwval$correlationNetwork$edges, rownames = FALSE, extensions = 'Scroller', options = list(pageLength = 25, scrollY = 600, scrollX = TRUE, scrollCollapse = TRUE)
  ))
  output$corPlot <- renderPlotly({
    if (input$computePair == 0) return()
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Drawing network", value = 0)
      for (i in 1:10) {
        progress$inc(1/10, detail = "...")
        Sys.sleep(0.2)
      }
    })
    plotlyNetwork(nodeList = nwval$correlationNetwork$nodes, edgeList = nwval$correlationNetwork$edges)
  })
  output$downloadPairnw <- downloadHandler(
    filename = function() {
      paste('outCorrNetwork.zip')
    },
    content = function(fname) {
      fs <- c("outCorrelationNodes.txt", "outCorrelationEdges.txt")
      write.table(nwval$correlationNetwork$nodes, file = 'outCorrelationNodes.txt', sep='\t', row.names = F, quote = FALSE)
      write.table(nwval$correlationNetwork$edges, file = 'outCorrelationEdges.txt', sep='\t', row.names = F, quote = FALSE)
      zip(zipfile = fname, files = fs)
    },
    contentType = "application/zip"
  )
  #save
  observeEvent(input$savePairnw, {
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Saving network", value = 0)
      for (i in 1:10) {
        progress$inc(1/5, detail = "...")
        Sys.sleep(0.05)
      }
    })
    updateSelectizeInput(session, 'networkls_enr', choices = names(nwval), server = TRUE)
    updateSelectizeInput(session, 'networkls_sub', choices = names(nwval), server = TRUE)
  })
  ###!--END Correlation--!###

  ###!--Partial correlation--!###
  #inputs
  datinput <- reactive({
    datfile <- input$datfl
    if (is.null(datfile))
      return(NULL)
    read.csv(datfile$datapath, header=TRUE, stringsAsFactors=FALSE, sep = input$datsep, row.names = 1)
  })
  #action
  observeEvent(input$computePar, {
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Computing network", value = 0)
      for (i in 1:10) {
        progress$inc(1/10, detail = "...")
        Sys.sleep(0.2)
      }
    })
    nwval$partialNetwork = computeParCorrelation(x=datinput(), xtype=input$dattype, coef=input$parcoef, pval=input$parpval, alpha=input$alpha, epsilon=input$epsilon, matrix.completion="IPF", returnas="dataframe")
  })
  #outputs
  output$datExTable <- renderDataTable({
    if (!is.null(dim(datinput()))) datatable(datinput(), extensions = 'Scroller', class = 'cell-border hover stripe', options = list(dom = 'tp', pageLength = 5,
      columnDefs = list(list(className = 'dt-left', targets = 0)), scrollY = 190, scrollX = TRUE, scrollCollapse = TRUE))
  })
  output$consolemsgparcor <- renderPrint({
    if (input$computePar == 0) return()
    summary(nwval$partialNetwork$edges)
  })
  output$parNtable <- renderDataTable(datatable(
    nwval$partialNetwork$nodes, rownames = FALSE, extensions = 'Scroller', options = list(pageLength = 25, scrollY = 600, scrollX = TRUE, scrollCollapse = TRUE, columnDefs = list(list(
      targets = ncol(nwval$partialNetwork$nodes)-1,
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 60 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 60) + ' ...</span>' : data;",
        "}"))))
  ))
  output$parEtable <- renderDataTable(datatable(
    nwval$partialNetwork$edges, rownames = FALSE, extensions = 'Scroller', options = list(pageLength = 25, scrollY = 600, scrollX = TRUE, scrollCollapse = TRUE)
  ))
  output$parPlot <- renderPlotly({
    if (input$computePar == 0) return()
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Drawing network", value = 0)
      for (i in 1:10) {
        progress$inc(1/10, detail = "...")
        Sys.sleep(0.2)
      }
    })
    plotlyNetwork(nodeList = nwval$partialNetwork$nodes, edgeList = nwval$partialNetwork$edges)
  })
  output$downloadParnw <- downloadHandler(
    filename = function() {
      paste('outPartialCorrNetwork.zip')
    },
    content = function(fname) {
      fs <- c("outPartialCorrelationNodes.txt", "outPartialCorrelationEdges.txt")
      write.table(nwval$partialNetwork$nodes, file = 'outPartialCorrelationNodes.txt', sep='\t', row.names = F, quote = FALSE)
      write.table(nwval$partialNetwork$edges, file = 'outPartialCorrelationEdges.txt', sep='\t', row.names = F, quote = FALSE)
      zip(zipfile = fname, files = fs)
    },
    contentType = "application/zip"
  )
  #save
  observeEvent(input$saveParnw, {
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Saving network", value = 0)
      for (i in 1:10) {
        progress$inc(1/5, detail = "...")
        Sys.sleep(0.05)
      }
    })
    updateSelectizeInput(session, 'networkls_enr', choices = names(nwval), server = TRUE)
    updateSelectizeInput(session, 'networkls_sub', choices = names(nwval), server = TRUE)
  })
  ###!--END Partial correlation--!###

  ###!--Similarity--!###
  #inputs
  cmptxtinput <- reactive({
    txtfile = input$cmptxtfl
    if (is.null(txtfile) && input$cmptxt==""){
      return(NULL)
    }else if(input$cmptxt!=""){#txtinput >important fileinput
      unlist(strsplit(input$cmptxt, " "))
    }else{
      read.csv(txtfile$datapath, header=TRUE, stringsAsFactors=FALSE)
    }
  })
  #action
  observeEvent(input$computeSim, {
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Computing network", value = 0)
      for (i in 1:10) {
        progress$inc(1/10, detail = "...")
        Sys.sleep(0.2)
      }
    })
    nwval$similarityNetwork = computeSimilarity(txtinput=cmptxtinput(), coef=input$tancoef, returnas="dataframe")
  })
  #outputs
  output$cmptable <- renderTable({
    if (!is.null(dim(cmptxtinput()))) head(cmptxtinput(), n = 5)
  })
  output$consolemsgsim <- renderPrint({
    if (input$computeSim == 0) return()
    summary(nwval$similarityNetwork$edges)
  })
  output$simNtable <- renderDataTable(datatable(
    nwval$similarityNetwork$nodes, rownames = FALSE, extensions = 'Scroller', options = list(pageLength = 25, scrollY = 600, scrollX = TRUE, scrollCollapse = TRUE, columnDefs = list(list(
      targets = ncol(nwval$similarityNetwork$nodes)-1,
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 60 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 60) + ' ...</span>' : data;",
        "}"))))
  ))
  output$simEtable <- renderDataTable(datatable(
    nwval$similarityNetwork$edges, rownames = FALSE, extensions = 'Scroller', options = list(pageLength = 25, scrollY = 600, scrollX = TRUE, scrollCollapse = TRUE)
  ))
  output$simPlot <- renderPlotly({
    if (input$computeSim == 0) return()
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Drawing network", value = 0)
      for (i in 1:10) {
        progress$inc(1/10, detail = "...")
        Sys.sleep(0.2)
      }
    })
    plotlyNetwork(nodeList = nwval$similarityNetwork$nodes, edgeList = nwval$similarityNetwork$edges)
  })
  output$downloadSimnw <- downloadHandler(
    filename = function() {
      paste('outSimNetwork.zip')
    },
    content = function(fname) {
      fs <- c("outSimilarityNodes.txt", "outSimilarityEdges.txt")
      write.table(nwval$similarityNetwork$nodes, file = 'outSimilarityNodes.txt', sep='\t', row.names = F, quote = FALSE)
      write.table(nwval$similarityNetwork$edges, file = 'outSimilarityEdges.txt', sep='\t', row.names = F, quote = FALSE)
      zip(zipfile = fname, files = fs)
    },
    contentType = "application/zip"
  )
  #reset
  observeEvent(input$cmptxtfl, {
    #reset textinput on file upload
    updateTextInput(session, "cmptxt", value = "")
  })
  #save
  observeEvent(input$saveSimnw, {
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Saving network", value = 0)
      for (i in 1:10) {
        progress$inc(1/5, detail = "...")
        Sys.sleep(0.05)
      }
    })
    updateSelectizeInput(session, 'networkls_enr', choices = names(nwval), server = TRUE)
    updateSelectizeInput(session, 'networkls_sub', choices = names(nwval), server = TRUE)
  })
  ###!--END Similarity--!###

  ###!--Subnetwork--!###
  #inputs
  nwls_sub <- reactive({
    input$networkls_sub
    switch(input$networkls_sub,
           "bioNetwork" = nwval$bioNetwork,
           "correlationNetwork" = nwval$correlationNetwork,
           "partialNetwork" = nwval$partialNetwork,
           "similarityNetwork" = nwval$similarityNetwork
    )
  })
  statsinput <- reactive({
    statsfile <- input$statfl
    if (is.null(statsfile))
      return(NULL)
    read.csv(statsfile$datapath, header=TRUE, stringsAsFactors=FALSE, row.names = 1, sep = input$subsep)
  })
  #action
  observeEvent(input$computeSub, {
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Computing network", value = 0)
      for (i in 1:10) {
        progress$inc(1/10, detail = "...")
        Sys.sleep(0.2)
      }
    })
    pval = statsinput()[,input$pcol]
    names(pval) = rownames(statsinput())
    if(input$fccol != 0){
      fc = statsinput()[,input$fccol]
      names(fc) = rownames(statsinput())
    }else{
      fc = NULL
    }
    nwval$subNetwork = computeSubnetwork(edgelist = nwls_sub()$edges, nodelist = nwls_sub()$nodes, pval=pval, fc=fc, fdr=input$subpval, method="bionet", returnas="dataframe")
  })
  #outputs
  output$statTable <- renderDataTable({
    if (!is.null(dim(statsinput()))) datatable(statsinput(), extensions = 'Scroller', class = 'cell-border hover stripe', options = list(dom = 'tp', pageLength = 5,
    columnDefs = list(list(className = 'dt-left', targets = 0)), scrollY = 190, scrollX = TRUE, scrollCollapse = TRUE))
  })
  output$consolemsgsub <- renderPrint({
    if (input$computeSub == 0) return()
    summary(nwval$subNetwork$edges)
  })
  output$subNtable <- renderDataTable(datatable(
    nwval$subNetwork$nodes, rownames = FALSE, extensions = 'Scroller', options = list(pageLength = 25, scrollY = 600, scrollX = TRUE, scrollCollapse = TRUE, columnDefs = list(list(
      targets = ncol(nwval$subNetwork$nodes)-2,
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 60 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 60) + ' ...</span>' : data;",
        "}"))))
  ))
  output$subEtable <- renderDataTable(datatable(
    nwval$subNetwork$edges, rownames = FALSE, extensions = 'Scroller', options = list(pageLength = 25, scrollY = 600, scrollX = TRUE, scrollCollapse = TRUE)
  ))
  output$subPlot <- renderPlotly({
    if (input$computeSub == 0) return()
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Drawing network", value = 0)
      for (i in 1:10) {
        progress$inc(1/10, detail = "...")
        Sys.sleep(0.2)
      }
    })
    plotlyNetwork(nodeList = nwval$subNetwork$nodes, edgeList = nwval$subNetwork$edges)
  })
  output$downloadSubnw <- downloadHandler(
    filename = function() {
      paste('outSubNetwork.zip')
    },
    content = function(fname) {
      fs <- c("outSubnetworkNodes.txt", "outSubnetworkEdges.txt")
      write.table(nwval$subNetwork$nodes, file = 'outSubnetworkNodes.txt', sep='\t', row.names = F, quote = FALSE)
      write.table(nwval$subNetwork$edges, file = 'outSubnetworkEdges.txt', sep='\t', row.names = F, quote = FALSE)
      zip(zipfile = fname, files = fs)
    },
    contentType = "application/zip"
  )
  #save
  observeEvent(input$saveSubnw, {
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Saving network", value = 0)
      for (i in 1:10) {
        progress$inc(1/5, detail = "...")
        Sys.sleep(0.05)
      }
    })
    updateSelectizeInput(session, 'networkls_enr', choices = names(nwval), server = TRUE)
  })
  ###!--END Subnetwork--!###

  ###!--Enrichment--!###
  #inputs
  nwls_enr <- reactive({
    input$networkls_enr
    switch(input$networkls_enr,
           "bioNetwork" = nwval$bioNetwork,
           "correlationNetwork" = nwval$correlationNetwork,
           "partialNetwork" = nwval$partialNetwork,
           "similarityNetwork" = nwval$similarityNetwork,
           "subNetwork" = nwval$subNetwork
    )
  })
  statinput <- reactive({
    statfile <- input$statdat
    if (is.null(statfile))
      return(NULL)
    read.csv(statfile$datapath, header=TRUE, stringsAsFactors=FALSE, sep = input$erasep, row.names = 1)
  })
  #action
  observeEvent(input$computeEra, {
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Computing enrichment analysis", value = 0)
      for (i in 1:10) {
        progress$inc(1/10, detail = "...")
        Sys.sleep(0.2)
      }
    })
    pval = statinput()[,input$pcol2]
    names(pval) = rownames(statinput())
    if(input$fccol2 != 0){
      fc = statinput()[,input$fccol2]
      names(fc) = rownames(statinput())
    }else{
      fc = NULL
    }
    eraval$dat = enrichmentAnalyzer(nodeList = nwls_enr()$nodes, pval = pval, fc = fc, method = input$eramethod)
  })
  #outputs
  output$statdatTable <- renderDataTable({
    if (!is.null(dim(statinput()))) datatable(statinput(), extensions = 'Scroller', class = 'cell-border hover stripe', options = list(dom = 'tp', pageLength = 5,
      columnDefs = list(list(className = 'dt-left', targets = 0)), scrollY = 190, scrollX = TRUE, scrollCollapse = TRUE))
  })
  output$consolemsgera <- renderPrint({
    if (input$computeEra == 0) return()
    summary(eraval$dat$res[,-c(1,ncol(eraval$dat$res)-1)])
  })
  output$eraattbtable <- renderDataTable(datatable(
    eraval$dat$attb, rownames = FALSE, options = list(columnDefs = list(list(
      targets = ncol(eraval$dat$attb)-1,
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 60 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 60) + ' ...</span>' : data;",
        "}"))))
  ))
  output$eratable <- renderDataTable({
    if (input$computeEra == 0) return()
    datatable(eraval$dat$res[,c(ncol(eraval$dat$res),1:ncol(eraval$dat$res)-1)], rownames = FALSE, extensions = 'Scroller',
              options = list(pageLength = 25, scrollY = 600, scrollX = TRUE, scrollCollapse = TRUE, columnDefs = list(list(
              targets = ncol(eraval$dat$res)-1,
              render = JS(
                "function(data, type, row, meta) {",
                "return type === 'display' && data.length > 5 ?",
                "'<span title=\"' + data + '\">' + data.slice(0, 4) + ' ...</span>' : data;}")
    )))) %>%
    formatStyle(
        'rank',
        backgroundColor = styleEqual(
          eraval$dat$res$rank[1:5], c('#FF0000', '#6AA3FF', '#FFD36A','#7EFF6A','#E76AFF')
        )
    )
  })
  output$frame <- renderUI({
    if (input$computeEra == 0) return(tags$span(style="font-size: 16px; color: #75A3FF;","Please provide inputs."))
    output$cyEra <- renderPrint({
      isolate({
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Drawing network", value = 0)
        for (i in 1:10) {
          progress$inc(1/10, detail = "...")
          Sys.sleep(0.2)
        }
      })
      cytoscapePie(nodeList = nwls_enr()$nodes, edgeList = nwls_enr()$edges, eraList = eraval$dat$res, nwpage = "frame")
    })
    tags$iframe(width=0,height=0,frameborder=0)
  })
  output$downloadEra <- downloadHandler(
    filename = function() {
      paste('outEnrichmentAnalysis.zip')
    },
    content = function(fname) {
      fs <- c("outNetworkNodes.txt","outNetworkEdges.txt","outAttributes.txt", "outEnrichmentTable.txt")
      write.table(nwls_enr()$nodes, file = 'outNetworkNodes.txt', sep='\t', row.names = F, quote = FALSE)
      write.table(nwls_enr()$edges, file = 'outNetworkEdges.txt', sep='\t', row.names = F, quote = FALSE)
      write.table(eraval$dat$attb, file = 'outAttributes.txt', sep='\t', row.names = F, quote = FALSE)
      write.table(formatOutTable(eraval$dat$res), file = 'outEnrichmentTable.txt', sep='\t', row.names = F, quote = FALSE)
      zip(zipfile = fname, files = fs)
    },
    contentType = "application/zip"
  )
  ###!--END Enrichment--!###

  ###!--Convert ID--!###
  #inputs
  txtinput <- reactive({
    txtfile = input$txtfl
    if (is.null(txtfile) && input$txt==""){
      return(NULL)
    }else if(input$txt!=""){#txtinput >important fileinput
      unlist(strsplit(input$txt, " "))
    }else{
      read.csv(txtfile$datapath, header=TRUE, stringsAsFactors=FALSE, sep = input$cvsep)
    }
  })
  #action
  observeEvent(input$convert, {
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Converting IDs", value = 0)
      for (i in 1:10) {
        progress$inc(1/10, detail = "...")
        Sys.sleep(0.2)
      }
    })
    conval$dat = convertId(x=txtinput(), nodetype=input$nodetype, searchby=input$searchby, exactmatch=input$exactmatch, returnas="dataframe")
  })
  #outputs
  output$idstable <- renderDataTable({
    if (!is.null(dim(txtinput()))) datatable(txtinput(), rownames = FALSE, extensions = 'Scroller', class = 'cell-border hover stripe', options = list(dom = 'tp', pageLength = 5,
    columnDefs = list(list(className = 'dt-left', targets = 0)), scrollY = 190, scrollX = TRUE, scrollCollapse = TRUE))
  })
  output$consolemsgconvert <- renderPrint({
    if (input$convert == 0) return()
    summary(conval$dat[,1:3])
  })
  output$converttable <- renderDataTable(datatable(
    conval$dat, rownames = FALSE, extensions = 'Scroller', options = list(pageLength = 25, scrollY = 600, scrollX = TRUE, scrollCollapse = TRUE)
  ))
  output$downloadid <- downloadHandler(
    filename = function() {
      paste('outGrinnId.txt')
    },
    content = function(file) {
      write.table(conval$dat, file, sep='\t', row.names = F, quote = FALSE)
    }
  )
  #reset
  observeEvent(input$txtfl, {
    #reset textinput on file upload
    updateTextInput(session, "txt", value = "")
  })
  ###!--END Convert ID--!###

  ###!--SetDatabase--!###
  #action
  observeEvent(input$settingdb, {
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Setting database", value = 0)
      for (i in 1:10) {
        progress$inc(1/10, detail = "...")
        Sys.sleep(0.1)
      }
    })
    dbval$dat = setGraphDb(url=input$dburl)
  })
  #output
  output$currentdb <- renderPrint({
    if (input$settingdb == 0 || input$resettingdb == 0) return(nld)
    dbval$dat
  })
  #reset
  observeEvent(input$resettingdb, {
    #reset db url to default
    isolate({
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Resetting to default database", value = 0)
      for (i in 1:10) {
        progress$inc(1/10, detail = "...")
        Sys.sleep(0.1)
      }
    })
    dbval$dat = setGraphDb(url="http://localhost:7474/db/data/")
  })
  ###!--END SetDatabase--!###

  ###!For cytoscape test --!##
#   ntext <- eventReactive(input$goButton, {
#     input$n
#   })
#   output$frame2 <- renderUI({
#     if (input$goButton == 0) return(tags$span(style="font-size: 16px; color: #75A3FF;","Please provide inputs."))
#     output$cy1 <- renderPrint({
#       cytoscapePlot(nodeList = mynw$nodes, edgeList = mynw$edges, nwpage="frame2")
#     })
#     tags$div()
#   })
  ##############################
  # Title.
  output$authorANDdate<- output$authorANDdate2 <- renderText({
    paste0(input$author.input, ", ", format(input$date.input, format = "%B %d %Y"))
  })
  output$"dataname" <- output$"dataname2" <- renderText({
    if(input$subtitle.input==""){
      paste0(gsub('\\.csv|\\.xlsx','',input$InputData$name))
    }else{
      input$subtitle.input
    }
  })
  # Data description.
  aggregated.data <- reactive({
    if(input$uploadtype == "Load example dataset"){
      return(list("expression"=NULL, "feature"=NULL,"phenotype"=NULL))
    }else{
      if(input$uploadtype == "Upload aggregated dataset"){
        result=tryCatch(load.aggregated.data(input$InputData),
                        error = function(e){
                          list("expression"=NULL, "feature"=NULL,"phenotype"=NULL)
                        })
        return(result) # e, f, p.
      }else if(input$uploadtype=="Upload expression, feature, phenotype datasets seperately"){

        result = list("expression" = tryCatch(load.expression.data(input$InputDataExp),error = function(e){NULL}),
                      "feature" =  tryCatch(load.feature.data(input$InputDataF),error = function(e){NULL}),
                      "phenotype" =tryCatch(load.phenotype.data(input$InputDataP),error = function(e){NULL}))
        return(result) # e, f, p.
      }else{
        #!!!
      }
    }
  })


  f<-rf <-NULL # dependency
  summary.data <- reactive({
    summary.aggregated.data(aggregated.data(),factor.name = list(factor(),f)[[which.max(sapply(list(factor(),f),length))]],
                            repeated.factor.name = list(repeatedfactor(),rf)[[which.max(sapply(list(repeatedfactor(),rf),length))]])
  })
  #dependency
  output$con1 <- output$con11 <- renderText({
    if(is.null(summary.data()[["warnings"]])){
      return("  ")
    }else{
      return("")
    }
  })

  output$editfactorUI = renderUI({
    fluidPage(
      p(summary.data()[["warnings"]])
      ,conditionalPanel(condition = "output.con1=='  '"
                        ,h4(strong("Experimental Design"))
                        ,div(
                          div(style="display:inline-block",h5(strong("Factors:")))
                          ,div(style="display:inline-block",paste(summary.data()[["factor.name"]],collapse = ", "))
                          ,div(style="display:inline-block",p("."))
                          ,div(style="display:inline-block",actionLink("editfactor","Edit"))
                        )
                        ,conditionalPanel(condition = "output.editfactor == '  '"
                          ,sidebarLayout(
                            sidebarPanel(
                              div(style="display:inline-block",
                                   checkboxGroupInput("factor", label = "Select Factors:",
                                                      choices = colnames(summary.data()[["dataset"]][["phenotype"]]),
                                                      selected = summary.data()[["factor.name"]]))
                              ,div(style="display:inline-block",
                                   actionButton("editfactordone","Submit"))
                            ),
                            mainPanel(
                              DT::dataTableOutput("View.pData2")
                            )
                          )
                        )
                        )
    )
  })
  #dependency.
  output$editfactor <- renderText({
    if(input$editfactor%%2){
      return("  ")
    }else{
      return(" ")
    }
  })
  factor <- reactive({
    if(input$editfactordone==0){# when just upload the data.
      return(NULL)
    }else{
        f <<- input$factor
    }
  })

  output$editrepeatedfactorUI<-renderUI({
    fluidPage(
      conditionalPanel(condition = "output.con11=='  '"
                       ,div(
                         div(style="display:inline-block",h5(strong("repeated Factors:")))
                         ,div(style="display:inline-block",paste(summary.data()[["repeated.factor.name"]],collapse = ", "))
                         ,div(style="display:inline-block",p("."))
                         ,div(style="display:inline-block",actionLink("editrepeatedfactor","Edit"))
                       )
                       ,conditionalPanel(condition = "output.editrepeatedfactor == '  '"
                                         ,sidebarLayout(
                                           sidebarPanel(
                                             div(style="display:inline-block",
                                                 checkboxGroupInput("repeatedfactor", label = "Select repeated Factors:"
                                                                    ,choices = summary.data()[["factor.name"]]
                                                                    ))
                                             ,div(style="display:inline-block",
                                                  actionButton("editrepeatedfactordone","Submit"))
                                           ),
                                           mainPanel(
                                             DT::dataTableOutput("View.pData3")
                                           )
                                         )

                       )
      )
    )
  })
  #dependency
  output$editrepeatedfactor <- renderText({
    if(input$editrepeatedfactor%%2){
      return("  ")
    }else{
      return(" ")
    }
  })
  repeatedfactor <- reactive({
    if(input$editrepeatedfactordone==0){# when just upload the data.
      return(NULL)
    }else{
      rf <<- input$repeatedfactor
    }
  })




  # For Viewdata tabs
  output$View.eData <- DT::renderDataTable(
  aggregated.data()[["expression"]], options = list(lengthChange = FALSE)
  )
  output$View.fData <- DT::renderDataTable(
    aggregated.data()[["feature"]], options = list(lengthChange = FALSE)
  )
  output$View.pData <- output$View.pData2 <- output$View.pData3 <- DT::renderDataTable(
    aggregated.data()[["phenotype"]], options = list(lengthChange = FALSE)
  )
  observeEvent(input$SubmitModificationpData, {
    updateCheckboxInput(session = session, "editViewpData",
                        "", FALSE)
  })


  # Data Exploratory
  ## 1st PCA.


}
