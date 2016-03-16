library(shiny)

shinyServer(function(input, output) {
  #source("script.R",local = TRUE)
  
  # Initialising variables
  node <- reactiveValues()
  corp <- reactiveValues()
  query <- reactiveValues()
  
  word <- eventReactive(input$lookup,{
    find_word_id(corpus=input$corpus,attr='lemma',what=input$query)
  })
  
  word_cpos <- eventReactive(input$lookup,{
       find_word_pos(corpus=input$corpus,attr=input$what,what.id=word())
 })
  # Retrieve corpus size
  corp$N <- reactive ({ corp_info(input$corpus,input$what)$N })
  
  # Build query
  set_query <- eventReactive(input$lookup,{
    node[["id"]] <<- word()
    node[["corpPos"]] <<- word_cpos()
    #node[["stats"]] <<- node_stats(corp_size = corp_info(input$corpus,input$what)$N)
    node[["stats"]] <<- node_stats(corp_pos = word_cpos(), corp_size = corp$N())
    query[["query"]] <<- input$query
    query[["corpus"]] <<- input$corpus
    query[["what"]] <<- input$what
    query[["string"]] <<- gsub("\\s","",paste(trimws(input$corpus),".",trimws(input$what),collapse=""))
    query[["string_forms"]] <<- gsub("\\s","",paste(trimws(input$corpus),".",trimws("word"),collapse=""))
  })
  
  output$vars <- renderText({
    set_query()
    unlist(node[["id"]])
  })
  
  output$vars1 <- renderText({
    set_query()
    node[["corpPos"]][1:10]
  })
  
  output$vars2 <- renderText({
    set_query()
    paste(query$string_forms,collapse = "+")
  })
  
  forms <- reactiveValues()
  
  #run_forms <-
  observe ({
    set_query()
    forms_stats <- forms_stats(string=query$string_forms,corp_pos = unlist(node[["corpPos"]]),corp_size=corp$N())
    forms$forms_cpos <<- forms_stats$forms_cpos
    forms$forms_cpos_sum <<- forms_stats$forms_cpos_sum
  })

  output$test <- renderText({
    #run_forms()
    forms$forms_cpos
    #node$id
    #scatter1()$test
    #forms$forms_cpos[1:10,]
    #scatter1()$test[1:10,]
    #forms$forms_cpos_sum
  })
  
  output$tab1 <- renderTable({
    #run_forms()
    forms$forms_cpos[1:10,]
  })
  
  # plot1 <- eventReactive(input$lookup,{
  #   plot_forms1(forms$forms_cpos)
  #   
  # })
  
forms_plot1 <- eventReactive(input$lookup,{
  list(
    a = plot_forms1(forms$forms_cpos),
    b = plot_forms2(forms$forms_cpos),
    c = plot_forms3(forms_cpos = forms$forms_cpos,forms$forms_cpos_sum)
    )
  })

output$forms_plot1 <- renderPlot({
  forms_plot1()$a
})
output$forms_plot2 <- renderPlot({
  forms_plot1()$b
})
output$forms_plot3 <- renderPlot({
  forms_plot1()$c
})

scatter1 <- eventReactive(input$lookup,{
  plot_forms_scatter(forms$forms_cpos)
})

output$scatt_pairs <- renderPlot({
  scatter1()$pairs
})

output$scatt_scatt1 <- renderPlot({
  scatter1()$scatt1
})

output$scatt_scatt2 <- renderPlot({
  scatter1()$scatt2
})

output$ecdf <- renderPlot({
  scatter1()$ecdf
})

output$boxpl <- renderPlot({
  scatter1()$boxpl
})

output$corrpl <- renderPlot({
  scatter1()$corrpl
})



meta_vis <- eventReactive(input$lookup,{
  meta_plot(corpus="", forms_cpos = forms$forms_cpos)
})

output$meta <- renderPlot({
  meta_vis()$meta_plot
})

# output$container <- renderUI({
#    tableOutput("tab1")
# })


### CORPUS ###

corpus_ranking <- reactive(
  corp_lemma_rank(corpus = input$corpus)
)

ranking <- isolate(corpus_ranking())

output$corpus_rank <- renderTable(
  ranking[1:10,]
)

# output$corpus_plot <- renderPlot(
#   plot_corpus_rank(ranks = corpus_ranking()$rank, ids = corpus_ranking()$ids, freqs = corpus_ranking()$freqs, emph_id = node[["id"]], string = input$query )
# )



output$corpus_plot <- renderPlot(
  plot_corpus_rank(rank_table = ranking, node[["id"]], string=input$query)
)

output$corpus_plot_log <- renderPlot(
  plot_corpus_rank_log(rank_table = ranking, node[["id"]], string=input$query)
)


})