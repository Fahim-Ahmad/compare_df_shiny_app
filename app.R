# Loading required packages
library(dplyr)
library(tidyr)
library(stringr)
library(shiny)
library(shinydashboard)

# Custom function for comparing two data frames
# source("https://raw.githubusercontent.com/Fahim-Ahmad/r_custom_functions/main/compare_df_function.R")
compare_df <- function(df1, df2, unique_id_df1, unique_id_df2) {
  df1 <- df1 %>% 
    select(KEY = all_of(unique_id_df1), everything()) %>% 
    mutate(across(-KEY, function(x)
      x = as.character(x)
    )) %>% 
    pivot_longer(-KEY, values_to = "value_1") %>% 
    mutate(value_1 = str_squish(value_1))
  
  df2 <- df2 %>% 
    select(KEY = all_of(unique_id_df2), everything()) %>% 
    mutate(across(-KEY, function(x)
      x = as.character(x)
    )) %>% 
    pivot_longer(-KEY, values_to = "value_2") %>% 
    mutate(value_2 = str_squish(value_2))
  
  df_both <- full_join(df1, df2, by = c("KEY", "name"))
  
  diff <- df_both %>% 
    filter((value_1 != value_2) | (is.na(value_1) & !is.na(value_2)) | (!is.na(value_1) & is.na(value_2))) %>%
    rename(column_name = name, value_in_df1 = value_1, value_in_df2 = value_2)
  
  if(nrow(diff) == 0) {
    paste0("No difference in df1 and df2")
  } else {
    return(diff) 
  }
}

##### test
dt1 <- dt2 <- mtcars
dt1$id <- dt2$id <- rownames(mtcars)
dt2$am[dt2$am == 1] <- 2
dt2$wt[dt2$wt > 100] <- 105
dt2$cyl[dt2$cyl == 8] <- 7
dt2 <- dt2[1:30, ]
compare_df(df1 = dt1, df2 = dt2, unique_id_df1 = "id", unique_id_df2 = "id")
# write.csv(dt1, "test_data/df1.csv", row.names = F)
# write.csv(dt2, "test_data/df2.csv", row.names = F)
#####

# ui part of the dashboard
ui <- dashboardPage(
  dashboardHeader(title = "compare_df( )"),
  dashboardSidebar(
    fileInput(inputId = "df1", label = "", accept = ".csv", placeholder = "Choose CSV File", buttonLabel = "Upload df1"),
    fileInput(inputId = "df2", label = NULL, accept = ".csv", placeholder = "Choose CSV File", buttonLabel = "Upload df2"),
    br(),
    uiOutput("unique_id_df1"),
    uiOutput("unique_id_df2")
  ),
  dashboardBody(
    splitLayout(cellWidths = c("30%", "30%", "40%"),
                tableOutput("glimpse_df1"),
                tableOutput("glimpse_df2"),
                list(
                  HTML("<b>Developed by:</b><a href='https://fahimahmad.netlify.app'> Fahim Ahmad</a><br>
                       <b>Purpose:</b> The purpose of developing this app is to compare two datasets.<br>
                       Please save your datasets in .csv files and use the upload buttons to read them.<br>
                       Please make sure to select the correct unique identifier in both datasets."),
                  br(),
                  downloadLink(outputId = "download_test_data", label = "Click here to download example data.",
                                 style = "color: steelblue"
                  ),
                  br(),br(),
                  HTML("<b>Source code:</b> You can have the script from my <a href='https://github.com/Fahim-Ahmad/compare_df_shiny_app'>GitHub</a> account.<br>
                       Please feel free to contribute or report typos, codebreak, or raise any concerns in the issues section.")
                )
                ),
    htmlOutput("select_identifier_msg"),
    htmlOutput("different_identifier_msg"),
    htmlOutput("identifier_not_unique_in_df1"),
    htmlOutput("identifier_not_unique_in_df2"),
    hr(),
    uiOutput("calc_diff"),
    br(),
    htmlOutput("select_identifier_note"),
    br(),
    DT::dataTableOutput("diff_table"),
    htmlOutput("diff_text")
  )
)

# server part of the dashboard
server <- function(input, output) {
  
  df1 <- reactive({
    file <- input$df1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    read.csv(file$datapath, header = TRUE)
  })
  
  df2 <- reactive({
    file <- input$df2
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    read.csv(file$datapath, header = TRUE)
  })
  
  output$unique_id_df1 <- renderUI({
    selectInput(inputId = "id_df1", label = "Please select the unique identifier in df1", choices = c("", colnames(df1())), selected = "")
  })
  
  output$unique_id_df2 <- renderUI({
    selectInput(inputId = "id_df2", label = "Please select the unique identifier in df2", choices = c("", colnames(df2())), selected = "")
  })
  
  output$glimpse_df1 <- renderTable({
    data.table::data.table(
     'Glimpse of df1' = c("Number of rows", "Number of columns", "Selected unique identifier"),
     '#' = c(nrow(df1()), ncol(df1()), input$id_df1)
    )
  })
  
  output$glimpse_df2 <- renderTable({
    data.table::data.table(
      'Glimpse of df2' = c("Number of rows", "Number of columns", "Selected unique identifier"),
      '#' = c(nrow(df2()), ncol(df2()), input$id_df2)
    )
  })
  
  output$select_identifier_msg <- renderUI({
    req(input$df1)
    req(input$df2)
    
    if(input$id_df1 == "" & input$id_df2 == ""){
      HTML('<font color="green">NOTE:</font> Please select the unique identifiers in df1 and df2.')
    } else if(input$id_df1 == "" & input$id_df2 != "") {
      HTML('<font color="green">NOTE:</font> Please select the unique identifier in df1.')
    } else if(input$id_df1 != "" & input$id_df2 == "") {
      HTML('<font color="green">NOTE:</font> Please select the unique identifier in df2.')
    }
  })
  
  output$different_identifier_msg <- renderUI({
    req(input$df1)
    req(input$df2)
    
    if((input$id_df1 != input$id_df2) & (input$id_df1 != "" & input$id_df2 != "")){
      HTML('<font color="green">NOTE:</font> Different unique identifier is selected in df1 and df2.')
    }
  })
  
  output$identifier_not_unique_in_df1 <- renderUI({
    req(input$df1)
    req(input$df2)
    
    if (input$id_df1 != "") {
      n <- count(df1(), get(input$id_df1)) %>% filter(n>1) %>% nrow()
      if (n != 0 ) { # n!=0
        HTML(paste0('<font color="red">WARNING:</font> the ', input$id_df1, ' variable is not uniquely identified in df1'))
      }
    }
  })
  
  output$identifier_not_unique_in_df2 <- renderUI({
    req(input$df1)
    req(input$df2)
    
    if (input$id_df2 != "") {
      n <- count(df2(), get(input$id_df2)) %>% filter(n>1) %>% nrow()
      if (n != 0 ) {
        HTML(paste0('<font color="red">WARNING:</font> the ', input$id_df2, ' variable is not uniquely identified in df2'))
      }
    }
  })
  
  output$calc_diff <- renderUI({
    req(input$df1)
    req(input$df2)
    actionButton(inputId = "compare_diff", label = "Click to compare df1 & df2",
                 style="color: #fff; background-color: #337ab7; border-color: gray")
  })
  
  diff <- reactive({
    diff_df1_df2 <- compare_df(df1 = df1(), df2 = df2(), unique_id_df1 = input$id_df1, unique_id_df2 = input$id_df2)
    
    if ("data.frame" %in% class(diff_df1_df2)) {
      diff_df1_df2 <- diff_df1_df2 %>%
        mutate(across(everything(), function(x)
          x = ifelse(is.na(x), "NA", x)
        ))
    } else {
      diff_df1_df2
    }
  })
  
  observeEvent(input$compare_diff, {
    if(input$id_df1 != "" & input$id_df2 != "") {
      if ("data.frame" %in% class(diff())) {
        output$diff_table <- DT::renderDataTable({
          DT::datatable(diff(),
                        extensions = c('Buttons','RowGroup'),
                        callback=DT::JS('$("button.buttons-copy").css("background","skyblue");
                    $("button.buttons-csv").css("background","skyblue");
                    return table;'),
                        options = list(
                          dom = 'Bfrtip',
                          lengthMenu = list(c(10, -1), c('10', 'All')),
                          pageLength = 10,
                          buttons = list(
                            c('copy', 'csv'),
                            list(
                              extend = "collection",
                              text = 'Show All',
                              action = DT::JS("function ( e, dt, node, config ) {
                                    dt.page.len(-1);
                                    dt.ajax.reload();

                                }
                                ")
                            )
                          ),
                          columnDefs = list(list(className = 'dt-center', targets = 3:4))
                        )
          ) %>%
            DT::formatStyle('value_in_df2',  color = 'red')
        })
      }
    }
  })
  
  observeEvent(input$compare_diff, {
    if(input$id_df1 != "" & input$id_df2 != "") {
      if (class(diff()) == "character") {
        output$diff_text <- renderText({
          HTML("<b>No differences observed!<b>")
        })
      }
    }
  })
  
  output$download_test_data <- downloadHandler(
    filename <- function() {
      paste("test_data", "zip", sep=".")
    },

    content <- function(file) {
      file.copy("test_data.zip", file)
    },
    contentType = "application/zip"
  )
    
}

# create Shiny app objects
shinyApp(ui, server)

