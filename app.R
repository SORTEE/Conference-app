library(shiny)
library(shinythemes)
library(shinyjs)
library(dplyr)
library(lubridate) # for the dates
library(DT)



# Deploy app

## Don't forget to set working directory to the folder where the app.R is
## Always the following lines as comments when running the app, otherwise the app won't run
## Run two lines of code below to deploy the app
# library(rsconnect)
# deployApp()


# Load data 

## The following lines upload the program data from a Google Sheets write it as a csv file on your computer
## After doing this, leave the lines as comments otherwise the app will take ages to run
# library(googlesheets4)
## gs4_auth() # This line of code is only if you need to authenticate with your Google account
# program <- read_sheet('https://docs.google.com/spreadsheets/d/1gNS9jzGhAI1xbf3wZkm3rATaAa0Z68ysGG0IJEQsNj0/edit?gid=510110456#gid=510110456',
#                       skip = 1) # skip the first line
# program$Program[which(is.na(program$Program))] <- "" # replace NA by ""
# program$`Start Time` <- as.character(format(program$`Start Time`)) ## to solve the problem with write.csv and the entry "2024-10-16 00:00:00"
# program$`End Time` <- as.character(format(program$`End Time`))
# write.csv(program, file = "Program.csv", row.names = FALSE)

## Once you have the program data on your computer, you can just read it directly from your computer
## The program data must store the start and end times as UTC format as "10/15/2024  7:00:00 AM" for example
program <- read.csv("Program.csv")
dat <- program


# Get time zone

## The following lines extract the time zones of different parts of the world on your meeting date
meeting <- ymd_hms("2024-10-15 07:00:00", tz = "UTC") # add your meeting date here, use UTC
zones <- data.frame(time_zone = OlsonNames()) %>%
  mutate(local = force_tz(meeting, time_zone),
         diff = as.numeric(meeting - local)/(60*60), # difference in hours
         plusminus = ifelse(diff<0, '', '+'), # minus already added
         hours = ifelse(diff<0, sprintf('%03d', floor(diff)),
                        sprintf('%02d', floor(diff))), # format depends on positive/negative
         minutes = ifelse(floor(diff) != diff, '30', '00'),
         utc = paste(plusminus, hours, ':', minutes, sep=''),
         time_zone_plus = paste(time_zone, ' (', utc, ')', sep='' )) %>%
  dplyr::select(-plusminus, -hours, -minutes)


# ui

ui <- fluidPage(
  useShinyjs(),
  # theme + logo + title
  theme = shinytheme("united"),
  tags$img(src = "SORTEE_logo_twitter.png",  height="10%", width="10%", align="left"), # logo stored in the www folder
  titlePanel("SORTEE 2024 conference program "),
  # adjust link colour
  tags$head( 
    tags$style(HTML("
      a {
        color: inherit; 
        text-decoration: underline;
      }
      a:hover {
        color: #0000EE; 
      }
    "))
  ),
  
  br(),
  
  # info displayed at the top of the web app
  fluidRow(
    column(10,
           p(
             "Welcome to the program app for the 2024 SORTEE Conference!",
             br()
             , "Find below the program table in UTC with all event information."),
           p(
             strong("Click on any event"),
             " on the table to access its information."
           ),
           p(
             strong("Type and select your country below"),
             "to have the program in your local time.",
             br(),
             "You can then export the program in csv, excel or pdf format."
           ),
           selectizeInput(inputId = "UTC",
                          label = NULL,
                          choices = c("",zones$time_zone_plus),
                          options = list(placeholder = 'Type or select your country name')),
           p(
             "If anything goes wrong, refer to the conference information pack, send a message on the '#conf2024-technicalassistance’Slack channel or a direct message on Slack to the technical assistance people mentioned in the last column 'Technical assistance'."           )
    ),
    
    # DT table output which shows the schedule
    column(DT::dataTableOutput("table"),style = "font-size:90%", width=12))
)


# server

server <- function(input, output, session)
{
  # DT table output
  output$table <- DT::renderDataTable(DT::datatable({
    
    # Convert the start time and end time depending on the time zone selected
    ## extract offset from the input
    if(input$UTC==""){ 
      offset <- "+00:00" # if no country is selected then use UTC
    }else{
      offset <- gsub("^.*\\((.*)\\)$", '\\1' , input$UTC)
    }
    
    ## change to numeric
    (UTC_hour <- as.numeric(substr(offset,2,3)))
    (UTC_minutes <- as.numeric(substr(offset,5,6)))
    
    if(substr(offset,1,1)=="+"){
      offset_minutes <- UTC_hour*60 + UTC_minutes
    } else {
      offset_minutes <- -UTC_hour*60 - UTC_minutes
    }
    
    ## adjust the date
    dat$convert_start <- ymd_hms(dat$Start.Time) + lubridate::minutes(offset_minutes)
    dat$convert_end <- ymd_hms(dat$End.Time) + lubridate::minutes(offset_minutes)
    
    # Create the columns of the table
    dat$Day <- wday(dat$convert_start, label = TRUE, week_start = 1)
    dat$`Date` <- paste0("Oct ", day(dat$convert_start)) # Adjust the month 
    dat$`Start Time` <- format.Date(dat$convert_start, "%H:%M")
    dat$`End Time` <- format.Date(dat$convert_end, "%H:%M")
    
    # Adjust column names
    names(dat)[which(names(dat)=="Technical.assistance")] <- "Technical assistance"
    
    # Extract the final columns to be displayed in the program table on the web app
    dat[,c("Day",
            "Date",
            "Start Time",
            "End Time",
            "Program",
            "Technical assistance")]
    
  },
  #DT table objects
  rownames = FALSE,
  extensions = 'Buttons', 
  options = list(scrollX=TRUE, lengthMenu = 97,
                 rowGroup = list(dataSrc = 1), extensions = 'RowGroup',
                 paging = FALSE, searching = TRUE,
                 fixedColumns = TRUE, autoWidth = FALSE,
                 ordering = TRUE, dom = 'Bftsp',
                 columnDefs = list(list(className = 'dt-center', targets = 0:5)), # adjust targets depending on the number of columns. In this case 6 columns (0:5)
                 buttons = list('csv', 'excel',list(extend = 'pdf', # options to export the program table
                                                    pageSize = 'A4',
                                                    orientation = 'landscape'))),
  
  class = c('cell-border stripe', "hover"),
  selection = list(mode="single", target="cell")
  
  ) %>% 
    # format size of table columns
    formatStyle(c("Date", "Day", "Start Time", "End Time"), width='5px') %>%
    formatStyle(c("Program"), width='450px') %>%
    formatStyle(c("Technical assistance"), width='150px')
  %>%
    # format colour depending on the first letter of the program title
    formatStyle("Program",  backgroundColor = styleEqual(
      dat$Program[substr(dat$Program,1,1) %in% c("I","L", "C")], "#ffeaa3"))
  %>%
    formatStyle("Program",  backgroundColor = styleEqual(
      dat$Program[substr(dat$Program,1,1)=="U"], "#eaffde"))
  %>% 
    formatStyle("Program",  backgroundColor = styleEqual(
      dat$Program[substr(dat$Program,1,1)=="W"], "#f2dbff"))
  %>%
    formatStyle("Program",  backgroundColor = styleEqual(
      dat$Program[substr(dat$Program,1,1)=="H"], "#d7eafb"))
  %>%
    formatStyle("Program",  backgroundColor = styleEqual(
      dat$Program[substr(dat$Program,1,1)=="P"], "#ffe0e5"))  
  %>%
    # format specific start and end times in bold
    formatStyle("End Time",
      fontWeight = styleRow(c(1,3,53), "bold"))
  %>%
    formatStyle("Start Time",
                fontWeight = styleRow(c(2,4,54), "bold"))
      )
  
  # observe when a cell is clicked.
  shiny::observeEvent(input$table_cell_clicked, {
    req(length(input$table_cell_clicked) > 0)
    cell.data <- (input$table_cell_clicked)
    
    # how input$table_cell_clicked looks like if you need to edit it
    #str(input$table_cell_clicked)
    #List of 3
    #$ row  : int 2 # row number of clicked cell
    #$ col  : int 4 # column number of clicked cell
    #$ value: chr "Plenary 1: Chuan-Peng Hu - Promoting Open Science via grassroots network in a developing country"
    
    # create a popup for an event based on where the cell was clicked, what row etc.
    if(cell.data[["col"]] == 4){ # col where the program is. Only create a popup for cells in the program column
      if(!is.null(cell.data[["value"]])){
        row <- cell.data[["row"]]
        showModal(modalDialog(
          title = dat$Program[row], # title of the popup
          # content of the popup
          HTML(paste("Facilitators =", program$Facilitators[row]),
               "<br>",
               "<br>",
               paste("Topic =", program$Topic[row]),
               "<br>",
               "<br>",
               paste("Outline =", program$Outline[row]),
               "<br>",
               "<br>",
               paste("Audience =", program$Audience[row]),
               "<br>",
               "<br>"),
          # footer of the popup
          footer = tags$div(style = "text-align: left;", HTML(
               paste("Slack channel name =", program$Slack.channel.name[row]),
               "<br>",
               "<br>",
               paste("Slack channel link = <a href=", program$Slack.channel.link[row],">",program$Slack.channel.link[row],"</a>"),
               "<br>",
               "<br>"
               )),
          #HTML("Zoom link = <a href=", paste(zooms[grepl(code, zooms$No), ][10]),">Click here</a>"),
          #"<br>",
          #"<br>",
          #paste("Slack channel name = ", zooms[grepl(code, zooms$No), ][4]),
          #"<br>",
          #"<br>",
          #"<br>"),
          #footer =  HTML("<a href= https://tinyurl.com/Join-SORTEE-Slack </a>", "Join SORTEE Slack",  "</p>"),
          easyClose = TRUE,
          size = "l"
        ))
      }
    }
  }
      )
}


# Run the application 
shinyApp(ui = ui, server = server)
