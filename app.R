#
# Interagency Suicide Prevention Task Force Dashboard
#
#
# ############ Libraries ############
# library(RSQLite) # use SQL Lite for storage
library(shiny) # the app
library(lubridate) # date formatting
library(dplyr) # data manipulation
library(DT) # for web style tables
library(stringr) # handle bad text inputs
# library(googlesheets4) # connecting to googlesheets
library(ggplot2) # plots
library(scales)  # plot axes / labels customization
library(bslib) # nice CSS themes
library(DBI)  # connect to databases (Digital Ocean)
library(RPostgres)  # PostgreSQL connectors
library(sodium)  # encrypted passkey protection
# auto match plots with the current theme
thematic::thematic_shiny(font = "auto")

# ############ Authentication ##############
# Header part is deprecated, from when we used GoogleSheets
# >>>>> BEGIN HEADER <<<<<
# # run once per project to set up Auto Authentication
# # keep any related project private in deployment
# library(googledrive)
# setwd(".") # whatever directory for your app.R file
# 
# # designate a cache
# options(gargle_oauth_cache=".secrets")
# 
# # check the value of the option
# gargle::gargle_oauth_cache()
# 
# # trigger auth on purpose --> store tocken in the cache
# drive_auth()
# 
# # check for the tocken
# list.files(".secrets/")
# options(
#   gargle_oauth_cache = ".secrets",
#   gargle_oauth_email = TRUE
# )
# >>>>> END HEADER <<<<<
#
## Save / Load the database connector password
## make sure to navigate to the app folder using setwd()
## and create a ".secrets" folder ahead of time
# passphrase <- charToRaw("")  # place passphrase here
# key <- sha256(charToRaw("NSSPDashboard"))
# nonce <- random(24)
# cipher <- data_encrypt(
#   msg = passphrase,
#   key = key,
#   nonce = nonce
# )
# 
# writeBin(c(cipher), ".secrets/cipher")
# writeBin(c(key), ".secrets/gatekeeper")
# writeBin(c(nonce), ".secrets/cipher_n")

key <- readBin(
  ".secrets/gatekeeper",
  what="raw",
  32L
)
nonce <- readBin(
  ".secrets/cipher_n",
  what="raw",
  24L
)
passphrase <- rawToChar(
  data_decrypt(
    readBin(".secrets/cipher", what = "raw", 40L),
    key = key,
    nonce = nonce
  )
)

## Digital Ocean Connector
con <- DBI::dbConnect(
  drv = Postgres(),
  host     = "db-postgresql-nyc3-39798-do-user-19196867-0.h.db.ondigitalocean.com",
  dbname   = "defaultdb",
  user     = "doadmin",
  password = passphrase,
  port     = 25060
)
############ FIRST TIME POSTGRESQL SET UP ################
# ## Set up the actions table using the existing data as
# ## a template
# test_data <- readr::read_csv(
#   "testing.csv"
# )
# # fix assessment data type
# test_data$Assessment <- as.character(test_data$Assessment)
# # rename for later dbSendQuery compatibility issues
# names(test_data) <- stringr::str_to_lower(names(test_data))
# names(test_data) <- stringr::str_remove(
# names(test_data), " "
# )
# # create table using zero rows
# DBI::dbCreateTable(
#   con,
#   name = "actions",
#   fields = head(
#     test_data,
#     0)
# )
# # test table creation
# DBI::dbListTables(con)
# DBI::dbReadTable(con, "actions")
# # Upload the values themselves now
# DBI::dbAppendTable(
# conn = con,
# name = "actions",
# test_data
# )
# # check for insert
# DBI::dbReadTable(con, "actions")

######## MAIN ################


# data fields to save
fields <- c(
  "primarysd",
  "primaryobjective",
  "allobjectives",
  "primaryagency",
  "actions",
  "assessment",
  "timeline",
  "status",
  "statuscomments",
  "reviewernotes",
  "lastupdate"
)

# # /path/to/sqlite/database
# sqlitePath <- "TaskForceDB"
# table <- "Actions"
# 
# saveData <- function(data) {
#   # Connect to the database
#   db <- dbConnect(SQLite(), sqlitePath)
#   # Construct the update query by looping over the data fields
#   query <- sprintf(
#     "INSERT INTO %s (%s) VALUES ('%s')",
#     table, 
#     paste(names(data), collapse = ", "),
#     paste(data, collapse = "', '")
#   )
#   # Submit the update query and disconnect
#   dbExecute(db, query)
#   dbDisconnect(db)
# }
# 
# loadData <- function() {
#   # Connect to the database
#   db <- dbConnect(SQLite(), sqlitePath)
#   # Construct the fetching query
#   query <- sprintf("SELECT * FROM %s", table)
#   # Submit the fetch query and disconnect
#   data <- dbGetQuery(db, query)
#   dbDisconnect(db)
#   data
# }

# table <- "https://docs.google.com/spreadsheets/d/1Xv64efGa2lqrj1J4bunzXKd0WuE3KTe5ftAYpCTsY7c/edit?gid=410839740#gid=410839740"

# redefine the pie graphic to lack label lines
pie <- function (x, labels = names(x), edges = 200, radius = 0.8, clockwise = FALSE, 
          init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45, 
          col = NULL, border = NULL, lty = NULL, main = NULL, ...) 
{
  if (!is.numeric(x) || any(is.na(x) | x < 0)) 
    stop("'x' values must be positive.")
  if (is.null(labels)) 
    labels <- as.character(seq_along(x))
  else labels <- as.graphicsAnnot(labels)
  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L]) 
    xlim <- (pin[1L]/pin[2L]) * xlim
  else ylim <- (pin[2L]/pin[1L]) * ylim
  dev.hold()
  on.exit(dev.flush())
  plot.window(xlim, ylim, "", asp = 1)
  if (is.null(col)) 
    col <- if (is.null(density)) 
      c("white", "lightblue", "mistyrose", "lightcyan", 
        "lavender", "cornsilk")
  else par("fg")
  if (!is.null(col)) 
    col <- rep_len(col, nx)
  if (!is.null(border)) 
    border <- rep_len(border, nx)
  if (!is.null(lty)) 
    lty <- rep_len(lty, nx)
  angle <- rep(angle, nx)
  if (!is.null(density)) 
    density <- rep_len(density, nx)
  twopi <- if (clockwise) 
    -2 * pi
  else 2 * pi
  t2xy <- function(t) {
    t2p <- twopi * t + init.angle * pi/180
    list(x = radius * cos(t2p), y = radius * sin(t2p))
  }
  for (i in 1L:nx) {
    n <- max(2, floor(edges * dx[i]))
    P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
    polygon(c(P$x, 0), c(P$y, 0), density = density[i], angle = angle[i], 
            border = border[i], col = col[i], lty = lty[i])
    P <- t2xy(mean(x[i + 0:1]))
    lab <- as.character(labels[i])
    if (!is.na(lab) && nzchar(lab)) {
      #lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
      text(1.1 * P$x, 1.1 * P$y, labels[i], xpd = TRUE,
           adj = ifelse(P$x < 0, 1, 0), ...)
    }
  }
  title(main = main, ...)
  invisible(NULL)
}

# integer breaks
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

# save data function
saveData <- function(
    data, new_data=TRUE, max_id=NULL
    ) {
  # The data must be a dataframe rather than a named vector
  data <- data %>% as.list() %>% data.frame()
  if (new_data==TRUE) {
    # Add ID and Update Counts
    data$id <- as.character( max_id + 1 )
    data$updatecount <- 0
    data$newestrecord <- "Yes"
    data$lastupdate <- today()
    # reorder
    data <- data[,c(12:14,1:11)]
    # Add the data as a new row
    # sheet_append(table, data, sheet = "R Actions") #google
    dbAppendTable(
      conn = con,
      name = "actions",  # table name
      value = data
    )
  } else {
    # Update the old data to be "No" for newest record
    # range_write(
    #   table,
    #   as.data.frame("x"="No"),
    #   sheet = "R Actions",
    #   range = paste0(
    #     # just edit one cell
    #     "C", row+1  # google sheets has header as first row
    #   ),
    #   col_names = FALSE
    # )
    dbSendQuery(
      # Some interesting notes:
      # # Something about this and the related functions is
      # broken in that it forces all table and column names
      # to lower case before sending to the database AND
      # enforces the new names in a case dependent manner.
      # # It also assumes "." in a col name to be a schema
      # delimiter.
      # # I had to rename the table and the pertinent
      # columns to make this work.
      conn=con,
      statement = paste0(
        "Update public.actions ",
        "set newestrecord = 'No' ",
        "where id ='",
        data$id[1],  # extract the current item's ID
        "'" # stored as a character for dashboard reasons
      )
    )
    # Append the updated row
    # Preprocess elsewhere in the update reactive function
    #  * needs to have the ID carry over, +1 update count,
    #    and today() as the latest update
    # sheet_append(table, data, sheet = "R Actions")
    RPostgres::dbAppendTable(
      conn = con,
      name = "actions",  # table name
      value = data
    )
  }
}

# # Deprecated Google Sheets code
# read_sheet(
#   table,
#   sheet = "R Actions"
# ) %>%
#   # convert Google text field to dates
#   # reformat a few others to make the filters nice
#   mutate(
#     "Primary SD"=as.character(`Primary SD`),
#     "Primary Agency"=factor(
#       `Primary Agency`,
#       levels=c(
#         "DHS CBP",
#         "DHS ICE",
#         "DHS OHS",
#         "DHS USCG",
#         "DOD",
#         "DOL OSHA",
#         "DOL VETS",
#         "DOJ",
#         "DOT FRA",
#         "DOT NHTSA",
#         "ED",
#         "HHS ACF",
#         "HHS ACL",
#         "HHS AHRQ",
#         "HHS ASPE",
#         "HHS CDC",
#         "HHS CMS",
#         "HHS FDA",
#         "HHS HRSA",
#         "HHS IHS",
#         "HHS NIH",
#         "HHS OSG",
#         "HHS OASH",
#         "HHS SAMHSA",
#         "HUD",
#         "SAMHSA",
#         "USDA ERS",
#         "USDA FS",
#         "USDA NASS",
#         "USDA NIFA",
#         "USDA OPPE",
#         "USDA RD",
#         "VA"
#       )
#     ),
#     "Timeline"=factor(
#       Timeline,
#       levels=fy_set()  # up to current FY + 3
#     ),
#     "Status"=factor(
#       Status,
#       levels=c(
#         "Not Started",
#         "Dependent On Funding",
#         "At Risk Of Delay",
#         "Delayed",
#         "On Track",
#         "Completed"
#       )
#     ),
#     "Last Update"=ymd(`Last Update`),
#     ID=as.character(ID)
#   )

loadData <- function() {
  # Read the data
  dbReadTable(
    conn = con,
    "actions"
    ) %>%
    rename(
      # translate from the SQL table col names to
      # something more human readable for the display table
      ID = id,
      "Primary SD" = primarysd,
      "Primary Agency" = primaryagency,
      "Update Count" = updatecount,
      "Newest Record" = newestrecord,
      "Primary Objective"=primaryobjective,
      "All Objectives" = allobjectives,
      "Reviewer Notes"=reviewernotes,
      "Last Update"=lastupdate,
      "Status Comments"=statuscomments,
      "Status"=status,
      "Timeline"=timeline,
      "Assessment"=assessment,
      "Actions"=actions
    ) %>%
    # reformat a few fields to make the filters nice
    mutate(
      "Primary SD"=as.character(`Primary SD`),
      "Primary Agency"=factor(
        `Primary Agency`,
        levels=c(
          "DHS CBP",
          "DHS ICE",
          "DHS OHS",
          "DHS USCG",
          "DOD",
          "DOL OSHA",
          "DOL VETS",
          "DOJ",
          "DOT FRA",
          "DOT NHTSA",
          "ED",
          "HHS ACF",
          "HHS ACL",
          "HHS AHRQ",
          "HHS ASPE",
          "HHS CDC",
          "HHS CMS",
          "HHS FDA",
          "HHS HRSA",
          "HHS IHS",
          "HHS NIH",
          "HHS OSG",
          "HHS OASH",
          "HHS SAMHSA",
          "HUD",
          "SAMHSA",
          "USDA ERS",
          "USDA FS",
          "USDA NASS",
          "USDA NIFA",
          "USDA OPPE",
          "USDA RD",
          "VA"
        )
        ),
      "Timeline"=factor(
        Timeline,
        levels=fy_set()  # up to current FY + 3
        ),
      "Status"=factor(
        Status,
        levels=c(
          "Not Started",
          "Dependent On Funding",
          "At Risk Of Delay",
          "Delayed",
          "On Track",
          "Completed"
          )
        ),
      ID=as.character(ID)
    )
}

# find current fiscal year and quarter for graphics
fy_Q <- function(){
  x <- quarter(today(), fiscal_start = 10, with_year = TRUE)
  y <- str_extract(str_remove(x, "^.."), "^..")
  z <- str_extract(x, ".$")
  paste0("Q", z, " FY", y)
  }

# find current fiscal year and build a sequence
# for FY+3 in the drop down menus
fy_set <- function(){
  current_FY <- case_when(
    # subtract 2000 to remove the leading two digits
    month(today()) >= 10 ~ year(today()) + 1 - 2000,
    TRUE ~ year(today()) - 2000
  )
  FYs <- seq(24, current_FY + 3)  # go out +3 years
  return(
    paste0("FY",FYs)
  )
}

##### set theme ####
primary_col <- "#003F72"
theme <- bs_theme(
  #preset = "morph",
  # Controls the default grayscale palette
  bg = "#FFFFFF", fg = "#404040",
  # Controls the accent (e.g., hyperlink, button, etc) colors
  primary = primary_col, secondary = "#11137C",
  base_font = c("Arial"),
  code_font = c("Courier", "monospace"),
  heading_font = c("Arial"),
  # Can also add lower-level customization
  "input-border-color" = "#003F72"
)

# # set color ramp for figures
cdc_blues <- colorRampPalette(
  c("#D2D1D0","#1C99CE")
)
cdc_blue <- function(x){
  y <- cdc_blues(x)
  y <- c(  # rotate the gray color to the end
    y[2:length(y)], y[1]
    )
  return(y)
}
status_ramp <- colorRampPalette(
  c("#6D467B","#D2D1D0","#115C7C")
)

# # set manual colors for figures
StatusColors <- c(
  "#404040",  #at risk of delay
  "#1C99CE", #completed
  "#D2D1D0", #delayed
  "#0065A3", #dep on funding 
  "#115C7C", #missing
  "#6D467B", #not started
  "#9D5FA7" #on track
)
names(StatusColors) <- c(
  "At Risk Of Delay"
  ,"Completed"
  ,"Delayed"
  ,"Dependent On Funding"
  ,"Missing"
  ,"Not Started"
  ,"On Track"
)
colScale1 <- scale_fill_manual(name = "Status",
                        values = StatusColors)

####### Define UI #########
ui <- navbarPage("Interagency Suicide Prevention Board |",
    theme = theme,
    tabPanel(
      "Actions Table",
        width=12,
        # Give usage text
        h3("General Usage:"),
        p(paste0(
          "The below table is interactive. By default ",
          "it shows the most recent update for each action",
          " item."
          )
        ),
        h6(em("Filters--")),
        p(paste0(
          "Use the 'Search' bar above the table to ",
          "execute a global search of the table, or use ",
          "the search field above a specific column to ",
          "filter based on just that column. The table ",
          "can be ordered using the arrows above each ",
          "column."
          )),
        h6(em("Entering Data--")),
        p(paste0(
          "To enter wholly new line items, use ",
          "the 'Data Entry' tab, above. To update ",
          "an existing item, find ",
          "it below, double click on the field to edit, ",
          "and click outside the table to commit your edit.",
          " If your edit is incorrectly formatted, it will ",
          "not be saved and you will get an error message.",
          " The incorrectly entered data will be visible ",
          "in the table for you to correct and resubmit."
        )),
      h6(em("Downloads--")),
      p(paste0(
          " To download a copy of the table, click on",
          " the 'CSV' or 'Excel' buttons above the table. ",
          "Note that the downloaded copy will only contain ",
          "the data that you have filtered using the search",
          " menus."
          )),
        # Print the current table
        DT::dataTableOutput(
          "responses"
          )
    ),
    # Data entry page
    tabPanel(
      "Data Entry",
      textInput("primarysd", "Primary SD", value = NULL),
      textInput("primaryobjective", "Primary Objective",""),
      textInput("allobjectives","All Objectives", ""),
      selectInput(
        "primaryagency","Primary Agency",
        list(
          "DHS CBP"="DHS CBP",
          "DHS ICE"="DHS ICE",
          "DHS OHS"="DHS OHS",
          "DHS USCG"="DHS USCG",
          "DOD"="DOD",
          "DOL OSHA"="DOL OSHA",
          "DOL VETS"="DOL VETS",
          "DOJ"="DOJ",
          "DOT FRA"="DOT FRA",
          "DOT NHTSA"="DOT NHTSA",
          "ED"="ED",
          "HHS ACF"="HHS ACF",
          "HHS ACL"="HHS ACL",
          "HHS AHRQ"="HHS AHRQ",
          "HHS ASPE"="HHS ASPE",
          "HHS CDC"="HHS CDC",
          "HHS CMS"="HHS CMS",
          "HHS FDA"="HHS FDA",
          "HHS HRSA"="HHS HRSA",
          "HHS IHS"="HHS IHS",
          "HHS NIH"="HHS NIH",
          "HHS OSG"="HHS OSG",
          "HHS OASH"="HHS OASH",
          "HHS SAMHSA"="HHS SAMHSA",
          "HUD"="HUD",
          "SAMHSA"="SAMHSA",
          "USDA ERS"="USDA ERS",
          "USDA FS"="USDA FS",
          "USDA NASS"="USDA NASS",
          "USDA NIFA"="USDA NIFA",
          "USDA OPPE"="USDA OPPE",
          "USDA RD"="USDA RD",
          "VA"="VA"
          )
        ),
      textInput("actions","Actions",""),
      textInput("assessment","Assessment",""),
      selectInput("timeline","Timeline",
        as.list(
          fy_set()
        )),
      selectInput(
        "status","Status",
        list(
          "Not Started"="Not Started"
           ,"Dependent On Funding"="Dependent On Funding"
           ,"At Risk Of Delay"="At Risk Of Delay"
           ,"Delayed"="Delayed"
           ,"On Track"="On Track"
           ,"Completed"="Completed"
           )
        ),
      textInput("statuscomments","Status Comments",""),
      tooltip(
        textInput("reviewernotes","Reviewer Notes",""),
        "Leave empty unless you are also the reviewer",
        id = "ReviewerTip",
        placement = "right"
        ),
      dateInput("lastupdate",
                "Last Update",
                value = today()
                ),
      actionButton(
        "submit",
        "Submit New Line Item")
    ),
    
    # Graphics page
    ## For demo:
    ## * Display a card for open and closed projects
    ## * Bars with counts of closed projects by agency
    ## * Bars with counts of project type 1 by agency
    ## * Bars of statuses, facet by agency
    ## * Histogram of time to close projects, facet by
    ##     agency
    tabPanel(
      "Dashboard",
      fluidRow(
        span(
        textOutput(
        "filters_applied"
        ),
        style=paste(
          paste0("color:",primary_col,";"),
          "font-size:18px; font-style:bold"
        )
      )),
      layout_column_wrap(
        value_box(
          title = "Actions Currently Open:",
          value = textOutput('OpenProjects'),
          height = "100px",
          fill = FALSE
        ),
        value_box(
          title = "Actions Completed:",
          value = textOutput('ClosedProjects'),
          height = "100px",
          fill = FALSE
        ),
        plotOutput(
          "StatusPie"
        ),
        plotOutput(
          "FY_total"
        ),
        plotOutput(
          "StratDirect"
        ),
        plotOutput(
          "ActionTimeline"
        ),
        selectInput(
          "FYSelect",
          label="Select a FY",
          choices = as.list(fy_set()),
          selected="FY24"
          ),
        plotOutput(
          "FYActions"
        ),
        # plotOutput(
        #   "FY"
        # ),
        # plotOutput(
        #   "Action1plot"
        # ),
        selectInput(
          "StatusSelect",
          label="Select a Status",
          choices = 
            list(
              "Not Started"="Not Started"
              ,"Dependent On Funding"="Dependent On Funding"
              ,"At Risk Of Delay"="At Risk Of Delay"
              ,"Delayed"="Delayed"
              ,"On Track"="On Track"
              ,"Completed"="Completed"
            ),
          selected="On Track"
        ),
        plotOutput(
          "Statuses"
          #,height = "700px"
        ),
        plotOutput(
          "SDTimeline"
        ),
        plotOutput(
          "Fallback"
        ),
        width = 0.5, # every item covers half the page width
        heights_equal = "row" # each row has a height
      )
    )
    
    # # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #     sidebarPanel(
    #         sliderInput("bins",
    #                     "Number of bins:",
    #                     min = 1,
    #                     max = 50,
    #                     value = 30)
    #     ),
    # 
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #        plotOutput("distPlot")
    #     )
    # )
)

####### Define server ########
server <- function(input, output, session) {

  # bs_themer()
  
  # Track the state of edits (saved / not saved)
  saved <- reactiveVal(
    value=FALSE,  # no edits or not saved yet
    label="Saved Value State Var"
    )
  
  # Aggregation function for all form data
  # formData <- reactive({
  #   data <- sapply (
  #     fields, function(x) {
  #       # transform any dates to character for Google
  #       if (is.Date(input[[x]]) && length(input[[x]]) > 0) {
  #         as.character(input[[x]])
  #       } else if (length(input[[x]])==0){
  #         ""
  #       } else input[[x]]
  #     }
  #     )
  #   data
  # })
  formData <- reactive({
    data <- sapply (
      fields, function(x) {
        if (length(input[[x]])==0){
          NA  # explicitly declare NA
        } else input[[x]]
      }
    )
    data
  })
  
  # When new data is submitted, save the form data
  # and send a notification to the user
  observeEvent(
    input$submit,
    {saveData(
      formData(),
      new_data = TRUE,
      max_id = max(as.numeric(
        reactiveData()$ID
        ))
    )
    saved(TRUE)
    }
  )

  observeEvent(
    list(input$submit, input$responses_cell_edit),
    showNotification(
      "Table reloading",
      type = 'message',
      duration = 3
    )
  )
  
  observeEvent(
    # edit the existing table and pass to the original data
    input$responses_cell_edit, {
      # If the status is being changed, verify that the
      # right text was input
      if (input$responses_cell_edit$col == 10 &
          !grepl( # if not in this group, problem
            paste0(
            "^not started$|^Delayed$|^on track$|^completed$",
            "|^at risk of delay$|^dependent on funding$"
            ),
            input$responses_cell_edit$value,
            ignore.case = TRUE)) {
        showNotification(
          paste0(
            "Data not saved, check your spelling. ",
            "Statuses must be: ",
            "Not Started, Dependent on Funding, ",
            "At Risk of Delay, ",
            "Delayed, On Track, or Completed"
          ),
          type = 'error',
          duration = 10
        )
        }
      if (input$responses_cell_edit$col == 10) {
        validate(
          need(
            grepl( # if in this group, great
            paste0(
              "^not started$|^Delayed$|^on track$|^completed$",
              "|^at risk of delay$|^dependent on funding$"
            ),
            input$responses_cell_edit$value,
            ignore.case = TRUE),
            "Check your spelling"
          )
        )
      }
      
      ### append to the original data ###
      # build a buffer for the edits
      dynamic_data <- reactiveData()[
        # pull the specific row
        # Note: this is unaffected by UI side filters
        input$responses_cell_edit$row,
      ]
      
      # make sure the edit is passed over
      # use if/else to handle issue with edited date formats
      dynamic_data[
        # col uses 0 base indexing in a breach of R standard
        1,input$responses_cell_edit$col+1
        ] <-
        if (input$responses_cell_edit$col > 12) {
          # used to be more date cols at the end,
          # now this will just grab the Last Update col
          ymd(input$responses_cell_edit$value)
        } else if (input$responses_cell_edit$col == 10) {
          # Force status updates to title case
          str_to_title(input$responses_cell_edit$value)
        } else {input$responses_cell_edit$value}
      
      # reformat for DigitalOcean, reindex the update count
      dynamic_data <- dynamic_data %>%
        mutate(
          `Update Count` = `Update Count` + 1,
          `Last Update` = today(),
        )
      # database names
      names(dynamic_data)[4:14] <- fields
      names(dynamic_data)[1:3] <- c(
        "id", "updatecount", "newestrecord"
      )
      
      # add the new row to the Google Sheet
      saveData(
        dynamic_data,
        new_data = FALSE # update, see function
      )
      saved(TRUE)  # change save state variable
    })

  # Show the previous responses
  # * update with the current response when Submit is clicked
  # * OR edit the current response in place
  reactiveData <- reactive({
      saved()
      # this reactive needs to be forced to FALSE after
      # the trigger
      saved(FALSE)
      loadData()
      })
  
  # render text to inform the user of current filters
  output$filters_applied <- renderText({
    paste("The following filters are currently applied:  ",
    paste(
      paste(# "Col name = "
        names(reactiveData())[
          input$responses_search_columns != ""
        ],
        "="
        ),
      input$responses_search_columns[
        input$responses_search_columns != ""
        ],
      collapse = ", "
      )
    )
  })
  
  ## Card Values as reactive calculations
  output$OpenProjects <- reactive({
    dim ( # find dimensions
    reactiveData() %>%
    slice( # current items
      input$responses_rows_all
    ) %>%
    filter(
      `Newest Record` == "Yes",
      Status != "Completed" # still open, not completed
    )
  )[1]}) # num of remaining rows
  
  output$ClosedProjects <- reactive({
    dim (
    reactiveData() %>%
      slice( # current items
        input$responses_rows_all
      ) %>%
      filter(
        `Newest Record` == "Yes",
        Status == "Completed" # closed
      )
  )[1]}) # num of remaining rows
  
  ###### Graphs ######
  ### Strategic Direction
  output$StratDirect <- 
    renderPlot({
      reactiveData() %>%
        slice( # current items
          input$responses_rows_all
        ) %>%
        filter(
          `Newest Record` == "Yes"
        ) %>%
        mutate(
          # make a factor for consistent coloring
          "Primary SD"=factor(
            `Primary SD`,
            levels = c("1","2","3","4")
          )
        ) %>%
        # then summarize
        group_by(`Primary SD`) %>%
        summarize("Count" = n()) %>% {
          pie(
            .$Count,
            labels=sprintf(
              "SD %s, n=%i",
              .$`Primary SD`,
              .$Count
            ),
            col = c(
              "#1C99CE",
              "#9D5FA7",
              "#404040",
              "#D2D1D0"
              ),
            border = "white",
            main = paste0(
              "Total Actions by Strategic Direction (n=",
              sum(.$Count),
              ")"
            ),
            radius = 1.05
          )
    }
  })
  
  ### Stacked bars of Strategic Direction by FY
  output$SDTimeline <- renderPlot({
    reactiveData() %>%
      slice( # current items
        input$responses_rows_all
      ) %>%
      filter(
        `Newest Record` == "Yes"
      ) %>%
      # transform any missing values
      mutate(
        Timeline = case_when(
          is.na(Timeline) ~ "Missing",
          Timeline == "" ~ "Missing",
          TRUE ~ Timeline
        ),
        "Primary SD" = factor(
          as.character(`Primary SD`),
          levels = c("1","2","3","4")
        )
      ) %>%
      group_by(Timeline, `Primary SD`) %>%
      summarize("Count" = n()) %>%
      # get percent by FY
      group_by(Timeline) %>%
      mutate(
        Percent = 100*Count / sum (Count),
        FY_Counts = sum (Count),
        # put labels in the middle
        y_label = 100 - (cumsum(Percent)-0.5*Percent),
        # # predefine colors
        # colors = case_when(
        #   `Primary SD` == '1' ~ "#1C99CE",
        #   `Primary SD` == '2' ~ "#9D5FA7",
        #   `Primary SD` == '3' ~ "#404040",
        #   `Primary SD` == '4' ~ "#D2D1D0"
        # )
      ) %>%
      {
        ggplot(
          .,
          aes(
            x = Timeline,
            y = Percent,
            fill = `Primary SD`
          )
        ) +
          geom_col() +
          ggtitle("Strategic Direction of Actions by Timeline") +
          scale_y_continuous(
            breaks = c(0,25,50,75,100),
            labels = c('0%','25%','50%','75%','100%')
          ) +
          scale_x_discrete(
            # The groupings produce a weird issue whereby
            # each count is of a status. This work around
            # allows us to keep those groupings for stacks
            # and gives the correct labels on the x-axis
            labels = paste0(
              unique(.[,c("Timeline","FY_Counts")])$Timeline,
              " (n=",
              unique(.[,c("Timeline","FY_Counts")])$FY_Counts,
              ")"
            )
          ) +
          scale_fill_manual(
            values = c(
              "#1C99CE",
              "#9D5FA7",
              "#404040",
              "#D2D1D0"
            )
          ) +
          geom_text(
            aes(y = y_label, label = Count), 
            #vjust = 1, #nudge_y = -.5,
            size = 6, fontface = "bold",
            color="white",
            show.legend = FALSE
          ) +
          theme(
            plot.title=element_text(size=18),
            axis.text=element_text(size=15),
            axis.text.x = element_text(angle = 45, vjust=0.5),
            axis.title=element_text(size=18, face = "bold")
          )}
  })
  # output$FY_Pie <- renderPlot({
  #   reactiveData() %>%
  #     slice( # current items
  #       input$responses_rows_all
  #     ) %>%
  #     filter(
  #       `Newest Record` == "Yes"
  #     ) %>%
  #     # transform any missing values
  #     mutate(
  #       Timeline = case_when(
  #         is.na(Timeline) ~ "Missing",
  #         Timeline == "" ~ "Missing",
  #         TRUE ~ Timeline
  #       )
  #     ) %>%
  #     # then summarize
  #     group_by(Timeline) %>%
  #     summarize("Count" = n()) %>%
  #   {
  #     pie(
  #       .$Count,
  #       labels=sprintf(
  #             "%s, n=%i",
  #             .$Timeline,
  #             .$Count
  #           ),
  #       col = cdc_blue(nrow(.)+1)[1:nrow(.)],
  #       border = "white",
  #       main = paste0(
  #         "Total Actions by Timeline (n=",
  #         sum(.$Count),
  #         ")"
  #       ),
  #       radius = 1.05
  #     )
  #   }
  # })
  
  output$StatusPie <- renderPlot({
    reactiveData() %>%
      slice( # current items
        input$responses_rows_all
      ) %>%
      filter(
        `Newest Record` == "Yes",
        !is.na(Timeline),
        Timeline!=""
      ) %>%
      # transform any missing values
      mutate(
        Status = case_when(
          is.na(Status) ~ "Missing",
          Status == "" ~ "Missing",
          Status == "Dependent On Funding" ~ 
            "Dependent\nOn Funding",
          Status == "At Risk Of Delay" ~
            "At Risk\nOf Delay",
          TRUE ~ Status
        )
      ) %>%
      # then summarize
      group_by(Status) %>%
      summarize("Count" = n()) %>%
      {
        pie(
          .$Count,
          labels=sprintf(
            "%s (%i%%) \nn=%i",
            .$Status,
            round(100*.$Count/sum(.$Count),0),  # percent
            .$Count
          ),
          col = if (any(.$Status=="Missing")){c(
            "#404040",  #at risk of delay
            "#1C99CE", #completed
            "#D2D1D0", #delayed
            "#0065A3", #dep on funding 
            "#115C7C", #missing
            "#6D467B", #not started
            "#9D5FA7" #on track
          )} else{
            c(
              "#404040",  #at risk of delay
              "#1C99CE", #completed
              "#D2D1D0", #delayed
              "#0065A3", #dep on funding 
              "#6D467B", #not started
              "#9D5FA7" #on track
            )
          },
          border = "white",
          main = paste0(
            "Status of all actions in the\nFederal",
            " Action Plan, as of ",
            fy_Q(),  # outputs "Qx FYxx"
            " (n=",
            sum(.$Count),
            ")"
          ),
          radius = 0.96
        )
      }
  })
  
  ### Stacked bars of Status by FY
  output$ActionTimeline <- renderPlot({
    reactiveData() %>%
      slice( # current items
        input$responses_rows_all
      ) %>%
      filter(
        `Newest Record` == "Yes"
      ) %>%
      # transform any missing values
      mutate(
        Status = case_when(
          is.na(Status) ~ "Missing",
          Status == "" ~ "Missing",
          # Status == "Dependent On Funding" ~ 
          #   "Dependent\nOn Funding",
          # Status == "At Risk Of Delay" ~
          #   "At Risk\nOf Delay",
          TRUE ~ Status
        ),
        Timeline = case_when(
          is.na(Timeline) ~ "Missing",
          Timeline == "" ~ "Missing",
          TRUE ~ Timeline
        )
      ) %>%
      group_by(Timeline, Status) %>%
      summarize("Count" = n()) %>%
      # get percent by FY
      group_by(Timeline) %>%
      mutate(
        Percent = 100*Count / sum (Count),
        FY_Counts = sum (Count),
        # put labels in the middle
        y_label = 100 - (cumsum(Percent)-0.5*Percent)
        ) %>%
    {
    ggplot(
      .,
      aes(
        x = Timeline,
        y = Percent,
        fill = Status
      )
    ) +
      geom_col() +
      ggtitle("Status of Actions by Timeline") +
      colScale1 +
      scale_y_continuous(
        breaks = c(0,25,50,75,100),
        labels = c('0%','25%','50%','75%','100%')
      ) +
      scale_x_discrete(
        # The groupings produce a weird issue whereby
        # each count is of a status. This work around
        # allows us to keep those groupings for stacks
        # and gives the correct labels on the x-axis
        labels = paste0(
          unique(.[,c("Timeline","FY_Counts")])$Timeline,
          " (n=",
          unique(.[,c("Timeline","FY_Counts")])$FY_Counts,
          ")"
        )
      ) +
      geom_text(
        aes(y = y_label, label = Count), 
        #vjust = 1, #nudge_y = -.5,
        size = 6, fontface = "bold",
        color="white",
        show.legend = FALSE
      ) +
      theme(
        plot.title=element_text(size=18),
        axis.text=element_text(size=15),
        axis.text.x = element_text(angle = 45, vjust=0.5),
        axis.title=element_text(size=18, face = "bold")
      )}
  })
  
  ### Horizontal Bars of FY Actions, with selectized input
  output$FYActions <- renderPlot({
    # check that the given FY is valid
    validate(
      need(
        input$FYSelect %in% reactiveData()$Timeline[
          # given the newest records
          reactiveData()$`Newest Record` == "Yes"
        ],
        "No data for that FY: Please select a different FY"
      )
    )
    
    reactiveData() %>%
      slice( # current items
        input$responses_rows_all
      ) %>%
      filter(
        `Newest Record` == "Yes",
        input$FYSelect == Timeline
      ) %>%
      # transform any missing values
      mutate(
        Status = case_when(
          is.na(Status) ~ "Missing",
          Status == "" ~ "Missing",
          # Status == "Dependent On Funding" ~ 
          #   "Dependent\nOn Funding",
          # Status == "At Risk Of Delay" ~
          #   "At Risk\nOf Delay",
          TRUE ~ Status
        )
      ) %>%
      group_by(Status) %>%
      summarize("Count" = n()) %>%
      mutate(
        Percent = 100*Count / sum(Count),
        color = case_when(
          Status == "At Risk Of Delay" ~ "#D09E00",
          Status == "Completed" ~ "#9D5FA7",
          Status == "Delayed" ~ "#9E4326",
          Status == "On Track" ~ "#6D467B",
          TRUE ~ "#D2D1D0"
        )
        ) %>%
      {
      ggplot(
        .,
        aes(
          y = Status,
          x = Percent,
          fill= color
        )
      ) +
      geom_col() +
      ggtitle(
        paste0(
        "Status of ",
        input$FYSelect,
        " Actions from the\nFederal Action Plan,\nas of ",
        fy_Q(),
        " (n=",
        sum(.$Count),
        ")"
      )) +
      scale_x_continuous(
        breaks = c(0,25,50,75,100),
        labels = c('0%','25%','50%','75%','100%')
      ) +
      scale_y_discrete(
        # The groupings produce a weird issue whereby
        # each count is of a status. This work around
        # allows us to keep those groupings for stacks
        # and gives the correct labels on the x-axis
        labels = paste0(
          .$Status,
          " (",
          round(.$Percent,0),
          "%)"
        )
      ) +
      xlab("") +
      ylab("") +
      scale_fill_identity(guide = "none") +
      geom_label(
        aes(label = Count), 
        hjust = 1, nudge_x = -.5,
        size = 6, fontface = "bold",
        ## turn into white box without outline
        fill = "white", label.size = 0
      ) +
      theme_void() +
      theme(
        plot.title=element_text(size=18,color = "#404040"),
        axis.text=element_text(size=15,color = "#404040"),
        axis.text.x = element_blank()
      )}
  })
  
  ### Bar chart of statuses by agency
  output$Statuses <- renderPlot({# filter the data for closed projects
    reactiveData() %>%
      slice( # current items
        input$responses_rows_all
      ) %>%
      filter(
        `Newest Record` == "Yes",
        Status == input$StatusSelect
      ) %>%
      # then summarize
      group_by(`Primary Agency`) %>%
      summarize("Count" = n()) %>%
      {
    ggplot(
      .,
      aes(
        x = `Primary Agency`,
        y = Count,
        fill= `Primary Agency`
      )
      ) +
      geom_col() +
      scale_fill_manual(
        values = status_ramp(
          dim(.)[1]  # number of rows
        )
      ) +
      scale_y_continuous(
        breaks = function(Count){
          if (max(Count) >= 4) {
            (max(Count) %/% 4) * c(1,2,3,4)
          } else {
            c(1,2,3,4)
          }
        }
      ) +
      ggtitle(paste0(
        "Count of ",
        input$StatusSelect,
        " Projects by Agency"
        )) +
      # theme_void() +
      theme(
        plot.title=element_text(size=18),
        axis.text=element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust=0.5),
        axis.title=element_text(size=16, face = "bold")
      )
  }})
  
  output$FY_total <- renderPlot({
    ggplot(
      # filter the data
      reactiveData() %>%
        slice( # current items
          input$responses_rows_all
        ) %>%
        filter(
          `Newest Record` == "Yes"
        ) %>%
        # transform any missing values
        mutate(
          Timeline = case_when(
            is.na(Timeline) ~ "Missing",
            Timeline == "" ~ "Missing",
            TRUE ~ Timeline
          )
        ) %>%
        # then summarize
        group_by(Timeline) %>%
        summarize("Count" = n()),
      aes(
        x = Timeline,
        y = Count,
        fill=Timeline
      )
    ) +
      geom_bar(stat = "identity") + # identity b/c summarized
      ggtitle("Total Actions by Fiscal Year") +
      xlab("Fiscal Year") +
      scale_fill_manual(
        values = cdc_blue(
          length(unique(reactiveData()$Timeline))
        )
      ) +
      scale_y_continuous(
        breaks = function(Count){
          if (max(Count) >= 4) {
          (max(Count) %/% 4) * c(1,2,3,4)
          } else {
            c(1,2,3,4)
          }
        }
      ) +
      theme(
        plot.title=element_text(size=18),
        axis.text=element_text(size=15),
        axis.text.x = element_text(angle = 45, vjust=0.5),
        axis.title=element_text(size=18, face = "bold")
      )
  })
  
  ### Timeline of FY Counts
  output$Fallback <- renderPlot({
    reactiveData() %>%
      slice( # current items
        input$responses_rows_all
      ) %>%
      filter(
        `Newest Record` == "Yes",
        !is.na(Timeline)
      ) %>%
      group_by(Timeline) %>%
      summarize("Count" = n()) %>%
      mutate(
        Timeline = as.numeric(
          str_replace(Timeline, "^..", "20")
          )
      ) %>%
      rename("Fiscal Year" = Timeline) %>%
      {
        ggplot(
          .,
          aes(
            x = `Fiscal Year`,
            y = Count,
            group = 1
          )
          ) +
          geom_line(
            col = "#115C7C",
            linewidth=3,
            lineend = "round"
            ) +
          ggtitle("Count of Actions by Timeline") +
          ylim(0,max(.$Count)+1) +
          annotate(
            "text",
            x=mean(.$`Fiscal Year`),  # center
            y=1,
            label= paste(
              "Number of actions missing an FY:",
              sum(is.na(reactiveData()$Timeline))
            ),
            size=5.5
          ) +
          scale_x_continuous(
            breaks = integer_breaks()
          ) +
          scale_y_continuous(
            breaks = integer_breaks()
          ) +
          theme(
            plot.title=element_text(size=18),
            axis.text=element_text(size=15),
            axis.text.x = element_text(angle = 45, vjust=0.5),
            axis.title=element_text(size=18, face = "bold")
          )}
  })
  
  
  ###### Main DT #####
  output$responses <- DT::renderDataTable(
    reactiveData(),
    filter = 'top',
    editable = list(
      target = 'cell',
      disable = list(
        # disable ID, update count, newest record
        # latest update and timeline from editing
        columns = c(0:2,9,13)
        )
      ),
    options = list(
      paging=FALSE,
      autoWidth = TRUE,
      searchCols = list(
        # NULL,  # rownum col
        NULL, NULL, list(search='Yes'),  # newest record col
        NULL, NULL, NULL,
        NULL, NULL, NULL, NULL, NULL,
        NULL, NULL, NULL
        ),
      columnDefs = list(
        # Long Text Columns
        list(width = '200px', targets = c(7:8,11)),
        # Date Columns and short text
        list(width = '75px', targets = c(9,13))
        ),
      scrollX=TRUE,
      scrollCollapse=TRUE,
      dom = 'Bfrtip',
      buttons = list(
        extend = 'collection',
        buttons = c('csv', 'excel'),
        text = 'Download'
      )
      ),
    extensions = "Buttons",
    rownames = FALSE
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
