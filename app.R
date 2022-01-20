## Only run examples in interactive R sessions
## Necessary libraries
library(readr)
library(shiny)
library(stringr)
library(shinyjs)
library(rmarkdown)
library(here)
library(googlesheets4)
library(tidyverse)

## Read in our metadata files/sheets
#relevant variables
dir <- here()
path <- here("csvs")
checkBox = "&#9744; "
paperURL <- a("Link to Full Text", href="https://doi.org/10.1038/s41592-021-01156-w")
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)
mySheet <-  as_sheets_id("https://docs.google.com/spreadsheets/d/1hxw2bsIQFB9pn555de7FbTlE8j4Smc7kT8NrAfy-q_8/edit?usp=sharing")
core_list <- sheet_names(mySheet)
master = read.delim(
  "master.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  quote = "",
  sep = "\t",
  na = ""
)

## functions

addToChecklist <- function(category, printExamples, scope, df, input=""){
  #searches category_when_printed for category, finds metadata to print
  logidx <- str_detect(master$Category_when_printed, category) #logical
  indices <- which(logidx)
  metadataList <- master$Metadata[logidx]
  output <- ""
  if(printExamples){
    for(idx in indices){
      metadata <- master$Metadata[idx]
      key <- master$Gsheets_map[idx]
      scopeTag <- "e.g.,"
      if(!is.na(key) & key!="" & key!=" " & scope!="Use default examples"){
        example <- df[df$Microscope_name==key,scope]
        if(!is.na(example) & example!="" & example!=" "){
          scopeTag <- paste0("for ",scope, ": ")
        }
        else{
          example <- master$Example[idx]
        }
      }
      else{
        example <- master$Example[idx]
      }
      if(is.na(example) | example==""| example==" "){
        example=""
        scopeTag=""
      }
      output <- paste(output, checkBox, na.omit(metadata), "&nbsp; &nbsp;", "<em style=color:CornflowerBlue>", scopeTag, example, "</em>", "<br>")
      #output <- append(output, item)
    }
  }
  else{
    for(i in 1:length(metadataList)){
      output <- paste(output, checkBox, na.omit(metadataList[i]), "<br>")
    }
  }
  finalOutput <- append(input,output)
  return(str_replace_all(finalOutput,"\"",""))
}

sortAllItems <-function(chr_list){
  items <- strsplit(chr_list, "(?<=<br>)", perl=TRUE)
  items <- unlist(items)
  items <- unique(items)
  sortedlist <- c()
  hold <- c()
  for(item in items){
    if(grepl("***", item, fixed=TRUE)){
      hold <- append(hold, item)
    }
    else{
      sortedlist <- append(sortedlist, item)
    }
  }
  return(append(sortedlist,hold))
}

checkNull <- function(input){
  if(!is.null(input)){
    return(input)
  }
  else{
    return("null")
  }
}
#get element from a ";"-delimited list of rownames in the spreadsheet (e.g. rowName="Optical_config")
getScopeVal <- function(df, rowName, scope){
  logIdx <- str_detect(df$Microscope_name, rowName)
  scopeVals <- df[logIdx,scope]
  return(na.omit(scopeVals))
}
getChoices <- function(df, rowName, scope, elm="All") {
  OCs <- unlist(df[str_detect(df$Microscope_name, rowName),scope])
  if (elm !="All"){
    opts <- na.omit(str_split(OCs, pattern="; ", simplify=TRUE))[,elm]
  }
  else{
    opts <- na.omit(str_split(OCs, pattern="; ", simplify=TRUE))
  }
  return(opts)
}

## App itself
  shinyApp(
    ui = fluidPage(
      useShinyjs(),
      titlePanel("Microscopy Metadata Checklist Generator (MicCheck) v2"),
      
      sidebarLayout(
        sidebarPanel(
          width = 5,
          "For more information, see our paper: Montero Llopis, P., Senft, R.A., Ross-Elliott, T.J. et al. Best practices and tools for reporting reproducible fluorescence microscopy methods. Nat Methods 18, 1463–1476 (2021)",
          paperURL,
          HTML("<br><br>"),
          selectInput("core", "Select a microscopy core:", choices=c(core_list)),
          selectInput("scope", "Select a microscope:", choices="Use default examples"),
          ####################
          #Questions
          ####################
          # Q1: Modality
          checkboxGroupInput(
            "Q1",
            "Which image modality are you using?",
            choices=c(
              "Widefield" = "WF",
              "Spinning disk confocal" = "SConfocal",
              "Point-scanning confocal" = "PConfocal",
              "Multiphoton" = "MPhoton"
            ),
            selected = character(0)
          ),
          checkboxGroupInput(
            "modalityHelpText",
            "",
            choices=c(),
            selected = character(0)
          ),
          # Q1b: WF wheels/cubes
          checkboxGroupInput(
            "opticalConfig",
            "Choose optical configuration(s) used:",
            choices = NULL
          ),
          # Q1c: WF laser vs LED
          checkboxGroupInput(
            "Q1c",
            "What light source(s) did you use to illuminate your sample?",
            choiceNames = list("Non-laser light source (e.g., LED,  metal halide)", "Lasers"),
            choiceValues = list("LEDs", "Lasers"),
            selected=character(0)
          ),
          # Q2: Uni-meta-related
          radioButtons(
            "Q2",
            "Did you acquire transmitted light images (e.g., phase contrast, brightfield, DIC)?",
            c("Yes" = "Y",
              "No" = "N"),
            selected = character(0)
          ),
          radioButtons(
            "Q3",
            "Did you use additional magnification (e.g., optovar, relay lens)?",
            c("Yes" = "Y",
              "No" = "N"),
            selected = character(0)
          ),
          # Q3b: Objectives
          checkboxGroupInput(
            "objectives",
            "Choose objective(s) used",
            choices = NULL
          ),
          # Application-related
          checkboxGroupInput(
            "Q4",
            "Did you perform any of these multidimensional acquisitions?",
            choiceNames = list("Multi-color", "Z-stack", "Time-lapse"),
            choiceValues = list("multi-color", "z-stack", "time-lapse")
          ),
          radioButtons(
            "Q5",
            "Did you do a multi-point acquisition (e.g., imaging different wells in a multi-well plate)?",
            c("Yes" = "Y", "No" = "N"),
            selected = character(0)
          ),
          radioButtons(
            "Q6",
            "Did you do tiling, scan large area, or mosaic acquisition involving image stitching?",
            c("Yes" = "Y", "No" = "N"),
            selected = character(0)
          ),
          radioButtons(
            "Q7",
            "Did you do live cell imaging?",
            c("Yes" = "Y",
              "No, my sample is fixed or dead" = "N"),
            selected = character(0)
          ),
          checkboxGroupInput(
            "Q8",
            "What kind of fluoropohore(s) did you use in your sample?",
            choiceNames = list("Fluorescent protein", "Fluorescent dye/organic dye"),
            choiceValues = list("FP", "Dye")
          ),
          radioButtons(
            "Q9",
            "Did you use any transfection reagents to express the fusion protein of interest in your sample?",
            c("Yes" = "Y", "No" = "N"),
            selected = character(0)
          ),
          helpText("App developed by Rebecca Senft (2022).")
        ),
        mainPanel(
          #Styles:
          tags$head(tags$style("#newline{font-size: 10px;")),
          tags$head(
            tags$style(
              "#title{color: black;
              font-size: 20px;
              text-align: center;
              }"
        )
            ),
        
        tags$head(
          tags$style(
            "#c1,#c2,#c3,#c4,#c5,#c5,#c6,#c7,#c8,#cModality{color: white;
            background-color: black;
            text-align: center;
            }"
        )
          ),
        tags$head(
          tags$style(
            "#opticalConfigTitle,#objectivesTitle{color: black;
            font-weight: bold;
            padding: 6px 12px; 
            margin-top: 8px;
            background-color: #DCDCDC;
            text-align: center;
            }"
          )
        ),
        # Checklist output:
        htmlOutput("title"),
        #htmlOutput("newline"),
        helpText("***Asterisks indicate optional items."),
        textOutput("c1"),
        htmlOutput("Stand_motorized"),
        textOutput("cModality"),
        htmlOutput("nonWF_Stand_motorized"),
        textOutput("c2"),
        htmlOutput("Illumination"),
        textOutput("c3"),
        htmlOutput("WaveSelection"),
        textOutput("opticalConfigTitle"),
        tableOutput("opticalConfig"),
        textOutput("c4"),
        htmlOutput("Optics"),
        textOutput("objectivesTitle"),
        tableOutput("objectives"),
        textOutput("c5"),
        htmlOutput("Detector"),
        textOutput("c6"),
        htmlOutput("Software"),
        textOutput("c7"),
        htmlOutput("SampleContent"),
        textOutput("c8"),
        htmlOutput("Acknowledgements"),
        htmlOutput("newline2"),
        downloadButton("download", "Download Checklist"),
        actionButton("exampleToggle", "Show Examples"),
        htmlOutput("newline3"),
        width = 7
        )
    )
    ),
    server = function(input, output, session) {
      exampleToggle <- reactiveVal(FALSE)
      observeEvent(input$exampleToggle, {
        exampleToggle(!exampleToggle())
      })
      val <-
        reactiveValues(
          df = NULL,
          scope = NULL,
          scope_df = NULL,
          titles = NULL,
          possibleModality = NULL,
          chosenModality = NULL,
          Zstack = TRUE,
          tile_scan = TRUE,
          multicolor = TRUE,
          Laser = FALSE,
          LED = FALSE,
          selected_OC = NULL,
          Stand=NULL,
          Illum=NULL,
          Wave=NULL,
          waveTable=NULL,
          Optics=NULL,
          Detect=NULL,
          Soft=NULL,
          Sample=NULL,
          Ack = NULL
        )
      observe({
        core <- input$core
        if(is.null(core)) { } else {
          if(core != "Use default examples"){
            df <- read_sheet(mySheet, sheet=input$core)
            colnames(df) <- df[1,]
            df <- df[,c(1,3:ncol(df))]
            val$df <- df
            scope_names <- as.character(colnames(df)[2:length(colnames(df))]) # present to user after core is selected
            updateSelectInput(session, "scope", choices = c("Use default examples",scope_names))
          }
          else{
            scope_names="Use default examples"
            updateSelectInput(session, "scope", choices = c(scope_names))
          }
        }
      })
      observeEvent(input$scope, {
        modalities <- c("Widefield", "Spinning disk confocal", "Point-scanning confocal", "Multiphoton")
        val$scope <- input$scope
        scope <- val$scope
        df <- val$df
        if(input$scope != "Use default examples"){
            scope_df <- df[[input$scope]]
            titles <- df[1]
            #Modality
            WF <- getChoices(df,"Widefield", scope)
            SConfocal <- getChoices(df,"Sconfocal", scope)
            PConfocal <- getChoices(df,"Pconfocal", scope)
            MPhoton <- getChoices(df,"Mphoton", scope)
            modeChoice <- tolower(c(WF,SConfocal,PConfocal,MPhoton))=="yes"
            val$possibleModality <- modalities[modeChoice]
            if (length(getScopeVal(df, "wheel", scope)>0)){
              val$wheel=TRUE
            }
            else{
              val$wheel=FALSE
            }
            #Optical configs and objectives
            opticalConfigList <- getChoices(df, "Optical_config",input$scope,1)
            objectiveList <- str_sort(getChoices(df, "Objective_",input$scope,2), numeric=TRUE)
            reset("opticalConfig")
            reset("objectives")
            if (length(opticalConfigList)>0){
              show("opticalConfig", anim=TRUE)
              updateCheckboxGroupInput(session, "opticalConfig", choices=opticalConfigList)
            }
            else{
              hide("opticalConfig",anim = TRUE)
            }
            if (length(objectiveList)>0){
              show("objectives", anim=TRUE)
              updateCheckboxGroupInput(session, "objectives", choices=objectiveList)
            }
            else{
              hide("objectives",anim = TRUE)
            }
            val$Zstack <- toupper(scope_df[titles=="Z-stack"])=="YES"
            val$tile_scan <- toupper(scope_df[titles=="Tile_scan"])=="YES"
            val$multicolor <- toupper(scope_df[titles=="Multicolor"])=="YES"
            val$scope_df <- scope_df
            val$titles <- titles
            #Display only capabilities of current microscope to MicCheck user:
            idx <- c(val$multicolor, val$Zstack,TRUE)
            names = c("Multi-color", "Z-stack", "Time-lapse")[idx]
            values = c("multi-color", "z-stack", "time-lapse")[idx]
            updateCheckboxGroupInput(session, "Q4", choiceNames = names, choiceValues = values)
        }
        else{
          val$possibleModality <- modalities
          modeChoice <- c(TRUE, TRUE, TRUE, TRUE)
          reset("opticalConfig")
          reset("objectives")
          hide("opticalConfig",anim = TRUE)
          hide("objectives",anim = TRUE)
          val$selected_OC <- NULL
        }
        #Show modality question if multiple modalities are possible
        if(length(val$possibleModality)>1){
          show("", anim = TRUE)
          reset("Q1")
          show("Q1", anim=TRUE)
          updateCheckboxGroupInput(session, "Q1", choices=modalities[modeChoice])
          output$cModality <- NULL
          hide("modalityHelpText",anim = FALSE)
        }
        else{
          hide("Q1",anim = TRUE)
          val$chosenModality <- val$possibleModality
          show("modalityHelpText",anim = TRUE)
          updateTextInput(session, "modalityHelpText", label=paste0("Modality of chosen microscope: ",val$chosenModality))
        }
      })
      observe(if (!is.null(val$chosenModality)){
        if("Widefield" %in% val$chosenModality){
          show("Q1b", anim = TRUE)
          reset("Q1b")
          show("Q1c", anim=TRUE)
          reset("Q1c")
        }
        else{
          reset("Q1b")
          reset("Q1c")
          hide("Q1b",anim = FALSE)
          hide("Q1c",anim = FALSE)
        }
      })
      observeEvent(input$Q1,{
        val$chosenModality <-input$Q1
      })
      output$title <- renderUI({
        HTML(paste("Microscopy Metadata Checklist <br>"))
      })
      output$newline<- renderUI({
        HTML(paste("<br>"))
      })
      output$newline2<- renderUI({
        HTML(paste("<br>"))
      })
      output$newline3<- renderUI({
        HTML(paste("<br> <br>"))
      })
      output$c1 <- renderText({
        paste("Microscope Stand and Motorized Components")
      })
      output$c2 <- renderText({
        paste("Illumination")
      })
      output$c3 <- renderText({
        paste("Wavelength Selection")
      })
      output$c4 <- renderText({
        paste("Optics")
      })
      output$c5 <- renderText({
        paste("Detection")
      })
      output$c6 <- renderText({
        paste("Acquisition Software")
      })
      output$c7 <- renderText({
        paste("Sample Preparation")
      })
      output$c8 <- renderText({
        paste("Acknowledgements")
      })
  
  #### Conditional checklist categories
      
      ##### 1. Microscope Stand and Motorized Components
      
      output$Stand_motorized <- renderUI({
        all = ""
        all <- addToChecklist("all_Stand", exampleToggle(), val$scope, val$df, input=all)
        if (length(input$Q4)>0 | checkNull(input$Q5)=="Y" | checkNull(input$Q6)=="Y"){
          all <- addToChecklist("Zstack_multiPoint_timeLapse_Stand", exampleToggle(), val$scope, val$df, input=all)
        }
        if ("wheel" %in% checkNull(input$Q1b) | checkNull(val$wheel)==TRUE){
          all <- addToChecklist("emissionWheel_Stand", exampleToggle(), val$scope, val$df, input=all)
        }
        all <- sortAllItems(all)
        val$Stand <- all
        HTML(paste(all))
      })
      output$nonWF_Stand_motorized <- renderUI({
        all = ""
        if ("Spinning disk confocal" %in% val$chosenModality) {
          all <- addToChecklist("Sconfocal_Stand", exampleToggle(), val$scope, val$df, input=all)
        }
        if ("Point-scanning confocal" %in% val$chosenModality) {
          all <- addToChecklist("Pconfocal_Stand", exampleToggle(), val$scope, val$df, input=all)
        }
        if ("Multiphoton" %in% val$chosenModality) {
          all <- addToChecklist("Mphoton_Stand", exampleToggle(), val$scope, val$df, input=all)
        }
        all <- sortAllItems(all)
        val$Stand<- all
        HTML(paste(all))
      })
      
      ##### 2. Illumination
      
      output$Illumination <- renderUI({
        all = ""
        # #append all default items
        if ("Widefield" %in% val$chosenModality) {
          all <- addToChecklist("WF_nolaser_Illumination", exampleToggle(), val$scope, val$df, all)
        }
        # #append those specific to scope chosen / other variables like modality
        if ("Widefield" %in% val$chosenModality & val$Laser==TRUE) {
          all <- addToChecklist("WF_laser_Illumination", exampleToggle(), val$scope, val$df, all)
        }
        if ("Spinning disk confocal" %in% val$chosenModality & val$Laser==TRUE) {
          all <- addToChecklist("Sconfocal_laser_Illumination", exampleToggle(), val$scope, val$df, all)
        }
        if ("Spinning disk confocal" %in% val$chosenModality & val$LED==TRUE) {
          all <- addToChecklist("Sconfocal_LED_Illumination", exampleToggle(), val$scope, val$df, all)
        }
        if ("Point-scanning confocal" %in% val$chosenModality){
          all <- addToChecklist("Pconfocal_Illumination", exampleToggle(), val$scope, val$df, all)
        }
        if ("Multiphoton" %in% val$chosenModality) {
          all <- addToChecklist("Mphoton_Illumination", exampleToggle(), val$scope, val$df, all)
        }
        all <- sortAllItems(all)
        val$Illum <- all
        HTML(paste(all))
        
      })
      observeEvent(input$opticalConfig,{
        scope<- val$scope
        scope_df <- val$scope_df
        df <- val$df
        #get sub-table of all optical configs
        OC <- na.omit(data.frame(scope_df[grep("Optical_config", df[[1]])]))
        opticalString=OC[1,]
        if(str_count(opticalString,';')==5){
          OC <- separate(
            OC,
            1,
            into=c("Name", "Illumination Type", "Light source", "Excitation Filter", "Dichroic Mirror/Beamsplitter", "Emission Filter"),
            sep = ";",
            remove = TRUE,
            convert = FALSE,
            extra = "warn",
            fill = "warn"
          )
        }
        else if(OC.count(";")==3){
          OC <- separate(
            OC,
            1,
            into=c("Name", "Illumination Type", "Light source", "Filter Cube"),
            sep = ";",
            remove = TRUE,
            convert = FALSE,
            extra = "warn",
            fill = "warn"
          )
        }
        patterns <- paste(input$opticalConfig,collapse="|")
        patterns <- gsub("([()])","\\\\\\1", patterns)
        selected_OC <- OC[grepl(patterns, OC$Name),]
        val$selected_OC <- selected_OC
        if(length((str_detect(tolower(unlist(selected_OC)),"laser"))>0)){
          val$Laser <- TRUE
        }
        if(length((str_detect(unlist(selected_OC),"LED"))>0)){
          val$LED <- TRUE
        }
      })
      observe({if(length(input$opticalConfig)>0){
                       output$opticalConfigTitle <- renderText({paste("Selected Optical Configurations for your microscope:",val$scope)})
                       output$opticalConfig <- renderTable(val$selected_OC, bordered=TRUE, striped = TRUE)
      }
        else{
          output$opticalConfig <- renderText({""})
          output$opticalConfigTitle <- renderText({""})
        }
      })
      observe({if(length(input$objectives)>0){
        output$objectivesTitle <- renderText({paste("Selected Objectives for your microscope:",val$scope)})
        output$objectives <- renderTable(val$selected_objectives, bordered=TRUE, striped = TRUE)
      }
        else{
          output$objectives <- renderText({""})
          output$objectivesTitle <- renderText({""})
        }
      })
      #make objectives table
      observeEvent(input$objectives,{
        scope<- val$scope
        df <- val$df
        objectives <- getChoices(df, "Objective_", scope, elm="All")
        objectives <- as.data.frame(objectives)
        colnames(objectives) <- c("Objective Correction", "Magnification", "Numerical aperture (NA)", "Application", "Immersion media")
        
        # objectives <- separate(
        #     objectives,
        #     1,
        #     into=c("Objective Correction", "Magnification", "Numerical aperture (NA)", "Application", "Immersion media"),
        #     sep = ";",
        #     remove = TRUE,
        #     convert = FALSE,
        #     extra = "warn",
        #     fill = "warn"
        #   )
        patterns <- paste(input$objectives,collapse="|")
        patterns <- gsub("([()])","\\\\\\1", patterns)
        selected_objectives <- objectives[grepl(patterns, objectives$Magnification),]
        val$selected_objectives <- selected_objectives
      })

  ##### Wavelength Selection
  
  output$WaveSelection <- renderUI({
    all = ""
    all <- addToChecklist("All_Wave", exampleToggle(), val$scope, val$df, all)
    if ("Widefield" %in% val$chosenModality & ("cube" %in% checkNull(input$Q1b) | length(str_detect(tolower(unlist(val$selected_OC)),"cube"))>0)) {
      all <- addToChecklist("WF_filterCube_Wave", exampleToggle(), val$scope, val$df, all)
    }
    all <- sortAllItems(all)
    val$Wave <- all
    HTML(paste(all))
  })
  
  ##### Optics
  
  output$Optics <- renderUI({
    all = ""
    all <- addToChecklist("all_Optics", exampleToggle(), val$scope, val$df, all)
    if (checkNull(input$Q2)=="Y") {
      all <- addToChecklist("transmitted_Optics", exampleToggle(), val$scope, val$df, all)
    }
    if (checkNull(input$Q3)=="Y") {
      all <- addToChecklist("mag_Optics", exampleToggle(), val$scope, val$df, all)
    }
    all <- sortAllItems(all)
    val$Optics <- all
    HTML(paste(all))  
    })
  
  ##### Detector
  
  output$Detector <- renderUI({
    all = ""
    if ("Widefield" %in% val$chosenModality){
      all <- addToChecklist("WF_Detection", exampleToggle(), val$scope, val$df, all)
    }
    if ("Spinning disk confocal" %in% val$chosenModality) {
      all <- addToChecklist("Sconfocal_Detection", exampleToggle(), val$scope, val$df, all)
    }
    if ("Point-scanning confocal" %in% val$chosenModality) {
      all <- addToChecklist("Pconfocal_Detection", exampleToggle(), val$scope, val$df, all)
    }
    if ("Multiphoton" %in% val$chosenModality) {
      all <- addToChecklist("Mphoton_Detection", exampleToggle(), val$scope, val$df, all)
    }
    all <- sortAllItems(all)
    val$Detect <- all
    HTML(paste(all))
  })
  
  ##### Software
  
  output$Software <- renderUI({
    all = ""
    all <- addToChecklist("all_Software", exampleToggle(), val$scope, val$df, all)
    if (length(input$Q4)>0) {
      all <- addToChecklist("multiDim_Software", exampleToggle(), val$scope, val$df, all)
    }
    if ("multi-color" %in% checkNull(input$Q4)) {
      all <- addToChecklist("multiColor_Software", exampleToggle(), val$scope, val$df, all)
    }
    if ("z-stack" %in% checkNull(input$Q4)) {
      all <- addToChecklist("Z_stack_Software", exampleToggle(), val$scope, val$df, all)
    }
    if ("time-lapse" %in% checkNull(input$Q4)) {
      all <- addToChecklist("timeLapse_Software", exampleToggle(), val$scope, val$df, all)
    }
    if(checkNull(input$Q6)=="Y"){
      all <- addToChecklist("tile_Software", exampleToggle(), val$scope, val$df, all)
    }
    all <- sortAllItems(all)
    val$Soft <- all
    HTML(paste(all))
  })
  
  ##### Sample-related
  
  output$SampleContent <- renderUI({
    all = ""
    all <- addToChecklist("all_Sample", exampleToggle(), val$scope, val$df, all)
    if (checkNull(input$Q7)=="Y") {
      all <- addToChecklist("live_Sample", exampleToggle(), val$scope, val$df, all)
    } 
    else if(checkNull(input$Q7)=="N"){
      all <- addToChecklist("fixed_Sample", exampleToggle(), val$scope, val$df, all)
    }
    if ("FP" %in% checkNull(input$Q8)) {
      all <- addToChecklist("FP_Sample", exampleToggle(), val$scope, val$df, all)
    }
    if ("Dye" %in% checkNull(input$Q8)) {
      all <- addToChecklist("dye_Sample", exampleToggle(), val$scope, val$df, all)
    }
    if (checkNull(input$Q9)=="Y") {
      all <- addToChecklist("transfect_Sample", exampleToggle(), val$scope, val$df, all)
    }
    all <- sortAllItems(all)
    val$Sample <- all
    HTML(paste(all))
  })
  
  ##### Acknowledgements
  
  output$Acknowledgements <- renderUI({
    all = ""
    if (checkNull(input$Q10)=="Y" | !is.null(input$core)) {
      all <- addToChecklist("core_Ack", exampleToggle(), val$scope, val$df, all)
    }
    all <- sortAllItems(all)
    val$Ack <- all
    HTML(paste(all))
  })
  #################
  # Download
  ################
  output$download = downloadHandler(
    filename = 'report.pdf',
    content = function(file) {
      params <- list(Stand= val$Stand,
                     Illum=val$Illum,
                     Wave=val$Wave,
                     waveTable=val$selected_OC,                     
                     Optics=val$Optics,
                     opticsTable=val$selected_objectives,
                     Detect=val$Detect,
                     Soft=val$Soft,
                     Sample=val$Sample,
                     Ack=val$Ack,
                     showExamples=exampleToggle())
      src <- normalizePath('report.Rmd')
      src2 <- normalizePath('checkbox.png')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      file.copy(src2, 'checkbox.png', overwrite = TRUE)
      
      id <- showNotification("Rendering checklist...",
                             duration = NULL,
                             closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)

      out <- render('report.Rmd', params=params, envir = new.env()) #add , pdf_document() here if you want default latex font
      file.rename(out, file)
    })
    })