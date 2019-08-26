#################################################################
##                                                             ##
##   (c) Carlos Alexandre Romani <carlos.romani@inpe.br>       ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##        R script with shiny ui to web service                ##
##                                                             ##
##                                             2018-07-20      ##
##                                                             ##
##            Land Use and Cover Data Analysis                 ##
##                                                             ##
##                                                             ##
#################################################################


#' @title server
#' @name server
#' @author Carlos Alexandre Romani
#'
#' @description Analysis 
#' 
#' @usage 
#' 
#' @export server
#' @import shiny
#' @import raster
#' 


#runApp("~/Dropbox/MESTRADO/LuccPL/R")


# install packages
packages <- c("parallel","raster","rasterVis","doParallel","foreach","pracma","shiny",
              "tools","jpeg","ensurer","ggplot2","reshape2")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), dependencies = TRUE)
}


# install LuccPL
packageLuccPL <- c("LuccPL")
if (length(setdiff(packageLuccPL, rownames(installed.packages()))) > 0) {
  devtools::install_github("car13romani/LuccPL")
}


library(LuccPL)
library(jpeg)
library(shiny)


options(shiny.maxRequestSize=3000*1024^2) 
server <- function(input, output, session) {
  
  ########################################  PAGE 1 (BRICK)  #####################################  
  
  if(!dir.exists("~/lucc_data")){
    dir.create("~/lucc_data")
  }

  folderPath <- reactive({
    return(print("~/lucc_data"))
    })
  # print project folder path
  output$dir1 <- renderPrint(
    if(length(folderPath()) == 0) return("Choose the project folder")
    else return(folderPath())
  )
  
  # if makeBrick button is true
  observeEvent(input$makeBrick,{
    # creates the brick from a list of rasters
    
    # order files by digits
    numbers = as.numeric(regmatches(input$rawInput$datapath, regexpr("[0-9]+", input$rawInput$datapath)))
    pathh <- input$rawInput$datapath[order(numbers)]
    
    
    
    message(paste(c("GeoTIFF images: \n", pathh, "\n"), collapse="\n"))
    
    
    
    print('Making brick')
    showNotification("Making brick", duration = 20)
    
    pracma::tic("Make brick")
    brick_created <- raster::brick(raster::stack(pathh),  progress = "text", datatype='INT4S')
    t <- pracma::toc()
    #print(paste("Brick created in:", (end.time-start.time), "seconds"))
    showNotification(paste("Brick created in:", as.numeric(t), "seconds"), duration = NULL)
    
    
    # write a brick in project folder
    filename <- paste(folderPath(), 'stInput.tif', sep = '/')
    showNotification("Export raster", duration = 20)
    print('Export raster')
    pracma::tic("Write raster brick")
    raster::writeRaster(brick_created, filename = filename, datatype='INT4S', overwrite=TRUE, progress = "text")
    t <- pracma::toc()
    #print(paste("Brick exported in:", as.numeric(t), "seconds"))
    showNotification(paste("Brick exported in:", as.numeric(t), "seconds"), duration = NULL)
    
    gc()
    showNotification("Done", duration = 20)
    print('Done')
  })             
  
  
  
  output$listaa <- renderPrint(input$rawInput$datapath)
  

  
  ########################################  PAGE 2 (IMPORT)  #####################################
  
  # import datacube file (tif)
  brickRasterInput <- reactive( raster::brick(input$dataFolder$datapath) )
  
  # import metadata file
  metadata <- reactive( as.character(read.table(input$mdataPath$datapath)$V1) )
  
  # import dates
  dates <- reactive( as.character(read.table(input$datesPath$datapath)$V1) )
  
  # if plotResult button is clicked
  observeEvent(input$plotInput2, {
    print(dates)
    
    colors <- reactive( as.character(read.table(input$colors$datapath)$V1) )
    print(colors)
    
    print('create subfolder')
    
    # create folder to jpeg images
    dir.create(file.path(folderPath(), "jpeg"), showWarnings = FALSE)
    showNotification("Export jpeg", duration = 20)
    print('Export jpeg')
    pracma::tic("Write jpeg")
    # parallel export jpeg images
    apply(as.list(1:(length(dates))),function(i) {
      file <-  file.path(folderPath(), "jpeg", paste0("plotIn",i,".jpeg"))
      print(file)
      jpeg(file = file, bg = "gray", height=nrow(brick_created[[i]]), width=ncol(brickRasterInput[[i]]))
      print(rasterVis::levelplot(brickRasterInput[[i]], col.regions=colors,  contour=F, margin=F, scales = list(draw=FALSE), colorkey=NULL,
                                 par.settings = list(axis.line = list(line=0), mar=c(0,0,0,0), omi=c(0,0,0,0), 
                                                     xaxt='n', yaxt='n', bg='gray')))
      dev.off()
    })
    pracma::toc()
    #print(paste("Jpeg exported in:", as.numeric(t), "seconds"))
    showNotification(paste("Jpeg exported in:", as.numeric(t), "seconds"), duration = 60)
    
    
    
    
    insertUI(
      selector = "#plotInput2",
      where = "beforeBegin",
      ui =  fluidRow(
        
        selectInput("tstepIn2", label = 'Date', choices = dates(), multiple=FALSE, selectize=TRUE)
        # function to plot images imported
        
        
      )
      
    )
    output$imageInput2 <- renderImage({
      width  <- session$clientData$output_imageInput2_width
      list(
        src = file.path(folderPath(), "jpeg", paste0("plotIn", (which(dates() == input$tstepIn2)),".jpeg")),
        width = width,
        contentType = "image/jpeg",
        alt = input$tstepIn2
      )
      
    }, deleteFile = FALSE)
    
  })
  
  
  
  
  #########################################  PAGE 3 (PROCESS)  #################################################
  observeEvent(input$add, {
    insertUI(
      selector = "#add",
      where = "beforeBegin",
      ui =  fluidRow(
        # hr(),
        column(2,
               selectInput(paste0("inputFun", input$add), paste("Function", input$add), c("","before","after","meets","metby","holds","recur","convert","evolve"), multiple=FALSE, selectize=FALSE)         
        ),
        column(2,
               selectInput(paste0("patternI",input$add), label = '', choices = metadata(), multiple=TRUE, selectize=TRUE)
        ),
        column(2,
               selectInput(paste0("patternJ",input$add), label = '', choices = metadata(), multiple=TRUE, selectize=TRUE)
        ),
        column(2,
               selectInput(paste0("dateS",input$add), '  ', dates(), multiple=FALSE, selectize=FALSE)
        ),
        column(2,
               selectInput(paste0("dateF",input$add), '  ', dates(), multiple=FALSE, selectize=FALSE)
        ),
        column(1,
               radioButtons(paste0("logicalConnector",input$add), '', c('and', 'or'))
               
        )
        
      )
    )
  })
  
  ################################################
  
  output$teste <- renderText(input$dataFolder$datapath)
  
  
  output$table <- renderTable({
    if (input$query[[1]] == 0) {
      
      NULL
    } 
    else {
      
      qout <<- reactive({ 
        fList <- NULL
        for (i in 1:input$add[[1]]) {
          
          funct <- eval(parse(text=paste0(
            eval(parse(text = paste0("input$inputFun",as.character(i)))),           #function
            "(c(",LuccPL::format1(eval(parse(text = paste0("input$patternI",i)))),"),",       #patternI
            "c(",LuccPL::format1(eval(parse(text = paste0("input$patternJ",i)))),"),'",       #patternJ
            eval(parse(text = paste0("input$dateS",i))),"','",                        #dateS
            eval(parse(text = paste0("input$dateF",i))),"', dates(), metadata())"))       #dateF
          )
          
          if(i==1){ fList <- funct }
          else { fList <- c(fList, (eval(parse(text=paste0("input$logicalConnector",i-1)))), funct) }
          
        }
        
        print(fList)
        LuccPL::query(input_dc = brickRasterInput(), FUN_list = fList)
        
      })
      #print(qout())
      as.array(qout())
      
    }
  })
  
  #############################################
  
  
  
  
  observeEvent(input$process, {
    print("Processing...")
    showNotification("Processing...", duration = 20)
    pracma::tic("Process total")
    pracma::tic("Process")
    brickRasterOutput <- LuccPL::event(brickRasterInput(), qout())
    t <- pracma::toc()
    #print(paste("Processed in:", (end.time-start.time), "seconds"))
    showNotification(paste("Processed in:", as.numeric(t), "seconds"), duration = 60)
    print(paste("Processed in:", as.numeric(t), "seconds"))
    print("Export brick")
    showNotification("Export brick", duration = 20)
    filenameOut <- paste(folderPath(), 'stOutput.tif', sep = '/')
    pracma::tic("Write raster")
    raster::writeRaster(brickRasterOutput, filename = filenameOut, datatype='INT4S', overwrite=TRUE, progress = "text")
    t <- pracma::toc()
    #print(paste("Brick exported in:", (end.time-start.time), "seconds"))
    showNotification(paste("Brick exported in:", as.numeric(t), "seconds"), duration = 60)
   
    rm(brickRasterOutput)
    gc()
    print('Done')
    showNotification("Done" ,duration = 60)
  })
  # COUNT
  observeEvent(input$count, {
    filenameOutCount <- paste(folderPath(), 'rasterCount.tif', sep = '/')
    print("Processing count...")
    showNotification("Processing count...", duration = 20)
    brickOutput <- raster::brick(paste(folderPath(), 'stOutput.tif', sep = '/'))
    pracma::tic("Count")
    rasterCount <- count(brickOutput)
    t <- pracma::toc()
    #print(paste("Count in:", (end.time-start.time), "seconds"))
    showNotification(paste("Count in:", as.numeric(t), "seconds"), duration = 60)
    print('Export raster')
    pracma::tic("Write raster")
    raster::writeRaster(rasterCount, filename = filenameOutCount, datatype='INT4S', overwrite=TRUE, progress = "text")
    t <- pracma::toc()
    #print(paste("Brick exported in:", (end.time-start.time), "seconds"))
    showNotification(paste("Brick exported in:", as.numeric(t), "seconds"), duration = NULL)
    print('Done')
    showNotification("Done", duration = NULL)
    rm(rasterCount)
    gc()
  })
  
  # VARIANCE
  observeEvent(input$variance, {
    filenameOutVariance <- paste(folderPath(), 'rasterVariance.tif', sep = '/')
    print("Processing variance...")
    showNotification("Processing variance...", duration = 20)
    brickOutput <- raster::brick(paste(folderPath(), 'stOutput.tif', sep = '/'))
    pracma::tic("Variance")
    rasterVariance <- variance(brickOutput)
    t <- pracma::toc()
    #print(paste("Variance in:", (end.time-start.time), "seconds"))
    showNotification(paste("Variance in:", as.numeric(t), "seconds"), duration = 60)
    print('Export raster')
    pracma::tic("Write raster")
    raster::writeRaster(rasterVariance, filename = filenameOutVariance, datatype='INT4S', overwrite=TRUE, progress = "text")
    t <- pracma::toc()
    #print(paste("Brick exported in:", (end.time-start.time), "seconds"))
    showNotification(paste("Brick exported in:", as.numeric(t), "seconds"), duration = 60)
    print('Done')
    showNotification("Done", duration = NULL)
    rm(rasterVariace)
    gc()
  })
  
  # PLOTBAR
  observeEvent(input$plotbar, {
    filenameOutBarplot <- paste(folderPath(), 'bargraph.jpeg', sep = '/')
    print("Ploting...")
    showNotification("Ploting...", duration = 20)
    brickOutput <- raster::brick(paste(folderPath(), 'stOutput.tif', sep = '/'))
    
    graph <- count(brickOutput, for_time_step = TRUE, metadata = metadata, dates = dates)
    
    lucc_barplot_data(df = graph, dates = dates, style="bar",
                      colors = colors, path_save_jpeg = paste0(path,"/results"))
    
    
    showNotification("Done", duration = NULL)
    gc()
  })
  
  
  
  #########################################  PAGE 4 (EXPORT)  #################################################    
  
  # paste0(strsplit(x=path, split = "stBrick.tif"), "stBrickOut.tif")
  
  output$stOut.tif <- downloadHandler(
    
    filename = function() {
      paste("output", "tif", sep=".")
    },
    
    content = function(file) {
      file.copy(paste0(strsplit(x=input$dataFolder$datapath, split = ".tif"), "stBrickOut.tif"),file)
    },
    contentType = "tif"
  )
  

  observeEvent(input$plotResult1, {
    
    output$imageOutput1 <- renderImage({
      width  <- session$clientData$output_imageOutput1_width
      list(
        src = file.path(folderPath(), "jpeg", paste0("plotOut",input$tstepOut1,".jpeg")),
        width = width,
        contentType = "image/jpeg",
        alt = input$tstepOut1
      )
      
    }, deleteFile = FALSE)
    
  })
  
}


