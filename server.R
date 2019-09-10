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


#runApp("~/Dropbox/MESTRADO/PACKAGES/LuccWEB")


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


options(shiny.port = 80, shiny.maxRequestSize=3000*1024^2, shiny.launch.browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe") 
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
    rbrick <- raster::brick(raster::stack(pathh),  progress = "text", datatype='INT4S')
    t <- pracma::toc()
    #print(paste("Brick created in:", (end.time-start.time), "seconds"))
    showNotification(paste("Brick created in:", as.integer(t), "seconds"),duration = 20)
    
    
    # write a brick in project folder
    filename <- paste(folderPath(), 'stInput.tif', sep = '/')
    showNotification("Export raster", duration = 20)
    print('Export raster')
    pracma::tic("Write raster brick")
    raster::writeRaster(rbrick, filename = filename, datatype='INT4S', overwrite=TRUE, progress = "text")
    t <- pracma::toc()
    #print(paste("Brick exported in:", as.numeric(t), "seconds"))
    showNotification(paste("Brick exported in:", as.integer(t), "seconds"),duration = 20)
    
    gc()
    showNotification("Done", duration = 20)
    print('Done')
  })             
  
  
  
  output$listaa <- renderPrint(input$rawInput$datapath)
  

  
  ########################################  PAGE 2 (IMPORT)  #####################################
  
  # import datacube file (tif)
  rbrick <- reactive( raster::brick(input$dataFolder$datapath) )
  
  # import metadata file
  metadata <- reactive( as.character(read.table(input$mdataPath$datapath)$V1) )
  
  # import dates
  dates <- reactive( as.character(read.table(input$datesPath$datapath)$V1) )
  
  colors <- reactive( as.character(read.table(input$colors$datapath)$V1) )

  
  # if plotResult button is clicked
  observeEvent(input$plotInput2, {


    showNotification("Plotting input data...", duration = 20)
    LuccPL::plot_input(rbrick(),dates(),metadata(),colors(),paste0(folderPath(),"/mpInput.jpeg"), map_title="13 classes MT recorte", Width = 1200, Height = 1200)
    showNotification("Plotting done", duration = 20)
    
    output$imageInput2 <- renderImage({
      width  <- session$clientData$output_imageInput2_width
      list(
        src = file.path(folderPath(), "mpInput.jpeg"),
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
        LuccPL::query(input_dc = rbrick(), FUN_list = fList)
        
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
    rbrickO <- LuccPL::event(rbrick(), qout())
    t <- pracma::toc()
    #print(paste("Processed in:", (end.time-start.time), "seconds"))
    showNotification(paste("Processed in:", as.integer(t), "seconds"), duration = 20)
    print(paste("Processed in:", as.integer(t), "seconds"))
    print("Export brick")
    showNotification("Export output brick", duration = 20)
    filenameOut <- paste(folderPath(), 'stOutput.tif', sep = '/')
    pracma::tic("Write raster")
    raster::writeRaster(rbrickO, filename = filenameOut, datatype='INT4S', overwrite=TRUE, progress = "text")
    t <- pracma::toc()
    #print(paste("Brick exported in:", (end.time-start.time), "seconds"))
    showNotification(paste("Brick exported in:", as.integer(t), "seconds"), duration = 20)
    showNotification("Process done", duration = NULL)
    rm(rbrickO)
    gc()
    print('Done')
    
  })
  
  
  
  # PLOT OUTPUT
  # if plotResult button is clicked
  observeEvent(input$plotOutput1, {
    
    filenameOut <- paste(folderPath(), 'stOutput.tif', sep = '/')
    rbrickO <- raster::brick(filenameOut)
    showNotification("Plotting output...", duration = 20)
    LuccPL::plot_output(rbrickO,dates(),paste0(folderPath(),"/mpOutput.jpeg"), map_title="", Width = 1200, Height = 1200)
    
    showNotification("Plotting done...", duration = 20)
    
    output$imageOutput1 <- renderImage({
      width  <- session$clientData$output_imageInput2_width
      list(
        src = file.path(folderPath(), "mpOutput.jpeg"),
        width = width,
        contentType = "image/jpeg",
        alt = input$tstepIn2
      )
      
    }, deleteFile = FALSE)
    
  })
  

  
  ##############################################################################################
  # COUNT
  observeEvent(input$count, {
    filenameOutCount <- paste(folderPath(), 'rasterCount.tif', sep = '/')
    print("Processing count...")
    showNotification("Processing count...", duration = 20)
    rbrickO <- raster::brick(paste(folderPath(), 'stOutput.tif', sep = '/'))
    pracma::tic("Count")
    rasterCount <- LuccPL::count(rbrickO)
    t <- pracma::toc()
    #print(paste("Count in:", (end.time-start.time), "seconds"))
    showNotification(paste("Count in:", as.integer(t), "seconds"), duration = 20)
    print('Export raster')
    pracma::tic("Write raster")
    raster::writeRaster(rasterCount, filename = filenameOutCount, datatype='INT4S', overwrite=TRUE, progress = "text")
    t <- pracma::toc()
    #print(paste("Brick exported in:", (end.time-start.time), "seconds"))
    showNotification(paste("Brick exported in:", as.integer(t), "seconds"), duration = 20)
    print('Count done')
    showNotification("Count done", duration = NULL)
    rm(rasterCount)
    gc()
  })
  
  # VARIANCE
  observeEvent(input$variance, {
    filenameOutVariance <- paste(folderPath(), 'rasterVariance.tif', sep = '/')
    print("Processing variance...")
    showNotification("Processing variance...", duration = 20)
    rbrickI <- raster::brick(paste(folderPath(), 'stInput.tif', sep = '/'))
    pracma::tic("Variance")
    rasterVariance <- LuccPL::variance(rbrickI)
    t <- pracma::toc()
    #print(paste("Variance in:", (end.time-start.time), "seconds"))
    showNotification(paste("Variance in:", as.integer(t), "seconds"), duration = 20)
    print('Export raster')
    pracma::tic("Write raster")
    raster::writeRaster(rasterVariance, filename = filenameOutVariance, datatype='INT4S', overwrite=TRUE, progress = "text")
    t <- pracma::toc()
    #print(paste("Brick exported in:", (end.time-start.time), "seconds"))
    showNotification(paste("Brick exported in:", as.integer(t), "seconds"), duration = 20)
    print('Done')
    showNotification("Variance done", duration = NULL)
    rm(rasterVariance)
    gc()
  })
  
  
  # TRUE
  observeEvent(input$true, {
    filenameOutTrue <- paste(folderPath(), 'rasterTrue.tif', sep = '/')
    print("Processing True...")
    showNotification("Processing True...", duration = 20)
    rbrickO <- raster::brick(paste(folderPath(), 'stInput.tif', sep = '/'))
    pracma::tic("True")
    rasterTrue <- LuccPL::true(rbrickO)
    t <- pracma::toc()
    #print(paste("Count in:", (end.time-start.time), "seconds"))
    showNotification(paste("True in:", as.integer(t), "seconds"), duration = 20)
    print('Export raster')
    pracma::tic("Write raster")
    raster::writeRaster(rasterTrue, filename = filenameOutTrue, datatype='INT4S', overwrite=TRUE, progress = "text")
    t <- pracma::toc()
    #print(paste("Brick exported in:", (end.time-start.time), "seconds"))
    showNotification(paste("Raster exported in:", as.integer(t), "seconds"), duration = 20)
    print('Done')
    showNotification("True done", duration = NULL)
    rm(rasterTrue)
    gc()
  })
  
  
  
  #####################################################################################################
  # PLOTBAR Input
  observeEvent(input$plotbarI, {
    print("Ploting...")
    showNotification("Ploting...", duration = 20)
    rbrick <- raster::brick(paste(folderPath(), 'stInput.tif', sep = '/'))
    graphIn <- LuccPL::count(rbrick, for_time_step = TRUE, metadata = metadata(), dates = dates())
    write.csv(graphIn, paste0(folderPath(),"/graphIn.csv"))
    
    LuccPL::lucc_barplot_data(df = graphIn, dates = dates(), graph_title=input$name_plot, style="bar", colors = colors(), path_save_jpeg = folderPath())
    
    output$imageOutput5 <- renderImage({
      width  <- session$clientData$output_imageInput2_width
      list(
        src = file.path(folderPath(), "barplotI.jpeg"),
        width = width,
        contentType = "image/jpeg",
        alt = input$tstepIn2
      )
      
    }, deleteFile = FALSE)
    
    showNotification("Done", duration = NULL)
    gc()
  })
  
  
  # PLOTLINE Input
  observeEvent(input$plotlineI, {
    print("Ploting...")
    showNotification("Ploting...", duration = 20)
    rbrick <- raster::brick(paste(folderPath(), 'stInput.tif', sep = '/'))
    graphIn <- LuccPL::count(rbrick, for_time_step = TRUE, metadata = metadata(), dates = dates())
    write.csv(graphIn, paste0(folderPath(),"/graphIn.csv"))
    
    LuccPL::lucc_barplot_data(df = graphIn, dates = dates(), graph_title=input$name_plot, style="line", colors = colors(), path_save_jpeg = folderPath())
    
    output$imageOutput5 <- renderImage({
      width  <- session$clientData$output_imageInput2_width
      list(
        src = file.path(folderPath(), "lineplotI.jpeg"),
        width = width,
        contentType = "image/jpeg",
        alt = input$tstepIn2
      )
      
    }, deleteFile = FALSE)
    
    
    showNotification("Done", duration = NULL)
    gc()
  })
  
  
  
  
  # PLOTBAR Output
  observeEvent(input$plotbarO, {
    print("Ploting...")
    showNotification("Ploting...", duration = 20)
    rbrickO <- raster::brick(paste(folderPath(), 'stOutput.tif', sep = '/'))
    graphOut <- LuccPL::count(rbrickO, for_time_step = TRUE, metadata = c("0.false","1.true"), dates = dates())
    write.csv(graphOut, paste0(folderPath(),"/graphOut.csv"))
    
    LuccPL::lucc_barplot_result(df = graphOut, dates = dates(), graph_title=input$name_plot, style="bar", colors = c("white","black"), path_save_jpeg = folderPath())
   
    output$imageOutput5 <- renderImage({
      width  <- session$clientData$output_imageInput2_width
      list(
        src = file.path(folderPath(), "barplotO.jpeg"),
        width = width,
        contentType = "image/jpeg",
        alt = input$tstepIn2
      )
      
    }, deleteFile = FALSE)
    
    
    showNotification("Done", duration = NULL)
    gc()
  })
  

  
  
  # PLOTLINE Output
  observeEvent(input$plotlineO, {
    print("Ploting...")
    showNotification("Ploting...", duration = 20)
    rbrickO <- raster::brick(paste(folderPath(), 'stOutput.tif', sep = '/'))
    graphOut <- LuccPL::count(rbrickO, for_time_step = TRUE, metadata = c("0.false","1.true"), dates = dates())
    write.csv(graphOut, paste0(folderPath(),"/graphOut.csv"))
    
    LuccPL::lucc_barplot_result(df = graphOut, dates = dates(), graph_title=input$name_plot, style="line", colors = c("white","black"), path_save_jpeg = folderPath())
    
    output$imageOutput5 <- renderImage({
      width  <- session$clientData$output_imageInput2_width
      list(
        src = file.path(folderPath(), "lineplotO.jpeg"),
        width = width,
        contentType = "image/jpeg",
        alt = input$tstepIn2
      )
      
    }, deleteFile = FALSE)
    
    
    showNotification("Done", duration = NULL)
    gc()
  })
  
  
  #########################################  PAGE 4 (EXPORT)  #################################################    
  
  

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


