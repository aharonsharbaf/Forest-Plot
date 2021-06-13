library("shiny")
library("XLConnect")
library("data.table")
# library("sas7bdat")
library("ggplot2")
# library("survminer")
library("DT")
library("forestplot")
#	,n/N (%)	,Rates per 1000 \n person- years	,n/N (%)	,Rates per 1000 \n person- years	,HR	,low_CI	,high_CI	,Hazard Ratio (95% CI)	,p-value	,p-interaction
server<-function(input,output) { 
  
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    loadWorkbook(file = file1$datapath)
    # fread(file = file1$datapath,data.table = F)
  })  
  output$choose_sheet <- renderUI({
    radioButtons("choosesheet","Select sheet",choices =as.list(getSheets(data())))
  })

  output$format_one <- renderUI({
    numericInput("formatone", label = h3("Format to data in one coulmn"), value = NA)
  })
  
  output$column_names <-renderUI({
    textInput("columnnames", label = h3("Coulmn Names"), value = "")
  })
  
  output$remove_row <-renderUI({
    textInput("removerow", label = h3("choice nmbers row to remove"), value = "")
  })
  
  data_1 <- reactive({
    sheet1 <- input$choosesheet
    if(is.null(sheet1)){return()}
    dat_org <- readWorksheet(object = data(),sheet = sheet1)
    # print(input$removerow)
    # dat_org <- data()
    # print(input$formatone)
    func_HR_CI <- function(val){
      if(grepl(',',x = val)){
        val <- unlist(strsplit(x = val,split = ' (',fixed = T))
        val <- c(val[1],
                 unlist(strsplit(val[2],',')))
        pelet <- c(HR = val[1],
                   low_CI = val[2],
                   upp_CI = unlist(strsplit(val[3],')',fixed = T)))
      }else{
        val <- unlist(strsplit(x = val,split = ' (',fixed = T))
        val <- c(val[1],
                 unlist(strsplit(val[2],'-')))
        pelet <- c(HR = val[1],
                   low_CI = val[2],
                   upp_CI = unlist(strsplit(val[3],')',fixed = T)))
      }
      # pelet <- as.numeric(pelet)
      return(pelet)
    }
    #,n/N (%),	n/N (%)	,Hazard ratio	,Low,High,	Hazard ratio,	interaction p-value
    if(!is.na(input$formatone)){
      tmp <- t(sapply(X = dat_org[,input$formatone],FUN = func_HR_CI))
      tmp <- apply(tmp,2,as.numeric)
      dat_org <- cbind.data.frame(dat_org[1:(input$formatone - 1)],
                                  tmp,
                                  dat_org[(input$formatone):ncol(dat_org)])
    }
    
    if(!input$columnnames == ""){
      tmp <- unlist(strsplit(input$columnnames,","))
      tmp <- gsub(pattern = "\\\\n",x = tmp,replacement = '\n')
      colnames(dat_org) <- tmp

    }
    if(!input$removerow == ""){
      dat_org <- dat_org[-as.numeric(unlist(strsplit(input$removerow,","))),]
    }
    # print(dat_org)
    dat_org
  })  
  
  
  output$table <- renderTable({
    if(is.null(data_1())){return ()}
    data_1()
  })
  
  output$tb1 <- renderUI({
    tableOutput("table")
  })
  
  
  #######   filtering
  
  output$box_size_num <- renderUI({
    numericInput("boxsizenum", label = h3("Box Size"), value = 0.3)
  })
  
  output$xtick_name <- renderUI({
    textInput("xtickname", label = h3("x tick name"), value = NA)
  })
  
  output$graph_pos <-renderUI({
    numericInput("graphpos", label = h3("Graph posision"), value = 6)
  })
  
  output$title_name <-renderUI({
    textInput("titlename", label = h3("title name"),
              value = "Dapagliflozin                       Placebo")
  })
  
  output$xlab_name <-renderUI({
    textInput("xlabname", label = h3("x lab name"),
              value = "<---Dapagliflozin---   ---Placebo--->")
  })
  
  output$limit_val <-renderUI({
    textInput("limitval", label = h3("limit CI in graph"),
              value = NA)
  })
  
  output$is_summary <-renderUI({
    textInput("issummary", label = h3("bold row"),
              value = "1")
  })
  
  output$gray_row <- renderUI({
    textInput("grayrow", label = h3("Rows with Gray background"), value = NA)
  })
  
  output$gray_height <- renderUI({
    textInput("grayheight", label = h3("height Gray background"), value = NA)
  })
  
  output$x_log <-renderUI({
    checkboxInput("xlog", label = "Format log", value = TRUE)
  })
  
  output$graph_width_val <-renderUI({
    numericInput("graphwidthval", label = h3("Graph width"), value = 100)
  })
  
  output$file_type <-renderUI({
    radioButtons("filetype", label = h3("file type png/pdf"),
                 choices = list("png" = 1, "pdf" = 2),selected = 1)
  })
  
  output$height_size <-renderUI({
    numericInput("heightsize", label = h3("Height Size"), value = 900)
  })
  
  output$width_size <-renderUI({
    numericInput("widthsize", label = h3("Width Size"), value = 1400)
  })
  
  output$res_size <-renderUI({
    numericInput("ressize", label = h3("Resulusion Size"), value = 100)
  })
  
  output$name_file <-renderUI({
    textInput("namefile", label = h3("Name file"),
              value = "name file")
  })
  
  data_new <- reactive({
    dat <- data_1()
    
    low_col <- which(grepl(pattern = "low",tolower(names(dat))) == 1)
    upp_col <- which(grepl(pattern = "upp",tolower(names(dat))) == 1 | 
                       grepl(pattern = "high",tolower(names(dat))) == 1)
    hr_col <- which(grepl(pattern = "hazard ratio",tolower(names(dat))) == 1 |
                      grepl(pattern = "HR",names(dat)) == 1 |
                      grepl(pattern = "point estimate",tolower(names(dat))) == 1)[1]
    
    tabletext <- NULL
    for(i in 1:ncol(dat)){
      if(i %in% c(low_col,upp_col,hr_col)){
        dat[,i] <- as.numeric(dat[,i])
      }else{
        tabletext <- cbind(tabletext,
                           c(names(dat)[i],dat[,i]))
                           # c(NA,names(dat)[i],NA,dat[,i]))
      }
    }
    print('tabletext:')
    print(tabletext)
    # tabletext <- tabletext[,-input$formatone]
    return(list(tabletext=tabletext,
                data_o = dat,
                l_u_hr = c(low_col,upp_col,hr_col)))
  })

  #### plotin results
  # plot_val <- reactive({
  plot_val <- function(){
    dat_log <- list(choosesheet = input$choosesheet,
                  columnnames = input$columnnames,
                  removerow = input$removerow,
                  boxsizenum = input$boxsizenum,
                  xtickname = input$xtickname,
                  graphpos = input$graphpos,
                  titlename = input$titlename,
                  xlabname = input$xlabname,
                  limitval = input$limitval,
                  issummary = input$issummary,
                  grayrow = input$grayrow,
                  grayheight = input$grayheight,
                  xlog = input$xlog,
                  graphwidthval = input$graphwidthval,
                  filetype = input$filetype,
                  heightsize = input$heightsize,
                  widthsize = input$widthsize,
                  ressize = input$ressize,
                  namefile = input$namefile
    )
    pelet <- loadWorkbook("C:\\Users\\aharo\\OneDrive\\מסמכים\\עבודה עם עליזה ואילן\\template_table_1.xlsx")
    tmp <- 1
    for(i in seq(1,length(dat_log)*3,3)){
      writeWorksheet(object = pelet,data = dat_log[tmp],sheet = 1,startRow = i)
      tmp <- tmp+1
    }
    saveWorkbook(object = pelet,file = paste0("C:\\Users\\aharo\\OneDrive\\מסמכים\\עבודה עם עליזה ואילן\\forestplot\\",
                                              dat_log$choosesheet," log information.xlsx"))
    
    tabletext <- data_new()[[1]]
    dat <- data_new()[[2]]
    l_u_hr <- data_new()[[3]]
    issummary_val <- c(1:nrow(tabletext)) %in% as.numeric(unlist(strsplit(input$issummary,",")))
    if(input$limitval == ""){
      # print(c(min(dat[,l_u_hr[1]]),max(dat[,l_u_hr[2]])))
      clip_val <- c(min(dat[,l_u_hr[1]],na.rm = T),max(dat[,l_u_hr[2]],na.rm = T))
    }else{
      clip_val <- as.numeric(unlist(strsplit(input$limitval,",")))
    }
    
    
    if(input$grayrow == ''){
      hrzl_lines_val <- list("3" = gpar(lwd = 0.2, columns = 1:(ncol(tabletext)+1), col = 'white'))
    }else{
      grayrow_tmp <- as.numeric(unlist(strsplit(input$grayrow,",")))
      grayheight_tmp <- as.numeric(unlist(strsplit(input$grayheight,",")))
      hrzl_lines_func <- function(row, height_val, tabletext){
        if(length(row) != length(height_val)){
          stop('gray row  and gray height need to be same legth')
        }
        pelet <- NULL
        for(i in 1:length(row)){
          pelet <- c(pelet, list(gpar(lwd = height_val[i], lineend="butt",
                                               columns = c(1:(ncol(tabletext)+1)), col = "#99999922")))
        }
        names(pelet) <- row
        return(pelet)
      }
      print(grayrow_tmp)
      print(grayheight_tmp)

      hrzl_lines_val <- hrzl_lines_func(grayrow_tmp, grayheight_tmp, tabletext)
    }
    print(hrzl_lines_val)
    if(input$xtickname == ""){
      plot.new()
      par(bg = 'blue')

      forestplot(labeltext=tabletext, graph.pos=input$graphpos,mean=c(NA,dat[,l_u_hr[3]]), 
                 lower=c(NA,dat[,l_u_hr[1]]), upper=c(NA,dat[,l_u_hr[2]]),
                 xlab=input$xlabname,
                 txt_gp=fpTxtGp(label=gpar(cex=1.25),
                                # ticks=gpar(cex=1.1),
                                xlab=gpar(cex = 1.2),
                                title=gpar(cex = 1.7)),title = input$titlename,
                 col=fpColors(box="black", lines="black", zero = "gray50"),
                 is.summary = issummary_val,graphwidth = unit(input$graphwidthval,"mm"),
                 # zero=1,
                 cex=0.9, lineheight = "auto", boxsize=input$boxsizenum, colgap=unit(4,"mm"),
                 xlog = input$xlog,clip = clip_val,xticks = NULL,
                 lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.2)
    }else{
      xtick_name_tmp <- as.numeric(unlist(strsplit(input$xtickname,",")))
      plot.new()
      par(bg = 'blue')
      forestplot(labeltext=tabletext, graph.pos=input$graphpos,mean=c(NA,dat[,l_u_hr[3]]), #גודל המרובע
                 lower=c(NA,dat[,l_u_hr[1]]), upper=c(NA,dat[,l_u_hr[2]]),# גבולות הרווח סמך
                 xlab=input$xlabname,
                 ###
                 hrzl_lines = hrzl_lines_val,
                 # hrzl_lines = list(#"3" = gpar(lwd = 4, columns = c(1:4,6:8), col = "#99999922"),
                 #                   "4" = gpar(lwd = 80, lineend="butt", columns = c(1:8), col = "#99999922"),
                 #                   #"7" = gpar(lwd = 2, columns = c(1:4,6:8), col = "#99999922"),
                 #                   "8" = gpar(lwd = 140, lineend="butt", columns = c(1:8), col = "#99999922"),
                 #                   #"15" = gpar(lwd = 6, columns = c(1:4,6:8),  col= "#99999922"),
                 #                   "16" = gpar(lwd = 80, lineend="butt", columns = c(1:8), col = "#99999922"),
                 #                   # "19" = gpar(lwd = 1, columns = c(1:8), col = "#99999922"),
                 #                   "20" = gpar(lwd = 140,lineend="butt", columns = c(1:8), col = "#99999922"),
                 #                   # "24" = gpar(lwd = 1, columns = c(1:8), col = "#99999922"),
                 #                   "25" = gpar(lwd = 120, lineend="butt", columns = c(1:8), col = "#99999922"),
                 #                   # "31" = gpar(lwd = 1, columns = c(1:8), col = "#99999922"),
                 #                   "32" = gpar(lwd = 80, lineend="butt", columns = c(1:8), col = "#99999922")),
                 #                   # "35" = gpar(lwd = 1, columns = c(1:8), col = "#99999922")),
                 ###
                 txt_gp=fpTxtGp(label=gpar(cex=1.25),
                                ticks=gpar(cex=1.1),
                                xlab=gpar(cex = 1.2),
                                title=gpar(cex = 1.7)),title = input$titlename,
                 col=fpColors(box=c(rep(c("black",'red'),5),'blue'), lines="black", zero = "gray50"),
                 is.summary = issummary_val,graphwidth = unit(input$graphwidthval,"mm"),
                 # zero=1,
                 cex=0.9, lineheight = "auto", boxsize=input$boxsizenum, colgap=unit(4,"mm"),
                 xlog = input$xlog, xticks = xtick_name_tmp,clip = clip_val,
                 lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.2)
    }
    

  }
  
  observeEvent(input$ressize, {
    output$gr_BA <- renderPlot(res=input$ressize,expr={
      plot_val()
    })
  })
  # output$gr_BA <- renderPlot({
  #   plot_val()
  # })
  
  output$size_plot_ui <- renderUI({
    plotOutput('gr_BA',width = input$widthsize, height = input$heightsize)
  })
    
  output$downloadData <- downloadHandler(
    
    filename = ifelse(input$filetype == 1,paste0(input$choosesheet,".png"),paste0(input$choosesheet,".pdf")),
    content = function(filename) {
      # png(file = filename, height = input$heightsize, width = input$widthsize,res = input$ressize)
      if(input$filetype == 1){
        png(file = filename, height = input$heightsize, width = input$widthsize, res = input$ressize)
      }else{
        pdf(file = filename, height = input$heightsize, width = input$widthsize)
      }
      plot_val()
      dev.off()
    }
    # png(paste0(getwd(),"/",input$choosesheet,".png"),width=input$widthsize, height=input$heightsize),
  )
  output$save_file <- renderUI({
    downloadButton(outputId = "downloadData", label = "Download")
  })

  #label,	UACR Group	,n/N (%)	,Adjusted HR	,Lower CI	,Upper CI	,Hazard Ratio (95% CI)*	,P-Value,	P for trend
  #0.25,0.5,1,2,3
  
}