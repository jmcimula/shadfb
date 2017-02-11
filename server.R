library(shiny)
library(shinydashboard)
library(googleVis)
library(dplyr)
library(sqldf)
library(slopegraph)
library(bubbles)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(ggplot2)
library(RColorBrewer)
require(datasets)
library(DT)
source('global.R')

function(input, output) {
  
  new_data = reactive({
    
    cf = subset(cf, segment==tolower(input$selected)) %>%
         group_by(status) %>%
         summarise(cnt = sum(cnt)) %>%
         select(status, cnt)
    
  })
new_cft = reactive({
    
    cft = sum(subset(cf, segment==tolower(input$selected))$cnt)
    
})

new_slope = reactive({
  
  slope <- sqldf::sqldf(paste0("select status, startdate, cnt from cf where segment = '",tolower(input$selected),"'"))
  slp <- tidyr::spread(slope,startdate,cnt)
  slp[is.na(slp)] <- 0
  tmp <- slp  
  rownames(slp) <- tmp$status
  remove(tmp)
  slp <- slp[,-1]
  begin <- last <- 1:nrow(slp)
  slp <- cbind(begin,slp, last)
  slp
  
})

new_tree = reactive({
  rd <- sqldf::sqldf(paste0("select country, status, startdate, count(*) cnt from rawcf where segment = '",tolower(input$selected),"' and country not like '%ALI%' and country not like '%Mark%' and country not like '%mobilisation %' group by status, startdate, country"))
  rdtree <- tidyr::spread(rd,startdate,cnt)
  rdtree[ is.na(rdtree) ] <- 0 
  rdtree[["sums"]] <- rowSums(rdtree[,3:ncol(rdtree)])
  rdtree <- dplyr::filter(rdtree, sums >= input$sldinput)
  
})


new_draft <- reactive({
  
  dtrs <- sqldf::sqldf(paste0("select * from cf where segment = '",tolower(input$selected),"'"))
  draft <- sqldf::sqldf(paste0("select startdate, sum(cnt) cnt from dtrs where segment = '",tolower(input$selected),"' group by startdate"))
  draft
  
})

new_mkit = reactive({
  
  tf <- sqldf::sqldf(paste0("select status, startdate,  sum(cnt) cnt from cf where segment = '",tolower(input$selected),"' group by status, startdate"))
  mm <- transform(tf,newcol=interaction(status,paste0("(",cnt,")"), sep=''))
  
  mkit <- mm %>%  group_by(startdate) %>%
    summarise(newcol = toString(sort(unique(newcol))), total =sum(cnt)) %>%
    select(startdate, newcol, total)
  
  mkit

})

new_rcv = reactive({
  
 tf <- sqldf::sqldf(paste0("select status, startdate,  sum(cnt) cnt from cf where segment = '",tolower(input$selected),"' group by status, startdate"))
 nice <- transform(tf,newcol=interaction(paste0("rep('",status,"',",cnt,")"), sep=''))

 nkit <- nice %>%  group_by(startdate) %>%
    filter(cnt >= 1 ) %>% #Only cnt greater or egal to 5
    summarise(newcol = toString(sort(unique(newcol))),total =sum(cnt)) %>%
    select(startdate, newcol, total)
 
  nkit <- as.data.frame(nkit)
  nkit <- transform(nkit,newcols=interaction(paste0("'",startdate,"' = " ,"c(",newcol,")"), sep=''))
  nkit <- nkit[,-2]
  nkit <- head(nkit[order(nkit$total,decreasing = T),],n=4)
  nkit <- nkit %>% 
    summarise(newcol = toString(sort(unique(newcols)))) %>%
    select(newcol)
  
  nkit
  
})


  output$stapp = renderValueBox({
    valueBox(new_data()$cnt[1], "Approved Project", icon = icon('star'), color = 'green')
  })
  
  output$stapp2 = renderValueBox({
    valueBox(new_data()$cnt[1], "Approved Project", icon = icon('star'), color = 'green')
  })
  
  output$stapp3 = renderValueBox({
    valueBox(new_data()$cnt[1], "Approved Project", icon = icon('star'), color = 'green')
  })
  
  output$ston = renderValueBox({
    valueBox(new_data()$cnt[3], "Ongoing Project", icon = icon('ticket'), color = 'yellow')
    })
  output$ston2 = renderValueBox({
    valueBox(new_data()$cnt[3], "Ongoing Project", icon = icon('ticket'), color = 'yellow')
  })
  output$ston3 = renderValueBox({
    valueBox(new_data()$cnt[3], "Ongoing Project", icon = icon('ticket'), color = 'yellow')
  })
  
  output$stlend = renderValueBox({
    valueBox(new_data()$cnt[2], "Lending Project", icon = icon('ticket'), color = 'yellow')
  })
  output$stlend2 = renderValueBox({
    valueBox(new_data()$cnt[2], "Lending Project", icon = icon('ticket'), color = 'yellow')
  })
  output$stlend3 = renderValueBox({
    valueBox(new_data()$cnt[2], "Lending Project", icon = icon('ticket'), color = 'yellow')
  })
  
  output$stpipe = renderValueBox({
    valueBox(new_data()$cnt[4], "Pipeline Project", icon = icon('ticket'), color = 'yellow')
  })
  output$stpipe2 = renderValueBox({
    valueBox(new_data()$cnt[4], "Pipeline Project", icon = icon('ticket'), color = 'yellow')
  })
  output$stpipe3 = renderValueBox({
    valueBox(new_data()$cnt[4], "Pipeline Project", icon = icon('ticket'), color = 'yellow')
  })
  
  output$sttotal = renderValueBox({
    valueBox(new_cft(), "Dataset", icon = icon('ticket'), color = 'yellow')
  })
  
  output$sttotal2 = renderValueBox({
    valueBox(new_cft(), "Dataset", icon = icon('ticket'), color = 'yellow')
  })
  
  output$sttotal3 = renderValueBox({
    valueBox(new_cft(), "Dataset", icon = icon('ticket'), color = 'yellow')
  })
  
  output$text1 = renderText({
    paste("Statistical data matrix for the sector of ", paste0(toupper(substr(input$selected, 1, 1)), substr(input$selected, 2, nchar(input$selected))))
  })
  
  output$text2 = renderText({
    paste("Statistical data matrix for the sector of ", paste0(toupper(substr(input$selected, 1, 1)), substr(input$selected, 2, nchar(input$selected))))
  })
  
  output$text3 = renderText({
    paste("Statistical data matrix for the sector of ", paste0(toupper(substr(input$selected, 1, 1)), substr(input$selected, 2, nchar(input$selected))))
  })
  
  
  output$plot1 = renderPlot({
    
    slopegraph::ggslopegraph(new_slope(),main = paste("About",paste0(toupper(substr(input$selected,1,1)),substr(input$selected,2,nchar(input$selected))),"at AfDB") + theme_bw()
    )  
  })
  
  output$info = renderText({
    paste0("Attendance = ", round(input$plot_click$x * 1000))
  })
  
  output$bbbles = renderDataTable({
    
    DT::datatable(new_tree(), options = list(scrollX = TRUE))
    
  })
  
  output$plotMonth = renderPlot({
 
    fit <- rpart(formula = `2010` ~ ., data = new_tree()[, -c(1,ncol(new_tree()))], control = rpart.control(minsplit = 2, minbucket = 1, cp = 0.001), parms = list(split = "information"))
    rpart.plot(fit, box.palette="GnBu",branch.lty=3, shadow.col="gray", nn=TRUE)
    
  })
  
  output$plotSeason = renderPlot({
    
    VisualResume::VisualResume(
      titles.left = c("Project Tracker Dashboard","Miyagi University", 
                      "*Togashi Sensei Lab"),
      titles.left.cex = c(3, 2.5, 1),
      titles.right.cex = c(3, 2.5, 1),
      titles.right = c("African Development Bank","Removing blindfold", 
                       "https://www.afdb.org"),
      timeline.labels = c("Report"),
      timeline = data.frame(title = new_draft()$cnt,
                            sub = "Nb.of Project",
                            start = as.integer(new_draft()$startdate),
                            end = as.integer(new_draft()$startdate),
                            side = rep(c(0,1), times = length(unique(as.integer(new_draft()$startdate))),
                                       length.out = length(unique(as.integer(new_draft()$startdate))), each = 1)),
      milestones = data.frame(title = 0,sub = 0,year = 0),
      events = data.frame(years=new_mkit()$startdate,title=new_mkit()$newcol),
      ##Replace by countries per sector. E.g. programming = Agriculture R=country 10= number projects in this sector
      interests = eval(parse(text=paste("list(",new_rcv()$newcol,")"))),
      year.steps = 1
    )
    
  })
  
  output$table2 = renderGvis({
    gvisTable(df[-c(1,4,7,13)],
              options = list(page='enable'))
    })
  
}

