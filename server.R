library(shiny)
library(ggplot2)
library(scales)
library(grid)
library(lubridate)
library(DT)
library(dplyr)
library(dlnm)
library(splines)
library(reshape2)
library(leaflet)

shinyServer(function(input, output, session) {

 # DATA TABLE####
output$ex1 <- DT::renderDataTable({
   inFile <- input$file1
   if (is.null(inFile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
     cdcdata<-read.csv(inFile$datapath, header=input$header, sep=input$sep, 
			 quote=input$quote)
       DT::datatable(cdcdata, options = list(pageLength = 25),rownames = FALSE,class = 'cell-border stripe',
         caption = 'Table: Uploaded data')
       
  })
  cdcdata1 <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
			 quote=input$quote)
  })
  cdcdata <- reactive({
    if (is.null(cdcdata1())) {
      # User has not uploaded a file yet
      return(NULL)
    }
    mutate(cdcdata1(),display_name=Disease_name,Year=as.factor(Year),rdate = as.Date(Reporting_date, format="%y-%m-%d"),c=Cases,precip= Precipitation,ntu=Turbidity,cdd8=Dry.Days,mon=Month)})
  
 # SELECTED DATA
  selectedData = reactive({
    if (is.null(cdcdata())) {
      # User has not uploaded a file yet
      return(NULL)
    }
    filter(cdcdata(), display_name == input$disease_name,rdate >= input$years[1], rdate<=input$years[2] )})
  

# Plot data - either a single plot for one location, or faceted plots for all locations of a single type
output$plot1 <- renderPlot({
  if(is.null(selectedData()))return(NULL)
    switch(input$plotty,
           "week"  =  {aesthetics1 = aes(x=rdate, y=c)
                       geom=geom_line()
                       smoothn=80
                       smoothaes=NULL
                       xl="Date"},
           "weeky"  =  {aesthetics1 = aes(x=Week, y=c, group=Year, colour=Year)
                      geom=geom_point(alpha=0.2)
                      smoothn=0
                      smoothaes=aes(colour=Year)
                      xl="Week of the Year"} ,
           "mon" =   {aesthetics1 = aes(factor(mon), y=c)
                      geom=geom_boxplot(outlier.shape = NA)
                      xl="Month"
                     smoothaes=NULL})
    
    # Create the main ggplot
     ggplot(selectedData(), aesthetics1)+ geom +
     geom_point(position = position_jitter(width = 0.1),alpha=0.4)+
     stat_smooth(smoothaes, method = "glm", formula = y ~ splines::bs(x, input$range[2]), se = FALSE)+
     ylab("Number Reported")+scale_color_brewer(palette="Set2",name="Weekly case counts")+
    ggtitle(paste(input$disease_name, "Reports"))+xlab(xl)
})
output$plotweather <- renderPlot({
  if(is.null(selectedData()))return(NULL)
    #CREATE EXTREME ABLINE
      abln<-as.vector(quantile(cdcdata()$precip,0.9))
      abln_d<-as.vector(quantile(cdcdata()$precip[cdcdata()$Season=="Dry"],0.9))
      abln_r<-as.vector(quantile(cdcdata()$precip[cdcdata()$Season=="Rainy"],0.9))
         switch(input$plotty,
           "week"  =  {aesthetics1 = aes(x=rdate, y=precip)
                       geom=geom_line()
                       smoothn=80
                       smoothaes=NULL
                       xl="Date"
                       annotate=annotate('text',x=(max(selectedData()$rdate)-20),y=c(abln,abln_d,abln_r),label=c('90th overall',
                         "90th dry season","90th rainy season"),color=c("darkred","darkred","darkred"))},
           "weeky"  =  {aesthetics1 = aes(x=Week, y=precip, group=Year, colour=Year)
                      geom=geom_point(alpha=0.2)
                      smoothn=0
                      smoothaes=aes(colour=Year)
                      xl="Week of the Year"
                      annotate=annotate('text',x=(52/2),y=c(abln,abln_d,abln_r),label=c('90th overall',
                        "90th dry season","90th rainy season"),color=c("darkred","darkred","darkred"))},
            "mon" =   {aesthetics1 = aes(factor(mon), y=precip)
                      geom=geom_boxplot(outlier.shape = NA)
                      xl="Month"
                     smoothaes=NULL
                     annotate=annotate('text',x=c(7,7,7),y=c(abln,abln_d,abln_r),label=c('90th overall',
                       "90th dry season","90th rainy season"),color=c("darkred","darkred","darkred"))})
    
    # Create the main ggplot
     ggplot(selectedData(), aesthetics1)+ geom +
     geom_point(position = position_jitter(width = 0.2),alpha=0.4)+
     stat_smooth(smoothaes, method = "glm", formula = y ~ splines::bs(x, input$range[2]), se = FALSE)+
     ylab("Precipitation (mm/week)")+scale_color_brewer(palette="Set2",name="Weekly Precipitation")+
    ggtitle("Precipitation patterns")+xlab(xl)+geom_hline(yintercept = c(abln,abln_r,abln_d),show.legend = T,color="red",linetype=2)+ annotate
})
output$plotturbidity <- renderPlot({
  if(is.null(selectedData()))return(NULL)
    switch(input$plotty,
           "week"  =  {aesthetics1 = aes(x=rdate, y=ntu)
                       geom=geom_line()
                       xl="Date"
                       smoothaes=NULL},
           "weeky"  =  {aesthetics1 = aes(x=Week, y=ntu, group=Year, colour=Year)
                      geom=geom_point(alpha=0.2)
                      smoothn=0
                      smoothaes=aes(colour=Year)
                      xl="Week of the Year"} ,
           "mon" =   {aesthetics1 = aes(factor(mon), y=ntu)
                      geom=geom_boxplot(outlier.shape = NA)
                      xl="Month"
                     smoothaes=NULL}
                           )
  
  # Create the main ggplot
     ggplot(selectedData(), aesthetics1)+ geom +
     geom_point(position = position_jitter(width = 0.2),alpha=0.4)+
       stat_smooth(smoothaes, method = "glm", formula = y ~ splines::bs(x, input$range[2]), se = FALSE)+
     ylab("Mean Turbidity (NTU)")+scale_color_brewer(palette="Set2",name="Weekly Mean NTU")+
    ggtitle("Turbidity patterns")+xlab(xl)
})  
output$plotcdd <- renderPlot({
  if(is.null(selectedData()))return(NULL)
    switch(input$plotty,
           "week"  =  {aesthetics1 = aes(x=rdate, y=cdd8)
                       geom=geom_line()
                       xl="Date"
                       smoothaes=NULL},
           "weeky"  =  {aesthetics1 = aes(x=Week, y=cdd8, group=Year, colour=Year)
                      geom=geom=geom_point(alpha=0.2)
                      smoothn=0
                      smoothaes=aes(colour=Year)
                      xl="Week of the Year"} ,
           "mon" =   {aesthetics1 = aes(factor(mon), y=cdd8)
                      geom=geom_boxplot(outlier.shape = NA)
                      xl="Month"
                     smoothaes=NULL},
       "pvt" =   {aesthetics1 = aes(precip, y=cdd8)
                      geom=geom_point(alpha=0.2)
                      xl="Precipitation (mm/week)"
                     smoothaes=NULL})
  
  # Create the main ggplot
       ggplot(selectedData(), aesthetics1)+ geom +
       geom_point(position = position_jitter(width = 0.2),alpha=0.4)+
       stat_smooth(smoothaes, method = "glm", formula = y ~ splines::bs(x, input$range[2]), se = FALSE)+
    ylab("Dry days")+scale_color_brewer(palette="Set2",name="Dry days")+
    ggtitle("No of dry days (precipitation <0.1mm/day) in the last two months")+xlab(xl)
})

output$plotturbidity1 <- renderPlot({
  if(is.null(selectedData()))return(NULL)
  if(input$var2==input$var1)return(NULL)
    selectedData = reactive({filter(cdcdata(), display_name == "Cryptosporidiosis",rdate >= input$yearsq[1], rdate<=input$yearsq[2] )})
    #lagpad <- function(x, k) { c(rep(NA, k), x)[1 : length(x)]  }
    switch(input$var2,
      "Precipitation" = {col2=select(selectedData(),matches(input$var2))
                        col1=select(selectedData(),matches(input$var1))
                        dat<-data.frame(col1,col2)
                        xp<-data.frame(exphist(dat$Precipitation, lag=6))
      switch(input$plottyq,
      "A"  =  {dat2<-data.frame(c1=dat$Turbidity,c2=xp$lag0)},
      "B"  =  {dat2<-data.frame(c1=dat$Turbidity,c2=xp$lag1)},
      "C" =  {dat2<-data.frame(c1=dat$Turbidity,c2=xp$lag2)},
      "D"  =  {dat2<-data.frame(c1=dat$Turbidity,c2=xp$lag3)},
      "E"  =  {dat2<-data.frame(c1=dat$Turbidity,c2=xp$lag4)},
      "F"  =  {dat2<-data.frame(c1=dat$Turbidity,c2=xp$lag5)},
      "G"  =  {dat2<-data.frame(c1=dat$Turbidity,c2=xp$lag6)})                  
    },
      
      "Turbidity" = {col2=select(selectedData(),matches(input$var2))
                    col1=select(selectedData(),matches(input$var1))
                    dat<-data.frame(col1,col2)
                    xp<-data.frame(exphist(dat$Turbidity, lag=6))
         switch(input$plottyq,
      "A"  =  {dat2<-data.frame(c1=dat$Precipitation,c2=xp$lag0)},
      "B"  =  {dat2<-data.frame(c1=dat$Precipitation,c2=xp$lag1)},
      "C" =  {dat2<-data.frame(c1=dat$Precipitation,c2=xp$lag2)},
      "D"  =  {dat2<-data.frame(c1=dat$Precipitation,c2=xp$lag3)},
      "E"  =  {dat2<-data.frame(c1=dat$Precipitation,c2=xp$lag4)},
      "F"  =  {dat2<-data.frame(c1=dat$Precipitation,c2=xp$lag5)},
      "G"  =  {dat2<-data.frame(c1=dat$Precipitation,c2=xp$lag6)})
      }
    )
     switch(input$var1,
      "Precipitation" = {col2=select(selectedData(),matches(input$var2))
                        col1=select(selectedData(),matches(input$var1))
                        dat<-data.frame(col1,col2)
                         xp<-data.frame(exphist(dat$Turbidity, lag=6))
          switch(input$plottyq,
      "A"  =  {dat2<-data.frame(c1=dat$Precipitation,c2=xp$lag0)},
      "B"  =  {dat2<-data.frame(c1=dat$Precipitation,c2=xp$lag1)},
      "C" =  {dat2<-data.frame(c1=dat$Precipitation,c2=xp$lag2)},
      "D"  =  {dat2<-data.frame(c1=dat$Precipitation,c2=xp$lag3)},
      "E"  =  {dat2<-data.frame(c1=dat$Precipitation,c2=xp$lag4)},
      "F"  =  {dat2<-data.frame(c1=dat$Precipitation,c2=xp$lag5)},
      "G"  =  {dat2<-data.frame(c1=dat$Precipitation,c2=xp$lag6)})                  
    },
      
      "Turbidity" = {col2=select(selectedData(),matches(input$var2))
                    col1=select(selectedData(),matches(input$var1))
                    dat<-data.frame(col1,col2)
                      xp<-data.frame(exphist(dat$Precipitation, lag=6))
     switch(input$plottyq,
      "A"  =  {dat2<-data.frame(c1=dat$Turbidity,c2=xp$lag0)},
      "B"  =  {dat2<-data.frame(c1=dat$Turbidity,c2=xp$lag1)},
      "C" =  {dat2<-data.frame(c1=dat$Turbidity,c2=xp$lag2)},
      "D"  =  {dat2<-data.frame(c1=dat$Turbidity,c2=xp$lag3)},
      "E"  =  {dat2<-data.frame(c1=dat$Turbidity,c2=xp$lag4)},
      "F"  =  {dat2<-data.frame(c1=dat$Turbidity,c2=xp$lag5)},
      "G"  =  {dat2<-data.frame(c1=dat$Turbidity,c2=xp$lag6)})
      }
    )
      # Create the main ggplot

         ggplot(dat2,aes(x=c1, y=c2))+ geom_point(alpha=0.2)+geom_smooth()+
     ggtitle(paste(input$var1,"vs", input$var2))+xlab(input$var1)+ylab(input$var2)
})
output$plotcdd1 <- renderPlot({
  if(is.null(selectedData()))return(NULL)
    selectedData = reactive({filter(cdcdata(), display_name == input$disease_name,rdate >= input$yearsq[1], rdate<=input$yearsq[2] )})
    switch(input$plottyq,
           "week"  =  {aesthetics1 = aes(x=rdate, y=cdd8)
                       geom=geom_line()
                       xl="Date"
                       smoothaes=NULL},
           "weeky"  =  {aesthetics1 = aes(x=Week, y=cdd8, group=Year, colour=Year)
                      geom=geom_line()
                      smoothn=0
                      smoothaes=aes(colour=Year)
                      xl="Week of the Year"} ,
           "mon" =   {aesthetics1 = aes(factor(mon), y=cdd8)
                      geom=geom_boxplot()
                      xl="Month"
                     smoothaes=NULL},
       "pvt" =   {aesthetics1 = aes(precip, y=cdd8)
                      geom=geom_point(alpha=0.2)
                      xl="Precipitation (mm/week)"
                     smoothaes=NULL})
  
  # Create the main ggplot
     ggplot(selectedData(), aesthetics1)+ geom +
     ylab("CDD")+scale_color_brewer(palette="Set2",name="CDD")+geom_point(position = position_jitter(width = 0.2),alpha=0.4,color="red")+
    ggtitle("No of dry days in last two months preceding current week")+xlab(xl)
})
output$plotdlnm <- renderPlot({
  if(is.null(selectedData()))return(NULL)
  selectedData = reactive({filter(cdcdata(), display_name == "Cryptosporidiosis+Giardiasis" )})
    #MODELS
dat<-selectedData()  
wk<-seq(min(dat$Week_no):max(dat$Week_no))
spl<-ns(wk,df=5*round(nrow(dat)/52))

model1 <- glm(Turbidity~factor(mon),family = quasipoisson(),
              data=dat)
lagknots<-logknots(6,3)
ns.basis <- crossbasis(dat$Precipitation,lag=6, argvar=list(fun="ns",df=3,cen=0),
arglag=list(fun="ns",knots=lagknots))#Precipitation centered at 0 mm/week all comparisions will be to this value
ns<-update(model1,.~.+ns.basis)
ns.pred <- crosspred(ns.basis,ns,at=c(0:max(dat$Precipitation)),lag=6)#512 obs
##Plots##
abln<-round(as.vector(quantile(cdcdata()$precip,0.9)))
#par(mfrow=c(1,2))
#plot(ns.pred,"overall",ylab= "Times increase in NTU",xlab="Weekly Precipitation")
#rug(dat$precip)
plot(ns.pred,var=abln,type="p",ci="bars",col=1,pch=19,
xlab="Weeks after extreme precipitation",ylab="Times increase in NTU")
box(which = "plot", lty = "solid")
#plot(ns.pred,"overall",ylab= "Change in NTU",xlab="Weekly Precipitation")
#rug(dat$precip)
})
output$Map <- renderLeaflet({
	leaflet() %>% addProviderTiles("CartoDB.Positron")%>%setView(lng=-123.1179, lat=49.2604078, zoom=10)  
})
#TAB 4####  
  Dec <- reactive({
	x <- sort(as.numeric(substr(input$dec, 1, 4)))
	if(any(is.na(x))) return(NULL) else return(c("1960-1989", paste(x, x+9, sep="-")))
})
climdat<-read.csv("locationA.csv",header = T)
nDec <- reactive({ length(Dec()) })
Colors <- reactive({ if(input$variable=="Temperature" & nDec()) c("#666666", colorRampPalette(c("gold", "orange", "orangered", "darkred"))(nDec()-1)) else c("#666666", colorRampPalette(c("aquamarine", "dodgerblue4"))(nDec()-1)) })

RCPLabel <- reactive({ switch(input$rcp, "4.5 (low)"="Low-Range Emissions (RCP 4.5)", "6.0 (medium)"="Mid-Range Emissions (RCP 6.0)", "8.5 (high)"="High-Range Emissions (RCP 8.5)") })

Unit <- reactive({ if(input$variable=="Temperature") paste0("°","C" ) else paste0("mm/day" )})

Min <- reactive({ if(input$variable=="Temperature") NULL else 0 })

CRU_var <- reactive({ subset(climdat, Var==input$variable) })

d0 <- reactive({
	if(input$variable=="Temperature" | input$variable=="Precipitation" ){
		if(!exists("climdat")){
			prog <- Progress$new(session, min=0, max=1)
			on.exit(prog$close())
			prog$set(message="Loading data...", value=1)
			load(paste0("climdat.RData"), envir=.GlobalEnv)
		}
		return(climdat)
	}
})

d2_var <- reactive({ subset(d0(), Var==input$variable) })
d3_scen <- reactive({
	x <- rbind(subset(d2_var(), Scenario==substr(RCPLabel(), nchar(RCPLabel())-7, nchar(RCPLabel())-1)))
	x
})
d4_dec <- reactive({ if(is.null(d3_scen())) NULL else subset(d3_scen(), Decade %in% Dec()) })

output$Chart1 <- renderChart2({
 
  if(is.null(d4_dec())) return(Highcharts$new())
	if(!length(input$dec) || input$dec=="") return(Highcharts$new())
	p <- Highcharts$new()
	p$colors(Colors())
	p$title(text=paste("Average Monthly", input$variable, "for Location A"), style=list(color="#000000"))
	p$subtitle(text=paste("Historical and 12-Model Projections using", RCPLabel()), style=list(color="gray"))
	p$legend(verticalAlign="top", y=50, itemStyle=list(color="gray"))
	p$xAxis(categories=month.abb)
	p$yAxis(title=list(text=paste0(input$variable, " (", Unit(), ")"), style=list(color="gray")), min=Min())
	d <- d4_dec()[5:7]
	ddply(d, .(Decade), function(x) {
	  
	g <- unique(x$Decade); x$Decade <- NULL; json <- toJSONArray2(x, json=F, names=F)
	p$series(data=json, name=g, type="columnrange")
	return(NULL)
	})
	p$exporting(enabled=F, scale=4)
	p$set(height=400)
	p
})


#FOR EXTREME PRECIPITATION
rcp<-read.csv("RCP 8.5_wk.csv",stringsAsFactors = F)  %>%
  select(week:decade)%>%filter(year>=2020)%>%
  mutate(stweek=as.Date(stweek,"%m/%d/%Y"),
    endweek=as.Date(endweek,"%m/%d/%Y"))
datmelt<-melt(select(rcp,ACCESS1.0:HadGEM2.ES,decade),id=c("decade"))
#prec<-cdcdata()$precip
#qtl<-as.vector(quantile(prec,probs=c(0.25,0.5,0.75,0.9)))
qtl<-c(10.39999,30.44996,59.62496,91.14992)
datmelt$per<-cut(datmelt$value,breaks =c(-1,0.1,qtl[1],qtl[2],qtl[3],qtl[4],max(datmelt$value)),labels = c("0-0.1",paste("0.1 -",round(qtl[1],2)),paste(round(qtl[1],2),"-",round(qtl[2],2)),paste(round(qtl[2],2),"-",round(qtl[3],2)),paste(round(qtl[3],2),"-",round(qtl[4],2)),paste(">",round(qtl[4],2))))
#meltcdcdata<-data.frame(value=cdcdata()$precip,decade=rep("Historical",length.out(nrow(cdcdata#()))))
datmelt2<-datmelt%>%group_by(decade,variable)%>%tally()%>%mutate(N=n)%>%select(-n)%>%ungroup()
datmelt3<-datmelt%>%group_by(decade,variable,per)%>%tally()%>%ungroup()
datmelt3<-left_join(datmelt3,datmelt2,by=c("decade","variable"))
datmelt3<-datmelt3%>%mutate(nn=ifelse(decade=="2020s",round(n/N*100),
                            ifelse(decade=="2040s",round(n/N*100),
                                   ifelse(decade=="2060s",round(n/N*100),
                                          round(n/N*100)))))
datmelt4<-datmelt3%>%group_by(decade,per)%>%summarize(l=min(nn),h=max(nn),m=median(nn))%>%ungroup()

output$Chart2 <- renderChart2({
 	  p <- Highcharts$new()
Colors <- c("#666666", colorRampPalette(c("aquamarine", "dodgerblue4"))(4-1)) 
p$colors(Colors)
p$title(text=paste("Percentage of precipitation falling in different categories by decades"), style=list(color="#000000"))
p$subtitle(text=paste("Historical and 12-Model Projections using High-Range Emissions", "(RCP 8.5)"), style=list(color="gray"))
p$legend(verticalAlign="top", y=50, itemStyle=list(color="gray"))
p$xAxis(title=list(text=paste0("Precipitation (mm/week)")),categories=c(levels(datmelt4$per)))
p$yAxis(title=list(text=paste0("Percent"),style=list(color="gray")))
d <- datmelt4[c(1,3:4)]
ddply(d, .(decade), function(x) {
  g <- unique(x$decade)  
  x$decade <- NULL
  json <- toJSONArray2(x, json=F, names=F)
  p$series(data=json, name=g, type="columnrange")
  return(NULL)
 })
p$exporting(enabled=F, scale=4)
p$set(height=400)
p
})

#######
output$plotimpact<-renderPlot({
   if(is.null(selectedData()))return(NULL)
  #MODELS
  selectedDataim <-read.csv("sample_data_final3.csv",header=T)#EXAMPLE ONLY
dat<-selectedDataim%>%filter(Disease_name=="Cryptosporidiosis+Giardiasis") 
wk<-seq(min(dat$Week_no):max(dat$Week_no))
spl<-ns(wk,df=9*round(nrow(dat)/52))

model1 <- glm(Cases2~spl,
              data=dat)
lagknots<-logknots(6,3)
ns.basis <- crossbasis(dat$Precipitation,lag=6, argvar=list(fun="ns",df=3,cen=0),
arglag=list(fun="ns",knots=lagknots))#Precipitation centered at 0 mm/week all comparisions will be to this value
ns<-update(model1,.~.+ns.basis)
ns.pred <- crosspred(ns.basis,ns,at=c(0:max(dat$Precipitation)),lag=6)#512 obs
##Plots##
abln<-round(as.vector(quantile(cdcdata()$Precipitation,0.9)))
#par(mfrow=c(1,2))
#plot(ns.pred,"overall",ylab= "Percent increase in cases",xlab="Weeks after extreme precipitation")
#rug(dat$precip)
plot(ns.pred,var=abln,type="p",ci="bars",col=1,pch=19,
xlab="Weeks after extreme precipitation",ylab=paste("Percent increase in Cryptosporidiosis+Giardiasis",input$disease_name,"cases"))
box(which = "plot", lty = "solid")
#plot(ns.pred,"overall",ylab= "Change in NTU",xlab="Weekly Precipitation")
#rug(dat$precip)
})
##TAB NO 5###
output$pif<-renderPlot({
  if(is.null(selectedData()))return(NULL)
#EXAMPLE ONLY
#WILL BE CALCULATED IN THE FINAL DELIVERABLE
mn<-c(18,19,13,1,-27,-25,-31,-22,2,21,29,19)
min<-c(10,11,7,-0.5,-11,-21,-17,-20,5,12,21,13)
mx<-c(24,30,19,8,-30,-31,-36,-39,-5,27,32,25)
decade<-c(rep("2080s",times=12))
month<-month.abb

datmeltmn<-data.frame(decade,month,min,mn,mx,month.id = 1:12)
limits <- aes(ymax = mx, ymin=min)
ggplot(datmeltmn,aes(reorder(month,month.id),mn))+geom_bar(stat="identity",position="dodge",color="red",fill="lightblue")+
  geom_errorbar(limits, position="dodge",size=0.5,width=0.3)+ylab("Percent deviation from historical")+xlab("Months")+scale_y_continuous(breaks=seq(-40,50,by=10),limits=c(-40,40))+theme(text = element_text(size=12))

})
})


  
  
  

