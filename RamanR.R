setwd(
  "C:\\Users\\mBrain\\luckyCloud\\Seafile\\Meine_Bibliothek\\Uni\\Studentische_Hilfskraft_KI_Medizin\\Raman"
)
library(manipulate)  
library(plotly)

Legende <- read.delim("Legende_neu.txt", sep = c("\t", "\n"))
#View(Legende)
measurements_quantity<-202

measurements <- list(rep(0, measurements_quantity))

for (file in list.files()) {
  if (suppressWarnings(is.na(as.numeric(substring(file, 1, 1))))) {
    next
  }
  fileData <- read.table(file, sep = "\t", header = F)
  index <- as.numeric(substring(file, 1, regexpr(" ", file)[1] - 1))
  
  if(fileData[1,1]==fileData[2,1]){
    fileData<-fileData[,2:3]
    fileData<-fileData[1:length(fileData[,1])/2,]
    #print(head(fileData))
  }
  xdata<-fileData[,1]
  ydata<-fileData[,2]
  data<-data.frame(xdata,ydata)
  
  measurements[[index]] <- list("data"=data,
                                "file"=index,
                                "Fall"=Legende[index,2],
                                "Laser"=Legende[index,3],
                                "Spezies"=Legende[index,4],
                                "Gewebe"=Legende[index,5],
                                "dateiname"=Legende[index,6] 
  )
}
#given x, will plot the spectrum of the measurements,
show_spectrum<-function(x){
  if(x==0){
    x==1
  }
  plot_ly(data,x=~xdata,y=~ydata,
          mode="lines",type="scatter")%>%
    layout(title = paste("File:",measurements[[x]]$file,", Spezies:",measurements[[x]]$Spezies,", Gewebe:",measurements[[x]]$Gewebe,", Laser:",measurements[[x]]$Laser))
}

#calculate the biggest 2 peaks (to transform given data to 2 dim Data -> for plotting)
#gap -> gap between biggest and second biggest
#returns data.frame
get_peaks_dataFrame<-function(gap=200){
  all_index_biggest<-rep(measurements_quantity)
  all_index_second_biggest<-rep(0,measurements_quantity)
  all_Spezies<-rep(0,measurements_quantity)
  all_Laser<-rep(0,measurements_quantity)
  all_Gewebe<-rep(0,measurements_quantity)
  all_Fall<-rep(0,measurements_quantity)
  all_File<-rep(0,measurements_quantity)
  for(m in measurements){
    biggest<-0
    index_biggest<-0
    second_biggest<-0
    index_second_biggest<-0
    
    count<-1
    for(d in m$data[,2]){
      if(count >10 & count < length(m$data[,1])-10){#skip the first and last 10 points, bc they may be measurement errors, see file 146
        if (d>=biggest){
          if(abs(m$data[count,1]-index_biggest)>gap ){
            index_second_biggest<-index_biggest
            second_biggest<-biggest
          }
          biggest<-d
          index_biggest<-m$data[count,1]
        }
        else{
          if(d>second_biggest & abs(m$data[count,1]-index_biggest)>gap){
            second_biggest<-d
            index_second_biggest<-m$data[count,1]
          }
        }
      }
      count<-count+1
    }
    #print(paste("index biggest and seccond biggest of",m$file,index_biggest,index_second_biggest))
    
    all_index_biggest[m$file]<-as.numeric(index_biggest)
    all_index_second_biggest[m$file]<-as.numeric(index_second_biggest)
    all_Spezies[m$file]<-as.numeric(m$Spezies)
    all_Gewebe[m$file]<-as.numeric(m$Gewebe)
    all_Laser[m$file]<-as.numeric(m$Laser)
    all_File[m$file]<-as.numeric(m$file)
    all_Fall[m$file]<-as.numeric(m$Fall)
    
  }
  dataFrame<-data.frame(all_File,all_Spezies,all_Gewebe,all_Laser,all_Fall,all_index_biggest,all_index_second_biggest)
  View(dataFrame)
  return(dataFrame)
}

two_peaks_dataFrame<-get_peaks_dataFrame()


#for plotting 2 peaks data
plot_2_peaks_data<-function(plot_what){
  #plot_what<-c("Laser"="all","Gewebe"="all","Spezies"="all","file"=0,"Fall"=0)
  plot(NA,NA,main="classes",xlim=c(0,4000),ylim=c(0,4000),xlab="first peak",ylab="second peak")
  for(i in 1:measurements_quantity){
    if(plot_what$Spezies!="all"){
      if(two_peaks_dataFrame[i]$all_Spezies!=plot_what$Spezies){
        next
      }
    }
    if(plot_what$Gewebe!="all"){
      if(two_peaks_dataFrame[i]$all_Gewebe!=plot_what$Gewebe){
        next
      }
    }
    if(plot_what$file!=0){
      if(two_peaks_dataFrame[i]$all_File!=plot_what$file){
        next
      }
    }
    if(plot_what$Laser!="all"){
      if(two_peaks_dataFrame[i]$all_Laser!=plot_what$Laser){
        next
      }
    }
    if(plot_what$Fall!=0){
      if(two_peaks_dataFrame[i]$all_Fall-as.numeric(plot_what$Fall)!=0){
        next
      }
    }
    if(length(two_peaks_dataFrame[i]$all_index_biggest)==length(two_peaks_dataFrame[i]$all_index_second_biggest)){
      points(two_peaks_dataFrame[i]$all_index_biggest,two_peaks_dataFrame[i]$all_index_second_biggest,col=two_peaks_dataFrame[i]$all_Spezies,pch=15+two_peaks_dataFrame[i]$all_Gewebe)
    }}
}


plotBoth<-function(plot_what){
  par(mfrow=c(1,2))
  
  plot_2_peaks_data(plot_what)
  
  x<-plot_what$file
  #x<-identify(x_of_2peaks_data, y_of_2peaks_data,tolerance=1,n=1)
  if(x!=0){
    show_spectrum(x)
  }
  else{
    show_spectrum(1)
  }
  
  
}
show_2peaks_and_spectrum<-function(){
  manipulate(plotBoth(list("Spezies"=Spezies,"Gewebe"=Gewebe,"file"=File,"mouse"=mouse,"Laser"=Laser,"Fall"=Fall)),
             Spezies=picker("all","1","2"),
             Gewebe=picker("all","1","2","3","4","5","6","7","8","9","10"),
             Laser=picker("all","1","2"),
             Fall=slider(min=0,max=75,step=1,initial=0),
             File=slider(min=0,max=measurements_quantity,step=1,initial=0),
             mouse=picker("update","update2")
  )
}
show_plotly<-function(){
  p<-plot_ly(two_peaks_dataFrame,x=~all_index_biggest,y=~all_index_second_biggest,
             text=paste("Spezies",two_peaks_dataFrame$all_Spezies,
                        "\nGewebe",two_peaks_dataFrame$all_Gewebe,
                        "\nfile",two_peaks_dataFrame$all_File,
                        "\nFall",two_peaks_dataFrame$all_Fall,
                        "\nLaser",two_peaks_dataFrame$all_Laser
             ),
             type='scatter',
             mode="markers",
             color=~all_Gewebe,
             symbol=~all_Spezies
  )
  return(p)
}
show_plotly()

show_spectrum_groups<-function(Laser,Spezies,Gewebe){
  allSpectra<-list()
  count<-1
  for(m in measurements){
    if(m$Laser==Laser & m$Gewebe==Gewebe & m$Spezies==Spezies){
      df=m$data
      p<-plot_ly(df,x=~xdata,y=~ydata)%>%add_lines(name=m$file)
      allSpectra[[count]]<-p
      count<-count+1
    }
  }
  subplot(allSpectra,nrows = length(allSpectra))
}

show_spectrum(28)
show_spectrum_groups(Laser=1,Spezies=2,Gewebe=4)

learning_data_frame<-0
for(m in measurements){
  
  
}

