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
  data<-measurements[[x]]$data
  p<-plot_ly(data,x=~xdata,y=~ydata,
             mode="lines",type="scatter")%>%
    layout(title = paste("File:",measurements[[x]]$file,", Spezies:",measurements[[x]]$Spezies,", Gewebe:",measurements[[x]]$Gewebe,", Laser:",measurements[[x]]$Laser))
  return(p)
}

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
  rows=1
  if (length(allSpectra)>8){
    rows=floor(length(allSpectra)/4)
  }
  subplot(allSpectra,nrows = rows)
}


#data is x and y of measurement
stretch_data<-function(data,desired_length=4000){
  stretched<-rep(0,desired_length)
  for(i in 1:length(data[,1])){
    x=floor(data[i,1])
    y=data[i,2]
    if(x>4000){
      x<-4000
    }
    #TODO sometimes the conditions has lenght >1
    if(stretched[x]==0){
      stretched[x]<-y
    }
  }
  
  #fill gaps in y data
  last_y_value<-0
  for(i in 1:4000){
    
    if(stretched[i]==0){
      stretched[i]=last_y_value
    }
    else{
      last_y_value=stretched[i]
    }
  }
  #plot(1:4000,stretched)
  return(stretched)
}
#strech the data so that it has a lenght of 4000
show_stretched_vs_original<-function(xx){
  dat<-measurements[[xx]]$data
  stretch<-data.frame("xdat"=1:4000,"ydat"=stretch_data(dat))
  p<-plot_ly(dat,x=~xdata,y=~ydata,name="original",mode="lines",type="scatter")%>%
    add_trace(x=stretch$xdat,y=stretch$ydat,
              mode="lines",type="scatter",name="stretched")
  return(p)
}

load_learning_data_frame<-function(){
  #TODO export learnin data frame
  #TODO dont process if exported learnign data frame file exists
  learning_data_frame<-list()
  for(i in 1:measurements_quantity){
    data<-measurements[[i]]$data
    learning_data_frame[[i]]<-c(stretch_data(data))
  }
  
  names(learning_data_frame)<-1:202
  learning_data_frame<-data.frame(learning_data_frame)
  return(learning_data_frame)
}
old_learning_data_frame<-load_learning_data_frame()

#write.table(learning_data_frame,"learning_data_frame.txt",sep="\t")

#show_stretched_vs_original(120)
#show_spectrum(3)
#show_spectrum_groups(Laser=1,Spezies=1,Gewebe=2)
library(ggfortify)
pca<-prcomp(old_learning_data_frame)
#biplot(pca)
autoplot(pca,loadings=T)
