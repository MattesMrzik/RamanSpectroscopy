setwd(
  "C:\\Users\\mBrain\\luckyCloud\\Seafile\\Meine_Bibliothek\\Uni\\Studentische_Hilfskraft_KI_Medizin\\Raman"
)
library(manipulate)
Legende <- read.delim("Legende.txt", sep = c("\t", "\n"))
#View(Legende)
#print(length(Legende[,1]))

Measurements <- list(rep(0, 183))
onlyXfiles<-10
for (file in list.files()) {
  if (suppressWarnings(is.na(as.numeric(substring(file, 1, 1))))) {
    next
  }
  fileData <- read.table(file, sep = "\t", header = F)
  index <- as.numeric(substring(file, 1, regexpr(" ", file)[1] - 1))
  if(index>onlyXfiles){
    next
  }
  if(fileData[1,1]==fileData[2,1]){
    fileData<-fileData[,2:3]
  }
  Measurements[[index]] <- list("data"=fileData,"file"=index,"fall"=Legende[index,2],"Laser"=Legende[index,3],"Spezies"=Legende[index,4],"Gewebe"=Legende[index,5],"dateiname"=Legende[index,6] )
}
Measurements[[10]]$Laser

plotData<-function(x){
  plot(Measurements[[x]]$data[,1],Measurements[[x]]$data[,2],type="l",ylab="",xlab=paste(Legende[x,6]))
}

manipulate(plotData(x),x=slider(1,182))

plot_classes_biggest_2_peeks<-function(){
  plot(NA,NA,main="classes",xlim=c(0,4000),ylim=c(0,4000))
  plot_points<-0
  for(m in Measurements){
    biggest<-0
    index_biggest<-0
    second_biggest<-0
    index_second_biggest<-0
    count<-1
    for(d in m$data[,2]){
      if (d>biggest){
        biggest<-d
        index_biggest<-m$data[count,1]
      }
      count<-count+1
    }
    count<-1
    for(d in m$data[,2]){
      if (d>second_biggest & abs(m$data[count,1]-index_biggest)>200){
        second_biggest<-d
        index_second_biggest<-m$data[count,1]
      }
      count<-count+1
    }
    print(paste("index biggest and seccond biggest of",m$file,index_biggest,index_second_biggest))
    
    plot_points<-append(plot_points,list("index_biggest"=as.numeric(index_biggest),
                                         "index_second_biggset"=as.numeric(index_second_biggest),
                                         "Laser"=m$Laser,
                                         "Fall"=m$Fall,
                                         "Spezies"=m$Spezies,
                                         "Gewebe"=m$Gewebe
    ))
  }
  points("plotting Data",plot_points$index_biggest,plot_points$index_second_biggset)
  print(unlist(plot_points$index_biggest))
}
plot_classes_biggest_2_peeks()

