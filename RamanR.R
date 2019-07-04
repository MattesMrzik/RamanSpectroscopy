setwd(
  "C:\\Users\\mBrain\\luckyCloud\\Seafile\\Meine_Bibliothek\\Uni\\Studentische_Hilfskraft_KI_Medizin\\Raman"
)
library(plotly)
library(ggfortify)

Legende <- read.delim("Legende_neu.txt", sep = c("\t", "\n"))


#reading data
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
#given x, will plot the spectrum of the measurements
show_spectrum<-function(x){
  data<-measurements[[x]]$data
  p<-plot_ly(data,x=~xdata,y=~ydata,
             mode="lines",type="scatter")%>%
    layout(title = paste("File:",measurements[[x]]$file,", Spezies:",measurements[[x]]$Spezies,", Gewebe:",measurements[[x]]$Gewebe,", Laser:",measurements[[x]]$Laser))
  return(p)
}

#show all spectrums of a certain group
#uses masurements for plotting
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

#stretching data, so that is contains 4000 points
#data is x and y of measurement
stretch_data<-function(data,desired_length=4000){
  stretched<-rep(0,desired_length)
  maxx=max(data[,2])
  for(i in 1:length(data[,1])){
    x=floor(data[i,1])
    y=data[i,2]/maxx
    if(x>4000 | x<0){
      next
    }
    if(stretched[x]==0){
      stretched[x]<-y
    }
  }
  #fill gaps in y data
  last_y_value<-0
  first_non_zero_value<-0
  for (i in 1:4000){
    if(stretched[i]!=0){
      first_non_zero_value<-stretched[i]
      break
    }
  }
  for(i in 1:4000){
    if(stretched[i]==0){
      if(last_y_value==0){
        stretched[i]<-first_non_zero_value
      }
      else{
        stretched[i]=last_y_value
      }
    }
    else{
      last_y_value=stretched[i]
    }
  }
  #plot(1:4000,stretched)
  return(stretched)
}

#shows the stretched data vs original
#input is integer
#measurements is used for plotting
show_stretched_vs_original<-function(xx){
  dat<-measurements[[xx]]$data
  stretch<-data.frame("xdat"=1:4000,"ydat"=stretch_data(dat)*max(dat[,2]))
  p<-plot_ly(dat,x=~xdata,y=~ydata,name="original",mode="lines",type="scatter")%>%
    add_trace(x=stretch$xdat,y=stretch$ydat,
              mode="lines",type="scatter",name="stretched")
  return(p)
}

#creates a dataframe with all relevant information
load_learning_data_frame<-function(skipGewebe,skipSpezies,skipFile){
  learning_data_frame<-data.frame()
  gewebe<-list("Muskel","Sehne","Haut","Gehirn","Niere","Meniskus","knorpel","Faszie","Nerv","Gefäß")
  for(i in 1:measurements_quantity){
    m<-measurements[[i]]
    data<-m$data
    #skipping files
    if(measurements[[i]]$Spezies %in% skipSpezies | measurements[[i]]$Gewebe %in% skipGewebe | measurements[[i]]$file %in% skipFile){
      next
    }
    #adding first row to dataframe
    firstRow<-list()
    if(length(learning_data_frame)==0){
      firstRow<-append(firstRow,m$file)
      firstRow<-append(firstRow,m$Spezies)
      firstRow<-append(firstRow,gewebe[m$Gewebe])
      firstRow<-append(firstRow,m$Gewebe)
      firstRow<-append(firstRow,m$Laser)
      firstRow<-append(firstRow,paste(gewebe[m$Gewebe],as.character(m$Laser)))
      firstRow<-append(firstRow,m$Fall)
      addingStretchedDataToFirstRow<-stretch_data(data)
      for(i in 1:4000){
        firstRow<-append(firstRow,addingStretchedDataToFirstRow[i])
      }
      learning_data_frame<-data.frame(firstRow)
      names(learning_data_frame)<-c("file","Spezies","Gewebe","GewebeNum","Laser","GewebeLaser","Fall",1:4000)
    }
    #adding every row but the first
    else{
      newRow=data.frame(c(m$file,m$Spezies,gewebe[m$Gewebe],m$Gewebe,m$Laser,paste(gewebe[m$Gewebe],as.character(m$Laser)),m$Fall,stretch_data(data)))
      names(newRow)<-c("file","Spezies","Gewebe","GewebeNum","Laser","GewebeLaser","Fall",1:4000)
      learning_data_frame<-rbind(learning_data_frame,newRow)
    }
    print(paste("creating learning data frame, current size: ",nrow(learning_data_frame)))
  }
  return(learning_data_frame)
}

learning_data_frame<-load_learning_data_frame(skipSpezies=c(2),skipGewebe = c(4,5,7,8,10),skipFile=c())
View(learning_data_frame)

#show_stretched_vs_original(64)

#show_spectrum(1)

#show_spectrum_groups(Laser=1,Spezies=1,Gewebe=2)

#plot(1:4000,learning_data_frame[1,][7:4006])

pca<-prcomp(learning_data_frame[200:3800][learning_data_frame["Laser"]==1,])
autoplot(pca,loadings=F,data=learning_data_frame[1:172,][learning_data_frame["Laser"]==1,],colour="Gewebe",
         label=T,
         loadings.label = F,#eigen_vectors
         frame=T,frame.type = 'norm'
         )

pca<-prcomp(learning_data_frame[200:3800])
autoplot(pca,loadings=F,data=learning_data_frame[1:172,],colour="GewebeLaser",
         label=T,
         loadings.label = F,#eigen_vectors
         frame=T,frame.type = 'norm'
)

summary(pca)

#svm
library(caret)

#missing data on whether tissue was canser or not
heart_df <- read.csv("heart_tidy.csv", sep = ',', header = FALSE)
View(heart_df)
set.seed(3033)
intrain <- createDataPartition(y = heart_df$V14, p= 0.7, list = FALSE)
intrain<- createDataPartition(y = learning_data_frame$GewebeNum, p= 0.7, list = FALSE)

training <- heart_df[intrain,]
training<- learning_data_frame[intrain,][!names(learning_data_frame)%in% c("Gewebe","file","Fall","GewebeLaser")]

testing <- heart_df[-intrain,]
testing<- learning_data_frame[-intrain,][!names(learning_data_frame)%in% c("Gewebe","file","Fall","GewebeLaser")]


#turn spezies gewebe laser into catigorical data
training[["V14"]] = factor(training[["V14"]])
testing[["V14"]] = factor(testing[["V14"]])

training[["Spezies"]] = factor(training[["Spezies"]])
training[["GewebeNum"]] = factor(training[["GewebeNum"]])
training[["Laser"]] = factor(training[["Laser"]])

testing[["Spezies"]] = factor(testing[["Spezies"]])
testing[["GewebeNum"]] = factor(testing[["GewebeNum"]])
testing[["Laser"]] = factor(testing[["Laser"]])

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
#V14 or GewebeNum
svm_Linear <- train(V14 ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneGrid = grid,
                    tuneLength = 10)
svm_Linear         

test_pred <- predict(svm_Linear, newdata = testing)
test_pred

confusionMatrix(test_pred, testing$V14)

plot(svm_Linear)

ma <- function(x, n = 10){
  result<-0
  for(i in 1:length(x)-n){
    result[i]<-median(x[i+n])
  }
  return(result)
}

cbind(testing,test_pred)
