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
  #only read the files which begin with numeric value
  if (suppressWarnings(is.na(as.numeric(substring(file, 1, 1))))) {
    next
  }
  fileData <- read.table(file, sep = "\t", header = F)
  #index of file
  index <- as.numeric(substring(file, 1, regexpr(" ", file)[1] - 1))
  
  #if 3 collumns in file, only save the last 2 collumns 
  if(fileData[1,1]==fileData[2,1]){
    fileData<-fileData[,2:3]
    fileData<-fileData[1:length(fileData[,1])/2,]
  }
  xdata<-fileData[,1]
  ydata<-fileData[,2]
  data<-data.frame(xdata,ydata)
  
  measurements[[index]] <- list("data"=data,
                                "index"=index,
                                "case"=Legende[index,2],
                                "laser"=Legende[index,3],
                                "species"=Legende[index,4],
                                "tissue"=Legende[index,5],
                                "note"=Legende[index,6] 
  )
}
#given x, will plot the spectrum of the measurements
show_spectrum<-function(x){
  data<-measurements[[x]]$data
  p<-plot_ly(data,x=~xdata,y=~ydata,
             mode="lines",type="scatter")%>%
    layout(title = paste("File:",measurements[[x]]$index,", species:",measurements[[x]]$species,",tissue:",measurements[[x]]$tissue,", laser:",measurements[[x]]$laser))
  return(p)
}

#show all spectrums of a certain group
#uses masurements for plotting
show_spectrum_groups<-function(laser,species,tissue){
  allSpectra<-list()
  count<-1
  for(m in measurements){
    if(m$laser==laser & m$tissue==tissue & m$species==species){
      df=m$data
      p<-plot_ly(df,x=~xdata,y=~ydata)%>%add_lines(name=m$index)
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
#scaling specturm to take values in [0,1], i.e. dividing each point by max value of spectrum
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
  #fill gaps in y data i.e. set value of 0 value points to neighbouring value
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
  return(stretched)
}

#shows the stretched data vs original
#input is integer (file index)
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
load_learning_data_frame<-function(skip_tissue,skip_species,skip_index){
  learning_data_frame<-data.frame()
  tissue_names<-list("Muskel","Sehne","Haut","Gehirn","Niere","Meniskus","knorpel","Faszie","Nerv","Gefäß")
  for(i in 1:measurements_quantity){
    m<-measurements[[i]]
    data<-m$data
    #skipping files
    if(measurements[[i]]$species %in% skip_species | measurements[[i]]$tissue %in% skip_tissue | measurements[[i]]$index %in% skip_index){
      next
    }
    #adding first row to dataframe
    firstRow<-list()
    if(length(learning_data_frame)==0){
      firstRow<-append(firstRow,m$index)
      firstRow<-append(firstRow,m$species)
      firstRow<-append(firstRow,tissue_names[m$tissue])
      firstRow<-append(firstRow,m$tissue)
      firstRow<-append(firstRow,m$laser)
      firstRow<-append(firstRow,paste(tissue_names[m$tissue],as.character(m$laser)))
      firstRow<-append(firstRow,m$case)
      addingStretchedDataToFirstRow<-stretch_data(data)
      for(i in 1:4000){
        firstRow<-append(firstRow,addingStretchedDataToFirstRow[i])
      }
      learning_data_frame<-data.frame(firstRow)
      names(learning_data_frame)<-c("index","species","tissue","tissue_num","laser","tissue_laser","case",1:4000)
    }
    #adding every row but the first
    else{
      newRow=data.frame(c(m$index,m$species,tissue_names[m$tissue],m$tissue,m$laser,paste(tissue_names[m$tissue],as.character(m$laser)),m$case,stretch_data(data)))
      names(newRow)<-c("index","species","tissue","tissue_num","laser","tissue_laser","case",1:4000)
      learning_data_frame<-rbind(learning_data_frame,newRow)
    }
    print(paste("creating learning data frame, current size: ",nrow(learning_data_frame)))
  }
  return(learning_data_frame)
}

learning_data_frame<-load_learning_data_frame(skip_species=c(2),skip_tissue = c(4,5,7,8,10),skip_index=c())
View(learning_data_frame)

#show_stretched_vs_original(64)

#show_spectrum(1)

#show_spectrum_groups(laser=1,species=1,tissue=2)

#plot(1:4000,learning_data_frame[1,][7:4006])

pca<-prcomp(learning_data_frame[200:3800][learning_data_frame["laser"]==1,])
autoplot(pca,loadings=F,data=learning_data_frame[1:172,][learning_data_frame["laser"]==1,],colour="tissue",
         label=T,
         loadings.label = F,#eigen_vectors
         frame=T,frame.type = 'norm'
         )

pca<-prcomp(learning_data_frame[200:3800])
autoplot(pca,loadings=F,data=learning_data_frame[1:172,],colour="tissue_laser",
         label=T,
         loadings.label = F,#eigen_vectors
         frame=T,frame.type = 'norm'
)

summary(pca)

#svm
library(caret)

#set.seed(3033)
intrain<- createDataPartition(y = learning_data_frame$tissue_num, p= 0.7, list = FALSE)
training<- learning_data_frame[intrain,][!names(learning_data_frame)%in% c("species","tissue","index","case","tissue_laser")]
testing<- learning_data_frame[-intrain,][!names(learning_data_frame)%in% c("species","tissue","index","case","tissue_laser")]


#turn spezies gewebe and laser into catigorical data
training[["tissue_num"]] = factor(training[["tissue_num"]])
training[["laser"]] = factor(training[["laser"]])

testing[["tissue_num"]] = factor(testing[["tissue_num"]])
testing[["laser"]] = factor(testing[["laser"]])


library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)
#method = "repeatedcv"
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
#set.seed(3233)
svm_Linear <- train(tissue_num ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 7)
stopCluster(cl)#maybe tuning paramter not constant at c=1
svm_Linear         

#predict one specific measument
testOne=learning_data_frame[98,][!names(learning_data_frame)%in% c("species","tissue","index","case","tissue_num")]
testOne[["tissue_num"]]=factor(testOne[["tissue_num"]])
testOne[["laser"]]=factor(testOne[["laser"]])
test_pred <- predict(svm_Linear, newdata = testOne)

#predict the testing group
test_pred <- predict(svm_Linear, newdata = testing)
cbind(testing$tissue_num,test_pred)

confusionMatrix(test_pred, testing$tissue_num)


