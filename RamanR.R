# classification of spectral Raman data

# Author: Mattes Mrzik
# E-Mail: mattes@mrzik.de
# Last modified on: 29.08.19

# License

# Copyright <2023> <Mattes Mrzik>

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the “Software”), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is furnished
# to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
# IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

###################################################################

library(plotly) # for plotting
library(ggfortify) # for principal component analysis
library(caret) # classification library
library(doParallel) # to train on multiple cores

###################################################################

# set working directory

# should contain spectral data dir
# each measurement in a separate file, named beginning with numerical value -> index
# containing 2 columns, the first containing the x values and the second the y value
# if file contains 3 columns, then the x and y data should be in the second and
# third column

# should contain "Legend" tsv file
# should have a header
# should contain file index (numerical), case (numerical), laser = 1 or 2,
# species (numerical), tissue(numerical), additional info, in that order

setwd(
   "/home/mattes/Seafile/Meine_Bibliothek/Uni/SHK_KI_Medizin/Raman"
)

###################################################################

# reading data
# how many measurements are available
measurements_quantity <- 202
name_of_legend_file <- "Legende_neu.txt"

legend <- read.delim(name_of_legend_file, sep = c("\t", "\n"))

measurements <- list(rep(0, measurements_quantity))
for (file in list.files()) {
  # only read the files which begin with numeric value
  if (suppressWarnings(is.na(as.numeric(substring(file, 1, 1))))) {
    next
  }
  fileData <- read.table(file, sep = "\t", header = F)
  # index of file
  index <- as.numeric(substring(file, 1, regexpr(" ", file)[1] - 1))

  # if 3 columns are present in file, only save the last 2 columns and only
  # half the data points in the file, this is due to an artifact in data collection
  # bc in that case the file contains 2 spectra
  if (length(fileData[1,]) == 3) {
    fileData <- fileData[,2:3]
    fileData <- fileData[1:length(fileData[,1])/2,]
  }
  xdata <- fileData[,1]
  ydata <- fileData[,2]
  data <- data.frame(xdata,ydata)

  measurements[[index]] <- list("data"=data,
                                "index"=index,
                                "case"=legend[index,2],
                                "laser"=legend[index,3],
                                "species"=legend[index,4],
                                "tissue"=legend[index,5],
                                "note"=legend[index,6]
  )
}

###################################################################

# given an index, will plot the spectrum of the measurement
show_spectrum <- function(x) {
  data <- measurements[[x]]$data
  p <- plot_ly(data, x=~xdata, y=~ydata,
             mode = "lines", type="scatter")%>%
    layout(title = paste("File:", measurements[[x]]$index,
                         ", species:", measurements[[x]]$species,
                         ",tissue:",measurements[[x]]$tissue,
                         ", laser:",measurements[[x]]$laser))
  return(p)
}
# use:
# show_spectrum(1)

###################################################################

# show all spectra of a certain group
# laser, species, and tissue should be integers
show_spectrum_groups <- function(laser, species,tissue) {
  allSpectra <- list()
  count<-1
  for (m in measurements) {
    if (m$laser == laser & m$tissue == tissue & m$species == species) {
      df = m$data
      p <- plot_ly(df, x=~xdata, y=~ydata) %>% add_lines(name = m$index)
      allSpectra[[count]] <- p
      count <- count + 1
    }
  }
  rows = 1
  if (length(allSpectra) > 8) {
    rows = floor(length(allSpectra)/4)
  }
  subplot(allSpectra,nrows = rows)
}
# use:
# show_spectrum_groups(laser=1,species=1,tissue=2)

###################################################################

# stretching data, so that is contains 4000 points, necessary for training model
# scaling spectrum to be in [0,1], i.e. dividing each point by max value
# data is x and y of measurement
stretch_data <- function(data, desired_length = 4000) {
  stretched <- rep(0, desired_length)
  maxx=max(data[,2])
  for (i in 1:length(data[,1])) {
    x = floor(data[i,1])
    y = data[i,2]/maxx
    if (x > desired_length | x < 0) {
      next
    }
    if (stretched[x]==0) {
      stretched[x] <- y
    }
  }
  # fill gaps in y data i.e. set value of 0 value points to neighboring non zero value
  last_y_value <- 0
  first_non_zero_value <- 0
  for (i in 1:desired_length) {
    if (stretched[i] != 0) {
      first_non_zero_value <- stretched[i]
      break
    }
  }
  for (i in 1:desired_length) {
    if (stretched[i] == 0) {
      if (last_y_value == 0) {
        stretched[i] <- first_non_zero_value
      }
      else {
        stretched[i] = last_y_value
      }
    }
    else {
      last_y_value = stretched[i]
    }
  }
  return(stretched)
}

###################################################################

# shows the stretched data vs original
# input xx is integer (file index)
show_stretched_vs_original <- function(xx, desired_length = 4000) {
  dat <- measurements[[xx]]$data
  stretch <- data.frame("xdat"=1:desired_length, "ydat" = stretch_data(dat)*max(dat[,2]))
  p <- plot_ly(dat, x=~xdata, y=~ydata, name="original", mode="lines", type="scatter")%>%
    add_trace(x = stretch$xdat, y = stretch$ydat,
              mode = "lines", type = "scatter", name = "stretched")
  return(p)
}
# use:
# show_stretched_vs_original(1)

###################################################################

# creates a data frame with all relevant information
# skip... should be integers, these are then not included in learning data frame
load_learning_data_frame <- function(skip_tissue, skip_species,
                                     skip_index, skip_laser,
                                     desired_length = 4000) {
  learning_data_frame <- data.frame()
  # insert names for tissues
  # order should correspond to indexing of tissues in legend
  tissue_names <- list("Muskel", "Sehne", "Haut", "Gehirn", "Niere",
                       "Meniskus", "knorpel", "Faszie", "Nerv", "Gef??")
  for (i in 1:measurements_quantity) {
    m <- measurements[[i]]
    data <- m$data
    # skipping measurements
    if (measurements[[i]]$species %in% skip_species |
        measurements[[i]]$tissue %in% skip_tissue |
        measurements[[i]]$index %in% skip_index |
        measurements[[i]]$laser %in% skip_laser) {
      next
    }
    # adding first row to data frame
    firstRow <- list()
    if(length(learning_data_frame) == 0) {
      firstRow <- append(firstRow, m$index)
      firstRow <- append(firstRow, m$species)
      firstRow <- append(firstRow, tissue_names[m$tissue])
      firstRow <- append(firstRow, m$tissue)
      firstRow <- append(firstRow, m$laser)
      firstRow <- append(firstRow, paste(tissue_names[m$tissue],as.character(m$laser)))
      firstRow <- append(firstRow, m$case)
      addingStretchedDataToFirstRow<-stretch_data(data)
      for (i in 1:desired_length) {
        firstRow<-append(firstRow,addingStretchedDataToFirstRow[i])
      }
      learning_data_frame <- data.frame(firstRow)
      names(learning_data_frame) <- c("index", "species", "tissue", "tissue_num",
                                      "laser", "tissue_laser", "case", 1:desired_length)
    }
    # adding every row but the first
    else{
      newRow = data.frame(c(m$index, m$species, tissue_names[m$tissue], m$tissue,
                            m$laser, paste(tissue_names[m$tissue], as.character(m$laser)),
                            m$case, stretch_data(data)))
      names(newRow) <- c("index", "species", "tissue", "tissue_num",
                         "laser", "tissue_laser", "case", 1:desired_length)
      learning_data_frame <- rbind(learning_data_frame,newRow)
    }
    print(paste("creating learning data frame, current size: ", nrow(learning_data_frame)))
  }
  return(learning_data_frame)
}
# skipping measurements of mice and certain tissues
# -> classification algo achieves better results
learning_data_frame <- load_learning_data_frame(skip_species=c(2),
                                                skip_tissue = c(4,5,7,8,10),
                                                skip_index=c(), skip_laser = c())
# View(learning_data_frame)

###################################################################

# principal component analysis
# labels of points are the rows in learning data frame
pca <- prcomp(learning_data_frame[200:3800])
autoplot(pca, loadings = F, data = learning_data_frame, colour = "tissue_laser",
         label = T,
         loadings.label = F, # Eigen vectors
         frame = T,frame.type = 'norm'
)
# principal component analysis of only the laser 1 (or 2)
pca_laser <- 1

pca <- prcomp(learning_data_frame[200:3800][learning_data_frame["laser"]==pca_laser,])
autoplot(pca, loadings = F,
         data = learning_data_frame[learning_data_frame["laser"] == pca_laser,],
         colour="tissue",
         label = T,
         loadings.label = F, # Eigen vectors
         frame = T,
         frame.type = 't'
)

# for additional info
# summary(pca)

###################################################################

# classification and prediction with support vector machine
set.seed(3033)
# split data in training and testing partition
intrain <- createDataPartition(y = learning_data_frame$tissue_num, p= 0.7, list = FALSE)
training <- learning_data_frame[intrain,][!names(learning_data_frame)
                                          %in% c("species", "tissue", "index",
                                                 "case", "tissue_laser")]
testing <- learning_data_frame[-intrain,][!names(learning_data_frame)
                                          %in% c("species", "tissue", "index",
                                                 "case", "tissue_laser")]

# turn species, tissue and laser into categorical data
training[["tissue_num"]] = factor(training[["tissue_num"]])
training[["laser"]] = factor(training[["laser"]])

testing[["tissue_num"]] = factor(testing[["tissue_num"]])
testing[["laser"]] = factor(testing[["laser"]])

# insert integer -> how many cores should be used for training
cl <- makeCluster(4)
registerDoParallel(cl)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

svm_Linear <- train(tissue_num ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 7)
stopCluster(cl)
svm_Linear
# Accuracy   Kappa
# 0.9330697  0.9059581

# predict the testing group
test_pred <- predict(svm_Linear, newdata = testing)
cbind(testing$tissue_num,test_pred)
print("accuracy of testing:")
sum(rep(1,nrow(testing))[testing$tissue_num == test_pred])/nrow(testing)

# predict all measurements
for (i in 1:172) {
  test_one_row <- i # integer corresponding to row of learning data frame
  testOne = learning_data_frame[test_one_row,][!names(learning_data_frame)
                                               %in% c("species", "tissue", "index",
                                                      "case", "tissue_laser")]
  testOne[["tissue_num"]] = factor(testOne[["tissue_num"]])
  testOne[["laser"]] = factor(testOne[["laser"]])
  test_pred1 <- predict(svm_Linear, newdata = testOne)
  cat("predict row", test_one_row,":", levels(test_pred1)[test_pred1],
      ", true value:",as.numeric(as.character(unlist(testOne["tissue_num"]))), "\n")
}

confusionMatrix(test_pred, testing$tissue_num)

# Confusion Matrix and Statistics
#
# Reference
# Prediction  1  2  3  6  9
# 1 19  2  0  0  0
# 2  0 11  0  1  0
# 3  2  0  4  1  0
# 6  1  0  0  7  0
# 9  0  1  0  0  2

# Overall Statistics

# Accuracy : 0.8431
# 95% CI : (0.7141, 0.9298)
# No Information Rate : 0.4314
# P-Value [Acc > NIR] : 1.6e-09

# Kappa : 0.7812

# Mcnemar's Test P-Value : NA

# Statistics by Class:

#                      Class: 1 Class: 2 Class: 3 Class: 6 Class: 9
# Sensitivity            0.8636   0.7857  1.00000   0.7778  1.00000
# Specificity            0.9310   0.9730  0.93617   0.9762  0.97959
# Pos Pred Value         0.9048   0.9167  0.57143   0.8750  0.66667
# Neg Pred Value         0.9000   0.9231  1.00000   0.9535  1.00000
# Prevalence             0.4314   0.2745  0.07843   0.1765  0.03922
# Detection Rate         0.3725   0.2157  0.07843   0.1373  0.03922
# Detection Prevalence   0.4118   0.2353  0.13725   0.1569  0.05882
# Balanced Accuracy      0.8973   0.8793  0.96809   0.8770  0.98980
