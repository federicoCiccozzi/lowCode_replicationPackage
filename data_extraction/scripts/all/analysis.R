library(tidyr)
library(dplyr)
library(plyr)
library(reshape)
library(ggplot2)
library(stringr)

setwd("/Users/federico/Dropbox/SMS_low_code/data_extraction/analysis")

# Read data

data <- read.delim("./input/data_extraction_form_all.txt", header = TRUE, quote = "\"", dec = ".", fill = TRUE, na.strings = c(""," "))
studies <- c(49)

# Barplot function

plot <- function(var, fileName, title, labels, width, height, leftMargin) {

  filePath <- paste("./output/vertical_analysis_all/", fileName, "_CUT.pdf", sep="")
  pdf(filePath, width=width, height=height)
  
  par(mar=c(3, leftMargin+7, 2, 1))
  par(mfrow=c(1, 1))
  par(las=1)
  var <- as.factor(var)
  levels(var)<-str_replace_all(levels(var), "[_]", " ")
  
  dataToPlot <- table(var)
  dataToPlot <- dataToPlot[order(dataToPlot, decreasing=FALSE)]
  plot <- barplot(dataToPlot, col="#69b3a2", cex.main=1.8, xlim=c(0, 30), cex=1.5, cex.names=2, las=1, horiz=TRUE)
  text(x=22, y = plot, label = dataToPlot, cex = 1.5, col = "black", align = "right")
  
  dev.off()
}  

# Bubble chart function

plotBubbles <- function(var1, var2, name1, name2) {
  
  n1<-str_replace_all(name1, " ", "_")
  n2<-str_replace_all(name2, " ", "_")
  fileName <- paste(n1, "_", n2)
  fileName <- str_replace_all(fileName, " ", "")
  filePath <- paste("./output/horizontal_analysis_all/", fileName, ".pdf", sep="")
  pdf(filePath, width=10, height=10)
  par(mar=c(5, 5, 5, 5))
  par(mfrow=c(1, 1))
  par(las=1)
  
  currentData <- merge(var1, var2, by="ID", all = T)
  var1Name <- str_replace_all(names(currentData)[2], "[_]", " ")
  var2Name <- str_replace_all(names(currentData)[3], "[_]", " ")
  counts <- count(currentData, c(var1Name, var2Name))
  
  levels(counts[,2])<-str_replace_all(levels(counts[,2]), "[_]", " ")
  levels(counts[,1])<-str_replace_all(levels(counts[,1]), "[_]", " ")
  
  plot <- ggplot(counts, aes(counts[,2], counts[,1], color=freq)) + geom_point(aes(size = freq)) +
    scale_color_gradient2(low="yellow", high="green", name="freq") +
    scale_size_continuous(range=c(0, 30)) + geom_text(aes(label = freq), color="black") +
    theme(legend.position = "none") +
    labs(x=name2, y=name1) +
    theme(axis.text.x = element_text(angle = 90))#+  ggtitle(paste(name1, " -- ", name2))
  
  print(plot)
  
  dev.off()
}

plotBubbles_wide <- function(var1, var2, name1, name2) {
  
  n1<-str_replace_all(name1, " ", "_")
  n2<-str_replace_all(name2, " ", "_")
  fileName <- paste(n1, "_", n2)
  fileName <- str_replace_all(fileName, " ", "")
  filePath <- paste("./output/horizontal_analysis/", fileName, "_CUT.pdf", sep="")
  pdf(filePath, width=8.5, height=7)
  par(mar=c(5, 5, 5, 5))
  par(mfrow=c(1, 1))
  par(las=1)
  
  currentData <- merge(var1, var2, by="ID", all = T)
  var1Name <- str_replace_all(names(currentData)[2], "[_]", " ")
  var2Name <- str_replace_all(names(currentData)[3], "[_]", " ")
  counts <- count(currentData, c(var1Name, var2Name))
  
  levels(counts[,2])<-str_replace_all(levels(counts[,2]), "[_]", " ")
  levels(counts[,1])<-str_replace_all(levels(counts[,1]), "[_]", " ")
  
  plot <- ggplot(counts, aes(counts[,2], counts[,1], color=freq)) + geom_point(aes(size = freq)) +
    scale_color_gradient2(low="yellow", high="green", name="freq") +
    scale_size_continuous(range=c(0, 30)) + geom_text(aes(label = freq), color="black") +
    theme(legend.position = "none") +
    labs(x=name2, y=name1) #+  ggtitle(paste(name1, " -- ", name2))
  
  print(plot)
  
  dev.off()
}

### Foundations
labels <- c("Declarative_programming", "GUI", "MDE", "Component_based", "Rapid_app_development", "Code_generation", "Visual_programming", "Databases", "PaaS", "DSL", "Web_technologies", "DevOps", "Cloud", "API")
vars5 <- data %>% 
  gather("var5", "value", labels, na.rm = TRUE, convert = TRUE) %>% 
  #dplyr::filter(grepl("x",value)) %>% dplyr::rename(ID = X) %>% 
  select(ID, var5) 
vars5$var5 <- as.factor(vars5$var5)
plot(vars5$var5, "foundations", "Foundations", labels, 10, 7, 13)

### Domains
labels <- c("Education", "Web", "Mobile", "Business_process", "Enterprise_services", "IoT", "Databases", "Healthcare")
vars6 <- data %>% 
  gather("var6", "value", labels, na.rm = TRUE, convert = TRUE) %>% 
  #dplyr::filter(grepl("x",value)) %>% dplyr::rename(ID = X) %>% 
  select(ID, var6) 
vars6$var6 <- as.factor(vars6$var6)
plot(vars6$var6, "domains", "Domains", labels, 10, 7, 13)

### Tools
labels <- c("Appian", "Kony", "Mendix", "MS_PowerApps", "OutSystems", "Salesforce", "Aurea", "RESTsec", "Node_RED", "AtmosphericIoT", "Simplifier", "Siemens_MindSphere", "PTCThingWork", "IDM_Cloud", "MS_Azure", "ADAMOS", "INTELLIT", "Zoho_Creator", "Google_App_Maker", "App_Cloud", "ZappDev", "Nintex_Workflow_Cloud", "MIT_App_Inventor", "Sysdev_Kalipso", "KissFlow", "HiCuMES", "Camunda", "AutoML", "AgilePoint", "Sagitec_S3", "Gherkin2OSA", "Lightening", "Temenos_Quantum", "Pega", "ColdFusion_Builder", "Cyclr", "Genio", "TimeSeries", "Axonivy", "Lansa")
vars7 <- data %>% 
  gather("var7", "value", labels, na.rm = TRUE, convert = TRUE) %>% 
  #dplyr::filter(grepl("x",value)) %>% dplyr::rename(ID = X) %>% 
  select(ID, var7) 
vars7$var7 <- as.factor(vars7$var7)
plot(vars7$var7, "tools", "Tools", labels, 10, 7, 13)

### Benefits
labels <- c("GUI_based_development", "Digitalization_BP", "Abstraction", "Automation", "Flexibility", "Customisability", "Usability", "Maintainability", "Interoperability", "Privacy_or_security", "Improved_RoI",	"Reusability", "Openness")
vars8 <- data %>% 
  gather("var8", "value", labels, na.rm = TRUE, convert = TRUE) %>% 
  #dplyr::filter(grepl("x",value)) %>% dplyr::rename(ID = X) %>% 
  select(ID, var8) 
vars8$var8 <- as.factor(vars8$var8)
plot(vars8$var8, "benefits", "Benefits", labels, 10, 7, 13)

##Bubble charts
plotBubbles(vars5, vars6, "Foundations", "Domains")
plotBubbles(vars5, vars7, "Foundations", "Tools")
plotBubbles(vars5, vars8, "Foundations", "Benefits")
plotBubbles(vars6, vars7, "Domains", "Tools")
plotBubbles(vars6, vars8, "Domains", "Benefits")
plotBubbles(vars7, vars8, "Tools", "Benefits")
