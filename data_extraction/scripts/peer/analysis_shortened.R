library(tidyr)
library(dplyr)
library(plyr)
library(reshape)
library(ggplot2)
library(stringr)

setwd("/Users/federico/Dropbox/SMS_low_code/data_extraction/analysis")

# Read data

data <- read.delim("./input/data_extraction_form_peer.txt", header = TRUE, quote = "\"", dec = ".", fill = TRUE, na.strings = c(""," "))
studies <- c(26)

# Barplot function

plot <- function(var, fileName, title, labels, width, height, leftMargin) {

  filePath <- paste("./output/vertical_analysis_peer/", fileName, "_CUT.pdf", sep="")
  pdf(filePath, width=width, height=height)
  
  par(mar=c(3, leftMargin+7, 2, 1))
  par(mfrow=c(1, 1))
  par(las=1)
  var <- as.factor(var)
  levels(var)<-str_replace_all(levels(var), "[_]", " ")
  
  dataToPlot <- table(var)
  dataToPlot <- dataToPlot[order(dataToPlot, decreasing=FALSE)]
  plot <- barplot(dataToPlot, col="#69b3a2", cex.main=1.8, xlim=c(0, studies), cex=1.5, cex.names=2, las=1, horiz=TRUE)
  text(x=22, y = plot, label = dataToPlot, cex = 1.5, col = "black", align = "right")
  
  dev.off()
}  

# Bubble chart function

plotBubbles <- function(var1, var2, name1, name2) {
  
  n1<-str_replace_all(name1, " ", "_")
  n2<-str_replace_all(name2, " ", "_")
  fileName <- paste(n1, "_", n2)
  fileName <- str_replace_all(fileName, " ", "")
  filePath <- paste("./output/horizontal_analysis_peer/", fileName, "_FONT.pdf", sep="")
  pdf(filePath, width=6, height=6)
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
  filePath <- paste("./output/horizontal_analysis/", fileName, ".pdf", sep="")
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

### Publication type
labels <- c("Conference", "Workshop", "Journal")
vars0 <- data %>% 
  gather("var0", "value", labels, na.rm = TRUE, convert = TRUE) %>% 
  #dplyr::filter(grepl("x",value)) %>% dplyr::rename(ID = X) %>% 
  select(ID, var0) 
vars0$var0 <- as.factor(vars0$var0)
plot(vars0$var0, "publication_type", "Publication type", labels, 10, 7, 13)


### Research type
labels <- c("Experience_paper",	"Evaluation_research",	"Solution_proposal",	"Survey_paper",	"Opinion_paper", "Validation_research")
  vars1 <- data %>% 
  gather("var1", "value", labels, na.rm = TRUE, convert = TRUE) %>% 
  #dplyr::filter(grepl("x",value)) %>% dplyr::rename(ID = X) %>% 
  select(ID, var1) 
vars1$var1 <- as.factor(vars1$var1)
plot(vars1$var1, "research_type", "Research type", labels, 10, 7, 13)

### Questions type
labels <- c("Method_development",	"Method_analysis",	"Particular_instance",	"Feasibility",	"Characterisation")
vars2 <- data %>% 
  gather("var2", "value", labels, na.rm = TRUE, convert = TRUE) %>% 
  #dplyr::filter(grepl("x",value)) %>% dplyr::rename(ID = X) %>% 
  select(ID, var2) 
vars2$var2 <- as.factor(vars2$var2)
plot(vars2$var2, "questions_type", "Questions type", labels, 10, 7, 13)

### Results type
labels <- c("Specific_solution", "Report", "Answer_or_judgment", "Notation_or_tool", "Procedure_or_technique", "Qualitative_or_descriptive_model")
vars3 <- data %>% 
  gather("var3", "value", labels, na.rm = TRUE, convert = TRUE) %>% 
  #dplyr::filter(grepl("x",value)) %>% dplyr::rename(ID = X) %>% 
  select(ID, var3) 
vars3$var3 <- as.factor(vars3$var3)
plot(vars3$var3, "results_type", "Results type", labels, 10, 7, 13)

### Validation type
labels <- c("Experience", "Evaluation", "Persuasion", "Example", "Analysis", "Blatant_assertion")
vars4 <- data %>% 
  gather("var4", "value", labels, na.rm = TRUE, convert = TRUE) %>% 
  #dplyr::filter(grepl("x",value)) %>% dplyr::rename(ID = X) %>% 
  select(ID, var4) 
vars4$var4 <- as.factor(vars4$var4)
plot(vars4$var4, "validation_type", "Validation type", labels, 10, 7, 13)

### Foundations
labels <- c("Declarative_programming", "GUI", "MDE", "Component_based", "Code_generation", "Visual_programming", "Databases", "PaaS", "DSL", "Web_technologies")
vars5 <- data %>% 
  gather("var5", "value", labels, na.rm = TRUE, convert = TRUE) %>% 
  #dplyr::filter(grepl("x",value)) %>% dplyr::rename(ID = X) %>% 
  select(ID, var5) 
vars5$var5 <- as.factor(vars5$var5)
plot(vars5$var5, "foundations", "Foundations", labels, 10, 7, 13)

### Domains
labels <- c("Education", "Web", "Mobile", "Business_process", "Enterprise_services", "IoT", "Databases", "AI")
vars6 <- data %>% 
  gather("var6", "value", labels, na.rm = TRUE, convert = TRUE) %>% 
  #dplyr::filter(grepl("x",value)) %>% dplyr::rename(ID = X) %>% 
  select(ID, var6) 
vars6$var6 <- as.factor(vars6$var6)
plot(vars6$var6, "domains", "Domains", labels, 10, 7, 13)

### Tools
labels <- c("OutSystems", "Mendix", "MS_PowerApps", "Salesforce", "Google_App_Maker", "Appian", "Zoho_Creator", "RESTsec", "MS_Azure")
vars7 <- data %>% 
  gather("var7", "value", labels, na.rm = TRUE, convert = TRUE) %>% 
  #dplyr::filter(grepl("x",value)) %>% dplyr::rename(ID = X) %>% 
  select(ID, var7) 
vars7$var7 <- as.factor(vars7$var7)
plot(vars7$var7, "tools", "Tools", labels, 10, 7, 13)

### Benefits
labels <- c("GUI_based_development", "Digitalization_BP", "Abstraction", "Automation", "Customisability", "Usability", "Maintainability", "Privacy_or_security", "Improved_RoI")
vars8 <- data %>% 
  gather("var8", "value", labels, na.rm = TRUE, convert = TRUE) %>% 
  #dplyr::filter(grepl("x",value)) %>% dplyr::rename(ID = X) %>% 
  select(ID, var8) 
vars8$var8 <- as.factor(vars8$var8)
plot(vars8$var8, "benefits", "Benefits", labels, 10, 7, 13)


### Bubble charts
plotBubbles(vars1, vars2, "Research type", "Questions type")
plotBubbles(vars1, vars3, "Research type", "Results type")
plotBubbles(vars1, vars4, "Research type", "Validation type")
plotBubbles(vars2, vars3, "Questions type", "Results type")
plotBubbles(vars2, vars4, "Questions type", "Validation type")
plotBubbles(vars3, vars4, "Results type", "Validation type")

plotBubbles(vars5, vars6, "Foundations", "Domains")
plotBubbles(vars5, vars7, "Foundations", "Tools")
plotBubbles(vars5, vars8, "Foundations", "Benefits")
plotBubbles(vars6, vars7, "Domains", "Tools")
plotBubbles(vars6, vars8, "Domains", "Benefits")
plotBubbles(vars7, vars8, "Tools", "Benefits")
