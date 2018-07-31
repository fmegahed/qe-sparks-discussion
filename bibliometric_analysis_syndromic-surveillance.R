# Created by: Fadel Megahed (Email: fmegahed@miamioh.edu)
# To perform a quick bibliometric analysis on Web of Sci
# Data obtained using "syndromic surveillance" in Topic
# without limiting the document type, years, language

# The search was performed using WoS Core Collection
# Resulted in the download of a plain txt file
# containing the full record with cited references for 
# 992 results (7/24/2018 - 10:00 pm ET)

# Package Installation and Setting the Working Dir
#--------------------------------------------------
#install.packages("pacman") # install if needed
library(pacman)
p_load(bibliometrix,ggplot2,data.table,rvest,dplyr,
       rstudioapi) # Assuming R Studio is the IDE
# automatically getting the file's wd using rstudio api
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # only tested in RStudio environment 

# Cleaning screen and global environment
cat("\014") 
rm(list=ls())
graphics.off()

# Reading the WoS Data File
# Option 1: File Containing Searches Using "Syndromic Surveillance"
D.SyndromicSurveillance <- readFiles("syndromic-surveillance-papers(wos-core-collection).txt")
M <-convert2df(D.SyndromicSurveillance)

#------------------------------------------------------
# Ranking of Journals based on Publications
table.Sources <- table(M$SO) %>% 
  sort(decreasing = TRUE) %>%
  head(n=10)
row.names(table.Sources)[8] <- "JAMIA" # Abbreviated to fit in figure
op <- par(mar=c(4,18.5,0,1))
sources.chart <- barplot(table.Sources,
                         names.arg =row.names(table.Sources),
                         xlab = "Counts",
                         ylab = "",horiz = TRUE,cex.names = 0.82,cex.axis = 1,space = 1,
                         xlim = c(0,60),
                         las=1)
rm(op)

#---------------------------------------------------------
# Number of WoS Categories
# Scrapping the Categories as Defined by Web of Science
webpage <- "https://images.webofknowledge.com/images/help/WOS/hp_subject_category_terms_tasca.html"
scrapped.categories.list <- webpage %>% read_html() %>%
                  html_node(css = "table.content") %>%
                  html_table()
categories.list <- t(scrapped.categories.list[[1]])
categories.list <- categories.list[1,]
categories.list <- toupper(categories.list)
# Now obtaining the categories from our bibliometrics data
categories <- as.data.frame(tstrsplit(M$WC,
                            "[;]",type.convert=TRUE),
                            stringsAsFactors=FALSE)
# Initilizing a matrix
categories.matrix <- matrix(0,nrow=nrow(categories),
                            ncol=length(categories.list))
colnames(categories.matrix) <- categories.list
# Creating a counts matrix
for (i in 1:nrow(categories)) {
  for (j in 1:ncol(categories)) {
    placeholder = which((categories.list %in%categories[i,j]))
    categories.matrix[i,placeholder] <- 1
  }
}
#convert into dataframe
categories.matrix<- as.data.frame(categories.matrix, stringsAsFactors=FALSE) 

for (c in 1:ncol(categories.matrix)) {
  categories.matrix[,c] <- as.integer(categories.matrix[,c])
} #convert from characters to integers

categories.total <- colSums(categories.matrix)
categories.total <- data.frame(Counts=sort(categories.total, decreasing=TRUE))                  

op <- par(mar=c(4,20,0,1))
wos.chart <- barplot(head(categories.total$Counts,n=20),
     xlab = "Counts",
     ylab = "",horiz = TRUE,
     names.arg = head(row.names(categories.total),n=20),las=1,
      cex.names = 0.82,cex.axis = 1,space = 1,
     xlim = c(0,300))
rm(op)

#----------------------------------------------------------
# Bibliometric Analysis (Per the Bibliometrix Guide)
# See https://cran.r-project.org/web/packages/bibliometrix/vignettes/bibliometrix-vignette.html
results <- biblioAnalysis(M, sep = ";")
# Options modified for this analysis purposes
options(width=100)
S <- capture.output(summary(object = results, k = 10, pause = FALSE))
cat("Bibliometric Summary for Syndromic Surveillance ",
    S,file = "biblio-summary.txt", sep = "\n")
plot(x = results, k = 10, pause = FALSE)

# Bibliographic Network Matrices
#-------------------------------
# [1] Sources - i.e. Journal Names
NetMatrix <- biblioNetwork(M, analysis = "coupling", 
                           network = "sources", 
                           sep = ";")
net=networkPlot(NetMatrix,  normalize = "salton", 
                weighted=NULL, n = 25, 
                Title = "Coupling of Journal Names", 
                type = "auto", size=13,size.cex=T,
                remove.multiple=TRUE,labelsize=0.55,
                label.n=25,label.cex=F,
                cluster="optimal",edges.min = 5)

#  [2] Create keyword co-occurrences network
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
# Plot the network
net=networkPlot(NetMatrix, normalize="salton", 
                weighted=NULL, n = 25, 
                Title = "Keyword Co-Occurrences", 
                type = "auto", size=13,size.cex=T,
                remove.multiple=TRUE,labelsize=0.55,
                label.n=25,label.cex=F,
                cluster="optimal",edges.min = 5)
netstat <- networkStat(NetMatrix)
out <- capture.output(summary(netstat, k=10))
cat("Network Statistics",
    S,file = "keyword-network-statistics.txt", 
    sep = "\n")


# [3] Creating a Conceptual Structure Map from the Titles
# Using the MCA Method with terms mentioned at least 25
# times in the title
CS <- conceptualStructure(M,field="TI", method="MCA", 
                          minDegree=25, k.max=4, 
                          stemming=FALSE, labelsize=15)
