#second Analysis 

#https://uc-r.github.io/hc_clustering
#https://ourcodingclub.github.io/tutorials/ordination/
#https://rawgit.com/e-oerton/disease-similarity-fusion/master/Disease_Similarity_Fusion_Main.html
#https://norcalbiostat.github.io/AppliedStatistics_notes/hierarchical-clustering.html
#https://cran.r-project.org/web/packages/htmlTable/vignettes/general.html
#https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html
#https://rpubs.com/emwozniak/195942
#http://ianmadd.github.io/pages/PeakDensityDistribution.html 
#https://www.rdocumentation.org/packages/ape/versions/5.4/topics/mantel.test


library(ggplot2)
library(ggsci)
library(cowplot)
library(lubridate)
library(ggpubr)
library(patchwork)
library(plyr)
library(dplyr)
library(vegan)
library(ape)
library(ggrepel)
library(table1)
library(Table1)
library(htmlTable)
library(readr)
library(factoextra)
library(htmlwidgets)
library(tableHTML)

setwd("~/Box/XinZhouFiles/Projects/ZXP3_Lymphedema/02.individualdata/")

data <- read.csv("./combined_ZXE3.csv", header = T, row.names = 1)

#set the year to be the current year
foo <- function(x, year=2020){
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
}

data$Dateofsurgery <- foo(as.Date(data$Dateofsurgery, format= "%m/%d/%y"))
data$Dateofonset <- foo(as.Date(data$Dateofonset, format= "%m/%d/%y"))
data$Date.of.Birth <- foo(as.Date(data$Date.of.Birth, format= "%m/%d/%y"))
data$Today <- foo(as.Date(data$Today, format= "%m/%d/%y"))

subset(data, data$Age_Onset < 0)

data$Age_OorS <- ifelse(is.na(data$Dateofsurgery), (data$Dateofonset - data$Date.of.Birth)/365,(data$Dateofsurgery - data$Date.of.Birth)/365)
data$Age_Onset <- (data$Dateofonset - data$Date.of.Birth)/365
data$Age_Surgery <- (data$Dateofsurgery - data$Date.of.Birth)/365

data$Condition <- factor(data$Condition, levels=c("NM", "SL", "LV", "LP"))
data <- subset(data, row.names(data) != 518)
data <- subset(data, row.names(data) != 540)

data$Age_Onset[1:3] <- NA

p <- ggplot(data, aes(x=Dateofsurgery)) + geom_histogram()
p <- p + xlab("Date of Surgery")
p
#ggsave(file="./figure/dateofsurgery.summarize.pdf", p, width = 3, height = 3, dpi = 300)

##################################################

#Figure 1

##################################################
data.NM_r <- subset(data, Condition != "NM")
page <- ggplot(data.NM_r, aes(x=Condition, y=Age_Onset, color=Gender)) + geom_boxplot(aes(color=Gender), na.rm = T)
page <- page + scale_color_d3() + theme_set(theme_cowplot())
page <- page + stat_compare_means(label = "p.signif",method = "wilcox.test")
page

page2 <- ggplot(data.NM_r, aes(x=Condition, y=Age_Onset)) + geom_violin(na.rm = T)
page2 <- page2 + stat_compare_means(label = "p.signif",comparisons = list(c("LV", "SL"),c("LP", "LV"),c("SL", "LP"))) + theme_set(theme_cowplot())

Figure1 <- page2 + page
Figure1

#ggsave(filename = "./figure/Figure1.pdf",Figure1, width = 7, height = 4, dpi=300)

##################################################

#Subset data 

##################################################
SL <- subset(data, Condition == "SL")
LV <- subset(data, Condition == "LV")
LP <- subset(data, Condition == "LP")
NM <- subset(data, Condition == "NM")

hist_SL <- ggplot(SL, aes(x=Age_Onset))+geom_density()#+ geom_histogram(bins = 40) 
hist_SL <- hist_SL + ggtitle("Density distribution of age in\nSecondary Lymphedema (SL) patient") + xlim(0,100)

SLage <- as.numeric(SL$Age_Onset)
head(density(SLage[!is.na(SLage)])$y)
which.min(density(SLage[!is.na(SLage)])$y)
density(SLage[!is.na(SLage)])$x[334]

DensityFaithfulX <- density(SLage[!is.na(SLage)])$x
DensityFaithfulY <- density(SLage[!is.na(SLage)])$y
MinYDensity<- min(DensityFaithfulY[DensityFaithfulX < 40 & DensityFaithfulX > 18])
which(DensityFaithfulY == MinYDensity)
DensityFaithfulX[147]

hist_SL <- hist_SL + geom_vline(xintercept = DensityFaithfulX[147], linetype="dotdash", color="blue")
hist_SL <- hist_SL + annotate(geom="text", x=26, y=0.02, label="Age=18.6",color="blue")
hist_SL

#ggsave(filename = "./figure/Figure1b.pdf", hist_SL, width = 7, height = 4, dpi=300)

SL$Age_Factor <- "Unknown"
SL$Age_Factor[SL$Age_Onset <= 18.6] <- "Younger"
SL$Age_Factor[SL$Age_Onset > 18.6] <- "Older"

SL$Age_Factor[SL$Age_Factor=="Unknown"] <- NA

SL$BMI_Factor <- "Overweight"
SL$BMI_Factor[SL$BMI>=30] <- "Obese"
SL$BMI_Factor[SL$BMI<25] <- "Normal"
SL$BMI_Factor[SL$BMI<18.5] <- "Underweight"

SL$BMI_Factor <- factor(SL$BMI_Factor, levels = c("Underweight", "Normal",  "Overweight", "Obese"))
SL$Age_Factor <- factor(SL$Age_Factor, levels = c("Younger", "Older"))

SL1 <- select(SL, Gender, Age_Factor, BMI_Factor)

SL1$Cardiovascular <- select(SL, colnames(SL)[grep("CVD_*", colnames(SL))]) %>% rowSums()
SL1$Cardiovascular[SL1$Cardiovascular>5] <- "Above 5"

label(SL1$Gender) <- "Sex"
label(SL1$Age_Factor) <- "Age by Group"
label(SL1$BMI_Factor) <- "Weight Status by BMI"

rndr <- function(x, name, ...) {
  if (length(x) == 0) {
    y <- lalonde[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- t.test(y ~ lalonde$treat)$p.value
    } else {
      p <- chisq.test(table(y, droplevels(lalonde$treat)))$p.value
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}

rndr.strat <- function(label, n, ...) {
  ifelse(n==0, label, render.strat.default(label, n, ...))
}


p <- table1(~ Gender + Age_Factor + BMI_Factor | Cardiovascular, data=SL1, droplevels=T, render = rndr, render.strat = rndr.strat, overall=F, test=T)
p

SL_clean <- select(SL, Gender, Age_Factor, BMI_Factor, colnames(SL)[grep("CVD_*", colnames(SL))],
                   colnames(SL)[grep("CVD_*", colnames(SL))],
                   colnames(SL)[grep("EnMT_*", colnames(SL))],
                   colnames(SL)[grep("MuSK_*", colnames(SL))],
                   colnames(SL)[grep("Nero_*", colnames(SL))],
                   colnames(SL)[grep("Hema_*", colnames(SL))],
                   colnames(SL)[grep("PsyD_*", colnames(SL))],
                   colnames(SL)[grep("GIHe_*", colnames(SL))],
                   colnames(SL)[grep("OBGYN_*", colnames(SL))], 
                   colnames(SL)[grep("ReUr_*", colnames(SL))],
                   colnames(SL)[grep("Auto_*", colnames(SL))],
                   colnames(SL)[grep("Opht_*", colnames(SL))],
                   Cancer_Breast.cancer)

t1 <- colSums(SL_clean[4:123])!=0

SLvari <- SL_clean[4:123]

SLvari_clean <- SLvari[,t1]

SL_clean.nozero <- cbind(SL_clean[,1:3],SLvari_clean)

for (i in 1:108){
p1 <- make.table(dat = SL_clean.nozero,
           strat = colnames(SL_clean.nozero)[i+3], 
           cat.varlist = c("Gender", "Age_Factor", "BMI_Factor"),
           cat.ptype    = c("fisher", "chisq"), 
           output = "html")

path <- paste("./SL/", colnames(SL_clean.nozero)[i+3], sep="")
readr::write_file(p1, paste(path, "html", sep = "."))
}


hist_LV <- ggplot(LV, aes(x=Age_Onset))+geom_density()#+ geom_histogram(bins = 40) 
hist_LV <- hist_LV + ggtitle("Density distribution of age in\nLymph Vascular (LV) patient")+ xlim(0,100)
hist_LV

LVage <- as.numeric(LV$Age_Onset)
head(density(LVage[!is.na(LVage)])$y)
which.min(density(LVage[!is.na(LVage)])$y)

density(LVage[!is.na(LVage)])$x[512]

DensityFaithfulX <- density(LVage[!is.na(LVage)])$x
DensityFaithfulY <- density(LVage[!is.na(LVage)])$y
MinYDensity<- min(DensityFaithfulY[DensityFaithfulX < 55 & DensityFaithfulX > 40])
which(DensityFaithfulY == MinYDensity)
DensityFaithfulX[305]

hist_LV <- hist_LV + geom_vline(xintercept = DensityFaithfulX[305], linetype="dotdash", color="blue")
hist_LV <- hist_LV + annotate(geom="text", x=56, y=0.02, label="Age = 46.7",color="blue")
hist_LV

ggsave(filename = "./figure/Figure1c.pdf", hist_LV, width = 7, height = 4, dpi=300)

LV$Age_Factor <- "Unknown"
LV$Age_Factor[LV$Age_Onset <= 46.7] <- "Younger"
LV$Age_Factor[LV$Age_Onset > 46.7] <- "Older"

LV$Age_Factor[LV$Age_Factor=="Unknown"] <- NA

LV$BMI_Factor <- "Overweight"
LV$BMI_Factor[LV$BMI>=30] <- "Obese"
LV$BMI_Factor[LV$BMI<25] <- "Normal"
LV$BMI_Factor[LV$BMI<18.5] <- "Underweight"

LV$BMI_Factor <- factor(LV$BMI_Factor, levels = c("Underweight", "Normal",  "Overweight", "Obese"))
LV$Age_Factor <- factor(LV$Age_Factor, levels = c("Younger", "Older"))

LV1 <- select(LV, Gender, Age_Factor, BMI_Factor)

LV1$Cardiovascular <- select(LV, colnames(LV)[grep("CVD_*", colnames(LV))]) %>% rowSums()
LV1$Cardiovascular[LV1$Cardiovascular>5] <- "Above 5"

label(LV1$Gender) <- "Sex"
label(LV1$Age_Factor) <- "Age by Group"
label(LV1$BMI_Factor) <- "Weight Status by BMI"

p2 <- table1(~ Gender + Age_Factor + BMI_Factor | Cardiovascular, data=LV1, droplevels=T, render = rndr, render.strat = rndr.strat, overall=F, test=T)
p2

LV_clean <- select(LV, Gender, Age_Factor, BMI_Factor, colnames(LV)[grep("CVD_*", colnames(LV))],
                   colnames(LV)[grep("CVD_*", colnames(LV))],
                   colnames(LV)[grep("EnMT_*", colnames(LV))],
                   colnames(LV)[grep("MuSK_*", colnames(LV))],
                   colnames(LV)[grep("Nero_*", colnames(LV))],
                   colnames(LV)[grep("Hema_*", colnames(LV))],
                   colnames(LV)[grep("PsyD_*", colnames(LV))],
                   colnames(LV)[grep("GIHe_*", colnames(LV))],
                   colnames(LV)[grep("OBGYN_*", colnames(LV))], 
                   colnames(LV)[grep("ReUr_*", colnames(LV))],
                   colnames(LV)[grep("Auto_*", colnames(LV))],
                   colnames(LV)[grep("Opht_*", colnames(LV))],
                   Cancer_Breast.cancer)

t <- colSums(LV_clean[!is.na(LV_clean$Age_Factor),4:123])>0

LVvari <- LV_clean[4:123]

LVvari_clean <- LVvari[,t]

LV_clean.nozero <- cbind(LV_clean[,1:3],LVvari_clean)

for (i in 1:62){
  p1 <- make.table(dat = LV_clean.nozero,
                   strat = colnames(LV_clean.nozero)[i+3], 
                   cat.varlist = c("Gender", "Age_Factor", "BMI_Factor"),
                   cat.ptype    = c("fisher", "chisq"),
                   output = "html")
  
  path <- paste("./LV/", colnames(LV_clean.nozero)[i+3], sep="")
  readr::write_file(p1, paste(path, "html", sep = "."))
}


hist_LP <- ggplot(LP, aes(x=Age_Onset))+geom_density()#+ geom_histogram(bins = 40)
hist_LP <- hist_LP + ggtitle("Density distribution of age in\nLipidema (LP) patient")
hist_LP <- hist_LP + xlim(0,100)

LPage <- as.numeric(LP$Age_Onset)
head(density(LPage[!is.na(LPage)])$y)
which.min(density(LPage[!is.na(LPage)])$y)

density(LPage[!is.na(LPage)])$x[512]

DensityFaithfulX <- density(LPage[!is.na(LPage)])$x
DensityFaithfulY <- density(LPage[!is.na(LPage)])$y
MinYDensity<- min(DensityFaithfulY[DensityFaithfulX < 50 & DensityFaithfulX > 40])
which(DensityFaithfulY == MinYDensity)
DensityFaithfulX[276]

hist_LP <- hist_LP + geom_vline(xintercept = DensityFaithfulX[276], linetype="dotdash", color="blue")
hist_LP <- hist_LP + annotate(geom="text", x=52, y=0.02, label="Age = 42.9",color="blue")
hist_LP

#ggsave(filename = "./figure/Figure1d.pdf", hist_LP, width = 7, height = 4, dpi=300)

LP$Age_Factor <- "Unknown"
LP$Age_Factor[LP$Age_Onset <= 42.9] <- "Younger"
LP$Age_Factor[LP$Age_Onset > 42.9] <- "Older"

LP$Age_Factor[LP$Age_Factor=="Unknown"] <- NA

LP$BMI_Factor <- "Overweight"
LP$BMI_Factor[LP$BMI>=30] <- "Obese"
LP$BMI_Factor[LP$BMI<25] <- "Normal"
LP$BMI_Factor[LP$BMI<18.5] <- "Underweight"

LP$BMI_Factor <- factor(LP$BMI_Factor, levels = c("Underweight", "Normal",  "Overweight", "Obese"))
LP$Age_Factor <- factor(LP$Age_Factor, levels = c("Younger", "Older"))

LP1 <- select(LP, Gender, Age_Factor, BMI_Factor)

LP1$Cardiovascular <- select(LP, colnames(LP)[grep("CVD_*", colnames(LP))]) %>% rowSums()
LP1$Cardiovascular[LP1$Cardiovascular>5] <- "Above 5"

label(LP1$Gender) <- "Sex"
label(LP1$Age_Factor) <- "Age by Group"
label(LP1$BMI_Factor) <- "Weight Status by BMI"

p3 <- table1(~ Gender + Age_Factor + BMI_Factor | Cardiovascular, data=LP1, droplevels=T, render = rndr, render.strat = rndr.strat, overall=F, test=T)
p3

LP_clean <- select(LP, Gender, Age_Factor, BMI_Factor, colnames(LP)[grep("CVD_*", colnames(LP))],
                   colnames(LP)[grep("CVD_*", colnames(LP))],
                   colnames(LP)[grep("EnMT_*", colnames(LP))],
                   colnames(LP)[grep("MuSK_*", colnames(LP))],
                   colnames(LP)[grep("Nero_*", colnames(LP))],
                   colnames(LP)[grep("Hema_*", colnames(LP))],
                   colnames(LP)[grep("PsyD_*", colnames(LP))],
                   colnames(LP)[grep("GIHe_*", colnames(LP))],
                   colnames(LP)[grep("OBGYN_*", colnames(LP))], 
                   colnames(LP)[grep("ReUr_*", colnames(LP))],
                   colnames(LP)[grep("Auto_*", colnames(LP))],
                   colnames(LP)[grep("Opht_*", colnames(LP))],
                   Cancer_Breast.cancer)

t <- colSums(LP_clean[!is.na(LP_clean$Age_Factor),4:123])>0

LPvari <- LP_clean[4:123]

LPvari_clean <- LPvari[,t]

LP_clean.nozero <- cbind(LP_clean[,1:3],LPvari_clean)

for (i in 1:74){
  p1 <- make.table(dat = LP_clean.nozero,
                   strat = colnames(LP_clean.nozero)[i+3], 
                   cat.varlist = c("Age_Factor", "BMI_Factor"),
                   cat.ptype    = c("fisher", "chisq"),
                   output = "html")
  
  path <- paste("./LP/", colnames(LP_clean.nozero)[i+3], sep="")
  readr::write_file(p1, paste(path, "html", sep = "."))
}

p2b <- hist_SL / hist_LV / hist_LP



hist_NM <- ggplot(NM, aes(x=Age_Surgery))+geom_density()#+ geom_histogram(bins = 40) 
hist_NM <- hist_NM + ggtitle("Density distribution of age (age of surgery) in\nRisk but not Lymphedema (NM) patient") + xlim(0,100)
hist_NM

NMage <- as.numeric(NM$Age_Surgery)
head(density(NMage[!is.na(NMage)])$y)
which.min(density(NMage[!is.na(NMage)])$y)
density(NMage[!is.na(NMage)])$x[334]

DensityFaithfulX <- density(NMage[!is.na(NMage)])$x
DensityFaithfulY <- density(NMage[!is.na(NMage)])$y
MinYDensity<- min(DensityFaithfulY[DensityFaithfulX < 75 & DensityFaithfulX > 55])
which(DensityFaithfulY == MinYDensity)
DensityFaithfulX[434]

#hist_NM <- hist_NM + geom_vline(xintercept = DensityFaithfulX[147], linetype="dotdash", color="blue")
#hist_NM <- hist_NM + annotate(geom="text", x=26, y=0.02, label="Age=18.6",color="blue")
hist_NM

ggsave(filename = "./figure/FigureS.NM_Age.pdf", hist_NM, width = 7, height = 4, dpi=300)

NM$Age_Factor <- "Unknown"
NM$Age_Factor[NM$Age_Onset <= 18.6] <- "Younger"
NM$Age_Factor[NM$Age_Onset > 18.6] <- "Older"

NM$Age_Factor[NM$Age_Factor=="Unknown"] <- NA

NM$BMI_Factor <- "Overweight"
NM$BMI_Factor[NM$BMI>=30] <- "Obese"
NM$BMI_Factor[NM$BMI<25] <- "Normal"
NM$BMI_Factor[NM$BMI<18.5] <- "Underweight"

NM$BMI_Factor <- factor(NM$BMI_Factor, levels = c("Underweight", "Normal",  "Overweight", "Obese"))
NM$Age_Factor <- factor(NM$Age_Factor, levels = c("Younger", "Older"))

NM1 <- select(NM, Gender,  BMI_Factor)

NM1$Cardiovascular <- select(NM, colnames(NM)[grep("CVD_*", colnames(NM))]) %>% rowSums()
NM1$Cardiovascular[NM1$Cardiovascular>5] <- "Above 5"

label(NM1$Gender) <- "Sex"

label(NM1$BMI_Factor) <- "Weight Status by BMI"

rndr <- function(x, name, ...) {
  if (length(x) == 0) {
    y <- lalonde[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- t.test(y ~ lalonde$treat)$p.value
    } else {
      p <- chisq.test(table(y, droplevels(lalonde$treat)))$p.value
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}

rndr.strat <- function(label, n, ...) {
  ifelse(n==0, label, render.strat.default(label, n, ...))
}


pNM <- table1(~ Gender+ BMI_Factor | Cardiovascular, data=NM1, droplevels=T, render = rndr, render.strat = rndr.strat, overall=F, test=T)
pNM


NM_clean <- select(NM, Gender,BMI_Factor, colnames(NM)[grep("CVD_*", colnames(NM))],
                   colnames(NM)[grep("CVD_*", colnames(NM))],
                   colnames(NM)[grep("EnMT_*", colnames(NM))],
                   colnames(NM)[grep("MuSK_*", colnames(NM))],
                   colnames(NM)[grep("Nero_*", colnames(NM))],
                   colnames(NM)[grep("Hema_*", colnames(NM))],
                   colnames(NM)[grep("PsyD_*", colnames(NM))],
                   colnames(NM)[grep("GIHe_*", colnames(NM))],
                   colnames(NM)[grep("OBGYN_*", colnames(NM))], 
                   colnames(NM)[grep("ReUr_*", colnames(NM))],
                   colnames(NM)[grep("Auto_*", colnames(NM))],
                   colnames(NM)[grep("Opht_*", colnames(NM))],
                   Cancer_Breast.cancer)

t1 <- colSums(NM_clean[3:122])!=0

NMvari <- NM_clean[3:122]

NMvari_clean <- NMvari[,t1]

NM_clean.nozero <- cbind(NM_clean[,1:2],NMvari_clean)

for (i in 1:78){
  p1 <- make.table(dat = NM_clean.nozero,
                   strat = colnames(NM_clean.nozero)[i+2], 
                   cat.varlist = c("Gender",  "BMI_Factor"),
                   cat.ptype    = c("fisher", "chisq"),
                   output = "html")
  
  path <- paste("./NM/", colnames(NM_clean.nozero)[i+3], sep="")
  readr::write_file(p1, paste(path, "html", sep = "."))
}


##################################################

#Figure 2

##################################################
table(data$Condition, data$Gender)

lm_eqn = function(df){
  m = lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

data_ori <- data

data <- subset(data_ori, Condition != "NM")

pbmi <- ggplot(data, aes(x=Age_Onset,y=BMI)) + geom_point()
pbmi <- pbmi + geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)
pbmi <- pbmi + facet_wrap(~Condition, ncol=4)
pbmi <- pbmi + geom_hline(yintercept = c(18,25,30), linetype=3)
pbmi

ggsave(pbmi)

p1 <- pbmi +stat_density_2d(aes(fill = ..density..), geom = "raster", contour = F) +
  scale_fill_gradientn(colors=c("black","white","white","#FFD31D","#ED0000FF","#AD002AFF","#AD002AFF"))
p1 <- p1 + geom_hline(yintercept = c(18,25,30), linetype=2, color="white")
p1
#ggsave(filename = "./figure/Figure2a.pdf",p1, width = 8, height = 5, dpi=300)

df<- select(data, Age_Onset, BMI, Condition)
colnames(df) <- c("x", "y", "Condition")

eq <- ddply(df,.(Condition),lm_eqn)

p2 = pbmi + geom_text(data=eq,aes(x = 40, y = 90,label=V1), parse = TRUE, inherit.aes=FALSE) + facet_grid(.~Condition)
p2 
#ggsave(filename = "./figure/Figure.S1.pdf",p2, width = 8, height = 5, dpi=300)


##################################################

#Figure 3

##################################################

data_no_NM <- subset(data_ori, Condition != "NM")
data.clean <- data_no_NM[,21:144]
data.clean[is.na(data.clean)] <- 0
data.clean2 <- data.clean[!(rowSums(data.clean)==0),!(colSums(data.clean)==0)]

Indi.dist <- vegdist(data.clean2, "jaccard",na.rm = T, binary = T)
df.dist <- as.data.frame(as.matrix(Indi.dist))
PCOA.dist <- pcoa(Indi.dist)

barplot(PCOA.dist$values$Relative_eig[1:10])
biplot.pcoa(PCOA.dist)

dim(PCOA.dist$vectors[,1:20])
databind <- data_no_NM
databind1 <- databind[!(rowSums(databind[,21:144])==0),]
databind2 <- cbind(PCOA.dist$vectors[,1:20],databind1)
row.names(databind2) <- paste("N", 1:616, sep="_")

pindi <- ggplot(databind2, aes(x=Axis.1, y=Axis.2, color=Condition, shape=as.factor(Obesity_Obesity))) + geom_point(na.rm = T)
pindi <- pindi + scale_color_jama() + theme(legend.position = "none")
pindi

pindi2 <- ggplot(databind2, aes(x=Axis.3, y=Axis.4,color=Condition, shape=as.factor(Obesity_Obesity))) + geom_point(na.rm = T)
pindi2 <- pindi2 + scale_color_jama()
pindi2

pindi3 <- ggplot(databind2, aes(x=Axis.5, y=Axis.6,color=Condition, shape=as.factor(Obesity_Obesity))) + geom_point(na.rm = T)
pindi3 <- pindi3 + scale_color_jama()
pindi3

pindividual <- (pindi + pindi2) 
pindividual
#ggsave(filename = "./figure/figure3.pdf",pindividual, width = 8, height = 3, dpi=300)

###################################################

#Figure 3 with normal

###################################################
data_ori$Condition<- factor(data_ori$Condition, levels = c("SL", "LV", "LP", "NM"))

data.clean3 <- data_ori[,21:144]
data.clean3[is.na(data.clean3)] <- 0
data.clean4 <- data.clean3[!(rowSums(data.clean3)==0),!(colSums(data.clean3)==0)]

Indi.dist.all <- vegdist(data.clean4, "jaccard",na.rm = T, binary = T)
Indi.dist.all <- as.data.frame(as.matrix(Indi.dist.all))
PCOA.dist.all <- pcoa(Indi.dist.all)

barplot(PCOA.dist.all$values$Relative_eig[1:10])
biplot.pcoa(PCOA.dist.all)

dim(PCOA.dist.all$vectors[,1:20])
databind1.1 <- data_ori
databind1.1 <- databind1.1[!(rowSums(databind1.1[,21:144])==0),]
databind2.1 <- cbind(PCOA.dist.all$vectors[,1:20],databind1.1)
row.names(databind2) <- paste("N", 1:715, sep="_")

pindi.all <- ggplot(databind2.1, aes(x=Axis.1, y=Axis.2, color=Condition, shape=as.factor(Obesity_Obesity))) + geom_point(na.rm = T)
pindi.all <- pindi.all + scale_color_jama() + theme(legend.position = "none")
pindi.all

pindi2.all <- ggplot(databind2.1, aes(x=Axis.3, y=Axis.4,color=Condition, shape=as.factor(Obesity_Obesity))) + geom_point(na.rm = T)
pindi2.all <- pindi2.all + scale_color_jama()
pindi2.all

pindi3.all <- ggplot(databind2.1, aes(x=Axis.5, y=Axis.6,color=Condition, shape=as.factor(Obesity_Obesity))) + geom_point(na.rm = T)
pindi3.all <- pindi3.all + scale_color_jama()
pindi3.all

pindividual.all <- (pindi.all + pindi2.all) 
pindividual.all
#ggsave(filename = "./figure/S.figure3.pdf",pindividual.all, width = 8, height = 3, dpi=300)

##################################################

#Figure 4

##################################################
df <- as.data.frame(t(data.clean2))
Disease.dist <- vegdist(as.data.frame(t(data.clean2)), "jaccard",na.rm = T, binary = T)
df2.dist <- as.data.frame(as.matrix(Disease.dist))
PCOA.dist_2 <- pcoa(df2.dist)
barplot(PCOA.dist_2$values$Relative_eig[1:10])
biplot.pcoa(PCOA.dist_2)
dim(PCOA.dist_2$vectors[,1:20])

df.disease <- as.data.frame(PCOA.dist_2$vectors[,1:20])
df.disease$Disease <- colnames(data.clean2)
df.disease$Type <- sub("(_[^_]+)$", "",df.disease$Disease)

p.di <- ggplot(df.disease, aes(x=Axis.1, y=Axis.2, color=Type, label=Disease)) + geom_point() + geom_text_repel(data=subset(df.disease, Type=="LYMP"))
p.di 

p.di2 <- ggplot(df.disease, aes(x=Axis.1, y=Axis.2, color=Type, label=Disease)) + geom_point() + geom_label_repel()
p.di2

hc1 <- hclust(Disease.dist, method = "complete")
hc2 <- hclust(Disease.dist, method = "ward.D")
plot(hc2, cex = 0.8, hang = -1)

fviz_dend(hc2, cex = 0.6, k=2 ,type = "circular", rect = TRUE)
fviz_dend(hc2, cex = 0.6, k=2 ,type = "rectangle", rect = TRUE)
#"#000000", "#555555"
pclust <- fviz_dend(hc2, cex = 1, k=2 ,type = "phylogenic", rect = TRUE, repel=T ) + scale_color_manual(values=c("#eaab00", "#e98300"))
pclust
#ggsave(file = "./figure/Figure4.pdf",pclust, height = 12, width = 15, dpi=300)

x1 <- fviz_nbclust(df, kmeans, diss=Disease.dist,method = "silhouette")
x2 <- fviz_nbclust(df, kmeans, diss=Disease.dist,method = "wss")
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
x3<- fviz_gap_stat(gap_stat)

x1/x2/x3


#Plot normal
data_NM <- subset(data_ori, Condition == "NM")
data.NM<- data_NM[,21:144]
data.NM[is.na(data.NM)] <- 0
data.NM2 <- data.NM[!(rowSums(data.NM)==0),!(colSums(data.NM)==0)]

NM.dist <- vegdist(as.data.frame(t(data.NM2)), "jaccard",na.rm = T, binary = T)
df2.dist <- as.data.frame(as.matrix(NM.dist))
PCOA.dist_3 <- pcoa(df2.dist)
pengi.nm<-barplot(PCOA.dist_3$values$Relative_eig[1:10])
biplot.pcoa(PCOA.dist_3)
dim(PCOA.dist_3$vectors[,1:20])

df.nm<- as.data.frame(PCOA.dist_3$vectors[,1:20])
df.nm$Disease <- colnames(data.NM2)
df.nm$Type <- sub("(_[^_]+)$", "",df.nm$Disease)

p.nm <- ggplot(df.nm, aes(x=Axis.1, y=Axis.2, color=Type, label=Disease)) + geom_point() + geom_text_repel(data=subset(df.disease, Type=="LYMP"))
p.nm 

p.nm2 <- ggplot(df.nm, aes(x=Axis.1, y=Axis.2, color=Type, label=Disease)) + geom_point() + geom_label_repel()
p.nm2

hc3 <- hclust(NM.dist, method = "ward.D")
plot(hc3, cex = 0.8, hang = -1)

#000000
pclust2 <- fviz_dend(hc3, cex = 1, k=1 ,type = "phylogenic", rect = TRUE, repel=T ) + scale_color_manual(values=c("#175e54"))
pclust2 <- pclust2 + ggtitle("Disease relationship without Lymphedema patient (n=106)")
pclust2
ggsave(file = "./figure/Figure4NM.pdf",pclust2, height = 12, width = 15, dpi=300)



#set up mantel test
NM.dist_matrix <- as.matrix(NM.dist)
Disease.dist_matrax <- as.matrix(Disease.dist)

Disease.dist_matrax_78 <- Disease.dist_matrax[colnames(Disease.dist_matrax) %in% colnames(NM.dist_matrix),colnames(Disease.dist_matrax) %in% colnames(NM.dist_matrix)]
NM.dist_matrix_78 <- NM.dist_matrix[colnames(NM.dist_matrix) %in% colnames(Disease.dist_matrax_78),colnames(NM.dist_matrix) %in% colnames(Disease.dist_matrax_78)]

#making sure that we are testing the same set of diseases
identical(colnames(NM.dist_matrix_78), colnames(Disease.dist_matrax_78))

mantel.test(NM.dist_matrix_78,Disease.dist_matrax_78, nperm = 999,graph = FALSE,alternative = "two.sided")

colnames(NM.dist_matrix_78)


