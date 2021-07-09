library(reshape2)
library(ggplot2)
library(cowplot)
library(ggrepel)
library(dplyr)
library(patchwork)

setwd("~/Box/XinZhouFiles/Projects/ZXP3_Lymphedema/03.Final/")
getwd()


NM.dist <- read.csv("./Normal_pairwised_distance.csv", header = T)
Di.dist <- read.csv("./Disease_pairwised_distance.csv", header = T)

comorbidity <- read.csv("./Co-morbidities.csv", header = T, row.names = 1 )
prev <- comorbidity[,-(1:20)]
map_prev <- round(colSums(prev)/7.24, 3)
map_prev <- as.data.frame(map_prev)
map_prev$names <- row.names(map_prev)

NM.melt <- melt(NM.dist)
Di.melt <- melt(Di.dist)

write.csv(NM.melt, file = "./NM.melt.csv")
write.csv(Di.melt, file = "./Di.melt.csv")

table(table(NM.melt$X)==79)

coexsit <- merge(NM.melt, Di.melt, by=c("X", "variable"))
colnames(coexsit) <- c("X", "variableX", "control", "lymphedema")
coexsit$prev.x  <- map_prev$map_prev[match(coexsit$X, map_prev$names)]
coexsit$prev.y  <- map_prev$map_prev[match(coexsit$variableX, map_prev$names)]
coexsit <- coexsit[which(!coexsit$X == coexsit$variable),]
coexsit$mean <-(coexsit$prev.x+coexsit$prev.y)/2
coexsit$label <- paste(coexsit$X, coexsit$variableX, sep="_")

coexsit2 <- coexsit[which(!coexsit$control==coexsit$lymphedema),]

coexsit_control <- subset(coexsit2, control==1)
coexsit_lymphedema <- subset(coexsit2, lymphedema==1)

dim(coexsit_control)
dim(coexsit_lymphedema)

colnames(coexsit_control)[3] <- "None_Lymphedema"
colnames(coexsit_control)[4]<- "Lymphedema"
coexsit_con_melt <- melt(data = coexsit_control, id.vars = c("X", "variableX","prev.x", "prev.y","mean","label"), measure.vars = c("None_Lymphedema", "Lymphedema"))
p_control <- ggplot(coexsit_con_melt, aes(x=mean, y=value, color=variable, group=label)) + geom_point(size=1) + geom_line(color="grey")
p_control <- p_control + theme_cowplot() #+  geom_text(data=coexsit_melt %>% filter(coexsit_melt$variable=="control" , coexsit_melt$value < 0.8), aes(label=label), size=3)
p_control <- p_control + labs(x = "mean prevalance (%)", y = "Jaccard Distance") + scale_color_manual(values = c("blue", "red")) +  theme(text = element_text(size=20))
p_control

colnames(coexsit_lymphedema)[3] <- "None_Lymphedema"
colnames(coexsit_lymphedema)[4]<- "Lymphedema"
coexsit_lym_melt <- melt(data = coexsit_lymphedema, id.vars = c("X", "variableX","prev.x", "prev.y","mean","label"), measure.vars = c("None_Lymphedema", "Lymphedema"))
p_lym <- ggplot(coexsit_lym_melt, aes(x=mean, y=value, color=variable, group=label)) + geom_point(size=1) + geom_line(color="grey")
p_lym <- p_lym + theme_cowplot() #+  geom_text_repel(data=filter(coexsit_melt, variable=="control"&mean > 20), aes(label=label), angle = 90, vjust = 0.5, hjust=-1)
p_lym <- p_lym + labs(x = "mean prevalance (%)", y = "Jaccard Distance") + scale_color_manual(values = c("blue", "red")) +  theme(text = element_text(size=20))
p_lym

p <- (p_control/p_lym) + plot_layout(guides = "collect")
p

ggsave(filename = "./prevalance_jaccard_adjusted.02.pdf", p, width = 16, height = 8, dpi=300)
write.csv(file = "./coexsit.csv", coexsit2)
