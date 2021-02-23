library(tiff)
library(ggplot2)
library(dplyr)
getwd()
setwd("C:/Users/sarap/Downloads/2021-02-23-pils2-5-1006-1005/2021-02-23-pils2-5-1006-1005_1/")
directories = list.dirs()[-1] # we remove the "./" folder with the [-1]

channel1 = c()
channel2 = c()
positions = c()
positionsRaw = c()
# pos = positions[[1]]
# pos
for (i in 1:length(directories)) {
  pos = directories[[i]] # I never know why, but you have to but double brackets
  print(pos)
  images = list.files(pos)
  #print(images)
  # imageTiff = readTIFF(source = images[2]) # 2 = gfp channel
  # rasterTiff = as.raster(imageTiff)
  channel1 = append(channel1,mean(readTIFF(source = paste(pos,images[2], sep = "/")))) # 2 is channel1
  channel2 = append(channel2,mean(readTIFF(source = paste(pos,images[3], sep = "/")))) # 3 is channel2
  positionsRaw = append(positionsRaw, pos)
  positions = append(positions, sub(".....", "", pos)) #removes ./Pos five first characters
}

dfData = data.frame(positions,positionsRaw, channel1, channel2)
dfData$positions = as.numeric(factor(dfData$positions, levels = as.numeric(dfData$positions)))
dfData[1,1] = 0
row.names(dfData) = dfData$positions
# dfData$rep = rep(1:9, times = 3*4)
# str(dfData)

dfDataRep = dfData
dfDataRep$rep = rep(1:9, times = 3*4)[1:106]
dfDataRep$strain = c(rep("PILS2", 9*3),rep("PILS5", 9*3),rep("PILS1006", 24),rep("PILS1005", 28))
dfDataRep$condition = c(rep(c(rep("Suc", 9), rep("Suc + Gal", 9), rep("1uM IAA", 9)),4))[1:106]
dfSum = dfDataRep %>% group_by(strain, condition) %>% summarise(avgC1 = mean(channel1),
                                                                avgC2 = mean(channel2),
                                                                seC1 = sd(channel1)/sqrt(9),
                                                                seC2 = sd(channel2)/sqrt(9))
dfSum$condition = factor(dfSum$condition, levels = c("Suc", "Suc + Gal", "1uM IAA"))

dir.create("DataAnalysis")
pdf(file = "DataAnalysis/avgPlots.pdf", width = 6.3, height = 4.5)
ggplot(dfData, aes(x = positions, y = channel1))+geom_line()+geom_point()+
  theme_classic()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylim(c(0,max(channel1)))+ scale_x_continuous(breaks = seq(0, length(positions), by = 15))

ggplot(dfData, aes(x = positions, y = channel2))+geom_line()+geom_point()+
  theme_classic()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylim(c(0,max(channel2)))

ggplot(dfData, aes(x = positions, y = channel1/channel2))+geom_line()+geom_point()+
  theme_classic()+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylim(c(0,max(channel1/channel2)))
dev.off()

pdf(file = "DataAnalysis/avgAndSe.pdf", width = 6.3, height = 4.5)

ggplot(dfSum, aes(x = condition, y = avgC1, fill = strain))+
  geom_bar(stat = "identity", position = "dodge", color = "black")+
  geom_errorbar(aes(ymin = avgC1 - seC1, ymax = avgC1 + seC1),  width=.2, position=position_dodge(.9))+
  theme_classic()+scale_fill_brewer(palette = "Set2")

ggplot(dfSum, aes(x = condition, y = avgC2, fill = strain))+
  geom_bar(stat = "identity", position = "dodge", color = "black")+
  geom_errorbar(aes(ymin = avgC2 - seC2, ymax = avgC2 + seC2),  width=.2, position=position_dodge(.9))+
  theme_classic()+scale_fill_brewer(palette = "Set2")

ggplot(dfSum, aes(x = condition, y = avgC1/avgC2, fill = strain))+
  geom_bar(stat = "identity", position = "dodge", color = "black")+
  theme_classic()+scale_fill_brewer(palette = "Set2")

dev.off()
# write.csv(dfData, file = "DataAnalysis/AvgPerPositions.csv")



ggplot(dfDataRep, aes( x = condition, y = channel1, fill = strain))+
  geom_violin(position="dodge")+theme_classic()

