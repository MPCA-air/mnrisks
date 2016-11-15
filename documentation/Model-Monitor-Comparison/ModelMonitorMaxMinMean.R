##### Model Monitor Data Analysis 

##Load (way too many) Packages
library (NADA)
library(openair)
library(ggplot2)
library(dplyr)
library(graphics)
library(Kendall)


##Set Working Directory (Where Files Are Located/Stored)
setwd("M:/KME Files/Model Monitor/2011_AllSources/Statistics/Data Tables and Processing/2011 Max Min Mean Analysis")

#################################################
#  Read in Cleaned Data Sets
#  All data was retrieved from AQS in ug/m3 (?)
#  All missing values have been removed
##################################################

buffers<-read.csv("M:/KME Files/Model Monitor/2011_AllSources/Statistics/Data Tables and Processing/allmeasurements_receptors_buffer.csv", stringsAsFactors=FALSE, header=TRUE, sep=",")
#buffers<-na.omit(buffers)
buffers <- filter(buffers, !is.na(mod) & !is.na(obs), !substance %in% "Copper")


##Subset Data by type of receptor value from the 1km buffer
average<-subset(buffers,buffers$summary=="Mean",select=c(site, mod, substance, CAS, obs, obsstddev, airpollutanttype))
minimum<-subset(buffers,buffers$summary=="Minimum",select=c(site, mod, substance, CAS, obs, obsstddev, airpollutanttype))
maximum<-subset(buffers,buffers$summary=="Maximum",select=c(site, mod, substance, CAS, obs, obsstddev, airpollutanttype))

##ScatterPlot All Data
png("averagescatterplotloglog.png", width=10, height=7, units="in", pointsize=12, res=300, type=c("cairo"))
scatterPlot(average, x="obs", y="mod", method="scatter", pch=c(1:25), group="substance", data.thresh=0,
            type="default", mod.line=TRUE, linear=TRUE, log.x=TRUE, log.y=TRUE, xlab="observed", ylab="average modeled", 
            main="Log-Log Plot of Measured and Air Pollutants and Average Modeled Pollutants within a 5km Buffer")
      dev.off()   

png("minimumscatterplotloglog.png", width=10, height=7, units="in", pointsize=12, res=300, type=c("cairo"))
scatterPlot(minimum, x="obs", y="mod", method="scatter", group="substance", pch=c(1:25),type="default", 
            mod.line=TRUE, linear=TRUE, log.x=TRUE, log.y=TRUE, xlab="observed", ylab="minimum modeled", 
            main="Log-Log Plot of Measured Air Pollutants and Minimum Modeled Pollutants within a 5km Buffer")
         dev.off()    
         
png("maximumplotloglog.png", width=10, height=7, units="in", pointsize=12, res=300, type=c("cairo"))
scatterPlot(maximum, x="obs", y="mod", method="scatter", group="substance", pch=c(1:25, 32:42),type="default", 
            mod.line=TRUE, linear=TRUE, log.x=TRUE, log.y=TRUE, xlab="observed", ylab="maximum modeled", 
            main="Log-Log Plot of Measured Air Pollutants and Maximum Modeled Pollutants within a 5km Buffer")
            dev.off()              

##Average plots split criteria and toxics
avgcrit=subset(average, airpollutanttype=="criteria")
png("Buffer Average scatterplotloglogcriteria2011.png", width=10, height=7, units="in", pointsize=12, res=300, type=c("cairo"))
scatterPlot(avgcrit, x="obs", y="mod", method="scatter", group="substance", type="default", simplify.names=FALSE,
            mod.line=TRUE, linear=TRUE, log.x=TRUE, log.y=TRUE, xlab="observed", ylab="modeled", pch=1:5,
            main="Log-Log Plot of Measured and Average Modeled Criteria Air Pollutants within a 5km Buffer")
dev.off()

avgtox=subset(average, airpollutanttype=="airtoxics")
png("Buffer Average scatterplotloglogtoxics2011.png", width=10, height=7, units="in", pointsize=12, res=300, type=c("cairo"))
scatterPlot(avgtox, x="obs", y="mod", method="scatter", group="substance", pch=c(1:25),type="default", simplify.names=FALSE,
            mod.line=TRUE, linear=TRUE, log.x=TRUE, log.y=TRUE, xlab="observed", ylab="modeled", 
            main="Log-Log Plot of Measured and Average Modeled Air Toxics Pollutants within a 5km Buffer")
dev.off()

##Maximum plots split criteria and toxics
maxcrit=subset(maximum, airpollutanttype=="criteria")
png("Buffer Max scatterplotloglogcriteria2011.png", width=10, height=7, units="in", pointsize=12, res=300, type=c("cairo"))
scatterPlot(maxcrit, x="obs", y="mod", method="scatter", group="substance", type="default", simplify.names=FALSE,
            mod.line=TRUE, linear=TRUE, log.x=TRUE, log.y=TRUE, xlab="observed", ylab="modeled", pch=1:5,
            main="Log-Log Plot of Measured and Maximum Modeled Criteria Air Pollutants within a 5km Buffer")
dev.off()

maxtox=subset(maximum, airpollutanttype=="airtoxics")
png("Buffer Max scatterplotloglogtoxics2011.png", width=10, height=7, units="in", pointsize=12, res=300, type=c("cairo"))
scatterPlot(maxtox, x="obs", y="mod", method="scatter", group="substance", pch=c(1:25, 33),type="default", simplify.names=FALSE,
            mod.line=TRUE, linear=TRUE, log.x=TRUE, log.y=TRUE, xlab="observed", ylab="modeled", 
            main="Log-Log Plot of Measured and Minimum Modeled Criteria Air Pollutants within a 5km Buffer")
dev.off()

##Minimum plots split criteria and toxics
mincrit=subset(minimum, airpollutanttype=="criteria")
png("Buffer Min scatterplotloglogcriteria2011.png", width=10, height=7, units="in", pointsize=12, res=300, type=c("cairo"))
scatterPlot(mincrit, x="obs", y="mod", method="scatter", group="substance", type="default", simplify.names=FALSE,
            mod.line=TRUE, linear=TRUE, log.x=TRUE, log.y=TRUE, xlab="observed", ylab="modeled", pch=1:5,
            main="Log-Log Plot of Measured and Modeled Criteria Air Pollutants at the Closest MNRiskS Receptors")
dev.off()

mintox=subset(minimum, airpollutanttype=="airtoxics")
png("Buffer Min scatterplotloglogtoxics2011.png", width=10, height=7, units="in", pointsize=12, res=300, type=c("cairo"))
scatterPlot(mintox, x="obs", y="mod", method="scatter", group="substance", pch=c(1:25, 33),type="default", simplify.names=FALSE,
            mod.line=TRUE, linear=TRUE, log.x=TRUE, log.y=TRUE, xlab="observed", ylab="modeled", 
            main="Log-Log Plot of Measured and Minimum Modeled Air Toxics Pollutants within a 5km Buffer")
dev.off()

##################################################
##Pollutant Specific Plots Modeled vs Monitored
##################################################
groups <- unique(average$substance)

pdf("Single Pollutant Modeled vs. Observed Plots_Average Modeled with 5km.pdf")
for(i in groups){
  pahs= subset(average, substance==i)
  pahs_lm <- lm(obs~mod, data=pahs)
  pahs_lm_text <- paste("RSquared = ", round(summary(pahs_lm)$r.squared, digits=2), "P Value = ", ifelse(anova(pahs_lm)$'Pr(>F)'[1] < 0.01, "<0.01", round(anova(pahs_lm)$'Pr(>F)'[1], digits=2)))
  p = ggplot(data=pahs, aes(x = obs, y = mod)) +
    geom_point() +
    geom_smooth(method="lm") +
    geom_abline(intercept=0, slope=1, linetype=2) +
    geom_text(aes(label=site),hjust=0, vjust=0, size=2) +
    xlab(paste("observed ug/m3, ", pahs_lm_text)) +
    ylab("modeled ug/m3") +
    xlim(min(c(pahs$mod, pahs$obs)), max(c(pahs$mod, pahs$obs))) +
    ylim(min(c(pahs$mod, pahs$obs)), max(c(pahs$mod, pahs$obs))) +
    ggtitle(paste("Model Monitor Scatter Plot for", i))
  print(p)
  print(i)
}
dev.off()


##Histogram Plots  ##write histograms to graphic files

png("histograms for buffers.png", width=10, height=7, units="in", pointsize=12, res=300, type=c("cairo"))
par(mfrow=c(2, 3))
hist(average$mod)
hist(maximum$mod)
hist(minimum$mod)
hist(average$obs)
hist(minimum$obs)
hist(maximum$obs)
dev.off()

##Model Monitor Comparison Statistics

avgmodmonstats<-modStats(average, mod="mod", obs="obs", type ="substance")
minmodmonstats<-modStats(minimum, mod="mod", obs="obs", type ="substance")
maxmodmonstats<-modStats(maximum, mod="mod", obs="obs", type ="substance")

##calculate means and standard deviations and mod-obs averages for merging with modmonstats for AVERAGES of buffers
avg_summaries <-group_by(average, substance) %>% mutate(avgmodmeans = mean(mod, na.rm=T), avgobsmeans = mean(obs, na.rm=T), avgmodstdev = sd(mod, na.rm=T), avgobsstdev = sd(obs, na.rm=T), modobsavg = mean((obs + mod)/2), Counts=n()) %>% ungroup()
                  
#################################
##Calculate the kendal tau b
#Remove rows less than 3
#################################
data <- filter(avg_summaries, Counts>2, !is.na(mod) & !is.na(obs))

#Run Kendall
avgkendall=by(data, data$substance, function(x) Kendall(x$obs, x$mod))

#Above creates a giant list, use below to transform into dataframe
avgkendall=t(sapply(avgkendall, function(x)x))

#Create row names as substances
pollutants<-row.names(avgkendall)

#Save row names as substance names
avgkendall<- data.frame(pollutants,avgkendall)

##make all kendall parameters numeric
tau=avgkendall$tau=as.numeric(avgkendall$tau)
sl=avgkendall$sl=as.numeric(avgkendall$sl)
varS=avgkendall$varS=as.numeric(avgkendall$varS)
D=avgkendall$D=as.numeric(avgkendall$D)
S=avgkendall$S=as.numeric(avgkendall$S)
avgkendall2=cbind(pollutants, tau, sl, varS, D, S)
##make it a data frame again
avgkendall2=data.frame(avgkendall2)

##Merge all of the columns into one data.frame
avg_summaries_kendall <- merge(avg_summaries, avgkendall2, by.x="substance", by.y="pollutants", all=TRUE, sort=FALSE)
avg_summaries_kendall_modmon <- merge(avg_summaries_kendall, avgmodmonstats, by = "substance", all=TRUE, sort=FALSE)
avg_summaries_kendall_modmon <- unique(avg_summaries_kendall_modmon)

##Divide Mean Bias by the mod+obs/2 value for fractional bias
avg_summaries_kendall_modmon <- mutate(avg_summaries_kendall_modmon, FrxBias = ((MB/modobsavg)*100))

##write model monitor stats to a csv file
write.csv(avg_summaries_kendall_modmon, file="avgmodmonstatsall.csv", row.names=TRUE)


##############################################################################
##calculate means and standard deviations and mod-obs averages for merging with modmonstats for MAXIMUM WITHIN buffers
##############################################################################

##calculate means and standard deviations and mod-obs averages for merging with modmonstats for AVERAGES of buffers
max_summaries <-group_by(maximum, substance) %>% mutate(maxmodmeans = mean(mod, na.rm=T), maxobsmeans = mean(obs, na.rm=T), maxmodstdev = sd(mod, na.rm=T), maxobsstdev = sd(obs, na.rm=T), modobsavg = mean((obs + mod)/2), Counts=n()) %>% ungroup()

#################################
##Calculate the kendal tau b
#Remove rows less than 3
#################################
data <- filter(max_summaries, Counts>2, !is.na(mod) & !is.na(obs))

#Run Kendall
maxkendall=by(data, data$substance, function(x) Kendall(x$obs, x$mod))

#Above creates a giant list, use below to transform into dataframe
maxkendall=t(sapply(maxkendall, function(x)x))

#Create row names as substances
pollutants<-row.names(maxkendall)

#Save row names as substance names
maxkendall<- data.frame(pollutants,maxkendall)

##make all kendall parameters numeric
tau=maxkendall$tau=as.numeric(maxkendall$tau)
sl=maxkendall$sl=as.numeric(maxkendall$sl)
varS=maxkendall$varS=as.numeric(maxkendall$varS)
D=maxkendall$D=as.numeric(maxkendall$D)
S=maxkendall$S=as.numeric(maxkendall$S)
maxkendall2=cbind(pollutants, tau, sl, varS, D, S)
##make it a data frame again
maxkendall2=data.frame(maxkendall2)

##Merge all of the columns into one data.frame
max_summaries_kendall <- merge(max_summaries, maxkendall2, by.x="substance", by.y="pollutants", all=TRUE, sort=FALSE)
max_summaries_kendall_modmon <- merge(max_summaries_kendall, maxmodmonstats, by = "substance", all=TRUE, sort=FALSE)
max_summaries_kendall_modmon <- unique(max_summaries_kendall_modmon)

##Divide Mean Bias by the mod+obs/2 value for fractional bias
max_summaries_kendall_modmon <- mutate(max_summaries_kendall_modmon, FrxBias = ((MB/modobsavg)*100))

##write model monitor stats to a csv file
write.csv(max_summaries_kendall_modmon, file="maxmodmonstatsall.csv", row.names=TRUE)


#############################################################################
##calculate means and standard deviations and mod-obs averages for merging with modmonstats for MINIMUM WITHIN buffers
#############################################################################

##calculate means and standard deviations and mod-obs averages for merging with modmonstats for AVERAGES of buffers
min_summaries <-group_by(minimum, substance) %>% mutate(minmodmeans = mean(mod, na.rm=T), minobsmeans = mean(obs, na.rm=T), minmodstdev = sd(mod, na.rm=T), minobsstdev = sd(obs, na.rm=T), modobsavg = mean((obs + mod)/2), Counts=n()) %>% ungroup()

#################################
##Calculate the kendal tau b
#Remove rows less than 3
#################################
data <- filter(min_summaries, Counts>2, !is.na(mod) & !is.na(obs))

#Run Kendall
minkendall=by(data, data$substance, function(x) Kendall(x$obs, x$mod))

#Above creates a giant list, use below to transform into dataframe
minkendall=t(sapply(minkendall, function(x)x))

#Create row names as substances
pollutants<-row.names(minkendall)

#Save row names as substance names
minkendall<- data.frame(pollutants,minkendall)

##make all kendall parameters numeric
tau=minkendall$tau=as.numeric(minkendall$tau)
sl=minkendall$sl=as.numeric(minkendall$sl)
varS=minkendall$varS=as.numeric(minkendall$varS)
D=minkendall$D=as.numeric(minkendall$D)
S=minkendall$S=as.numeric(minkendall$S)
minkendall2=cbind(pollutants, tau, sl, varS, D, S)
##make it a data frame again
minkendall2=data.frame(minkendall2)

##Merge all of the columns into one data.frame
min_summaries_kendall <- merge(min_summaries, minkendall2, by.x="substance", by.y="pollutants", all=TRUE, sort=FALSE)
min_summaries_kendall_modmon <- merge(min_summaries_kendall, minmodmonstats, by = "substance", all=TRUE, sort=FALSE)
min_summaries_kendall_modmon <- unique(min_summaries_kendall_modmon)

##Divide Mean Bias by the mod+obs/2 value for fractional bias
min_summaries_kendall_modmon <- mutate(min_summaries_kendall_modmon, FrxBias = ((MB/modobsavg)*100))

##write model monitor stats to a csv file
write.csv(min_summaries_kendall_modmon, file="minmodmonstatsall.csv", row.names=TRUE)

##############################
##Chart to Compare Pollutants
##############################

png("pollutantcomparison.png", width=10, height=7, units="in", pointsize=12, res=300, type=c("cairo"))
pol <- avg_summaries_kendall_modmon
pol <- mutate(pol, model_over_monitor_ratio = mod/obs)

pol$Color <- ifelse(pol$model_over_monitor_ratio >= 2, "> 200%", 
                    ifelse(pol$model_over_monitor_ratio > .5, "similar", 
                           "< 50%")) 

pol$Color <- factor(pol$Color, levels=c("< 50%", "similar", "> 200%"))

ggplot(pol, aes(x=model_over_monitor_ratio*100, y=reorder(substance, model_over_monitor_ratio))) +
  geom_point(size = 5.5, 
             aes(color=Color), 
             alpha=.25) + 
  geom_vline(xintercept = c(200, 50), linetype = 2) +
  scale_x_log10() +
  theme_bw() +
  theme(legend.position = 'bottom') +
  scale_color_manual(name = " Model / Monitor Ratio (%)", 
                     values=c("steelblue", "grey40", "darkred"),
                     guide=guide_legend(nrow=1, title.position = "top")) +
  labs(title = "Model / Monitor Ratio", 
       subtitle = "Air Toxics and Criteria Pollutants",
       x="", y="")
dev.off()


################################################
##Save file with just stats, no initial values
################################################
avg_summaries_kendall_modmon <- avg_summaries_kendall_modmon[, c("substance", "CAS", "airpollutanttype", "avgmodmeans", "avgobsmeans", "avgmodstdev", "avgobsstdev", "modobsavg", "Counts", "tau", "sl", "varS", "D", "S", "n", "FAC2", "MB", "MGE", "NMB", "NMGE", "RMSE", "r", "COE", "IOA", "FrxBias")]
avg_summaries_kendall_modmon <- unique(avg_summaries_kendall_modmon)
write.csv(avg_summaries_kendall_modmon, file="avgmodmonstatsall_noinitialvalues.csv", row.names=TRUE)

max_summaries_kendall_modmon <- max_summaries_kendall_modmon[, c("substance", "CAS", "airpollutanttype", "maxmodmeans", "maxobsmeans", "maxmodstdev", "maxobsstdev", "modobsavg", "Counts", "tau", "sl", "varS", "D", "S", "n", "FAC2", "MB", "MGE", "NMB", "NMGE", "RMSE", "r", "COE", "IOA", "FrxBias")]
max_summaries_kendall_modmon <- unique(max_summaries_kendall_modmon)
write.csv(max_summaries_kendall_modmon, file="maxmodmonstatsall_noinitialvalues.csv", row.names=TRUE)

min_summaries_kendall_modmon <- min_summaries_kendall_modmon[, c("substance", "CAS", "airpollutanttype", "minmodmeans", "minobsmeans", "minmodstdev", "minobsstdev", "modobsavg", "Counts", "tau", "sl", "varS", "D", "S", "n", "FAC2", "MB", "MGE", "NMB", "NMGE", "RMSE", "r", "COE", "IOA", "FrxBias")]
min_summaries_kendall_modmon <- unique(min_summaries_kendall_modmon)
write.csv(min_summaries_kendall_modmon, file="minmodmonstatsall_noinitialvalues.csv", row.names=TRUE)

