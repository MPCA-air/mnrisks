##### Model Monitor Data Analysis 

##Load (way too many) Packages
library(openair)
library(ggplot2)
library(dplyr)
library(Kendall)
library(NADA)

###Plot Data by Pollutant
setwd("M:/KME Files/Model Monitor/2011_AllSources/Statistics/Data Tables and Processing/2011 Analysis for Closest Air Monitors to MNRiskS Receptors")

closest<-read.csv("M:/KME Files/Model Monitor/2011_AllSources/Statistics/Data Tables and Processing/allmeasurements_receptors_closest.csv", header=TRUE, stringsAsFactors=F)

closest$substance=as.character(closest$substance)
#closest=na.omit(closest)
closest <- filter(closest, !is.na(mod) & !is.na(obs), !substance %in% "Copper")

groups <- unique(closest$substance)

pdf("Single Pollutant Modeled vs. Observed Plots_Closest Receptors_BIGSCALE.pdf")
for(i in groups){
  pahs= subset(closest, substance==i)
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

#################################################
# Read in Cleaned Data Sets
# All missing values have been removed
##################################################

png("scatterplotloglog2011.png", width=10, height=7, units="in", pointsize=12, res=300, type=c("cairo"))
scatterPlot(closest, x="obs", y="mod", method="scatter", group="substance", pch=c(1:25, 33),type="default", 
            mod.line=TRUE, linear=TRUE, log.x=TRUE, log.y=TRUE, xlab="observed", ylab="modeled", simplify.names=FALSE,
            main="Log-Log Plot of Measured and Modeled Air Pollutants at the Closest MNRiskS Receptors")
dev.off()

##plots split criteria and toxics
closestcrit=subset(closest, airpollutanttype=="criteria")
png("scatterplotloglogcriteria2011.png", width=10, height=7, units="in", pointsize=12, res=300, type=c("cairo"))
scatterPlot(closestcrit, x="obs", y="mod", method="scatter", group="substance", type="default", simplify.names=FALSE,
            mod.line=TRUE, linear=TRUE, log.x=TRUE, log.y=TRUE, xlab="observed", ylab="modeled", pch=1:5,
            main="Log-Log Plot of Measured and Modeled Criteria Air Pollutants at the Closest MNRiskS Receptors")
dev.off()

closesttox=subset(closest, airpollutanttype=="airtoxics")
png("scatterplotloglogtoxics2011.png", width=10, height=7, units="in", pointsize=12, res=300, type=c("cairo"))
scatterPlot(closesttox, x="obs", y="mod", method="scatter", group="substance", pch=c(1:21),type="default", simplify.names=FALSE,
            mod.line=TRUE, linear=TRUE, log.x=TRUE, log.y=TRUE, xlab="observed", ylab="modeled", 
            main="Log-Log Plot of Measured and Modeled Air Toxics Pollutants at the Closest MNRiskS Receptors")
dev.off()


##Histogram Plots
png("Histogram MNRiskS 2011.png", width=10, height=7, units="in", pointsize=12, res=300, type=c("cairo"))
hist(closest$mod)
dev.off()

png("Histogram Measured 2011.png", width=10, height=7, units="in", pointsize=12, res=300, type=c("cairo"))
hist(closest$obs)
dev.off()

#######################################
##Model Monitor Comparison Statistics
#######################################

closestmodmon2011<-modStats(closest, mod="mod", obs="obs", type ="substance")

##calculate means and standard deviations and mod-obs averages for merging with modmonstats for closest receptors to a monitor
closest_summaries <-group_by(closest, substance) %>% mutate(closestmodmeans = mean(mod, na.rm=T), closestobsmeans = mean(obs, na.rm=T), closestmodstdev = sd(mod, na.rm=T), closestobsstdev = sd(obs, na.rm=T), modobsavg = mean((obs + mod)/2), Counts=n()) %>% ungroup()

#################################
##Calculate the kendal tau b
#Remove rows less than 3
#################################
data <- filter(closest_summaries, Counts>2, !is.na(mod) & !is.na(obs))

#Run Kendall
closestkendall=by(data, data$substance, function(x) Kendall(x$obs, x$mod))

#Above creates a giant list, use below to transform into dataframe
closestkendall=t(sapply(closestkendall, function(x)x))

#Create row names as substances
pollutants<-row.names(closestkendall)

#Save row names as substance names
closestkendall<- data.frame(pollutants,closestkendall)

##make all kendall parameters numeric
tau=closestkendall$tau=as.numeric(closestkendall$tau)
sl=closestkendall$sl=as.numeric(closestkendall$sl)
varS=closestkendall$varS=as.numeric(closestkendall$varS)
D=closestkendall$D=as.numeric(closestkendall$D)
S=closestkendall$S=as.numeric(closestkendall$S)
closestkendall2=cbind(pollutants, tau, sl, varS, D, S)
##make it a data frame again
closestkendall2=data.frame(closestkendall2)

##Merge all of the columns into one data.frame
closest_summaries_kendall <- merge(closest_summaries, closestkendall2, by.x="substance", by.y="pollutants", all=TRUE, sort=FALSE)
closest_summaries_kendall_modmon <- merge(closest_summaries_kendall, closestmodmon2011, by = "substance", all=TRUE, sort=FALSE)
closest_summaries_kendall_modmon <- unique(closest_summaries_kendall_modmon)

##Divide Mean Bias by the mod+obs/2 value for fractional bias
closest_summaries_kendall_modmon <- mutate(closest_summaries_kendall_modmon, FrxBias = ((MB/modobsavg)*100))

##write model monitor stats to a csv file
write.csv(closest_summaries_kendall_modmon, file="closestmodmonstatsall.csv", row.names=TRUE)

##############################
##Chart to Compare Pollutants
##############################

png("pollutantcomparison.png", width=10, height=7, units="in", pointsize=12, res=300, type=c("cairo"))
pol <- closest_summaries_kendall_modmon
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


##Write model monitor comparison stats to csv file without original mod and obs averages
closest_summaries_kendall_modmon <- closest_summaries_kendall_modmon[, c("substance", "CAS", "airpollutanttype", "closestmodmeans", "closestobsmeans", "closestmodstdev", "closestobsstdev", "modobsavg", "Counts", "tau", "sl", "varS", "D", "S", "n", "FAC2", "MB", "MGE", "NMB", "NMGE", "RMSE", "r", "COE", "IOA", "FrxBias")]
closest_summaries_kendall_modmon <- unique(closest_summaries_kendall_modmon)
write.csv(closest_summaries_kendall_modmon, file="closestmodmonstatsall_noinitialvalues.csv", row.names=TRUE)
