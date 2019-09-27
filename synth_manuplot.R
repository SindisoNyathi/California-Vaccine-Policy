#Manuscript Plots
setwd("~/Box/Sindiso Nyathi's Files/California Vaccine Policy")
additional_label <- "Manuscript Plots"
require(ggplot2)
require(gridExtra)

#Main Outcome Plots. Trajectory of Synthetic Control and Treated Unit.
{
mmr <- read.csv("Data Files/Placebo Synthetic Controls. MMR.csv")
med <- read.csv("Data Files/Placebo Synthetic Controls. Medical.csv")
nme <- read.csv("Data Files/Placebo Synthetic Controls. Non.Medical.csv")

mmr <- mmr[mmr$Unit == "California", c(2:5)]
med <- med[med$Unit == "California", c(2:5)]
nme <- nme[nme$Unit == "California", c(2:5)]


mmr <- melt(mmr, id.vars = c("Year", "Unit"), direction = "long")
med <- melt(med, id.vars = c("Year", "Unit"), direction = "long")
nme <- melt(nme, id.vars = c("Year", "Unit"), direction = "long")

mmr$variable <- as.character(mmr$variable)
mmr$variable[which(mmr$variable == "Treated")] <- "Treated California"
mmr$variable[which(mmr$variable == "Synthetic")] <- "Synthetic California"

med$variable <- as.character(med$variable)
med$variable[which(med$variable == "Treated")] <- "Treated California"
med$variable[which(med$variable == "Synthetic")] <- "Synthetic California"


nme$variable <- as.character(nme$variable)
nme$variable[which(nme$variable == "Treated")] <- "Treated California"
nme$variable[which(nme$variable == "Synthetic")] <- "Synthetic California"

#Create the GG plots
mmr_plot <- ggplot(mmr, aes(x = Year, y = value, colour = variable)) +
  geom_point(aes(colour = mmr$variable), size = 1.75) +
  geom_line(aes(colour = mmr$variable), size = 1.25) +
  theme_linedraw() +
  labs(title = "MMR Coverage") +
  xlab("Year") +
  ylab("Percentage (%)") +
  ylim(c(90, 100)) +
  scale_x_discrete(limits = c(2011:2017)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.title = element_blank(), 
        axis.text = element_text(size=12),
        axis.title.x = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size = 14), 
        plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
        legend.text = element_text(size = 14)) +
  geom_vline(xintercept = 2015, linetype = 2, color = "blue", size = 0.5) +
  guides(fill = F)

med_plot <- ggplot(med, aes(x = Year, y = value, colour = variable)) +
  geom_point(aes(colour = med$variable), size = 1.75) +
  geom_line(aes(colour = med$variable), size = 1.25) +
  theme_linedraw() +
  labs(title = "Medical Exemptions") +
  xlab("Year") +
  ylab("Percentage (%)") +
  ylim(c(0, 10)) +
  scale_x_discrete(limits = c(2011:2017)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.title = element_blank(), 
        axis.text = element_text(size=12),
        axis.title.x = element_text(size = 14), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size = 14),
        plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
        legend.position = "none") +
  geom_vline(xintercept = 2015, linetype = 2, color = "blue", size = 0.5)

nme_plot <- ggplot(nme, aes(x = Year, y = value, colour = variable)) +
  geom_point(aes(colour = nme$variable), size = 1.75) +
  geom_line(aes(colour = nme$variable), size = 1.25) +
  theme_linedraw() +
  labs(title = "Non-medical Exemptions") +
  xlab("Year") +
  ylab("Percentage (%)") +
  ylim(c(0, 10)) +
  scale_x_discrete(limits = c(2011:2017)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.title = element_blank(), 
        axis.text = element_text(size=12),
        axis.title.x = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size = 14), 
        plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
        legend.position = "none") +
  geom_vline(xintercept = 2015, linetype = 2, color = "blue", size = 0.5)


setEPS()
postscript(paste("Plots/(", additional_label, ") Synthetic Control vs. Treated Unit (Raw).eps", sep = ""),  width = 13, height = 10)
grid.arrange(mmr_plot, med_plot, nme_plot, 
             widths = c(1, 1, 1, 1, 1, 1, 1, 1), 
             layout_matrix = rbind(c(NA, 1, 1, 1, 1, 1, 1, 1), c(3, 3, 3, 3, 2, 2, 2, 2)), 
             top = grid::textGrob("Treated California vs. Synthetic California", gp = grid::gpar(fontsize = 22)))
dev.off()
}     

#MWeights.
{
  mmr <- read.csv("Data Files/Control Weights. MMR.csv")
  med <- read.csv("Data Files/Control Weights. Medical.csv")
  nme <- read.csv("Data Files/Control Weights. Non.Medical.csv")
  
  #Create the GG plots 
  
  mmr_plot <- ggplot(mmr, aes(x = Name, y = Weight)) +
    geom_bar(stat = "Identity", fill = "steelblue4") +
    theme_linedraw() +
    labs(title = "MMR Coverage") +
    xlab("State") +
    ylab("Weight") +
    ylim(c(0, 0.75)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_blank(), 
          axis.text = element_text(size=12),
          axis.title.x = element_text(size = 14), 
          axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.y = element_text(size = 14), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.text = element_text(size = 14))
  
  med_plot <- ggplot(med, aes(x = Name, y = Weight)) +
    geom_bar(stat = "Identity", fill = "steelblue4") +
    theme_linedraw() +
    labs(title = "Medical Exemptions") +
    xlab("State") +
    ylab("Weight") +
    ylim(c(0, 0.75)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_blank(), 
          axis.text = element_text(size=12), 
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.x = element_text(size = 14), 
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.y = element_text(size = 14),
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.position = "none")
  
  nme_plot <- ggplot(nme, aes(x = Name, y = Weight)) +
    geom_bar(stat = "Identity", fill = "steelblue4") +
    theme_linedraw() +
    labs(title = "Non-medical Exemptions") +
    xlab("State") +
    ylab("Weight") +
    ylim(c(0, 0.75)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_blank(), 
          axis.text = element_text(size=12),
          axis.title.x = element_text(size = 14),
          axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.y = element_text(size = 14), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.position = "none")
  
  
  setEPS()
  postscript(paste("Plots/(", additional_label, ") Control Weights.eps", sep = ""),  width = 13, height = 10)
  grid.arrange(mmr_plot, med_plot, nme_plot, 
               widths = c(1, 1, 1, 1, 1, 1, 1, 1), 
               layout_matrix = rbind(c(NA, 1, 1, 1, 1, 1, 1, NA), c(3, 3, 3, 3, 2, 2, 2, 2)), 
               top = grid::textGrob("Synthetic California Weights", gp = grid::gpar(fontsize = 22)))
  dev.off()
} 

#Placebo Difference Plots
{
mmr <- read.csv("Data Files/Placebo Synthetic Controls. MMR.csv")
med <- read.csv("Data Files/Placebo Synthetic Controls. Medical.csv")
nme <- read.csv("Data Files/Placebo Synthetic Controls. Non.Medical.csv")
get_effectsize <- function(placebo){
  
  placebo[,6] <- placebo[, 4] - placebo[, 5]
  placebo <- placebo[,c(-1, -4, -5, -7, -8)]
  
  placebo2 <- reshape(placebo, idvar = c("Unit"), timevar = "Year", direction = "wide")
  placebo <- placebo2[,c(-2, -3, -4)]
  placebo[,6] <- (placebo[,2] + placebo[,3])/2
  placebo[,7] <- (placebo[,4] + placebo[,5])/2
  
  placebo <- placebo[,c(1, 6, 7)]
  placebo[,4] <- placebo[,3] - placebo[,2] #Post - Pre
  colnames(placebo) <- c("Unit", "Pre", "Post", "Dif")
  
  
  placebo <- placebo[order(placebo$Dif),]
  
  return(placebo)
}

mmr <- get_effectsize(mmr)
med <- get_effectsize(med)
nme <- get_effectsize(nme)

mmr_plot <- ggplot(mmr, aes(x = reorder(Unit, -Dif), y = Dif )) +
  geom_bar(stat = "identity", fill = "steelblue4") +
  theme_linedraw() +
  labs(title = "MMR Coverage") +
  ylab("Difference (%)") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.title = element_blank(), 
        axis.text = element_text(size=12),
        panel.grid.major.x = element_blank(),
        #panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle=90, hjust = 1),
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        legend.text = element_text(size=14))

med_plot <- ggplot(med, aes(x = reorder(Unit, -Dif), y = Dif )) +
  geom_bar(stat = "identity", fill = "steelblue4") +
  theme_linedraw() +
  labs(title = "Medical Exemptions") +
  ylab("Difference (%)") +
  xlab("") +
  ylim(c(-3, 2.25)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.title = element_blank(), 
        plot.subtitle = element_text(hjust = 0.5, size = 14), 
        axis.text = element_text(size=12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle=90, hjust = 1),
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        legend.text = element_text(size=14))

nme_plot <- ggplot(nme, aes(x = reorder(Unit, -Dif), y = Dif )) +
  geom_bar(stat = "identity", fill = "steelblue4") +
  theme_linedraw() +
  labs(title = "Non-medical Exemptions") +
  ylab("Difference (%)") +
  xlab("") +
  ylim(c(-3, 2.25)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        legend.title = element_blank(), 
        plot.subtitle = element_text(hjust = 0.5, size = 14), 
        axis.text = element_text(size=12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle=90, hjust = 1),
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        legend.text = element_text(size=14))

setEPS()
postscript("Plots/(Manuscript Plots) Synthetic Control vs. Treated Unit (PrePost Diff).eps",  width = 15, height = 12)
grid.arrange(mmr_plot, med_plot, nme_plot, 
             widths = c(1, 1, 1, 1, 1, 1, 1, 1), 
             layout_matrix = rbind(c(NA, 1, 1, 1, 1, 1, 1, NA), c(3, 3, 3, 3, 2, 2, 2, 2)), 
             top = grid::textGrob("Placebo Tests Effect Sizes", gp = grid::gpar(fontsize = 22)))
dev.off()
}

Can you make these vertical instead of horizontal?
#Supplementary Material Plots.

#1. Variable Selection
{
#Read in the files.
mmr <- read.csv("Data Files/(Cross Validation) Stepwise Variable Selection. MMR.csv")
nme <- read.csv("Data Files/(Cross Validation) Stepwise Variable Selection. Non.Medical.csv")
med <- read.csv("Data Files/(Cross Validation) Stepwise Variable Selection. Medical.csv")

mmr <- mmr[, c(3, 5, 7)]
nme <- nme[, c(3, 5, 7)]
med <- med[, c(3, 5, 7)]

mmr_plot <- ggplot(mmr, aes(x = reorder(CovariateName, Number), y = Test)) +
  geom_point(color = "black") +
  geom_line(color = "black", aes(group = 1)) + 
  theme_bw() +
  labs(title = "MMR Coverage") +
  xlab("Covariate Added") +
  ylab("Test RMSPE") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_blank(), 
        plot.subtitle = element_text(hjust = 0.5, size = 18), 
        axis.text = element_text(size=12),
        axis.text.x = element_text(angle=90, hjust = 1),
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        legend.text = element_text(size=18)) + 
  geom_vline(xintercept = 4, linetype = 2, color = "blue", size = 0.5)
plot(mmr_plot)

nme_plot <- ggplot(nme, aes(x = reorder(CovariateName, Number), y = Test)) +
  geom_point(color = "black") +
  geom_line(color = "black", aes(group = 1)) + 
  theme_bw() +
  labs(title = "Non-medical Exemptions") +
  xlab("Covariate Added") +
  ylab("Test RMSPE") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_blank(), 
        plot.subtitle = element_text(hjust = 0.5, size = 18), 
        axis.text = element_text(size=12),
        axis.text.x = element_text(angle=90, hjust = 1),
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        legend.text = element_text(size=18)) + 
  geom_vline(xintercept = 4, linetype = 2, color = "blue", size = 0.5)
plot(nme_plot)


med_plot <- ggplot(med, aes(x = reorder(CovariateName, Number), y = Test)) +
  geom_point(color = "black") +
  geom_line(color = "black", aes(group = 1)) + 
  theme_bw() +
  labs(title = "Medical Exemptions") +
  xlab("Covariate Added") +
  ylab("Test RMSPE") +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_blank(), 
        plot.subtitle = element_text(hjust = 0.5, size = 18), 
        axis.text = element_text(size=12),
        axis.text.x = element_text(angle=90, hjust = 1),
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14), 
        legend.text = element_text(size=18)) + 
  geom_vline(xintercept = 2, linetype = 2, color = "blue", size = 0.5)
plot(med_plot)


setEPS()
postscript("Plots/(Manuscript Plots) Stepwise Variable Selection.eps",  width = 15, height = 12)
grid.arrange(mmr_plot, med_plot, nme_plot, 
             widths = c(1, 1, 1, 1, 1, 1, 1, 1), 
             layout_matrix = rbind(c(NA, 1, 1, 1, 1, 1, 1, NA), c(3, 3, 3, 3, 2, 2, 2, 2)), 
             top = grid::textGrob("Stepwise Variable Selection", gp = grid::gpar(fontsize = 22)))
dev.off()
}

#2. Cross Validation
{
  ##Vaccination_data can be the state data or the county data.
  vaccination_data <- read.csv("Data Files/AggSynthCon.csv")
  intervention_year <- "2013" #Last year before intervention
  treated_unit <- "California"
  start_year <- 2011
  end_year <- 2015
  
  ##Outcomes are "MMR", "Medical", " and "Non-Medical".
  #Set the outcome, the lag and the covariate combination.
  mmr_preds <- c('MedAge', 'P_NoPrev', 'P_UnInsured', 'P_PrivIns') 
  med_preds <- c('P_PrivIns', 'PerCapHealth', 'P_BelPov')
  nmed_preds <- c('MedAge', 'Pop', 'P_BS', 'P_NoCov')
  
  
  #Run the Synthetic Control for the set Variables.
  mmr_out <- synth_unit(vaccination_data, treated_unit, "MMR",
                        intervention_year, c("AveLagMMR", mmr_preds), start_year, end_year)
  nme_out <- synth_unit(vaccination_data, treated_unit, "Non.Medical",
                        intervention_year, c("AveLagNMed", nmed_preds), start_year, end_year)
  med_out <- synth_unit(vaccination_data, treated_unit, "Medical",
                        intervention_year, c("AveLagMed", med_preds), start_year, end_year)
  
  mmr_out <- as.data.frame(mmr_out[[1]])
  nme_out <- as.data.frame(nme_out[[1]])
  med_out <- as.data.frame(med_out[[1]])
  
  colnames(mmr_out)[c(2,3)] <- c("Treated California", "Synthetic California")
  colnames(nme_out)[c(2,3)] <- c("Treated California", "Synthetic California")
  colnames(med_out)[c(2,3)] <- c("Treated California", "Synthetic California")
  
  mmr_out <- melt(mmr_out, id.vars = "Year", direction = "long")
  nme_out <- melt(nme_out, id.vars = "Year", direction = "long")
  med_out <- melt(med_out, id.vars = "Year", direction = "long")
  
  
  mmr_plot <- ggplot(mmr_out, aes(x = Year, y = value, colour = Synth)) +
    geom_point(aes(colour = mmr_out$variable), size = 1.75) +
    geom_line(aes(colour = mmr_out$variable), size = 1.25) +
    theme_bw() +
    labs(title = "MMR Coverage") +
    xlab("Year") +
    ylab("Percentage (%)") +
    ylim(c(90, 100)) +
    scale_x_discrete(limits = c(start_year:end_year)) +
    theme(plot.title = element_text(hjust = 0.5, size = 18),
          legend.title = element_blank(),  
          axis.text = element_text(size=14),
          axis.title.x = element_text(size = 16), 
          axis.title.y = element_text(size = 16), 
          legend.text = element_text(size=16)) +
    geom_vline(xintercept = 2013, linetype = 2, color = "blue", size = 0.5)
  
  nme_plot <- ggplot(nme_out, aes(x = Year, y = value, colour = Synth)) +
    geom_point(aes(colour = nme_out$variable), size = 1.75) +
    geom_line(aes(colour = nme_out$variable), size = 1.25) +
    theme_bw() +
    labs(title = "Non-medical Exemptions") +
    xlab("Year") +
    ylab("Percentage (%)") +
    ylim(c(0, 4)) +
    scale_x_discrete(limits = c(start_year:end_year)) +
    theme(plot.title = element_text(hjust = 0.5, size = 18),
          legend.title = element_blank(),  
          axis.text = element_text(size=14),
          axis.title.x = element_text(size = 16), 
          axis.title.y = element_text(size = 16), 
          legend.position = "none") +
    geom_vline(xintercept = 2013, linetype = 2, color = "blue", size = 0.5)
  
  med_plot <- ggplot(med_out, aes(x = Year, y = value, colour = Synth)) +
    geom_point(aes(colour = med_out$variable), size = 1.75) +
    geom_line(aes(colour = med_out$variable), size = 1.25) +
    theme_bw() +
    labs(title = "Medical Exemptions") +
    xlab("Year") +
    ylab("Percentage (%)") +
    ylim(c(0, 4)) +
    scale_x_discrete(limits = c(start_year:end_year)) +
    theme(plot.title = element_text(hjust = 0.5, size = 18),
          legend.title = element_blank(),  
          axis.text = element_text(size=14),
          axis.title.x = element_text(size = 16), 
          axis.title.y = element_text(size = 16), 
          legend.position = "none") +
    geom_vline(xintercept = 2013, linetype = 2, color = "blue", size = 0.5)
  
  setEPS()
  postscript(paste("Plots/(", additional_label, ") Cross Validation.eps", sep = ""),  width = 13, height = 10)
  grid.arrange(mmr_plot, med_plot, nme_plot, 
               widths = c(1, 1, 1, 1, 1, 1, 1, 1), 
               layout_matrix = rbind(c(NA, 1, 1, 1, 1, 1, 1, 1), c(3, 3, 3, 3, 2, 2, 2, 2)), 
               top = grid::textGrob("Cross Validation", gp = grid::gpar(fontsize = 22)))
  dev.off()
}

#3. Placebo Plots
{
#Now qw have syn values, which is basically, plabebo controls. Save this file.
mmr <- read.csv("Data Files/Placebo Synthetic Controls. MMR.csv")
nme <- read.csv("Data Files/Placebo Synthetic Controls. Non.Medical.csv")
med <- read.csv("Data Files/Placebo Synthetic Controls. Medical.csv")

mmr <- mmr[,1:5]
nme <- nme[,1:5]
med <- med[,1:5]

mmr[,6] <- mmr[,4] - mmr[, 5]
colnames(mmr)[6] <- "Gap"

nme[,6] <- nme[,4] - nme[, 5]
colnames(nme)[6] <- "Gap"

med[,6] <- med[,4] - med[, 5]
colnames(med)[6] <- "Gap"

mmr$Treat <- "Control States"
mmr$Treat[which(mmr$Unit == "California")] <- "Treated (California)"

nme$Treat <- "Control States"
nme$Treat[which(nme$Unit == "California")] <- "Treated (California)"

med$Treat <- "Control States"
med$Treat[which(med$Unit == "California")] <- "Treated (California)"

#filter controls
cali <- subset(mmr, Unit == "California")
mmr_plot <- ggplot(mmr, aes(x = Year, y = Gap, group = Unit, color = Treat)) +
  geom_point() +
  geom_line()+
  theme_bw() +
  labs(title = "MMR Coverage") +
  xlab("Year") +
  ylab("Difference (%)") +
  ylim(c(-10, 10)) +
  scale_x_discrete(limits = c(2011:2017)) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_blank(), 
        axis.text = element_text(size=12),
        axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        legend.text = element_text(size=14)) +
  scale_color_manual(values = c("grey70", "steelblue"), labels = c("Control States", "Treated (California)")) +
  geom_vline(xintercept = 2015, linetype = 2, color = "blue", size = 0.25)+
  geom_line(aes(Year, cali[,6], colour = Unit), data = cali, colour = "steelblue", size = 1.25) +
  geom_point(aes(Year, cali[,6], colour = Unit), data = cali, colour = alpha("steelblue", 1))


cali2 <- subset(nme, Unit == "California")
nme_plot <- ggplot(nme, aes(x = Year, y = Gap, group = Unit, color = Treat)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(title = "Non-medical Exemptions") +
  xlab("Year") +
  ylab("Difference (%)") +
  ylim(c(-4, 4)) +
  scale_x_discrete(limits = c(2011:2017)) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_blank(), 
        axis.text = element_text(size=12),
        axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        legend.position = "none") +
  scale_color_manual(values = c("grey70", "steelblue"), labels = c("Control States", "Treated (California)")) +
  geom_vline(xintercept = 2015, linetype = 2, color = "blue", size = 0.25)+
  geom_line(aes(Year, cali2[,6], colour = Unit), data = cali2, colour = "steelblue", size = 1.25) +
  geom_point(aes(Year, cali2[,6], colour = Unit), data = cali2, colour = alpha("steelblue", 1))

cali3 <- subset(med, Unit == "California")
med_plot <- ggplot(med, aes(x = Year, y = Gap, group = Unit, color = Treat)) +
  geom_point() +
  geom_line()+
  theme_bw() +
  labs(title = "Medical Exemptions") +
  xlab("Year") +
  ylab("Difference (%)") +
  ylim(c(-4, 4)) +
  scale_x_discrete(limits = c(2011:2017)) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        legend.title = element_blank(), 
        axis.text = element_text(size=12),
        axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        legend.position = "none") +
  scale_color_manual(values = c("grey70", "steelblue"), labels = c("Control States", "Treated (California)")) +
  geom_vline(xintercept = 2015, linetype = 2, color = "blue", size = 0.25)+
  geom_line(aes(Year, cali3[,6], colour = Unit), data = cali3, colour = "steelblue", size = 1.25) +
  geom_point(aes(Year, cali3[,6], colour = Unit), data = cali3, colour = alpha("steelblue", 1))

setEPS()
postscript(paste("Plots/(", additional_label, ") Placebo Plots (B).eps", sep = ""),  width = 13, height = 10)
grid.arrange(mmr_plot, med_plot, nme_plot, 
             widths = c(1, 1, 1, 1, 1, 1, 1, 1), 
             layout_matrix = rbind(c(NA, 1, 1, 1, 1, 1, 1, 1), c(3, 3, 3, 3, 2, 2, 2, 2)), 
             top = grid::textGrob("Placebo Plots", gp = grid::gpar(fontsize = 22)))
dev.off()
}

#4. DiD Plots.
{
  cov <- read.csv("Data Files/Did.csv")
  colnames(cov) <- c("County", "Overall", "Medical", "Nonmedical")
  
  mmr <- cov[, c(1, 2)]
  med <- cov[, c(1, 3)]
  nme <- cov[, c(1, 4)]

  #Create the GG plots
  mmr_plot <- ggplot(mmr, aes(x = reorder(County, Overall), y = Overall)) +
    geom_bar(stat = "Identity", fill = "steelblue4") +
    theme_linedraw() +
    labs(title = "Overall Coverage") +
    xlab("County") +
    ylab("Change (%)") +
    #ylim(c(0, 0.75)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_blank(), 
          axis.text = element_text(size=12),
          axis.title.x = element_text(size = 14), 
          axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid.major.x = element_blank(),
          axis.title.y = element_text(size = 14), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.text = element_text(size = 14))
  
  med_plot <- ggplot(med, aes(x = reorder(County, Medical), y = Medical)) +
    geom_bar(stat = "Identity", fill = "steelblue4") +
    theme_linedraw() +
    labs(title = "Overall Coverage") +
    xlab("County") +
    ylab("Change (%)") +
    #ylim(c(0, 0.75)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_blank(), 
          axis.text = element_text(size=10),
          axis.title.x = element_text(size = 12), 
          axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid.major.x = element_blank(),
          axis.title.y = element_text(size = 14), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.text = element_text(size = 14))
  
  nme_plot <- ggplot(nme, aes(x = reorder(County, Nonmedical), y = Nonmedical)) +
    geom_bar(stat = "Identity", fill = "steelblue4") +
    theme_linedraw() +
    labs(title = "Overall Coverage") +
    xlab("County") +
    ylab("Change (%)") +
    #ylim(c(0, 0.75)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          legend.title = element_blank(), 
          axis.text = element_text(size=10),
          axis.title.x = element_text(size = 12), 
          axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid.major.x = element_blank(),
          axis.title.y = element_text(size = 14), 
          plot.margin = margin(0.30, 0.30, 0.30, 0.30, "cm"),
          legend.text = element_text(size = 14))
  
  setEPS()
  postscript("Plots/County Level Changes.eps",  width = 13, height = 10)
  grid.arrange(mmr_plot, med_plot, nme_plot, 
               widths = c(1, 1, 1, 1, 1, 1, 1, 1), 
               layout_matrix = rbind(c(NA, 1, 1, 1, 1, 1, 1, NA), c(3, 3, 3, 3, 2, 2, 2, 2)), 
               top = grid::textGrob("Treated California vs. Synthetic California", gp = grid::gpar(fontsize = 22)))
  dev.off()
} 
#The END. 
