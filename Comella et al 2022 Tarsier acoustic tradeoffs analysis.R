# Load libraries
library(brms)
library(sjPlot)
library(bayesplot)
library(tidyr)
library(tidybayes)
library(ggplot2)
library(ggpubr)
library(flextable)

# Read in data
performance.tables.female <- read.csv("performance.tables.female.csv")
performance.tables.female$pair <- as.factor(performance.tables.female$pair)

# Check distribution of variables
hist(performance.tables.female$noterate)
hist(performance.tables.female$bandwidth)
table(performance.tables.female$pair) 


ggplot(performance.tables.female, aes(noterate, bandwidth)) + 
  geom_jitter(width = 0.05, alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE)
ggplot(performance.tables.female, aes(noterate, bandwidth, color = pair)) + 
  geom_jitter(width = 0.05, alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ pair) # noterate varies between and within (most) pairs

# Create models and add 'loo' to evaluate
m0female.bandwidth <-  brms::brm(bandwidth ~  (1|pair),
                                 data=performance.tables.female,iter = 4000)
m0female.bandwidth <- brms::add_criterion(m0female.bandwidth, "loo")

m1female.bandwidth <-  brms::brm(bandwidth ~ noterate + (1|pair), 
                                 data=performance.tables.female,iter = 4000)
m1female.bandwidth <- brms::add_criterion(m1female.bandwidth, "loo")
m1female.bandwidth
# Population-Level Effects: 
#           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept  8972.50    356.80  8279.50  9696.55 1.00     2731     3649
# noterate  -3730.89    297.41 -4310.41 -3139.28 1.00     9961     5765

# Compare models
brms::loo_compare(m0female.bandwidth, m1female.bandwidth, criterion = "loo")

# Check traceplots
plot(m1female.bandwidth)

# Calculate R^2
brms::bayes_R2(robust=T, m1female.bandwidth)

# Create coefficient plot
sjPlot::plot_model(m1female.bandwidth,type = 'est')

# Check residuals of top model
hist(residuals(m1female.bandwidth))

# Run posterior predictive check
bayesplot::pp_check(m1female.bandwidth, ndraws = 100, type='stat', stat='mean')

# Lynn
m2female.bandwidth <-  brms::brm(bandwidth ~ noterate + (1 + noterate|pair), 
                                 data=performance.tables.female, iter = 4000,
                                 control = list(adapt_delta = 0.999))
m2female.bandwidth <- brms::add_criterion(m2female.bandwidth, "loo")
m2female.bandwidth
# Population-Level Effects: 
#           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept  8976.35    360.81  8295.72  9719.24 1.00     3538     4103
# noterate  -3729.79    400.91 -4508.16 -2922.43 1.00     4997     4279
plot(m2female.bandwidth)

m3female.bandwidth <-  brms::brm(bandwidth ~ noterate + (1|pair) + (0 + noterate|pair), 
                                 data=performance.tables.female, iter = 4000,
                                 control = list(adapt_delta = 0.999))
m3female.bandwidth <- brms::add_criterion(m3female.bandwidth, "loo")
m3female.bandwidth
# Population-Level Effects: 
#           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept  8959.80    347.87  8280.59  9663.87 1.00     3770     5616
# noterate  -3716.99    385.31 -4446.14 -2955.13 1.00     5158     4243
plot(m3female.bandwidth)

brms::loo_compare(m0female.bandwidth, m1female.bandwidth, m2female.bandwidth, criterion = "loo")
# elpd_diff se_diff
# m1female.bandwidth   0.0       0.0  
# m3female.bandwidth  -0.6       0.8  
# m2female.bandwidth  -0.7       0.7  
# m0female.bandwidth -68.8      12.8 

# Now to males
# Read in data
performance.tables.male <- read.csv('performance.tables.male.csv')

performance.tables.male$pair <- as.factor(performance.tables.male$pair)

performance.tables.male <- subset(performance.tables.male,duet != 'TG3-20180810-052252.BoutA')

#performance.tables.male$bout <- str_split_fixed(performance.tables.male$duet,'[.]',n=2)[,2]


# Check distribution of variables
hist(performance.tables.male$noterate)
hist(performance.tables.male$bandwidth)

ggplot(performance.tables.male, aes(noterate, bandwidth)) + 
  geom_jitter(width = 0.05, alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE)
ggplot(performance.tables.male, aes(noterate, bandwidth, color = pair)) + 
  geom_jitter(width = 0.05, alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ pair) # noterate varies between and within (most) pairs

# Create models and add 'loo' to evaluate
m0male.bandwidth <-  brms::brm(bandwidth ~  (1|pair),
                                 data=performance.tables.male,iter = 4000,cores =4)
m0male.bandwidth <- brms::add_criterion(m0male.bandwidth, "loo")

m1male.bandwidth <- brms::brm(bandwidth ~ noterate + (1|pair), 
                                 data=performance.tables.male,iter = 4000,cores =4)
m1male.bandwidth <- brms::add_criterion(m1male.bandwidth, "loo")
m1male.bandwidth
#Population-Level Effects: 
#           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept  7757.00    315.55  7149.97  8379.60 1.00     1473     2361
# noterate   -552.66    104.32  -761.14  -349.05 1.00     5383     4894

# Compare models
brms::loo_compare(m0male.bandwidth, m1male.bandwidth, criterion = "loo")

# Check traceplots
plot(m1male.bandwidth)

# Calculate R^2
brms::bayes_R2(robust=T,m1male.bandwidth)

# Create coefficient plot
sjPlot::plot_model(m1male.bandwidth,type = 'est')

# Check residuals of top model
hist(residuals(m1male.bandwidth))

# Run posterior predictive check
bayesplot::pp_check(m1male.bandwidth, nsamples = 100,type='stat', stat='mean')

# Lynn
m2male.bandwidth <-  brms::brm(bandwidth ~ noterate + (1 + noterate|pair), 
                                data=performance.tables.male, iter = 4000,
                                control = list(adapt_delta = 0.999, max_treedepth = 15),cores =4)
m2male.bandwidth <- brms::add_criterion(m2male.bandwidth, "loo")
m2male.bandwidth
# Population-Level Effects: 
#           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept  7746.72    309.69  7137.91  8366.03 1.00     2875     3761
# noterate   -434.80    336.09 -1113.61   224.34 1.00     2366     3314

plot(m2male.bandwidth)

brms::loo_compare(m0male.bandwidth, m1male.bandwidth, 
                  m2male.bandwidth, criterion = "loo")
# elpd_diff se_diff
# m2male.bandwidth   0.0       0.0  
# m3male.bandwidth  -0.4       1.1  
# m1male.bandwidth -36.0       9.6  
# m0male.bandwidth -47.5      10.7  


#################################################################################
# Create plot for manuscript
# Extract samples from female model
female.modeldraws <- m1female.bandwidth %>%
  spread_draws(b_Intercept,b_noterate) 

female.modeldraws$Sex <- rep('F',nrow(female.modeldraws))

# Extract samples from male model
male.modeldraws <- m3male.bandwidth %>%
  spread_draws(b_Intercept,b_noterate) 

male.modeldraws$Sex <- rep('M',nrow(male.modeldraws))

# Combine into a single data frame
allmodel.draws <- rbind.data.frame(female.modeldraws, male.modeldraws)

# Combine into one plot
CoefPlot <- allmodel.draws %>%
  ggplot(aes( x = b_noterate, y=Sex, fill=Sex)) +
  theme_bw()+
  stat_halfeye(.width = c(.95, .5), alpha = 0.8)+
  geom_vline(xintercept=0,linetype = "dashed")+
  theme(legend.position = "none")+xlab('Estimates')+ylab('Sex')

CoefPlot

TradeoffsPlot <- ggscatter(data=performance.tables, x='noterate','bandwidth',  color ='Sex', position='jitter',
                           xlab='Note rate (notes per 3 sec)',ylab='Note bandwith (Hz)', facet.by = 'Sex')+
  theme(legend.position = "None")

PlotColors <- matlab::jet.colors(length(unique(performance.tables$pair)))

PlotColorsMales <-PlotColors[(unique(performance.tables$pair)) %in% (unique(performance.tables.male$pair))]
PlotColorsFemales <-PlotColors[(unique(performance.tables$pair)) %in% (unique(performance.tables.female$pair))]

levels(performance.tables$pair) <-  c("Pair1","Pair2","Pair3","Pair4","Pair5",
                                      "Pair6","Pair7","Pair8","Pair9","Pair10",
                                      "Pair11","Pair12","Pair13","Pair14","Pair15")

performance.tables$bout <- str_split_fixed(performance.tables$duet,pattern = '[.]',n=2) [,2]

performance.tables.female.scatter <- subset(performance.tables,Sex=='F' )
levels(performance.tables.female.scatter$pair)

performance.tables.female.scatter[588,] <- performance.tables.female.scatter[1,] 
performance.tables.female.scatter[588,]$pair <- 'Pair6'
performance.tables.female.scatter[588,]$noterate <- 3
performance.tables.female.scatter[588,]$bandwidth <- 12000

FemaleTradeoffs <-ggscatter(data=performance.tables.female.scatter, x='noterate','bandwidth',  color ='pair', position='jitter',
                            xlab='Note rate (notes per 3 sec)',ylab='Note bandwith (Hz)',shape='bout') + 
  #geom_jitter(width = 0.05, alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ pair)+
  scale_color_manual(values=PlotColors)+
  theme(legend.position="none")+ggtitle('Females')+xlim(0,2)+ylim(0,10000) # noterate varies between and within (most) pairs
FemaleTradeoffs

performance.tables.male.scatter <- subset(performance.tables,Sex=='M' )
performance.tables.male.scatter[643,] <- performance.tables.male.scatter[1,] 
performance.tables.male.scatter[643,]$pair <- 'Pair4'
performance.tables.male.scatter[643,]$noterate <- 3
performance.tables.male.scatter[643,]$bandwidth <- 12000

MaleTradeoffs <-ggscatter(data=performance.tables.male.scatter, x='noterate','bandwidth',  color ='pair', position='jitter',
                          xlab='Note rate (notes per 3 sec)',ylab='Note bandwith (Hz)',shape='bout') + 
  
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ pair)+
  scale_color_manual(values=PlotColors)+
  theme(legend.position="none")+ggtitle('Males')+xlim(0,2)+ylim(3000,9000) 

MaleTradeoffs

cowplot::plot_grid(FemaleTradeoffs,MaleTradeoffs)

## Tables
sjPlot::tab_model(m1female.bandwidth,m0female.bandwidth,
                  m2male.bandwidth,m0male.bandwidth,
                  pred.labels = c('Intercept','Note rate'),
                  dv.labels =c('Female Top Model','Female Null Model',
                               'Male Top Model','Male Null Model'),
                  file = 'AcousticTradeoffsModelResults1.doc'
) #

# Part 1. Table 1 Summary of outcome and predictor variables  ---------------------------------------------------------
library(dplyr)

cdata <- performance.tables %>%
  dplyr::group_by(Sex) %>%
  summarise(total = sum(noterate),Mean.noterate= mean(noterate),
            Sd.noterate = sd(noterate),
            Min.noterate = min(noterate),
            Max.noterate = max(noterate),
            Mean.bandwidth= mean(bandwidth),
            Sd.bandwidth= sd(bandwidth),
            Min.bandwidth = min(bandwidth),
            Max.bandwidth = max(bandwidth)
            ) 

colnames(tarsier.selection.tables[,c(6,7,9,24)])
tarsier.selection.tables <- droplevels(subset(tarsier.selection.tables,Sex=='F' | Sex=='M'))

TarsierFeatures <- tarsier.selection.tables %>%
  dplyr::group_by(Sex) %>%
  summarise(Mean.Low.Freq..Hz.= mean(Low.Freq..Hz.),
            Sd.Low.Freq..Hz. = sd(Low.Freq..Hz.),
            Min.Low.Freq..Hz. = min(Low.Freq..Hz.),
            Max.Low.Freq..Hz. = max(Low.Freq..Hz.),
            Mean.High.Freq..Hz.= mean(High.Freq..Hz.),
            Sd.High.Freq..Hz.= sd(High.Freq..Hz.),
            Min.High.Freq..Hz. = min(High.Freq..Hz.),
            Max.High.Freq..Hz. = max(High.Freq..Hz.),
            Mean.Dur.90...s.= mean(Dur.90...s.),
            Sd.Dur.90...s.= sd(Dur.90...s.),
            Min.Dur.90...s. = min(Dur.90...s.),
            Max.Dur.90...s. = max(Dur.90...s.)
            
  ) 

cdata <- cbind.data.frame(cdata,TarsierFeatures[,-c(1)])
# Convert from charcter to numeric and round to two decimal places
cdata[,2:ncol(cdata)] <- cdata[,2:ncol(cdata)] %>% mutate_if(is.character,as.numeric)
cdata[,2:ncol(cdata)] <- round(cdata[,2:ncol(cdata)],2)

# Create a range column
cdata$Range.noterate <- paste(cdata$Min.noterate,'-',cdata$Max.noterate,sep='')
cdata$Range.bandwidth <- paste(cdata$Min.bandwidth,'-',cdata$Max.bandwidth,sep='')
cdata$Range.Low.Freq..Hz. <- paste(cdata$Min.Low.Freq..Hz.,'-',cdata$Max.Low.Freq..Hz.,sep='')
cdata$Range.High.Freq..Hz. <- paste(cdata$Min.High.Freq..Hz.,'-',cdata$Max.High.Freq..Hz.,sep='')
cdata$Range.Dur.90...s. <- paste(cdata$Min.Dur.90...s.,'-',cdata$Max.Dur.90...s.,sep='')


# Create a column with mean ± SD
cdata$Mean.sd.noterate <- paste(cdata$Mean.noterate,'±',cdata$Sd.noterate,sep=' ')
cdata$Mean.sd.bandwidth <-  paste(cdata$Mean.bandwidth,'±',cdata$Sd.bandwidth,sep=' ')

cdata$Mean.sd.Low.Freq..Hz. <- paste(cdata$Mean.Low.Freq..Hz.,'±',cdata$Sd.Low.Freq..Hz.,sep=' ')
cdata$Mean.sd.High.Freq..Hz. <- paste(cdata$Mean.High.Freq..Hz.,'±',cdata$Sd.High.Freq..Hz.,sep=' ')
cdata$Mean.sd.Dur.90...s. <-  paste(cdata$Mean.Dur.90...s.,'±',cdata$Sd.Dur.90...s.,sep=' ')


# Add two dataframes together
Combinded.df <- cdata[c(1,2,28,23,29,24,30,25,31,26,32,27)]
colnames(Combinded.df)

# Add publication ready column names
colnames(Combinded.df) <- c('Sex','N phrases','Note Rate Mean ± SD', 'Note Rate Range',
                            'Bandwidth (Hz) Mean ± SD', 'Bandwidth (Hz) Range',
                            'Low Frequency (Hz) Mean ± SD', 'Low Frequency (Hz) Range',
                            'High Frequency (Hz) Mean ± SD', 'High Frequency (Hz) Range',
                            'Duration (s) Mean ± SD', 'Duration (s) Range'
                            )
Combinded.df <- as.data.frame(t(Combinded.df))
colnames(Combinded.df) <- c('Female','Male')

Features <- rownames(Combinded.df)

Combinded.df <- cbind.data.frame(Features,Combinded.df)

# Create as a flextable object and run some formatting
DataSummaryTable <- flextable(Combinded.df)
DataSummaryTable <- width(DataSummaryTable, width = 1)
DataSummaryTable <- bold(DataSummaryTable, part = "header") 
DataSummaryTable
save_as_docx(DataSummaryTable,path='TarsierSummary.docx')

length(unique(performance.tables$pair))
nrow(tarsier.selection.tables)

length(unique(performance.tables.female$pair))
length(unique(performance.tables.male$pair))

length(unique(performance.tables.female$duet))
length(unique(performance.tables.male$duet))

nrow(tarsier.selection.tables.female)
nrow(tarsier.selection.tables.male)

