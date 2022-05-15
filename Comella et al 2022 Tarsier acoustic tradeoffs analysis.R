# Load libraries
library(brms)
library(sjPlot)
library(bayesplot)
library(tidyr)
library(tidybayes)
library(ggplot2)
library(ggpubr)
library(flextable)
library(stringr)
library(randomForest)
library(cluster)
library(dplyr)

# Who starts the duets and histogram of duet durations ---------------------------------------------
# Set location of selection tables
file.dir <- 
  list.files('TarsierSelectionTables/',
             full.names = T,recursive = T,pattern = '.txt')

# Combine selection tables
tarsier.selection.tables <- data.frame()

for(a in 1:length(file.dir)){
  print(a)
  temp.table <-read.delim(file.dir[a],stringsAsFactors = T)
  temp.name <- file.dir[a]
  temp.name.updated <- str_split_fixed(temp.name,pattern = '.txt',n=2)[,1]
  n.slashes <- str_count(temp.name.updated,pattern = '/')+1
  temp.name.split <- str_split_fixed(temp.name.updated,pattern = '/',n=n.slashes)[,n.slashes]
  pair <- str_split_fixed(temp.name.split,pattern = '-',n=2)[,1]
  duet <- temp.name.split
  tarsier.table.single <- cbind.data.frame(temp.table,pair,duet,temp.name)
  tarsier.selection.tables <- rbind.data.frame(tarsier.selection.tables,tarsier.table.single )
}

# Clean up pair column
tarsier.selection.tables$pair <- str_split_fixed(tarsier.selection.tables$pair,
                                                 pattern = '_',n=2)[,1]

# Check output
head(tarsier.selection.tables)


# Collate data by duet to calculate durations
duet.id.index <- unique(tarsier.selection.tables$duet)

MFstart.df <- data.frame()

for(b in 1: length(duet.id.index)){
  temp.selection.table <- subset(tarsier.selection.tables, duet==duet.id.index[b])
  # sort by mpg
  temp.selection.table.sorted <- temp.selection.table[order(temp.selection.table$Begin.Time..s.),]
  first.annotation <- temp.selection.table.sorted[1,c("pair", "Sex","duet" )]
  duet.duration <- max(temp.selection.table$End.Time..s.)-min(temp.selection.table$Begin.Time..s.)
  print(duet.duration)
  MFstart.df.temp <- cbind.data.frame(first.annotation,duet.duration)
  MFstart.df <- rbind.data.frame(MFstart.df,MFstart.df.temp)
}

table(MFstart.df$pair, MFstart.df$Sex)
table(MFstart.df$Sex)

gghistogram(data=MFstart.df, x='duet.duration', fill='blue',
            xlab='Duet duration (s)',ylab='Number of duets',xlim=c(0,230))
range(MFstart.df$duet.duration)
median(MFstart.df$duet.duration)
sd(MFstart.df$duet.duration)


# Data processing for model selection -------------------------------------

# Combine selection tables
performance.tables <- data.frame()

for(a in 1:length(file.dir)){#tryCatch({
  print(a)
  temp.table <-read.delim(file.dir[a],stringsAsFactors = T)
  temp.name <- file.dir[a]
  temp.name.updated <- str_split_fixed(temp.name,pattern = '.txt',n=2)[,1]
  n.slashes <- str_count(temp.name.updated,pattern = '/')+1
  temp.name.split <- str_split_fixed(temp.name.updated,pattern = '/',n=n.slashes)[,n.slashes]
  pair <- str_split_fixed(temp.name.split,pattern = '-',n=2)[,1]
  duet <- temp.name.split
  tarsier.table.single <- cbind.data.frame(temp.table,pair,duet,temp.name)
  unique.sex <- unique(tarsier.table.single$Sex)
  print(pair)
  for(c in 1:length(unique.sex)){
    
    single.sex <- subset(tarsier.table.single,Sex==unique.sex[c])
    
    # defining the break points
    trill.start.time <- min(single.sex$Begin.Time..s.)
    trill.last.bin <- max(single.sex$End.Time..s.)
    n.bins <- as.integer(trill.last.bin-trill.start.time,length=0)
    bin.seq <- seq(from=0, to=n.bins,by=3 )
    bin.seq <- trill.start.time + bin.seq
    
    # create a list of all the segments
    bin.seq.length <- length(bin.seq)-1
    
    for(b in 1:bin.seq.length){
      temp.subset <- subset(single.sex,Begin.Time..s. >= bin.seq[b] & End.Time..s. <= bin.seq[b+1])
      noterate <- nrow(temp.subset)/3
      new.bandwidth <- temp.subset$High.Freq..Hz.- temp.subset$Low.Freq..Hz.
      bandwidth <- mean(new.bandwidth)
      print(noterate)
      print(b)
      if(noterate > 0){
        temp.df <- cbind.data.frame(temp.subset[1,c('pair','duet','Sex','temp.name')],noterate,bandwidth,b)
        performance.tables <- rbind.data.frame(performance.tables,temp.df )
      }
    }
  }
  
  #}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

performance.tables$pair <- str_split_fixed(performance.tables$pair,
                                           pattern = '_',n=2)[,1]

performance.tables<-performance.tables[!(performance.tables$bandwidth < 3000 & performance.tables$Sex=='M'),]

performance.tables$pair <- as.factor(performance.tables$pair)
levels(performance.tables$pair) <-  c("Pair1","Pair2","Pair3","Pair4","Pair5",
                                      "Pair6","Pair7","Pair8","Pair9","Pair10",
                                      "Pair11","Pair12","Pair13","Pair14","Pair15")


# Standardize note rate to notes per 1-sec
performance.tables$noterate <- performance.tables$noterate/3

# Plot to inspect data
ggscatter(performance.tables,x='noterate',y='bandwidth',facet.by ='Sex')

# Subset female
performance.tables.female <- subset(performance.tables,Sex=='F')
#write.csv(performance.tables.female,'performance.tables.female.csv',row.names = F)

performance.tables.male <- subset(performance.tables,Sex=='M')
# Remove irregular male
performance.tables.male <- subset(performance.tables.male,duet != 'TG3-20180810-052252.BoutA')
#write.csv(performance.tables.male,'performance.tables.male.csv',row.names = F)


# Unsupervised clustering analysis ----------------------------------------
# Start with females
# Run random forest
rf2.female <- randomForest(x = tarsier.selection.tables.female[,c(6,7,9,24)], ntree = 10000, proximity = TRUE)
rf2.female

# NOTE the iterative process takes a long time which is why it is commented out

# Run iteratively over multiple cluster solutions
prox <- rf2.female$proximity
# n.clusters <- seq(2,10,1)
# female.sil.df <- data.frame()
# for(a in 1:length(n.clusters)){
#   print(a)
#   pam.rf <- pam(prox, n.clusters[a])
#   
#   sil <-
#     cluster::silhouette(x = pam.rf$clustering,
#                         dist = dist(tarsier.selection.tables.female[,c(6,7,9,24)]))
#   
#   sil.val <- (summary(sil)$avg.width)
#   temp.sil.df <-  cbind.data.frame(sil.val,n.clusters[a])
#   female.sil.df <- rbind.data.frame(female.sil.df,temp.sil.df)
# }
# 
# pam.rf <- pam(prox, female.sil.df[which.max(female.sil.df$sil.val),]$`n.clusters[a]`)

# 
pam.rf <- pam(prox,3)

femaletarsierTypes.umap <- 
  umap::umap(tarsier.selection.tables.female[,c(6,7,9,24)],labels=as.factor(pam.rf$clustering),
             controlscale=TRUE,scale=3)


plot.for.femaletarsierTypes.temp <-
  cbind.data.frame(femaletarsierTypes.umap$layout[,1:2],
                   pam.rf$clustering,tarsier.selection.tables.female$pair)

colnames(plot.for.femaletarsierTypes.temp) <-
  c("Dim.1", "Dim.2","Cluster", "Pair")


my_plot_femaletarsierTypes <- ggpubr::ggscatter(data=plot.for.femaletarsierTypes.temp,x='Dim.1',y='Dim.2',facet.by = 'Pair',
                                                color='Cluster')+theme(legend.position = "none")+ggtitle('Females')

my_plot_femaletarsierTypes

plot.for.femaletarsierTypes.temp$Clusters <- as.factor(pam.rf$cluster)

Female.plot <- ggplot(plot.for.femaletarsierTypes.temp, aes(x = Dim.1, y = Dim.2, col = Clusters)) +
  geom_point(size = 3) +
  scale_color_manual(values = matlab::jet.colors (length(unique(plot.for.femaletarsierTypes.temp$Clusters))))+ggtitle('Female clusters') +theme_bw()

Female.plot

# Start with males
# Run random forest
rf2.male <- randomForest(x = tarsier.selection.tables.male[,c(6,7,9,24)], ntree = 10000, proximity = TRUE)
rf2.male

# NOTE the iterative process takes a long time which is why it is commented out
# # Run iteratively over multiple cluster solutions
prox <- rf2.male$proximity
# n.clusters <- seq(2,10,1)
# male.sil.df <- data.frame()
# for(a in 1:length(n.clusters)){
#   print(a)
#   pam.rf <- pam(prox, n.clusters[a])
#   
#   sil <-
#     cluster::silhouette(x = pam.rf$clustering,
#                         dist = dist(tarsier.selection.tables.male[,c(6,7,9,24)]))
#   
#   sil.val <- (summary(sil)$avg.width)
#   temp.sil.df <-  cbind.data.frame(sil.val,n.clusters[a])
#   male.sil.df <- rbind.data.frame(male.sil.df,temp.sil.df)
# }
# 
# pam.rf <- pam(prox, male.sil.df[which.max(male.sil.df$sil.val),]$`n.clusters[a]`)

pam.rf <- pam(prox,2)

maletarsierTypes.umap <- 
  umap::umap(tarsier.selection.tables.male[,c(6,7,9,24)],labels=as.factor(pam.rf$clustering),
             controlscale=TRUE,scale=3)


plot.for.maletarsierTypes.temp <-
  cbind.data.frame(maletarsierTypes.umap$layout[,1:2],
                   pam.rf$clustering,tarsier.selection.tables.male$pair)

colnames(plot.for.maletarsierTypes.temp) <-
  c("Dim.1", "Dim.2","Cluster", "Pair")


my_plot_maletarsierTypes <- ggpubr::ggscatter(data=plot.for.maletarsierTypes.temp,x='Dim.1',y='Dim.2',facet.by = 'Pair',
                                                color='Cluster')+theme(legend.position = "none")+ggtitle('males')

my_plot_maletarsierTypes

plot.for.maletarsierTypes.temp$Clusters <- as.factor(pam.rf$cluster)

Male.plot <- ggplot(plot.for.maletarsierTypes.temp, aes(x = Dim.1, y = Dim.2, col = Clusters)) +
  geom_point(size = 3) +
  scale_color_manual(values = matlab::jet.colors (length(unique(plot.for.maletarsierTypes.temp$Clusters))))+ggtitle('Male clusters') +theme_bw()

Male.plot

cowplot::plot_grid(Male.plot,Female.plot)

# Female tarsier model selection ------------------------------------------
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

plot(m2female.bandwidth)


brms::loo_compare(m0female.bandwidth, m1female.bandwidth, m2female.bandwidth, criterion = "loo")
# elpd_diff se_diff


# Male model selection ----------------------------------------------------
performance.tables.male$pair <- as.factor(performance.tables.male$pair)

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

save(m0male.bandwidth,file='m0male.bandwidth.rda')
save(m2male.bandwidth,file='m2male.bandwidth.rda')
save(m0female.bandwidth,file='m0female.bandwidth.rda')
save(m2female.bandwidth,file='m2female.bandwidth.rda')

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


# Scatterplot by pair -----------------------------------------------------
PlotColorsMales <-PlotColors[(unique(performance.tables$pair)) %in% (unique(performance.tables.male$pair))]
PlotColorsFemales <-PlotColors[(unique(performance.tables$pair)) %in% (unique(performance.tables.female$pair))]

levels(performance.tables$pair) <-  c("Pair1","Pair2","Pair3","Pair4","Pair5",
                                      "Pair6","Pair7","Pair8","Pair9","Pair10",
                                      "Pair11","Pair12","Pair13","Pair14","Pair15")

performance.tables$bout <- str_split_fixed(performance.tables$duet,pattern = '[.]',n=2) [,2]

performance.tables.female.scatter <- subset(performance.tables,Sex=='F' )
levels(performance.tables.female.scatter$pair)


FemaleTradeoffs <-ggscatter(data=performance.tables.female.scatter, x='noterate','bandwidth',  color ='pair', position='jitter',
                            xlab='Note rate (notes per 1-sec)',ylab='Note bandwith (Hz)',shape='bout') + 
  #geom_jitter(width = 0.05, alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ pair)+
  scale_color_manual(values=PlotColors)+
  theme(legend.position="none")+ggtitle('Females')#+ylim(0,10000) # noterate varies between and within (most) pairs

FemaleTradeoffs

performance.tables.male.scatter <- subset(performance.tables,Sex=='M' )

MaleTradeoffs <-ggscatter(data=performance.tables.male.scatter, x='noterate','bandwidth',  color ='pair', position='jitter',
                          xlab='Note rate (notes per 1-sec)',ylab='Note bandwith (Hz)',shape='bout') + 
  
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ pair)+
  scale_color_manual(values=PlotColors)+
  theme(legend.position="none")+ggtitle('Males')#+ylim(3000,9000) 

MaleTradeoffs

cowplot::plot_grid(FemaleTradeoffs,MaleTradeoffs,nrow=2)

# Table 2 Model summary table ---------------------------------------------
sjPlot::tab_model(m1female.bandwidth,m0female.bandwidth,
                  m2male.bandwidth,m0male.bandwidth,
                  pred.labels = c('Intercept','Note rate'),
                  dv.labels =c('Female Top Model','Female Null Model',
                               'Male Top Model','Male Null Model'),
                  file = 'AcousticTradeoffsModelResults1.doc'
) 


# Table 1 Summary of outcome and predictor variables  ---------------------------------------------------------
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

# Sample size summary
length(unique(performance.tables$pair))
nrow(tarsier.selection.tables)

length(unique(performance.tables.female$pair))
length(unique(performance.tables.male$pair))

length(unique(performance.tables.female$duet))
length(unique(performance.tables.male$duet))

nrow(tarsier.selection.tables.female)
nrow(tarsier.selection.tables.male)
nrow(tarsier.selection.tables.female) + nrow(tarsier.selection.tables.male)

