#Read the chemo data

MASTER_CTX <-read.csv("H:/Kondziolka Data/Sansosti_Final_ chemo_7.21.2016.csv")

# create the data from
dat <- data.frame(
  pos = c(1, 2, 3, 4, 5, 6),
  start = c(1,3, 6, 7, 10, 11),
  end = c(5, 6, 9, 9, 13, 12)
)

# we will denote each chemotherapy in a column
# will denote the date of start and the date of end for complete list of chemo

names(MASTER_CTX)
a <- MASTER_CTX[complete.cases(MASTER_CTX[c('CTX','first_noted_brainmet','Chemo_StartEnd','Primary_Histology','MRN')]),c('MRN','CTX','first_noted_brainmet','Chemo_StartEnd','Primary_Histology')]

write.csv(a,"H:/Kondziolka Data/test_CTX.csv")


######## Breast 

dat <- read.csv('H:/Kondziolka Data/test_CTX3.csv')

dat = dat[dat$Primary_Histology == 'Breast',] 
attach(dat)

dat$start2 = (dat$start2 / 365) * 12
dat$end2 = (dat$end2 / 365) * 12

dat$pos = 1:dim(dat)[1]

plot(pos, type = 'n', xlim = range(c(dat$start2, dat$end2)), ylim = c(72,1),ylab = 'chemotherapy', xlab = 'days from brain met diagnosis',yaxt='n', ann=FALSE)
grid(lty = 1, col = 'white')

lablist.y<-as.vector(dat$CTX)
axis(2, at=seq(1, 72, by=1), labels = FALSE)
text(y = seq(1, 72, by=1),par("usr")[1], cex = 0.7, labels = lablist.y[1:73], srt = 0, pos = 2, xpd = TRUE)
title('Breast')
segments(dat$start2, dat$pos, dat$end2, dat$pos,lwd = 6)



########## Melanooma

dat <- read.csv('H:/Kondziolka Data/test_CTX3.csv')

dat = dat[dat$Primary_Histology == 'Melanoma',] 
attach(dat)

dat$start2 = (dat$start2 / 365) * 12
dat$end2 = (dat$end2 / 365) * 12

dat$pos = 1:dim(dat)[1]

plot(pos, type = 'n', xlim = range(c(dat$start2, dat$end2)), ylim = c(46,1),ylab = 'chemotherapy', xlab = 'days from brain met diagnosis',yaxt='n', ann=FALSE)
grid(lty = 1, col = 'white')

lablist.y<-as.vector(dat$CTX)
axis(2, at=seq(1, 46, by=1), labels = FALSE)
text(y = seq(1, 46, by=1),par("usr")[1], cex = 0.7, labels = lablist.y[1:46], srt = 0, pos = 2, xpd = TRUE)
title('Melanoma')
segments(dat$start2, dat$pos, dat$end2, dat$pos,lwd = 6)
############ lung
dev.new()
par(mfrow =c(1,2))
dat <- read.csv('H:/Kondziolka Data/test_CTX3.csv')

dat = dat[dat$Primary_Histology == 'Lung',] 
attach(dat)

dat$start2 = (dat$start2 / 365) * 12
dat$end2 = (dat$end2 / 365) * 12

dat$pos = 1:dim(dat)[1]

plot(pos, type = 'n', xlim = range(c(dat$start2, dat$end2)), ylim = c(106,1),ylab = 'Systemic Therapy', xlab = 'days from brain met diagnosis',yaxt='n', ann=FALSE)
grid(lty = 1, col = 'white')

lablist.y<-as.vector(dat$CTX)
axis(2, at=seq(1, 106, by=1), labels = FALSE)
text(y = seq(1, 106, by=1),par("usr")[1], cex = 0.7, labels = lablist.y[1:106], srt = 0, pos = 2, xpd = TRUE)
title('Duration of First Line Systemic Therapy for \n NSCLC with Newly Diagnosed Brain Metastasis',xlab = 'Months from diagnosis')


segments(dat$start2[1:106], dat$pos[1:106], dat$end2[1:106], dat$pos[1:106],lwd = 6)

#####################
############ lung

dat <- read.csv('H:/Kondziolka Data/test_CTX3.csv')

dat = dat[dat$Primary_Histology == 'Lung',] 
attach(dat)

dat$start2 = (dat$start2 / 365) * 12
dat$end2 = (dat$end2 / 365) * 12

dat$pos = 1:dim(dat)[1]

plot(pos, type = 'n', xlim = range(c(dat$start2, dat$end2)), ylim = c(212,106),ylab = 'Systemic Therapy', xlab = 'days from brain met diagnosis',yaxt='n', ann=FALSE)
grid(lty = 1, col = 'white')

lablist.y<-as.vector(dat$CTX)
axis(2, at=seq(106,212, by=1), labels = FALSE)
text(y = seq(106,212, by=1),par("usr")[1], cex = 0.7, labels = lablist.y[106:212], srt = 0, pos = 2, xpd = TRUE)
segments(dat$start2[106:212], dat$pos[106:212], dat$end2[106:212], dat$pos[106:212],lwd = 6)
title('Duration of Subsequent Systemic Therapy for \n NSCLC with Newly Diagnosed Brain Metastasis',xlab = 'Months from diagnosis')

#####################

plot(pos, type = 'n', xlim = range(c(dat$start2, dat$end)), ylim = c(436,0),ylab = 'Systemic Therapy', xlab = 'days from brain met diagnosis',yaxt='n', ann=FALSE)
grid()

lablist.y<-as.vector(dat$CTX)
axis(2, at=seq(0, 432, by=1), labels = FALSE)
text(y = seq(0, 432, by=1), cex=0.1,par("usr")[1], labels = lablist.y, srt = 0, pos = 2, xpd = TRUE)

segments(dat$start2, dat$pos, dat$end2, dat$pos)

#$####################
dat <- read.csv('H:/Kondziolka Data/test_CTX2.csv')

plot(pos, type = 'n', xlim = range(c(dat$start2, dat$end)), ylim = c(43,0),ylab = '', xlab = 'days from brain met diagnosis',yaxt='n', col = 'goldenrod')
grid(lty = 1, col = 'white')

lablist.y<-as.vector(dat$CTX)
axis(2, at=seq(1, 44, by=1), labels = FALSE)
text(y = seq(1, 44, by=1),par("usr")[1], cex = 0.8, labels = lablist.y[1:43], srt = 0, pos = 2, xpd = TRUE)

segments(dat$start2, dat$pos, dat$end, dat$pos,lwd = 6)
#$####################
dat <- read.csv('H:/Kondziolka Data/test_CTX3.csv')

plot(pos, type = 'n', xlim = range(c(dat$start2, dat$end)), ylim = c(61,0),ylab = '', xlab = 'days from brain met diagnosis',yaxt='n', col = 'goldenrod')
grid(lty = 1, col = 'white')

lablist.y<-as.vector(dat$CTX)
axis(2, at=seq(1, 61, by=1), labels = FALSE)
text(y = seq(1, 61, by=1),par("usr")[1], cex = 0.5, labels = lablist.y[1:61], srt = 0, pos = 2, xpd = TRUE)

segments(dat$start2, dat$pos, dat$end, dat$pos,lwd = 6)



#To get it more exactly like your figure...

r <- par('usr') 
plot(pos, type = 'n', xlim = range(c(start, end)), ylim = c(13.5,0.5), xlab = '', 
    xaxt = 'n', yaxt = 'n', panel.first = rect(r[1], r[3], r[2], r[4], col = 'goldenrod'))
# abline(h = 1:13, col = 'white')
# abline(v = 1:13, col = 'white')
grid(lty = 1, col = 'white')
axis(1, 1:13, 1:13, cex.axis = 0.8)
axis(2, 1:13, 1:13, las = 1, cex.axis = 0.8)
segments(start, pos + 0.5, end, pos + 0.5, lwd = 2)