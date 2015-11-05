# Set up dataset for sensitivity analyses #
totaleels<-5462
poc<-0.8
samplesize<-100
totalrows<-round(totaleels/poc)
totaltags<-400
totaltraps<-50
totalchecks<-20

sample_me<-data.frame(eel_pres=c(rep(0,totalrows-totaleels), rep(1,totaleels)),
                      ordered=rank(rnorm(totalrows, mean=0, sd=7)),
                      tagged=0)

sample_me$tagged[sample(which(sample_me$eel_pres==1),totaltags)]<-1

sample_me<-sample_me[,-2]

df.data<-data.frame(expand.grid(trap=c(1:totaltraps),
                                check=c(1:totalchecks),
                                present=0,
                                tagged=0))
for(i in 1:nrow(df.data))
{
  results<-data.frame(sample_me%>%sample_n(samplesize)%>%summarise(present=sum(eel_pres), tagged=sum(tagged)))
  df.data$present[i]<-results$present
  df.data$tagged[i]<-results$tagged
}

df.data$trap<-paste("T",df.data$trap, sep="")
df.data$check<-paste("C",df.data$check, sep="")
#write.csv(df.data, "recapture_data.csv")
