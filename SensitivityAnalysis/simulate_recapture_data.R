# Set up data frame for sensitivity analysis tutorial #

df.data=NULL
check<-c()
present<-c()
tag<-c()
for(TRAPS in 1:50)
{
  trap<-TRAPS
  check1<-c(1:20)
  
  if(TRAPS %in% c(1:10))
  {
    total1<-round(rnorm(n=20, mean=5, sd=5),0)
    total1[total1<2]<-0
    tagged1<-round(rnorm(n=20, mean=1, sd=1),0)
    tagged1[tagged1<0]<-0
    trap1<-data.frame(check1, total1, tagged1)  
  }
  if(TRAPS %in% c(11:20))
  {
    total1<-round(rnorm(n=20, mean=7, sd=4),0)
    total1[total1<2]<-0
    tagged1<-round(rnorm(n=20, mean=3, sd=2),0)
    tagged1[tagged1<0]<-0
    trap1<-data.frame(check1, total1, tagged1)  
  }
  if(TRAPS %in% c(21:30))
  {
    total1<-round(rnorm(n=20, mean=2, sd=5),0)
    total1[total1<2]<-0
    tagged1<-round(rnorm(n=20, mean=0.75, sd=1),0)
    tagged1[tagged1<0]<-0
    trap1<-data.frame(check1, total1, tagged1)  
  }
  if(TRAPS %in% c(31:40))
  {
    total1<-round(rnorm(n=20, mean=6, sd=2),0)
    total1[total1<2]<-0
    tagged1<-round(rnorm(n=20, mean=4, sd=1),0)
    tagged1[tagged1<0]<-0
    trap1<-data.frame(check1, total1, tagged1)  
  }
  if(TRAPS %in% c(41:50))
  {
    total1<-round(rnorm(n=20, mean=5, sd=3),0)
    total1[total1<2]<-0
    tagged1<-round(rnorm(n=20, mean=2, sd=0.5),0)
    tagged1[tagged1<0]<-0
    trap1<-data.frame(check1, total1, tagged1)  
  }
    
  for(i in 1:nrow(trap1))
  {
    if(trap1$total1[i]==0)
    {
      check.in<-trap1$check[i]
      present.in<-0
      tag.in<-0
      check<-c(check, check.in)
      present<-c(present, present.in)
      tag<-c(tag, tag.in)
    }
    
    if(trap1$total1[i]>0)
    {
      for(ii in 1:trap1$total1[i])
      {
        check.in<-trap1$check1[i]
        present.in<-1
        if(ii %in% 1:trap1$tagged1[i]){tag.in<-1}
        if(!ii %in% 1:trap1$tagged1[i]){tag.in<-0}
        check<-c(check, check.in)
        present<-c(present, present.in)
        tag<-c(tag, tag.in)
      }
    }
  }
  
  df.data<-rbind(df.data,data.frame(trap, check, present, tag))
}

# write.csv(df.data, "recapture_data.csv")