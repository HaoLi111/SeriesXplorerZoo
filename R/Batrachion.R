Batrachion_Holfstadtler1980= function(n){
  stopifnot(n>2)
  a = numeric(n)
  a[1]<-a[2]<-1
  for(i in 3:n)    a[i] = a[i-a[i-1]] + a[i - a[i-2]]
  return(a)
}

#plot(1:10000,Batrachion_Holfstadtler1980(10000)/(1:10000),type='l')

Batrachion_Conway1988 = function(n){
  stopifnot(n>2)
  a = numeric(n)
  a[1]<-a[2]<-1
  for(i in 3:n)    a[i] = a[a[i-1]] + a[i - a[i-1]]
  return(a)
}
#plot(Batrachion_Conway1988(10000)/(1:10000),type='l')


Batrachion_Mallows1991 = function(n){
  stopifnot(n>2)
  a = numeric(n)
  a[1]<-a[2]<-1
  for(i in 3:n)    a[i] = a[a[i-2]] + a[i - a[i-2]]
  return(a)
}
#plot(Batrachion_Mallows1991(10000)/1:10000,type='l')

plot_Batrachion = function(x,type = c("rescale","restruct")){
  if("rescale" %in% type)    plot(x/seq_along(x),type='l',xlab = 'n',ylab =parse(text='x_n/n'))

  if("restruct"%in% type) plot(x[-length(x)]/seq_along(x[-length(x)]),x[-1]/seq_along(x[-1]),
                               xlab = 'x_n/n',type='l')
}
#plot_Batrachion(Batrachion_Conway1988(1000))
