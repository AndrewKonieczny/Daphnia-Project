# Yo Gamma Gamma Script
-----------------------------------------------------------------
setwd(paste("/Users/bmcdonnell/Desktop/Analysed Raw Data/",
            "D. magna Excel Workbooks", sep=""))
# This function concatenates the different columns of X's
# The inputs:
# Input = the raw .csv file, Intervals = the number of columns for each
# filming period (if there are two columns until the next filming period,
# e.g. area and x, then the Intervals value is 2).
# The output: 
# An array of normalized distance values (units are in pixels) that are
# concatinated together.
concat <- function(Inputs,Intervals){
  filter <- function(Input, Element, Interval){
    zero <- function(zeroInput, zeroColumn){
      # 640 is the width of the image. This subtraction is done to
      # correct the x-y coordinate system.
      numbers <- 640-data.matrix(zeroInput[zeroColumn])
      central <- mean(numbers,na.rm=TRUE, trim = 0.2)
      # 'central' is the 30% trimmed mean to give a better value for the
      # central position of the fiber
      out <- as.vector((numbers-central), mode = "numeric")
      return(out)
    } # end of zero() function
    area_dev <- function(areaInput, areaColumn){
      g <- data.matrix(areaInput[areaColumn])
      z_area <- abs(mean(g,na.rm = TRUE, trim = 0.3)-g)/sd(g,na.rm=TRUE)
      # 'z_area' uses the trimmed mean to find the absolute value of the
      # z-score for the fiber's area values, the z-score is then used to
      # remove any valuses that deviate too far from the area's trimmed
      # mean.
      out <- as.vector(ifelse(z_area<=2,1,NA), mode = "numeric")
      return(out)
    } # end of area_dev() function
    col_zero <- (Element)*Interval
    # The column to be entered into the zero() function
    col_area <- (Element)*Interval-1
    # The column to be intered into the area_dev() function
    out <- as.vector(zero(Input, col_zero)*area_dev(Input, col_area), mode = "numeric")
    return(out)
  } # end of filter() function
  final <- NULL
  Max <- dim(Inputs)[2]/2
  elementSequence <- seq(1,Max,1)
  for (i in elementSequence){ 
    final <- c(final,filter(Inputs,elementSequence[i],Intervals))
  } # end of for-loop
  return(final)
} # end of concat() function

<<<<<<< HEAD
=======
# Combine the data of each animal into one .csv file to be analyzed
# then after filtering the data using the functions below, combine
# them to be analyzed collectively. Note that each column category
# will be every other column (X:(3,7,11,...) : Area:(2,6,10,...)).
# Create a loop to analyze this situation
# (X[i+1],Area[i],i in 2:(col.count), i <- i+4)

loadCsvFiles <- function(directory){
  # Load all csv files within a file directory into Global Env
  # Usage: 
  # > loadCsvFiles("data")
  # > ls() # csv files should be present in Global Environment
  # > head(Workbook_A68930_0.1_uM)
  for (file in list.files(directory, full.names=TRUE)){
    name <- substr(file, start=6, nchar(file) - 4)
    name <- gsub(" ", "_", name)
    data <- read.csv(file)
    assign(name, data, envir=.GlobalEnv)
  }
}


concats <- function (data){
  # make a function that analyzes a matrix, M, with number of
  # columns being dim(M)[2]
  # for (i in 2:(dim(M)[2])){
  M <- data.matrix(data)
  
  z.score <- function (x){
    a <- abs((x-mean(x))/sd(x))
    return(a)
  }

  zero <- function(x, col, P.T_A.F){
    # the dimensions of the image are 640 x 480
    max <- ifelse(P.T_A.F==TRUE,(640),(640*480))
    bin <- seq(0,max,1)
    data <- data.matrix(x[col])
    binned <- .bincode(data, breaks = bin, right = TRUE, 
                       include.lowest = TRUE)
    out <- binned[which.max(tabulate(match(data,binned)))]-data
    return(out)
  }

  area.dev <- function(x){
    area <- x[2]
    area.bin <- seq(0,(640*480),100)
    area.binned <-.bincode(area, breaks = area.bin, right = TRUE, 
                           include.lowest = TRUE)
    area.mode <- area.binned[which.max(tabulate(match(area,
                                                      area.binned)))]
    area.dev <- area-area.dev
    z.area <- abs((area.dev-mean(area.dev))/sd(area.dev))
    z.area.mode <- abs((area.mode-mean(area.binned))/sd(area.binned))
    out <- ifelse(z.area<=z.area.mode && is.numeric(z.area)==TRUE,
                  TRUE,FALSE)
    return(out)
  }
  
}
>>>>>>> 833670961331206a6a65e4d5f0e24aa719f93797

# This function finds the max force values by removing values around a
# local maximum value. The input is the data to be filtered (preferably
# after being concatinated) with a bound that is simply the lower limit
# of which you want no values below it. The output is an array of the 
# filtered data.
first <- function(input,bound){
  data <- ifelse(input>=bound,input,0)
  x <- ifelse(is.na(data),0,data)
  y <- c(0,x[1:length(x)-1])
  z <- c(x[2:length(x)],0)
  one <- ifelse(x>y,1,NA)
  two <- ifelse(x>z,1,NA)
  three <- one*two*x
  four <- ifelse(three==0,NA,three)
  out <- four[!is.na(four)]
  return(out)
}
h <- function(input){
  v <- seq(0,100,1)
  out <- NULL
  for(i in v){
    out <- c(out,mean(first(concat(input,2),i)))
  }
  return(out)
}
-----------------------------------------------------------------
# Table of Contense
  # 1  : Moment function
  # 2  : Central Moment function
  # 3  : Moment Generating Function (MGF) function
  # 4  : Sum of Three Gamma Distributions function (Yo Gamma Gamma)
  # 5  : Random Number function
  # 6  : Zeroing function
  # 7  : Area Deviation Filter function
  # 8  : Filter function
  # 9  : Concatenate function
  # 10 : NA Filter function
  # 11 : All together
  # Scratch work
-----------------------------------------------------------------
#1
# This function finds the n'th moment of a set of data points,
# x is the data vector and n is the moment number
moment <- function(x,n){sum(x^n,na.rm=TRUE)/length(x)}
-----------------------------------------------------------------
#2
# Similarly, this function finds the n'th central moment of a set
# of data points, x is the data vector and n is the moment number
# (the first central moment of a set of data aproaches zero)
central.moment <- function(x,n){
  out <- sum((x-mean(x,na.rm=TRUE))^n,na.rm=TRUE)/(length(x)-1)
  return(out)}
-----------------------------------------------------------------
#3
# This function finds the n'th moment using the moment generating
# function for the gamma distribution, 
mgf <- function(shape,scale,n){
  out <- gamma(shape+n)/gamma(shape)*scale^(-n)
  return(out)}
-----------------------------------------------------------------
#4
# This function creates a vector of values that simulate the 
# sum of three gamma distributions, given the number of data
# points in the raw data and the other three parameter
# vectors each with three components shape (a1-3), scale
# (b1-3), function ratio (p1-3)
yogg     <- function(data, shape, scale, ratio){
  events.n <- ifelse(is.numeric(data),(data+1),(length(data)+1))
  out    <- c(rgamma((events.n*ratio[1]), shape[1], scale[1]),
              rgamma((events.n*ratio[2]), shape[2], scale[2]),
              rgamma((events.n*ratio[3]), shape[3], scale[3]))
  return(out)}
-----------------------------------------------------------------
#5
# This function that 'randomly' generates 3 values that can
# be used as the shape, scale, ratio vectors in yogg
rand.num <- function(min,max,int,sum.max){
  a <- runif(1,min,max)
  b <- runif(1,min,(max-a))
  c <- max-(a+b)
  d <- runif(1,min,max)
  e <- runif(1,min,max)
  f <- runif(1,min,max)
  .T <- c(a,b,c)
  .F <- c(d,e,f)
  if(sum.max==TRUE){
    return(.T)}
    else{
      return(.F)}}
-----------------------------------------------------------------
#6
# This function removes the mode value in the data from each
# value. x is the raw .csv file reference, col is the desired
# column to be converted, P.T_A.F is a TRUE/FALSE value (TRUE
# if the position/x-value column is selected and FALSE if the
# area column is selected)
zero <- function(data, col){
  bin <- seq(0,640,1)
  v.data <- (as.numeric(data.matrix(data[col])))
  binned <- .bincode(v.data, breaks = bin, right = TRUE,
                     include.lowest = TRUE)
  x <- v.data[as.numeric(which.max(table((.bincode(v.data, breaks = bin,
                                                   right = TRUE,
                                                   include.lowest = TRUE))))]
  y <- mean(v.data,na.rm=TRUE)
  x.mean <- mean((x-v.data),na.rm=TRUE)
  x.sd <- sd((x-v.data),na.rm=TRUE)
  x.z <- abs(mean(((x-v.data)-(x.mean))/(x.sd),na.rm=TRUE))
  y.mean <- mean((y-v.data),na.rm=TRUE)
  y.sd <- sd((y-v.data),na.rm=TRUE)
  y.z <- abs(mean(((y-v.data)-(y.mean))/(y.sd),na.rm=TRUE))
  out <- (ifelse(min(x.z,y.z)==x.z,x,y))-v.data
  return(out)}

zeros <- function(data, col,itt){
  r.data <- (as.numeric(data.matrix(data[col])))
  i.data <- (as.numeric(data.matrix(data[col])))
  r.mode <- numeric(0)
  r.z <- numeric(0)
  for(i in 1:as.numeric(itt)){
    bin <- seq(floor(min(i.data)),ceiling(max(i.data)),1)
    binned <- .bincode(i.data, breaks = bin, right = TRUE,
                       include.lowest = TRUE)
    r <- i.data[as.numeric(which.max(table(
      (.bincode(i.data, breaks = bin, right = TRUE,
                include.lowest = TRUE)))))]
    r.mode[i] <- r
    r.mean <- mean((r.mode[i]-i.data),na.rm=TRUE)
    r.sd <- sd((r.mode[i]-i.data),na.rm=TRUE)
    z <- abs(mean(((r.mode[i]-i.data)-(r.mean))/(r.sd),na.rm=TRUE))
    r.z[i] <- z
    if(min(r.z)==r.z[i] && !(min(r.mode)==r.z[1])){
      final.mode <- r.mode[i]
    }
    else{
      i.data <- r.mode[i]-i.data
    }
    i <- i+1
  }
  final.data <- final.mode-r.data
  return(final.data)}

concat.deriv <- function(x,init){
  final <- numeric(0)
  i <- init
  deriv <- function(data,col){
    Ddata <- numeric(0)
    x <- as.numeric(data.matrix(data[col]))
    for(i in 1:(length(x)-1)){
      Ddata[i] <- x[i+1]-x[i]}
   return(Ddata)
  }
  s <- seq(init,dim(x)[2],5)
  for(i in s){ 
    final <- c(final,deriv(x,i))
  }
 # finals <- c(mean(final),sd(final))
 # print(mean(final))
 # print(sd(final))
  z <- (final- mean(final))/sd(final)
 # hist(z)
  return(z)
}


  
  

-----------------------------------------------------------------
#7
# This function inputs the raw area data and outputs a Boolean
# TRUE/FALSE vector that expresses if the area deviates too far
# from the mean area value.
area.dev <- function(data,col,SD){
  area <- (as.numeric(data.matrix(data[col])))
  area.bin <- seq(0,(640*480),100)
  area.binned <-.bincode(area, breaks = area.bin, right = TRUE, include.lowest = TRUE)
  area.mode <- area[matrix(which.max(table((.bincode(area, breaks = area.bin, right = TRUE, include.lowest = TRUE)))),1,2)[1]]
  z.area <- (area.mode-area)/sd(area,na.rm=TRUE)
  out <- ifelse(z.area<=SD,1,NA)
  return(out)}
-----------------------------------------------------------------
#8
# This function combines zero() and area.dev() and removes the
# negative values.
filter <- function(Input, Element, Interval){
  zero <- function(ins, C){
    f <- data.matrix(ins[C])
    c <- mean(f,na.rm=TRUE, trim = 0.3)
    out <- c-f
    return(out)}
  area_dev <- function(In, c){
    g <- data.matrix(In[c])
    z_area <- (mean(g,na.rm = TRUE, trim = 0.3)-g)/sd(g,na.rm=TRUE)
    out <- ifelse(z_area<=2,1,NA)
    return(out)}
  col_zero <- (Element-1)*Interval+Element+1
  col_area <- (Element-1)*Interval+Element 
  out <- zero(Input, col_zero)*area_dev(Input, col_area)
  return(out)}
-----------------------------------------------------------------
#9
# This function concatenates the different columns of X's
# The inputs:
# Input = the raw .csv file, Intervals = the number of columns for each
# filming period (if there are two columns until the next filming period,
# e.g. area and x, then the Intervals value is 2).
# The output: 
# An array of normalized distance values (units are in pixels) that are
# concatinated together.
concat <- function(Inputs,Intervals){
  filter <- function(Input, Element, Interval){
    zero <- function(zeroInput, zeroColumn){
      numbers <- data.matrix(zeroInput[zeroColumn])
      central <- mean(f,na.rm=TRUE, trim = 0.3)
      # 'central' is the 30% trimmed mean to give a better value for the
      # central position of the fiber
      out <- as.vector((central-numbers), mode = "numeric")
      return(out)
    } # end of zero() function
    area_dev <- function(areaInput, areaColumn){
      g <- data.matrix(areaInput[areaColumn])
      z_area <- abs(mean(g,na.rm = TRUE, trim = 0.3)-g)/sd(g,na.rm=TRUE)
      # 'z_area' uses the trimmed mean to find the absolute value of the
      # z-score for the fiber's area values, the z-score is then used to
      # remove any valuses that deviate too far from the area's trimmed
      # mean.
      out <- as.vector(ifelse(z_area<=2,1,NA), mode = "numeric")
      return(out)
    } # end of area_dev() function
    col_zero <- (Element)*Interval
    # The column to be entered into the zero() function
    col_area <- (Element)*Interval-1
    # The column to be intered into the area_dev() function
    out <- as.vector(zero(Input, col_zero)*area_dev(Input, col_area), mode = "numeric")
    return(out)
  } # end of filter() function
  final <- NULL
  Max <- dim(Inputs)[2]/2
  elementSequence <- seq(1,Max,1)
  for (i in elementSequence){ 
    final <- c(final,filter(Inputs,elementSequence[i],Intervals))
  } # end of for-loop
  return(final)
} # end of concat() function
-----------------------------------------------------------------
#10

# This function removes the NA values.
dog <- function(x){
  deriv <- function(x){
    out <- NA
    for(i in 1:(length(x)-1)){
      out <- c(out,(x[i+1]-x[i]))}
    return(out)}
  swimfilter <- function(x,threshold){
    length(out) <- length(x)
    thresh <- ifelse(is.numeric(threshold),threshold,sqrt(1/0.0518))
    for(i in 2:(length(x)-1)){
      p <- ifelse((is.na(x[i-1]) && !is.na(x[i]) && is.na(x[i+1])),1,0)
      q <- ifelse(x[i-1] < x[i] && x[i+1] < x[i],1,0)
      r <- ifelse(x[i] >= thresh,1,0)
      s <- ifelse((p==1||q==1) && r==1, TRUE, FALSE)
      out[i] <- ifelse(s==TRUE,1,NA)
    }
    return(out)
  }
  feedfilter <- function(x,threshold){
    length(out) <- length(x)
    thresh <- ifelse(is.numeric(threshold),threshold,sqrt(1/0.0518))
    for(i in 2:(length(x)-1)){
      p <- ifelse((is.na(x[i-1]) && is.na(x[i]) && !is.na(x[i+1])),1,0)
      q <- ifelse((!is.na(x[i-1]) && is.na(x[i]) && is.na(x[i+1])),1,0)
      r <- ifelse(x[i] < thresh,1,0)
      s <- ifelse((p==1||q==1) && r==1, TRUE, FALSE)
      out[i] <- ifelse(s==TRUE,1,NA)
    }
    return(out)
  }
  for (i in 2:(length(x)-1)){
    if(((!is.na(x[i-1]) && is.na(x[i]) && !(is.na(x[i+1])))) ||
         (is.na(x[i-1]) && !is.na(x[i]) && (is.na(x[i+1])))){
      x[i] <- x[i]}
    else{x[i] <- NA}
    i <- i+1}
  return(x)}



#need fixing
dog.inv <- function(x){
  for (i in 2:(length(x)-1)){
    if(((!is.na(x[i-1]) && is.na(x[i]) && (is.na(x[i+1])))) ||
         (is.na(x[i-1]) && is.na(x[i]) && !(is.na(x[i+1])))){
      x[i] <- NA}
    else{x[i] <- x[i]}
    i <- i+1}
  return(x)}

swimfilter <- function(x,threshold){
  length(out) <- length(x)
  thresh <- ifelse(is.numeric(threshold),threshold,sqrt(1/0.0518))
  for(i in 2:(length(x)-1)){
    p <- ifelse((is.na(x[i-1]) && !is.na(x[i]) && is.na(x[i+1])),1,0)
    q <- ifelse(x[i-1] < x[i] && x[i+1] < x[i],1,0)
    r <- ifelse(x[i] >= thresh,1,0)
    s <- ifelse((p==1||q==1) && r==1, TRUE, FALSE)
    out[i] <- ifelse(s==TRUE,1,NA)
  }
  return(out)
}

feedfilter <- function(x){
  length(out) <- length(x)
  for(i in 2:(length(x)-1)){
    p <- ifelse((is.na(x[i-1]) && is.na(x[i]) && !is.na(x[i+1])),1,0)
    q <- ifelse((!is.na(x[i-1]) && is.na(x[i]) && is.na(x[i+1])),1,0)
    r <- ifelse(abs(x[i]-x[i-1])<200 || abs(x[i+1]-x[i])<200,1,0)
    s <- ifelse((p==1||q==1) , TRUE, FALSE) # && r==1
    out[i] <- ifelse(s==TRUE,1,NA)
  }
  return(out)
}
if(a<b || b>0){1}
-----------------------------------------------------------------
#11,INIT,ForceData_t_f
All <- function(DATA,STD){
  concat <- function(xdata,ySD){
    filter <- function(Data, Element, SD){
      deriv <- function(data,col){
        Ddata <- NULL
        Ddata[1] <- NA
        x <- as.numeric(data.matrix(data[col]))
        for(i in 2:(length(x)-1)){
          Ddata <- c(Ddata,(x[i]-x[i-1]))}
        return(Ddata)}
      zero <- function(Data, Col){
        z <-((deriv(Data,Col)-mean(deriv(Data,Col),na.rm=TRUE))
             /sd(deriv(Data,Col),na.rm=TRUE))
        f <- (as.vector(data.matrix(Data[Col])))
        c.data <- ifelse(abs(z)<=1,f,NA)
        c <- mean(c.data,na.rm=TRUE)
        out <- c-f
        return(out)}
      area.dev <- function(.data, .col, .SD){
        area <- (as.numeric(data.matrix(.data[.col])))
        area.bin <- seq(0,(640*480),100)
        area.binned <-.bincode(area, breaks = area.bin, right = TRUE, include.lowest = TRUE)
        area.mode <- area[matrix(which.max(table((.bincode(area, breaks = area.bin, right = TRUE, include.lowest = TRUE)))),1,2)[1]]
        z.area <- (area.mode-area)/sd(area,na.rm=TRUE)
        out <- ifelse(z.area<=.SD,1,NA)
        return(out)}
      col.zero <- (INIT-1)*2+INIT+1
      col.area.dev <- (INIT-1)*2+INIT
      out <- zero(data, col.zero)*area.dev(data, col.area.dev, SD)
      return(out)}}
#  dog <- function(x){
#    for (i in 2:(length(x))){
#      if((is.na(x[i]) && !(is.na(x[i+1]))) ||
#           (is.na(x[i+1]) && !(is.na(x[i])))){
#        x[i] <- x[i]}
#      else{x[i] <- NA}
#      i <- i+2}
#    return(x[!is.na(x)])}
  out <- (concat(DATA,STD))#*ifelse((ForceData_t_f)==T,0.0518,1)
  return(out)}
-----------------------------------------------------------------





-----------------------------------------------------------------
# Above this line are the important functions. Below is scratch work
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

E1 <- moment(W,1)   #first moment
E2 <- moment(W,2) #second moment
E3 <- moment(W,3) #third moment
E4 <- moment(W,4) #fourth moment
E5 <- moment(W,5) #fifth moment
V0  <- runif(1,0,(1/3)) #a number from 0-1
V000  <- runif(1,0,(1/3))
V00 <- ifelse((1-(V0+V000))<0,FALSE,(1-(V0+V000)))
v <- c(V0,V00,V000)
v
sum(v)
a  <-l1
b  <- l2
c  <-l3
d  <-
X1 <- a/b
X2 <- (a(a+1))/(b^2)
X3 <- (a(a+1)(a+2))/(b^3)
X4 <- (a(a+1)(a+2)(a+3))/(b^4)
X5 <- (a(a+1)(a+2)(a+3)(a+4))/(b^5)
Y1 <- c/d
Y2 <- (c(c+1))/(d^2)
Y3 <- (c(c+1)(c+2))/(d^3)
Y4 <- (c(c+1)(c+2)(c+3))/(d^4)
Y5 <- (c(c+1)(c+2)(c+3)(c+4))/(d^5)
Z1 <- W*X1 + w*Y1
Z2 <- W^2*X2 + 2*W*w*X1*Y1 + w^2*Y2
Z3 <- W^3*X3 + 3*W^2*w*X2*Y1 + 3*W*w^2*X1*Y2 + w^3*Y3
Z4 <- W^4*X4 + 4*W^3*w*X3*Y1 + 6*W^2*w^2*X2*Y2 + 4*W*w^3*X1*Y3 + w^4*Y4
Z5 <- W^5*X5 + 5*W^4*w*X4*Y1 + 10*W^3*w^2*X3*Y2 + 10*W^2*w^3*X2*Y3 + 5*W*w^4*X1*Y4 + w^5*Y5

# plot a random gamma distribution

n <-  length(Z)
.min <- 1
.max <- 30
l1 <- runif(1,min = 0, max = 0.5)
l20 <- runif(1,min = 0.5, max = 1)
l2 <- ifelse(l20==l1||(l1+l20)>=1,(runif(1,min = 0, max = 1)),l20)
l3 <- 1-(l1+l2)
p <- runif(1,min = .min, max = .max)
q <- runif(1,min = .min, max = .max)
r <- ifelse(runif(1,min = .min, max = .max)==p,(runif(1,min = .min, max = .max)),r0)
s <- ifelse(runif(1,min = .min, max = .max)==q,(runif(1,min = .min, max = .max)),s0)
t <- ifelse((runif(1,min = .min, max = .max)==p||runif(1,min = .min, max = .max)==r),(runif(1,min = .min, max = .max)),t0) 
u <- ifelse((runif(1,min = .min, max = .max)==q||runif(1,min = .min, max = .max)==s),(runif(1,min = .min, max = .max)),u0)
w1 <- c(l1,l2,l3)
w2 <- c(p,r,t)
w3 <- c(q,s,u)
w4 <- rgamma(n,shape = median(w2), scale = median(w3))
k <- rgamma(l1*n,shape = p, scale = q)
j <- rgamma(l2*n,shape = r, scale = s)
m <- rgamma(l3*n,shape = t, scale = u)
f <- c(k,j,m)
length(f) <- length(Z)
hist(Z,40)
hist(f,40)
phi0 <-c(p,r,t)
phi1 <-c(q,s,u)
#(mean(Z)-mean(f))^2+(min(Z)-min(f))^2+(max(Z)-max(f))^2
c(E1,sum(phi0*phi1)/3,(E1-sum(phi0*phi1)/3),pnorm(((sum(phi0*phi1)/3)-E1)/sd(Z),0,1))
bins <- seq(0,640,1)
databin <- .bincode(Z,breaks=bins,right=TRUE,include.lowest=TRUE)
simbin <- .bincode(f,breaks=bins,right=TRUE,include.lowest=TRUE)
simdev <- (databin-simbin)
length(simdev) <- length(databin)
hist(databin)
hist(simbin)
#plot(databin,simbin,'p')
plot(databin,simdev,'l')
moment(Z,1)-moment(f,1)
c.moment(Z,2)-c.moment(f,2)
matrix(c(l1,l2,l3,p,r,t,q,s,u),3,3)
l1+l2+l3

# <- function(){
#  gshape <- runif(1, 0, 5)
#  gscale <- runif(1, 0, 5)
#  nobs <- 2000
#  return(list(gshape= gshape,
#              gscale= gscale,
#              nobs= nobs))
#}
for(i in 1:10){
  hist(rgamma(1000,i,1))
  print(i*i)
}

gshape <- runif(1, 0, 5)
gscale <- runif(1, 0, 5)
nobs <- 2000

mygamma <- function(nobs, shape, scale){
  ## gamma distribution with variable shape and scale
  
  # manipulate shape for gamma distribution # ignore for now
  
  # manipulate scale for gamma distribution # ignore for now
  
  # generate gamma distribution
  output <- rgamma(nobs, shape, scale)
  return(output)
}

v1 <- mygamma(nobs= nobs,
              shape = 2,
              scale = 3)
hist(v1)
v2 <- mygamma(nobs= nobs,
              gshape = gshape,
              gscale = gscale)

newgamma <- function(v1, v2){
  output <- v1 + v2
  return(output)
}

hist(newgamma(v1,v2))
