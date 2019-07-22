# Check the description here:
# https://en.wikipedia.org/wiki/100_prisoners_problem

# Function to get length of cycles for a series:
get_cycles <- function(series){
  # n <- 10
  # series <- sample(1:n)
  # Preparation:
  n <- length(series)
  cycle_length <- NULL
  
  # s2 = remaining numbers not in a cycle
  s2 <- series
  
  while(length(s2)>0){
    
    # s3 = create cycle:
    s3 <- rep(0,length(s2))
    s3[1] <- s2[1]
    i <- 2
    cand <- 0
    
    # Get one cycle:
    while(cand != s3[1]){
      cand <- series[s3[i-1]]
      s3[i] <- cand
      s3
      i <- i+1
    }
    s3
    # length of one cycle:
    cycle_length <- c(cycle_length, sum(s3>0)-1 )
    s2 <- s2[!s2 %in% s3]
  }
  cycle_length
}

# Try once:
n <- 100
boxes <- sample(1:n)
get_cycles(boxes)

# Repeats:
n <- 100
m <- 1e5
boxes <- replicate(sample(1:n),n = m)
longest_cycles <- apply(boxes, 2, function(x) max(get_cycles(x)))

# Plot results:
h <- hist(longest_cycles, breaks = 100, plot=F)
plot(h, 
     xlim = c(0,100),
     freq = F,
     col = c("green","red")[cut(h$breaks, c(-Inf,50,Inf))])

# Now we have the distribution of longest cycles per trial. 
# If the longest cycle is <50, all prisoners get free, otherwise none.
# The probability of success is about 0.31 

sum(h$counts[h$breaks<50]) / sum(h$counts)
quantile(longest_cycles,probs = seq(0,1,0.1))

# Check results:
m <- 1e3
boxes <- replicate(sample(1:n),n = m)
cycles <- apply(boxes, 2, function(x) get_cycles(x))
lapply(cycles,max)

# Choose an interesting experiment:
b <- boxes[,6]
get_cycles(b)
res <- data.frame(prisoner=b, trials = NA)

# Algorithm:
# 1. Jeder Gefangene öffnet die Schublade mit seiner eigenen Nummer.
# 2. Als nächstes öffnet er die Schublade mit der neuen Nummer.
# 3. Der Gefangene wiederholt Schritt 2 so lange, bis er seine eigene Nummer gefunden hat oder bis er 50 Schubladen geöffnet hat.

success <- 0
i <- sample(b,1)
for(i in sample(b)){
  new <- i
  for(j in 1:50){
    new <- b[new]
    if(new == i) break()
  }
  if(new == i) { 
    res$trials[res$prisoner==i] <- j
  }else{ stop(i, " not found") }
  success <- success + 1
}  

success
res

