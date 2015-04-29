# read the data
t.in <- read.delim("~/projects/Danny/MichalExampleRecursion.txt")

t.in <- as.matrix(t.in)

# making order in the input table
splitIntoPairs <- function(t)
{
  outt <- NULL 
  nr <- dim(t)[1]
  for (i in 1:nr)
  {
    children <- strsplit(t[i,2],";")[[1]]
    nchildren <- length(children)
    if (nchildren >0)
    {
       for (j in 1:nchildren)
       {
         outt <- rbind(outt, c(t[i,1], children[j]))
       }
    }  
  }
  outt
}

# use it on the original data
t.in.pairs <- splitIntoPairs(t.in)

# single step for next generation search, output - a vector or NULL
getNextGen <- function(t, x) # table, id
{ 
  out <- NULL
  idx <- which (t[,1]==x)
  out <- t[idx,2]
  out
}

# for the final output
spitToVector <- function(l)
{
  out <- NULL
  for (i in 1:length(l))
    out[i] <- paste(l[[i]], collapse=";")
  out
}

# Recursive search
getAllGen <- function(t, x, vectorize=TRUE, maxgen=20) 
{
  out <- list()
  curr.gen <- x
  generation <- 1
  
  while (!is.null(curr.gen) && generation <=maxgen)
  {
    out[[generation]] <- curr.gen
    next.gen <- NULL
    ll <- length(curr.gen)
    for (i in 1:ll)
    {
      nnn <- getNextGen(t, curr.gen[i])
      if (length(nnn)>0)  next.gen <- c(next.gen, nnn)
    }
    curr.gen <- unique(next.gen)
    generation <- generation +1
  }  
  
  if (vectorize) out <- spitToVector(out)
  out
}

# use
#> tmp <- getAllGen(t.in.pairs, "FYPO:0000148")
#> tmp
#[1] "FYPO:0000148"                                       
#[2] "FYPO:0000059;FYPO:0001334"                          
#[3] "FYPO:0000011;FYPO:0000145;FYPO:0001331"             
#[4] "FYPO:0000628;FYPO:0000631;FYPO:0000114;FYPO:0001320"
#[5] "FYPO:0000114;FYPO:0003037;FYPO:0000002;FYPO:0000300"
#[6] "FYPO:0000002;FYPO:0000300;FYPO:0001985;FYPO:0000001"
#[7] "FYPO:0000001"                                       
 






