#Create landscape using the MW perlin noise algorithm as implemented by Barkoczi and Galesic
MasonWatts<-function(L){
  R <- 3 * (L/100) #variance term
  rho <- 0.7
  #1. create function matrix as a unimodal bivariate Gaussian with the mean randomly chosen, with variance R
  #generate two random means
  X <- dnorm(seq(1,L), mean = runif(1,1,L), sd=sqrt(R))
  Y <- dnorm(seq(1,L), mean = runif(1,1,L), sd=sqrt(R))
  #PJ IS TRYING PUTTING MEAN IN THE CENTER ALWAYS
  #X <- dnorm(seq(1,L), mean = round(L/2), sd=sqrt(R))
  #Y <- dnorm(seq(1,L), mean = round(L/2), sd=sqrt(R))
  
  
  fitnessMatrix <- X %*% t(Y)
  #scale to between 0 and 1 (Not sure if this is what they do)
  fitnessMatrix <- fitnessMatrix * (1/max(fitnessMatrix))
  #2. compute psuedorandom Perlin noise
  #2a loop through octaves and randomly draw values
  for (omega in 3:7){
    octave <- 2^omega  #scale octave to account for grids larger than original 100x100
    #create a smaller matrix, containing only randomly assigned payoffs corresponding to the cells affected by the octave
    octaveMatrix <- matrix(runif(octave^2),ncol=octave, nrow=octave)
    #center octave sequence on median of grid
    octaveSeq <- seq(1,L, length.out=octave)
    #2b. smooth values of all cell values using bicubic interpolation
    octaveMatrix <- bicubic.grid(x=octaveSeq,y=octaveSeq,z=octaveMatrix,xlim=c(1,L),ylim=c(1,L),dx=1,dy=1) #NOTE: dx= and dy= added by PJ to fix error in github code
    #2c. scale matrix by the persistence paramter
    octaveMatrix <- octaveMatrix$z * rho^omega
    #3. sum together
    fitnessMatrix <- fitnessMatrix + octaveMatrix
  }
  #3 continued... scale fitnessMatrix to between 1 and 100
  fitnessMatrix <- fitnessMatrix * (100/max(fitnessMatrix))
  
  
  grid_size <-L
  Grid <-tibble(expand.grid(x=c(1:grid_size),y=c(1:grid_size)))
  
  return(Grid %>% add_column(z=as.vector(fitnessMatrix)))
  #3D plotting example
  #https://cran.r-project.org/web/packages/plot3D/vignettes/volcano.pdf
  #persp3D(z = test, clab = "m")
}