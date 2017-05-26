
# Simulation of fall of ball
# Introduction to Computational Science -- Shiflet & Shiflet 
# Module 4.1, example 1, p.115
# R solution by Stephen Davies, University of Mary Washington

# Set up our time increment and our vector (array) of x (time) values
deltaX = 0.01                   # s

# from, to, interval
t = seq(0,14,deltaX)             # s

# Constants
r = 0.00218                     # infection rate: people/day
a = 0.5                         # recovery rate: (infected people)/day

rT = deltaX * r                 # r at at a single time step
aT = deltaX * a                 # a at at a single time step

# Set up our stock variables and initial conditions
S = vector(length=length(t))     # susceptibles
I = vector(length=length(t))        # infecteds
R = vector(length=length(t))       # recovereds

S[1] = 762
I[1] = 1
R[1] = 0


# Loop a standard number of times, starting with i=2 since we've already
# set up the simulation's initial conditions at i=1.
for (i in 2:length(t)) {
    rSI = rT * S[i-1] * I[i-1]
    aI = aT * I[i-1]

    dS = 0 - rSI
    dI = rSI - aI
    dR = aI

    S[i] = S[i-1] + dS
    I[i] = I[i-1] + dI
    R[i] = R[i-1] + dR
}

# speed = abs(velocity)

par(mfrow=c(1,4))

plot(t, S, type="l", col="black", lwd=2, xlab='Time (days)', ylab='Number of People')#, xlab='Time', ylab='People')
lines(t, I, col="green", lwd=2)
lines(t, R, col="blue", lwd=2)
legend(10, 400, c("Susceptible", "Infected", "Recovered"), lty=c(1, 1), lwd=c(2,2), col=c("black", "green", "blue") )

plot(t, S, type="l", col="black", lwd=2, xlab='Time (days)', ylab='Number of People')#, xlab='Time', ylab='People')
plot(t, I, type="l", col="green", lwd=2, xlab='Time (days)', ylab='Infected')#, ann=FALSE, axes=FALSE,)
plot(t, R, type="l", col="green", lwd=2, xlab='Time (days)', ylab='Recovered')#, ann=FALSE, axes=FALSE,)
# par(new=TRUE)
# plot(t,R,type="l",col="blue",lwd=2, ann=FALSE, axes=FALSE,)
# points(t, I, pch="*", col= 'red')

