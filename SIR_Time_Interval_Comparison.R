# Simulation of an SIR Model
# Anis Jonischkeit - Scientific and Parallel Computing


# Set up our time increment and our vector (array) of x (time) values
deltaT = 1                 # time interval

# from, to, time interval
t = seq(0,20,deltaT)

# Constants
r = 0.00218                     # infection rate: people/(1 day)
a = 0.5                         # recovery rate: (infected people)/(1 day)

rT = deltaT * r                 # r at at a single time step
aT = deltaT * a                 # a at at a single time step

# Set up our stock variables and initial conditions
S = vector(length=length(t))    # susceptibles
I = vector(length=length(t))    # infecteds
R = vector(length=length(t))    # recovereds

S[1] = 762
I[1] = 1
R[1] = 0


deltaT2 = 0.1                 # time interval
t2 = seq(0,20,deltaT2)

rT2 = deltaT2 * r                 # r at at a single time step
aT2 = deltaT2 * a                 # a at at a single time step

# Set up our stock variables and initial conditions
S2 = vector(length=length(t2))    # susceptibles
I2 = vector(length=length(t2))    # infecteds
R2 = vector(length=length(t2))    # recovereds

S2[1] = 762
I2[1] = 1
R2[1] = 0

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

# Loop a standard number of times, starting with i=2 since we've already
# set up the simulation's initial conditions at i=1.
for (i in 2:length(t2)) {
    rSI = rT2 * S2[i-1] * I2[i-1]
    aI = aT2 * I2[i-1]

    dS = 0 - rSI
    dI = rSI - aI
    dR = aI

    S2[i] = S2[i-1] + dS
    I2[i] = I2[i-1] + dI
    R2[i] = R2[i-1] + dR
}

par(mfrow=c(3,1))

plot(t, S, type="l", col="black", lwd=2, xlab='Time (days)', ylab='Number of Susceptibles', ylim=c(0, 800))#, xlab='Time', ylab='People')
# plot(t2, S2, type="l", col="green", lwd=2, xlab='Time (days)', ylab='Number of People', ylim=c(0, 800))#, xlab='Time', ylab='People')
lines(t2, S2, col="green", lwd=2)
# legend(16, 550, c("Susceptible", "Infected", "Recovered"), lty=c(1, 1), lwd=c(2,2), col=c("black", "green", "blue") )

plot(t, I, type="l", col="black", lwd=2, xlab='Time (days)', ylab='Number of Infected', ylim=c(0, 800))#, xlab='Time', ylab='People')
lines(t2, I2, col="green", lwd=2)

plot(t, R, type="l", col="black", lwd=2, xlab='Time (days)', ylab='Number of Recovered', ylim=c(0, 800))#, xlab='Time', ylab='People')
lines(t2, R2, col="green", lwd=2)


# plot(t, S, type="l", col="black", lwd=2, xlab='Time (days)', ylab='Susceptible', ylim=c(0, 800))#, xlab='Time', ylab='People')
# plot(t, I, type="l", col="green", lwd=2, xlab='Time (days)', ylab='Infected')#, ann=FALSE, axes=FALSE,)
# plot(t, R, type="l", col="blue", lwd=2, xlab='Time (days)', ylab='Recovered', ylim=c(0, 800))#, ann=FALSE, axes=FALSE,)

