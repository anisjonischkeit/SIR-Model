
# Simulation of an SIR Model
# Anis Jonischkeit - Scientific and Parallel Computing

# Set up our time increment and our vector (array) of x (time) values
deltaT = 0.01                   # time interval

# start day, end day, time interval
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

par(mfrow=c(1,1))

plot(t, S, type="l", col="black", lwd=2, xlab='Time (days)', ylab='Number of People', ylim=c(0, 800))
lines(t, I, col="green", lwd=2)
lines(t, R, col="blue", lwd=2)
legend(15, 550, c("Susceptible", "Infected", "Recovered"), lty=c(1, 1), lwd=c(2,2), col=c("black", "green", "blue") )