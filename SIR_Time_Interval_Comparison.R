# Simulation of an SIR Model
# Anis Jonischkeit - Scientific and Parallel Computing

# Set up time intervals
deltaTs = c(2, 1, 0.1, 0.01, 0.001)
colors = c("black", "green", "blue", "purple", "orange")

# create list for t for each time interval
ts = list()

# Constants
r = 0.00218                     # infection rate: people/(1 day)
a = 0.5                         # recovery rate: (infected people)/(1 day)

# list of S, I, R vectors for the different time steps
Ss = list()                     # Susceptibles for each time interval
Is = list()                     # Infected for each time interval
Rs = list()                     # Recovered for each time interval

# calculate SIR graphs for different time steps
for (i in 1:length(deltaTs)) {
    t = seq(0,14,deltaTs[i])

    rT = deltaTs[i] * r                 # r at at a single time step
    aT = deltaTs[i] * a                 # a at at a single time step

    S = vector(length=length(t))
    I = vector(length=length(t))
    R = vector(length=length(t))

    S[1] = 762
    I[1] = 1
    R[1] = 0

    # calculate SIR graph for current time step
    for (j in 2:length(t)) {
        rSI = rT * S[j-1] * I[j-1]
        aI = aT * I[j-1]

        dS = 0 - rSI
        dI = rSI - aI
        dR = aI

        S[j] = S[j-1] + dS
        I[j] = I[j-1] + dI
        R[j] = R[j-1] + dR
        
    }

    # Save S, I and R into list of Ss, Is, and Rs
    Ss[[i]] = S
    Is[[i]] = I
    Rs[[i]] = R

    # Save t into list of ts
    ts[[i]] = t
}

# Function to nicely write text in the legend
formatLegend = function (item) {
    if (item < 1) {
        return(paste(1/item, "th of a day time step", sep=""))
    } else{
        return(paste(item, " day time step", sep=""))
    }
}

# split view into 3 rows
par(mfrow=c(3,1))

# plot points for all Ss
plot(unlist(ts[[1]]), unlist(Ss[[1]]), type="l", col=colors[1], lwd=2, xlab='Time (days)', ylab='Number of Susceptibles', ylim=c(0, 800))
# Start at 2 since the first line has already been plotted
for (i in 2:length(deltaTs)) {
    lines(unlist(ts[[i]]), unlist(Ss[[i]]), col=colors[i], lwd=2)
}
legend(0, 600, sapply(deltaTs, formatLegend), lty=c(1, 1), lwd=c(2,2), col=colors )

# plot points for all Is
plot(unlist(ts[[1]]), unlist(Is[[1]]), type="l", col=colors[1], lwd=2, xlab='Time (days)', ylab='Number of Infected', ylim=c(0, 800))
# Start at 2 since the first line has already been plotted
for (i in 2:length(deltaTs)) {
    lines(unlist(ts[[i]]), unlist(Is[[i]]), col=colors[i], lwd=2)
}
legend(0, 800, sapply(deltaTs, formatLegend), lty=c(1, 1), lwd=c(2,2), col=colors )

# plot points for all Rs
plot(unlist(ts[[1]]), unlist(Rs[[1]]), type="l", col=colors[1], lwd=2, xlab='Time (days)', ylab='Number of Recovered', ylim=c(0, 800))
# Start at 2 since the first line has already been plotted
for (i in 2:length(deltaTs)) {
    lines(unlist(ts[[i]]), unlist(Rs[[i]]), col=colors[i], lwd=2)
}
legend(0, 800, sapply(deltaTs, formatLegend), lty=c(1, 1), lwd=c(2,2), col=colors )