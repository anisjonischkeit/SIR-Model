# Simulation of an SIR Model
# Anis Jonischkeit - Scientific and Parallel Computing

length(deltaTs) = 4
deltaTs = c(2, 1, 0.1, 0.01, 0.001)

colors = c("black", "green", "blue", "orange", "purple")

ts = list()

r = 0.00218                     # infection rate: people/(1 day)
a = 0.5                         # recovery rate: (infected people)/(1 day)

Ss = list()
Is = list()
Rs = list()

for (i in 1:length(deltaTs)) {
    t = seq(0,14,deltaTs[i])

    rT = deltaTs[i] * r                 # r at at a single time step
    aT = deltaTs[i] * a                 # a at at a single time step

    print(deltaTs[i])
    S = vector(length=length(t))
    I = vector(length=length(t))
    R = vector(length=length(t))

    S[1] = 762
    I[1] = 1
    R[1] = 0

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

    Ss[[i]] = S
    Is[[i]] = I
    Rs[[i]] = R

    ts[[i]] = t
    # sapply(x, function (l) l[2])
}

par(mfrow=c(3,1))

formatLegend = function (item) {
    if (item < 1) {
        return(paste(1/item, "th of a day time intervals", sep=""))
    } else{
        return(paste(item, " day time intervals", sep=""))
    }
}

plot(unlist(ts[[1]]), unlist(Ss[[1]]), type="l", col=colors[1], lwd=2, xlab='Time (days)', ylab='Number of Susceptibles', ylim=c(0, 800))
for (i in 2:length(deltaTs)) {
    lines(unlist(ts[[i]]), unlist(Ss[[i]]), col=colors[i], lwd=2)
}
legend(0, 600, sapply(deltaTs, formatLegend), lty=c(1, 1), lwd=c(2,2), col=colors )

plot(unlist(ts[[1]]), unlist(Is[[1]]), type="l", col=colors[1], lwd=2, xlab='Time (days)', ylab='Number of Susceptibles', ylim=c(0, 800))
for (i in 2:length(deltaTs)) {
    lines(unlist(ts[[i]]), unlist(Is[[i]]), col=colors[i], lwd=2)
}
legend(0, 800, sapply(deltaTs, formatLegend), lty=c(1, 1), lwd=c(2,2), col=colors )

plot(unlist(ts[[1]]), unlist(Rs[[1]]), type="l", col=colors[1], lwd=2, xlab='Time (days)', ylab='Number of Susceptibles', ylim=c(0, 800))
for (i in 2:length(deltaTs)) {
    lines(unlist(ts[[i]]), unlist(Rs[[i]]), col=colors[i], lwd=2)
}

legend(0, 800, sapply(deltaTs, formatLegend), lty=c(1, 1), lwd=c(2,2), col=colors )