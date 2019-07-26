# Simulate the time of day for melting outcomes
melting_data <- readRDS(here("output"), data.RDS)
plot(melting_data$Time)

time.adj <- ifelse(melting_data$Time < 0.2, 
                   melting_data$Time + 1, 
                   melting_data$Time)
med <- median(time.adj)
sdev <- sd(time.adj

sim <- rnorm(10000, med, sdev)
sim.dis <- round(sim*1440, 0)

# Simulate the times chosen

# Calculate expected value of each minute
quantile(time.adj, 0.01)
quantile(time.adj, 0.99)

bet.choices <- seq(386, 1549, by = 1)

#perc.choices <- ecdf(sim.dis)
#prob.choices <- perc.choices(bet.choices)
#one.sd <- perc.choices(seq(648, 1166, by = 1))
           
# Assuming only one winner
#sum(prob.choices * 250000) 
#prod(one.sd * 250000 - 2.5)
