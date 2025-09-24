
# Define the parameters
D <- 15000  # Annual demand
C <- 80     # Unit cost
H <- 0.18 * C  # Holding cost
S <- 220    # Ordering cost

# Define the Total Cost function
total_cost <- function(Q) {
  holding_cost <- (Q / 2) * H
  ordering_cost <- (D / Q) * S
  total_cost <- holding_cost + ordering_cost
  return(total_cost)
}

# Create a range of order quantities
Q_values <- seq(100, 2000, by = 100)

# Calculate total costs for each order quantity
total_costs <- sapply(Q_values, total_cost)

# optimal order quantity
optimal_Q <- Q_values[which.min(total_costs)]
optimal_cost <- min(total_costs)

# Print the results
cat("Optimal Order Quantity:", optimal_Q, "\n")
cat("Minimum Total Cost:", optimal_cost, "\n")

# Plot Total Cost vs. Order Quantity
plot(Q_values, total_costs, type = "l", xlab = "Order Quantity (Q)", ylab = "Total Cost", main = "Total Cost vs. Order Quantity")
abline(v = optimal_Q, col = "Darkblue", lty = 2)


#PART 2

library(triangle)

# Define the parameters
D_min <- 13000
D_max <- 17000
D_mode <- 15000
n_simulations <- 1000

# Perform the simulation
set.seed(123)  # For reproducibility
simulated_demands <- rtriangle(n_simulations, D_min, D_max, D_mode)

# calculate total cost for a given demand
calculate_total_cost <- function(D) {
  Q <- sqrt((2 * D * S) / H)
  holding_cost <- (Q / 2) * H
  ordering_cost <- (D / Q) * S
  total_cost <- holding_cost + ordering_cost
  return(total_cost)
}

# total costs for each simulated demand
simulated_costs <- sapply(simulated_demands, calculate_total_cost)

# Estimate expected minimum total cost and 95% confidence interval
mean_cost <- mean(simulated_costs)
ci_cost <- quantile(simulated_costs, c(0.025, 0.975))

cat("Expected Minimum Total Cost:", mean_cost, "\n")
cat("95% Confidence Interval for Total Cost:", ci_cost, "\n")
