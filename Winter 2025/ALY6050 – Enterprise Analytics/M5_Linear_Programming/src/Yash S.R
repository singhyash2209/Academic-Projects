#Authors: Yash S
#Created: 2025-03-23
#Edited: 2025-03-23
#Course: ALY6050
#Assignment 5

library(lpSolve)

# Pressure Washer: $169.99, Go-Kart: $359.99, Generator: $290.99, Water Pump: $714.95 (per case of 5)
obj <- c(169.99, 359.99, 290.99, 714.95)

# Constraint matrix (left-hand side coefficients)
# Row 1: Budget constraint (330x1 + 370x2 + 410x3 + 635x4 <= 170,000)
# Row 2: Warehouse space constraint (25x1 + 40x2 + 25x3 + 25x4 <= 12,300)
# Row 3: Marketing constraint 1 (0.7x1 + 0.7x2 - 0.3x3 - 0.3x4 >= 0)
# Row 4: Marketing constraint 2 (x3 - 2x4 >= 0)
con <- matrix(c(
  330, 370, 410, 635,    # Budget constraint
  25, 40, 25, 25,        # Warehouse space constraint
  0.7, 0.7, -0.3, -0.3,  # Marketing constraint 1
  0, 0, 1, -2            # Marketing constraint 2
), nrow = 4, byrow = TRUE)

# Inequality directions

dir <- c("<=", "<=", ">=", ">=")

# Right-hand side values
rhs <- c(170000, 12300, 0, 0)

# Step 2: Solve the LP problem
solution <- lp("max", obj, con, dir, rhs)

# Step 3: Display the results
cat("Optimal Values (x1, x2, x3, x4):\n")
print(solution$solution)

cat("\nMaximum Profit:\n")
print(solution$objval)

# Step 4: Sensitivity analysis
solution_sens <- lp("max", obj, con, dir, rhs, compute.sens = TRUE)

# Shadow prices (dual values)
cat("\nShadow Prices (Dual Values):\n")
print(solution_sens$duals)

# Sensitivity of objective coefficients
cat("\nSensitivity of Objective Coefficients:\n")
print(solution_sens$sens.coef)

# Step 5: Interpret the results
# Optimal inventory levels
x1 <- solution$solution[1]  # Pressure Washers
x2 <- solution$solution[2]  # Go-Karts
x3 <- solution$solution[3]  # Generators
x4 <- solution$solution[4]  # Cases of Water Pumps

# Total inventory
total_inventory <- x1 + x2 + x3 + x4

# Total cost
total_cost <- 330 * x1 + 370 * x2 + 410 * x3 + 635 * x4

# Total space used
total_space <- 25 * x1 + 40 * x2 + 25 * x3 + 25 * x4

# Step 6: Print summary
cat("\nSummary of Results:\n")
cat("Pressure Washers (x1):", x1, "\n")
cat("Go-Karts (x2):", x2, "\n")
cat("Generators (x3):", x3, "\n")
cat("Cases of Water Pumps (x4):", x4, "\n")
cat("Total Inventory:", total_inventory, "\n")
cat("Total Cost:", total_cost, "\n")
cat("Total Space Used:", total_space, "\n")
cat("Maximum Profit:", solution$objval, "\n")

# Step 7: Answer additional questions
# Smallest selling price for zero-value decision variable
# Check if any decision variable is zero
if (any(solution$solution == 0)) {
  zero_index <- which(solution$solution == 0)
  cat("\nDecision variable", zero_index, "has an optimal value of zero.\n")
  cat("Smallest selling price to make it non-zero:", obj[zero_index] + solution_sens$sens.coef[zero_index], "\n")
}

# Additional budget allocation
shadow_price_budget <- solution_sens$duals[1]
if (shadow_price_budget > 0) {
  cat("\nShadow price of budget constraint:", shadow_price_budget, "\n")
  cat("Recommend allocating additional budget. Each additional dollar will increase profit by", shadow_price_budget, "dollars.\n")
} else {
  cat("\nNo need to allocate additional budget.\n")
}

# Warehouse size recommendation
shadow_price_space <- solution_sens$duals[2]
if (shadow_price_space > 0) {
  cat("\nShadow price of warehouse space constraint:", shadow_price_space, "\n")
  cat("Recommend renting a larger warehouse. Each additional square foot will increase profit by", shadow_price_space, "dollars.\n")
} else {
  cat("\nNo need to rent a larger warehouse.\n")
}