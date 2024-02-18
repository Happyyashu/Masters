# 2.A factory makes three products called Bloom, Amber, and Leaf, from three materials
# containing Cotton, Wool and Nylon. The following table provides details on the sales
# price, production cost and purchase cost per ton of products and materials respectively.

# Install and load the lpSolve package
install.packages("lpSolve")
library(lpSolve)

# Define the objective function coefficients
obj <- c(-5, -4, -5)

# Define the constraint matrix (coefficients of the decision variables)
mat <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1, -0.5, -0.6, -0.5, -0.4, -0.4, -0.3), ncol = 3, byrow = TRUE)

# Define the direction of the constraints (<=)
dir <- rep("<=", 5)

# Define the right-hand side of the constraints
rhs <- c(4200, 3200, 3500, 0, 0)

# Solve the linear programming problem
result <- lp("max", obj, mat, dir, rhs)

# Print the result
print(result)

# Extract the optimal values and profit
optimal_values <- result$solution
optimal_profit <- -result$objval

# Print the optimal values and profit
print(paste("Optimal Values: Bloom =", optimal_values[1], ", Amber =", optimal_values[2], ", Leaf =", optimal_values[3]))
print(paste("Optimal Profit: $", optimal_profit))





# 3.Two construction companies, Giant and Sky, bid for the right to build in a field. The
# possible bids are $ 10 Million, $ 20 Million, $ 30 Million, $ 35 Million and $ 40 Million.
# The winner is the company with the higher bid.
# The two companies decide that in the case of a tie (equal bids), Giant is the winner and
# will get the field.
# Giant has ordered a survey and, based on the report from the survey, concludes that
# getting the field for more than $ 35 Million is as bad as not getting it (assume loss),
# except in case of a tie (assume win). Sky is not aware of this survey.

# Install the lpSolve package if not installed
if (!requireNamespace("lpSolve", quietly = TRUE)) {
  install.packages("lpSolve")
}

# Load the lpSolve library
library(lpSolve)

# Objective function coefficients
obj_coef <- c(1)

# Inequality constraint matrix
mat <- matrix(c(1), nrow = 1)

# Inequality constraint right-hand side
rhs <- c(40)

# Set the direction of inequalities (1 for <=)
direction <- "L"

# Solve the linear programming problem
lp_solution <- lp("min", obj_coef, mat, direction, rhs)

# Display the results
print(lp_solution)

# Extract the optimal bid amount for Sky
optimal_bid <- lp_solution$solution

cat("Optimal Bid Amount for Sky:", optimal_bid, "\n")

