#======================================================================================================================#
#                                     Convex Optimization Part 1.1. ----
#======================================================================================================================#
library(CVXR)
    
    # Generate Data
    N      <- 10
    sigma  <- matrix(runif(N ^ 2) - 0.2, ncol = N)
    sigma = t(sigma) %*% sigma
    alpha  <- data.frame(alpha = runif(N) - 0.2)
    factor <- data.frame(factor = runif(N) - 0.5)
    adv <- data.frame(adv = 3 * 50000 / runif(N))
    pos_prev <- 50000 * rep(1 / N, N)
    
    
    
    # Define constants
    port_exp_max_perc <- 0.05
    max_turnover_perc <- 0.5
    port_max_var      <- 4000
    pos_select        <- c(2, 3)
    pos_max_num       <- 4
    pos_max_num_long  <- 4
    pos_max_usd       <- 15000
    pos_max_usd_long  <- 10000
    pos_max_usd_short <- 5000
    pos_max_perc_adv  <- 0.05
    port_exp_max_usd  <- 500
    leverage_max      <- 3
    cash  <- 10000
    ratio <- 0.6
    
    # Define decision variables
    pos_usd  <- Variable(N)
    
    #==========================================#
    # Select an objective function ----
    #==========================================#
    
    # Min variance
    objective <- Minimize(quad_form(pos_usd, sigma))
    
    # Max Exposure
    objective <- Maximize(t(alpha) %*% pos_usd)
    
    
    # Markowitz
    #lambda<-0.5
    #objective <- Maximize(alpha %*% pos_usd - lambda*quad_form(pos_usd, sigma))
    
    
    #==================================================================#
    # Solve the quadratic program ----
    # Note: Comment out the constraints to be dropped from the program
    #==================================================================#
    qp = solve(Problem(
      objective = objective,
      constraints = list(
        # 1) No positions on selected position:
        pos_usd[2:3] == 0,
        
        # 2) Short Ban on selected instruments:
        pos_usd[3:4] >= 0,
        
        # 3) Maximum usd position (long and shorts)
        abs(pos_usd) <= pos_max_usd,
        
        # 4) Maximum long usd position
        pos_usd <= pos_max_usd_long,
        
        # 5) Maximum short usd position
        pos_usd >= -pos_max_usd_short,
        
        # 6) Maximum adv percent position (long and shorts)
        abs(pos_usd)  <= adv * pos_max_perc_adv,
        
        # 7) Risk factor exposure (absolute)
        abs(t(pos_usd) %*% factor) <= port_exp_max_usd,
        
        # 8) Leverage
        p_norm(pos_usd, 1) <= cash * leverage_max,
        
        # 9) No trading on selected position (prev_positions)pos_prev from position manager):
        pos_usd[c(1, 2)] == pos_prev[c(1, 2)],
        
        # 10) Max(weight) to Min(weight) ratio (long only)
        min_entries(pos(pos_usd)) >= ratio * max_entries(pos(pos_usd)),
        
        # 11) Max(weight) to Min(weight) ratio (long & short)
        min_entries(abs(pos_usd)) >= ratio * max_entries(abs(pos_usd)),
        
        #12 ) Max(weight) to portfolio_size ratio
        max_entries(abs(pos_usd)) <= ratio * p_norm(pos_usd, axis = 1),
        
        #13) Risk factor exposure (centered factor)
        abs(t(pos_usd) %*% factor) <= port_exp_max_perc * p_norm(abs(pos_usd), axis = 1),
        
        # 14) Portfolio turnover
        p_norm(pos_usd - pos_prev, 1) <= max_turnover_perc * p_norm(pos_usd, axis = 1),
        
        # 15) Cardinality (long & short)
        p_norm(pos_usd, 1) <= pos_max_num,
        
        # 16) Cardinality (long only)
        p_norm(pos_usd, axis = 1) <= pos_max_num_long,
        
        # 17) Variance constraints [No strict inequalities are allowed].
        quad_form(pos_usd, sigma) <= port_max_var
      )
    ))
    
    
    # Print the value of the objective function
    paste0("The minima is obtained at ", qp$value)
    
        
#Example 1:
    #Objective function: maximise alpha exposure
    #Constraints:
      #a-market neutrality
      #b-maximum turnover
      #c-norm = 1
    pos_usd = Variable(N)
    Objective<-Maximize(t(alpha) %*% pos_usd)
    Constraints<-list(sum_squares(pos_usd) <= 1,
                      sum(pos_usd)         == 0)
    Problem <- Problem(Objective,Constraints)
    Solution <- solve(Problem)      
    Solution$getValue(pos_usd)
    paste0("The minima is obtained at ", Solution$value)
    