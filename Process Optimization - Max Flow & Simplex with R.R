#**************************************************************
#MTH784 - Optimisation for Business Processes
#Igor Marques
#**************************************************************

library(igraph)
library(lpSolve)
library(writexl)

rm(list=ls())
setwd('/Users/igorm/OneDrive/Queen Mary University of London/MTH784 - Optimization for Business Processes/Coursework')

#Problem 1 - Broadband Service Provider (bsp)

#(b)

#Reading csv file
bsp <- read.csv("problem_1_df.csv")

#Renaming column names
colnames(bsp) <- c("from", "to", "capacity")

#Creating graph object (using igraph library)
g_bsp <- graph_from_data_frame(bsp)

#Plotting graph
#Note: when using tkplot, vertices and edges are manually organized (tkplot is an interactive graph drawing facility)
tkplot(g_bsp,
       edge.label=bsp$capacity, 
       canvas.width = 800, 
       canvas.height = 500, 
       vertex.color="orange")

#(c)
#Reading csv file
network <- read.csv("problem_1_df1.csv")

#Renaming column names
colnames(network) <- c("from", "to", "capacity")

#Creating graph object (using igraph library)
g_network <- graph_from_data_frame(network)

#Plotting graph
#Note: when using tkplot, vertices and edges are manually organized (tkplot is an interactive graph drawing facility)
tkplot(g_network,
       edge.label=network$capacity, 
       canvas.width = 1000, 
       canvas.height = 600, 
       vertex.color="orange")

#Finding max flow via minimum cut
max_flow(g_network, source = V(g_network)["23"], target=V(g_network)["24"], capacity = E(g_network)$capacity)


#Problem 2 - Manufacturing Company Hiring/Training Schedule Optimization

#Ut = number of untrained employees at the beginning of the month t
#represented by variables: x1,x5,x9,x13,...,x41,x45
#Yt = number of trained employees at the beginning of the month t
#represented by variables: x2,x6,x10,x14,...,x42,x46
#Zt = number of employees to train at the beginning of the month t
#represented by variables: x3,x7,x11,x15,...,x43,x47
#Wt = number of hired employees at the beginning of the month t
#represented by variables: x4,x8,x12,x16,...,x44,x48

# Setting coefficients of the objective function
f.obj <- c(2500, 2750, 2500, 2500, #Jan
           2500, 2750, 2500, 2500, #Feb
           2500, 2750, 2500, 2500, #Mar
           2500, 2750, 2500, 2500, #Apr
           2500, 2750, 2500, 2500, #May
           2500, 2750, 2500, 2500, #Jun
           2500, 2750, 2500, 2500, #Jul
           2500, 2750, 2500, 2500, #Aug
           2500, 2750, 2500, 2500, #Sep
           2500, 2750, 2500, 2500, #Oct
           2500, 2750, 2500, 2500, #Nov
           2500, 2750, 2500, 2500) #Dec

# Setting matrix corresponding to coefficients of constraints by rows
# The non-negative constraint is not nedded as it is automatically assumed
f.con <- matrix(c(1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0.95,0,0.1,1,-1,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0.95,0,0.1,1,-1,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0.95,0,0.1,1,-1,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0.95,0,0.1,1,-1,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.95,0,0.1,1,-1,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.95,0,0.1,1,-1,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.95,0,0.1,1,-1,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.95,0,0.1,1,-1,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.95,0,0.1,1,-1,0,-1,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.95,0,0.1,1,-1,0,-1,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.95,0,0.1,1,-1,0,-1,0,
                  0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,1,0.9,0,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,1,0.9,0,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,1,0.9,0,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,1,0.9,0,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0.9,0,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0.9,0,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0.9,0,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0.9,0,0,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0.9,0,0,-1,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0.9,0,0,-1,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0.9,0,0,-1,0,0,
                  1,1.2,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,1,1.2,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,1,1.2,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,1,1.2,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1.2,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1.2,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1.2,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1.2,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1.2,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1.2,0,1,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1.2,0,1,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1.2,0,1), nrow = 36, byrow = TRUE)

# Setting constraint equations signs
f.dir <- c("=",
           "=",
           "=",
           "=",
           "=",
           "=",
           "=",
           "=",
           "=",
           "=",
           "=",
           "=",
           "=",
           "=",
           "=",
           "=",
           "=",
           "=",
           "=",
           "=",
           "=",
           "=",
           "=",
           "=",
           ">=",
           ">=",
           ">=",
           ">=",
           ">=",
           ">=",
           ">=",
           ">=",
           ">=",
           ">=",
           ">=",
           ">=")

# Setting right hand side coefficients of the constraint equations
f.rhs <- c(174,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           0,
           120,
           120,
           138,
           150,
           168,
           180,
           198,
           210,
           228,
           240,
           252,
           264)

#Setting decision variables that are constrained to integer numbers
int_vec = c(3,4,7,8,11,12,15,16,19,20,23,24,27,28,31,32,35,36,39,40,43,44,47,48)

# Finding min cost using the lp funtion
lp1 = lp("min", f.obj, f.con, f.dir, f.rhs, int.vec = int_vec)
print(lp1$status) #0=success
lp1

# Creating a data frame with the solution results
solution <- data.frame("Jan" = c(lp1$solution[1:4]),
                      "Feb" = c(lp1$solution[5:8]),
                      "Mar" = c(lp1$solution[9:12]),
                      "Apr" = c(lp1$solution[13:16]),
                      "May" = c(lp1$solution[17:20]),
                      "Jun" = c(lp1$solution[21:24]),
                      "Jul" = c(lp1$solution[25:28]),
                      "Aug" = c(lp1$solution[29:32]),
                      "Sep" = c(lp1$solution[33:36]),
                      "Oct" = c(lp1$solution[37:40]),
                      "Nov" = c(lp1$solution[41:44]),
                      "Dec" = c(lp1$solution[45:48]))

rownames(solution)[1] <- "Untrained"
rownames(solution)[2] <- "Trained"
rownames(solution)[3] <- "To Train"
rownames(solution)[4] <- "Hired"
solution

#Exporting solution to excel
write_xlsx(solution,"/Users/igorm/OneDrive/Queen Mary University of London/MTH784 - Optimization for Business Processes/Coursework/solutionp2.xlsx")

####END####
