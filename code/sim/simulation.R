# This conducts the simulation used to illustate the impact of conflicts 
# ending in other countries on trade flows.

rm(list = ls())

library(ggplot2)
library(dplyr)

github_path <- "/Users/shotaro/GitHub/weapon_network/output/images/"

####### Example ######
test = 1
alpha   <- 2
beta    <- 1
c       <- 1

N       <- 2
N_1     <- 1
N_2     <- 1
if (test == 1){
  
  
  A <- matrix(data = c(1, c*N/(beta + c*N_1), 0, 0,
                       c*N_1/(beta + c*N), 1, 0, beta*N/(2*beta + 2*c*N),
                       0, 0, 1, c*N/(beta + c*N_2),
                       0, beta*N/(2*beta + 2*c*N), c*N_2/(beta + c*N), 1),
              nrow = 4,
              ncol = 4,
              byrow = TRUE
              )
  
  
  b <- matrix(data = c(alpha/(2*(beta + c*N_1)), 
                       alpha*N/(2*(beta + c*N)),
                       alpha/(2*(beta + c*N_2)),
                       alpha*N/(2*(beta + c*N))),
              nrow = 4,
              ncol = 1,
              byrow = FALSE
              )
  
  
  q <- solve(A, b)
  q
}

####### Function ######

twosupplier <- function(alpha, beta, c, N, N_1, N_2){
  
  cat("N:", N, "N_1:", N_1, "N_2:", N_2, "\n")
  
  df <- data.frame(alpha = alpha, 
                   beta = beta, 
                   c = c, 
                   N = N, 
                   N_1 = N_1, 
                   N_2 = N_2, 
                   n = N + N_1 + N_2,
                   q11 = 0,
                   q12 = 0,
                   q21 = 0,
                   q22 = 0)
  
  A <- matrix(data = c(1, c*N/(beta + c*N_1), 0, 0,
                       c*N_1/(beta + c*N), 1, 0, beta*N/(2*beta + 2*c*N),
                       0, 0, 1, c*N/(beta + c*N_2),
                       0, beta*N/(2*beta + 2*c*N), c*N_2/(beta + c*N), 1),
              nrow = 4,
              ncol = 4,
              byrow = TRUE
  )
  
  
  b <- matrix(data = c(alpha/(2*(beta + c*N_1)), 
                       alpha*N/(2*(beta + c*N)),
                       alpha/(2*(beta + c*N_2)),
                       alpha*N/(2*(beta + c*N))),
              nrow = 4,
              ncol = 1,
              byrow = FALSE
  )
  
  q <- solve(A, b)
  
  if (all(q >= 0) == TRUE){
    df$q11 <- q[1]
    df$q12 <- q[2]
    df$q21 <- q[3]
    df$q22 <- q[4]

    return(df)

  }
  
}

##### Simulation #######

alpha <- 2
beta <- 1
c <- 1

# df_check <- data.frame()

# for (alpha in 10:20){
  
  df_all <- data.frame()
  
  for (i in 10:15){
    for (j in 1:20){
      for (k in 1:20){
        
        N <- i
        N_1 <- j
        N_2 <- k
        
        tryCatch(
          {df <- twosupplier(alpha, beta, c, N, N_1, N_2)}, 
          error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
        
        df_all <- rbind(df_all, df)
        
      }
    }
  }
  
#   df_check_alpha <- data.frame(alpha = alpha, length = length(unique(df_all$N)))
#   df_check <- rbind(df_check, df_check_alpha)
# 
# }
  
##### Clean Data ######
  
df_all <- df_all %>%
  mutate(common_buyer = q12 + q22,
         s1_buyer = q11,
         s2_buyer = q21,
         N_1 = as.factor(N_1),
         N_2 = as.factor(N_2),
         N = as.factor(N))

##### Plot #####
  
## N as x axis
df_common_p <- df_all %>%
  group_by(N_1, N_2) %>%
  filter(n() > 1) %>%
  ggplot() +
  geom_line(aes(x = N, y = common_buyer, group=interaction(N_1, N_2), colour = "2 link buyer")) +
  geom_line(aes(x = N, y = s1_buyer, group=interaction(N_1, N_2), colour = "s1 buyer")) + 
  geom_line(aes(x = N, y = s2_buyer, group=interaction(N_1, N_2), colour = "s2 buyer")) +
  labs(title="Comparative Static with respect to 2 link buyer", 
       x ="Number of buyers supplied by both suppliers", 
       y = "Total weapon imported") 
df_common_p
ggsave(paste0(github_path, "n_alpha", unique(df_all$alpha), ".png"))

## N_1 as x axis
df_s1_p <- df_all %>%
  group_by(N, N_2) %>%
  filter(n() > 1) %>%
  ggplot() +
  geom_line(aes(x = N_1, y = common_buyer, group=interaction(N, N_2), colour = "2 link buyer")) +
  geom_line(aes(x = N_1, y = s1_buyer, group=interaction(N, N_2), colour = "s1 buyer")) + 
  geom_line(aes(x = N_1, y = s2_buyer, group=interaction(N, N_2), colour = "s2 buyer")) +
  labs(title="Comparative Static with respect to s1 buyer", 
       x ="Number of buyers only supplied by s1", 
       y = "Total weapon imported") +
  ylim(0.01,0.5)
df_s1_p
ggsave(paste0(github_path, "n1_alpha", unique(df_all$alpha), ".png"))

## N_2 as x axis
df_s2_p <- df_all %>%
  group_by(N, N_1) %>%
  filter(n() > 1) %>%
  ggplot() +
  geom_line(aes(x = N_2, y = common_buyer, group=interaction(N, N_1), colour = "2 link buyer")) +
  geom_line(aes(x = N_2, y = s1_buyer, group=interaction(N, N_1), colour = "s1 buyer")) + 
  geom_line(aes(x = N_2, y = s2_buyer, group=interaction(N, N_1), colour = "s2 buyer")) +
  labs(title="Comparative Static with respect to s2 buyer", 
       x ="Number of buyers only supplied by s2", 
       y = "Total weapon imported") +
  ylim(0.01,0.5)
df_s2_p
ggsave(paste0(github_path, "n2_alpha", unique(df_all$alpha), ".png"))

run <- 0
if (run == 1){
  comparison_plot <- function(compare_var, group_var, df_all){
    df_common_p <- df_all %>%
      group_by(group_var[1], group_var[2]) %>%
      filter(n() > 1) %>%
      ggplot() +
      geom_line(aes_string(x = compare_var, 
                           y = common_buyer, 
                           group=paste0("interaction(", paste0(c("N_1", "N_2"), collapse =  ", "), ")"), 
                           colour = "2 link buyer")) +
      geom_line(aes_string(x = compare_var,
                           y = s1_buyer, 
                           group=paste0("interaction(", paste0(c("N_1", "N_2"), collapse =  ", "), ")")
                           , colour = "s1 buyer")) + 
      geom_line(aes_string(x = compare_var,
                           y = s2_buyer,
                           group=paste0("interaction(", paste0(c("N_1", "N_2"), collapse =  ", "), ")"), 
                           colour = "s2 buyer"))
    
    df_common_p
  }
  comparison_plot("N", c("N_1", "N_2"), df_all)
}

