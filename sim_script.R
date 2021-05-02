#
# SIMPLE SIMULATION OF 4 STRAINS USING NEGATIVE BINOMIAL DISTRIBUTION 
#


library(tidyverse)
library(ggthemes)
library(gganimate)
library(wesanderson)
theme_set(tidybayes::theme_tidybayes())


# simulation function -----
strain_sim <- function(n = 10, # number of generation simulated
                       nn = 20, # number of simulations
                       mu = 2.6, # mean number of secondary cases, R0
                       k = 10,# k, the dispersion parameter
                       seeds = c(10, 10, 10, 10)) { # number of inital cases for each strain
  #preparing tibble
  simple <- tibble(A = rep(NA, n), B = rep(NA, n), C = rep(NA, n), D = rep(NA, n)) %>% 
    mutate_all(as.numeric) 
  
  # initial seed
  simple[1, ] <- cbind(t(seeds)) 
  
  # sim funciton
  sim_fun <- function(var) {
    for (i in 1:(length(var) - 1)) {
      var[i + 1] <- sum(rnbinom(n = var[i], size = k, mu = mu)) # takes the previous number of cases, and computes next generation number of cases ,assumes previous generation is no longer transmissible
    }
    var
  }
  
  df <- tibble(sim = 1:nn, data = vector("list", length = nn))
  for (i in 1:n) {
    df$data[[i]] <- apply(simple, 2, sim_fun) %>% 
      as_tibble() %>%
      mutate(gen = 1:nrow(.)) %>% 
      pivot_longer(cols = 1:4) %>% 
      mutate(name = as.factor(name)) %>%
      group_by(gen, name) %>%
      summarise(n = sum(value)) %>%
      mutate(perc = n / sum(n)) %>%
      select(gen, name, perc)
  }
  
  return(list(df = df, k = k, mu = mu, seeds = seeds))
}
df <- strain_sim()
strain_sim_plot <- function(x) {
  df <- x$df
  mu <- x$mu
  k <- x$k
  seeds <- x$seeds
  df %>%
    unnest(cols = c(data)) %>%
    ggplot(aes(x = gen, y = perc, fill = name)) +
    geom_area() +
    scale_x_continuous(name = "Generation", breaks = c(1:13)) +
    scale_y_continuous(name = "Proportion of all cases") +
    scale_fill_manual(name = "Strain (number of initial cases)", values = wes_palette("Zissou1", n = 4, type = "continuous"), labels = c(paste0(c("A=", "B=", "C=", "D="), seeds))) +
    transition_manual(sim) +
    labs(title = paste0("Evolution of share of strains with equal R0=", mu, " and k=", k))
}


strain_sim_plot(strain_sim(k = 0.16, mu = 2.8, seeds = c(25, 25, 25, 25)))

gganimate::anim_save("strain.gif")

strain_sim_plot(strain_sim(k = 0.16, mu = 2.8, seeds = c(25, 25, 50, 25)))

gganimate::anim_save("strain2.gif")

strain_sim_plot(strain_sim(k = 0.16, mu = 2.8, seeds = rep(100,4)))

gganimate::anim_save("strain3.gif")
