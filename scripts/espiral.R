library(ggplot2)
library(gganimate)
library(latex2exp)


u <- 200

vetores <- data.frame(x = 1:u, y = 1:u, s = NA, s2 = NA, vx = NA, vy = NA)

for(n in 2:u){
  # vetor unitario
  vetores$vx[n-1] <- -vetores$y[n-1]*(1/sqrt(n))
  vetores$vy[n-1] <- vetores$x[n-1]*(1/sqrt(n))
  
  vetores$x[n] <- vetores$x[n-1] + vetores$vx[n-1]
  vetores$y[n] <- vetores$y[n-1] + vetores$vy[n-1]
  vetores$s2[n] <- vetores$x[n]**2 + vetores$y[n]**2
  vetores$s[n] <- sqrt(vetores$s2[n])
}

data <- data.frame()
for (i in seq_along(1:u)) {
  
  idx <- order(c(1:i, 1:i))
  v_o <- rep(c(0), i)
  
  data <- rbind(data, 
                data.frame(
                  ox = unlist(c(v_o,vetores$x[1:i] ))[idx],
                  oy = unlist(c(v_o,vetores$y[1:i] ))[idx],
                  x = unlist(c(vetores$x[1:i],vetores$vx[1:i]+vetores$x[1:i]))[idx],
                  y = unlist(c(vetores$y[1:i],vetores$vy[1:i]+vetores$y[1:i]))[idx],
                  step = i
                ))
}

p <- 
  ggplot(data) +
  geom_hline(yintercept = 0, linewidth = .5) +
  geom_vline(xintercept = 0, linewidth = .5) +
  geom_segment(aes(x = ox, y = oy, xend = x, yend = y),
               arrow = arrow(type = "closed", length = unit(0.05, "inches")), 
               color = '#c72424') +
  theme_light() +
  transition_manual(step)

anim <- animate(p, nframes = length(1:u), fps = .5)
anim_save("images/espiral.gif", animation = anim)





