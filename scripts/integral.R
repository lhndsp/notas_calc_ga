library(ggplot2)
library(gganimate)

f <- function(dom, f_exp) {
  fx <- match.fun(f_exp)
  x <- dom
  y <- fx(x)
  lbl <- paste0("f(x) = ", f_exp, "(x)")
  return(list(x = x, y = y, fx = fx, lbl = lbl))
}

integral_riemann_anim <- function(delta_xi = .1,
                                  dom = seq(0, 5, .001),
                                  f_exp = "cos") {
  f_out <- f(dom = dom, f_exp = f_exp)
  x <- f_out$x
  y <- f_out$y
  f_fun <- f_out$fx
  lbl <- f_out$lbl
  
  n_xi <- floor(max(x) / delta_xi)
  
  x_0 <- min(dom)
  frames <- data.frame()
  
  for (i in seq_len(n_xi)) {
    x_end <- x_0 + delta_xi
    x_i <- (x_0 + x_end) / 2
    y_i <- f_fun(x_i)
    frames <- rbind(frames, data.frame(
      i = i,
      x_i = x_i,
      y_i = y_i,
      x_min = x_0,
      x_max = x_end
    ))
    x_0 <- x_end
  }
  
  grafico <- ggplot() +
    geom_line(aes(x = x, y = y), color = "blue", size = 1) +
    geom_rect(data = frames,
              aes(xmin = x_min, xmax = x_max, ymin = 0, ymax = y_i),
              fill = "orange", alpha = 0.5) +
    labs(title = paste0("Soma de Riemann para ", lbl),
         subtitle = "Iteração: {frame}",
         x = "x", y = "f(x)") +
    transition_manual(i)
  
  animate(grafico, nframes = n_xi, fps = 10, width = 600, height = 400)
}


anim <- integral_riemann_anim(f_exp = "cos", delta_xi = 0.1)
anim_save("images/riemann_cos.gif", animation = anim)
