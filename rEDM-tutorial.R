install.packages("rEDM")
library(rEDM)
vignette("rEDM-tutorial")
data(tentmap_del)
str(tentmap_del)
ts <- tentmap_del
lib <- c(1, 100)
pred <- c(201, 500)
simplex_output <- simplex(ts, lib, pred)
str(simplex_output)
par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))  # set up margins for plotting
plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
     ylab = "Forecast Skill (rho)")
simplex_output <- simplex(ts, lib, pred, E = 2, tp = 1:10)
par(mar = c(4, 4, 1, 1))
plot(simplex_output$tp, simplex_output$rho, type = "l", xlab = "Time to Prediction (tp)", 
     ylab = "Forecast Skill (rho)")
smap_output <- s_map(ts, lib, pred, E = 2)
par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
plot(smap_output$theta, smap_output$rho, type = "l", xlab = "Nonlinearity (theta)", 
     ylab = "Forecast Skill (rho)")
ts <- ts + rnorm(length(ts), sd = sd(ts) * 0.2)
smap_output <- s_map(ts, lib, pred, E = 2)
par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
plot(smap_output$theta, smap_output$rho, type = "l", xlab = "Nonlinearity (theta)", 
     ylab = "Forecast Skill (rho)")
smap_output <- s_map(tentmap_del, lib, pred, E = 2)
str(smap_output)
plot(rho ~ theta, data = smap_output, type = "l", xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)")
tentmap_noisy <- tentmap_del + rnorm(length(tentmap_del), sd = sd(tentmap_del) * 0.2)

smap_output <- s_map(tentmap_noisy, lib, pred, E = 2)

plot(rho ~ theta, data = smap_output, type = "l", xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)")

data(block_3sp)
str(block_3sp)

lib <- c(1,100)
pred <- c(101,200)

cols <- c(1, 2, 4)
target <- 1

block_lnlp_output <- block_lnlp(block_3sp, lib = lib, pred = pred, columns = cols,  target_column = target, first_column_time = TRUE)
block_lnlp_output <- block_lnlp(block_3sp, lib = lib, pred = pred, columns = c("x_t", "x_t-1", "y_t"), target_column = "x_t")
block_lnlp_output <- block_lnlp(block_3sp, lib = lib, pred = pred, columns = c("x_t", "x_t-1", "y_t"), target_column = "x_t", stats_only = FALSE)

model_output <- block_lnlp_output$model_output[[1]]

observed <- model_output$obs
predicted <- model_output$pred

plot_range <- range(c(observed, predicted), na.rm = TRUE)
plot(observed, predicted, xlim = plot_range, ylim = plot_range, xlab = "Observed", 
     ylab = "Predicted", asp = 1)
abline(a = 0, b = 1, lty = 2, col = "blue")

#SARDINE AND ANCHOVY

data(sardine_anchovy_sst)
str(sardine_anchovy_sst)

anchovy_xmap_sst <- ccm(sardine_anchovy_sst, E = 3, lib_column = "anchovy", target_column = "np_sst", lib_sizes = seq(10, 80, by = 10), num_samples = 100, random_libs = TRUE, replace = TRUE, silent = TRUE)

sst_xmap_anchovy <- ccm(sardine_anchovy_sst, E = 3, lib_column = "np_sst", target_column = "anchovy", lib_sizes = seq(10, 80, by = 10), num_samples = 100, random_libs = TRUE, replace = TRUE, silent = TRUE)

a_xmap_t_means <- ccm_means(anchovy_xmap_sst)
t_xmap_a_means <- ccm_means(sst_xmap_anchovy)

plot(a_xmap_t_means$lib_size, pmax(0, a_xmap_t_means$rho), type = "l", col = "red", xlab = "Library Size", ylab = "Cross Map Skill (rho)", ylim = c(0, 0.25))
lines(t_xmap_a_means$lib_size, pmax(0, t_xmap_a_means$rho), col = "blue")
legend(x = "topleft", legend = c("anchovy xmap SST", "SST xmap anchovy"), col = c("red", "blue"), lwd = 1, bty = "n", inset = 0.02, cex = 0.8)


anchovy_xmap_sst <- ccm(sardine_anchovy_sst, E = 3, lib_column = "anchovy", target_column = "np_sst", lib_sizes = seq(10, 80, by = 10), num_samples = 100, random_libs = TRUE, replace = TRUE, silent = TRUE)

sst_xmap_anchovy <- ccm(sardine_anchovy_sst, E = 3, lib_column = "np_sst", target_column = "anchovy", lib_sizes = seq(10, 80, by = 10), num_samples = 100, random_libs = TRUE, replace = TRUE, silent = TRUE)

a_xmap_t_means <- ccm_means(anchovy_xmap_sst)
t_xmap_a_means <- ccm_means(sst_xmap_anchovy)

plot(a_xmap_t_means$lib_size, pmax(0, a_xmap_t_means$rho), type = "l", col = "red", xlab = "Library Size", ylab = "Cross Map Skill (rho)", ylim = c(0, 0.25))
lines(t_xmap_a_means$lib_size, pmax(0, t_xmap_a_means$rho), col = "blue")
legend(x = "topleft", legend = c("anchovy xmap SST", "SST xmap anchovy"), col = c("red", "blue"), lwd = 1, bty = "n", inset = 0.02, cex = 0.8)

