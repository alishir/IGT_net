library(discretization);
library(bnlearn);
library(abind);

source('./igt_analysis.r');
igt_path <<- '~/doc/ms/thesis/all_in_one/data/payam/'
prepare_data_for_bnlearn <- function() {
	dat = load_exec_data(igt_path);
	group_fname = "sub_exp_map";
	sub_exp_map = read.csv(paste(igt_path, group_fname, sep="/"));
	
	# convert data to (1,2,3,4) format
	dat = apply(dat, c(1,2), function(x) { x - 96 })

	payoff_schema = matrix(c(-0.86, -0.86, -1.47,
							-0.86, 0.86, 0.34,
							0.86, -0.86, 0.79,
							0.86, 0.86, 0.34 ), nrow = 4, byrow = T);

	# convert user selections to feature weight in each trial
	dat = head(dat, 2)
	pat = apply(dat, 1, function(x) { lapply(x, function(y) {
											ret = matrix(payoff_schema[y, ], nrow = 1);
											colnames(ret) = c("outcome", "gain", "loss");
											ret
})});

	# rbind all rows on each subject
#	print(head(abind(pat[[1]], along = 1)))
	pat = lapply(pat, function(x) { apply(abind(x, along = 1), 2, function(y) { cumsum(y) }) });
#	print(head(pat[[1]]))

	######## discretize values ###########
	disc_labels = c("very low", "low", "norm", "high", "very high");
	disc_labels = seq(5);
	outcome_seq = seq(-69, 69);
	gain_seq = seq(-59, 59);
	loss_seq = seq(-59, 59);
	outcome_disc = cut(outcome_seq, 5, disc_labels);
	gain_disc = cut(seq(-59, 59), 5, disc_labels);
	loss_disc = cut(seq(-59, 59), 5, disc_labels);
	
#	print(tail(pat[[1]]))
	pat = lapply(pat, function(x) { 
				 ret = apply(x, 1, function(y) {
							 ret = c(outcome_disc[tail(which(outcome_seq < y["outcome"]), 1)],
									 gain_disc[tail(which(gain_seq < y["gain"]), 1)],
									 loss_disc[tail(which(loss_seq < y["loss"]), 1)]);
							 matrix(ret, nrow = 1);
})
				 rownames(ret) = colnames(x)
				 t(ret) 		# why should I use t()?
})
	# add decision
	pat = lapply(names(pat), function(x) { 
				 ret = cbind(pat[[x]], as.matrix(dat[x, ], ncol = 1))
				 colnames(ret) = c(colnames(pat[[x]]), "decision")
				 ret
})

	# pat is ready to train network
	bns = lapply(pat, function(x) {
				 ret = iamb(as.data.frame(x))
#				 plot(ret)
#				 png(sprintf("/tmp/", x))
#				 plot(ret)
#				 dev.off()
				 ret
})
}
