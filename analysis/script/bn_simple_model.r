library(discretization);
library(bnlearn);
library(abind);

source('./igt_analysis.r');
source('./js_subject_clusters.r');
igt_path <<- '~/doc/ms/thesis/all_in_one/data/payam/'
prepare_data_for_bnlearn <- function(dat) {

	# convert data to (1,2,3,4) format
	dat = apply(dat, c(1,2), function(x) { x - 96 })

# 	payoff_schema = matrix(c(-0.86, -0.86, -1.47,
# 							 -0.86, 0.86, 0.34,
# 							 0.86, -0.86, 0.79,
# 							 0.86, 0.86, 0.34 ), nrow = 4, byrow = T);

	payoff_schema = matrix(c(-250, 0.5, 1/2.013,
							 -250, 0.9, 1/15.625,
							 250, 0.5, 1/0.277,
							 250, 0.9, 1/0.625), nrow = 4, byrow = T);
	rownames(payoff_schema) = c("A", "B", "C", "D");
	colnames(payoff_schema) = c("outcome", "gain", "loss");
	# normalize payoff_schema
	payoff_schema = apply(payoff_schema, 2, function(x) { (x - mean(x)) / sd(x)});
	print(payoff_schema);

	# convert user selections to feature weight in each trial
#	dat = head(dat, 2)
	pat = apply(dat, 1, function(x) { lapply(x, function(y) {
											 index_to_deck = list("A", "B", "C", "D");
											 ret = matrix(payoff_schema[index_to_deck[[y]], ], nrow = 1);
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
	disc_seq = apply(payoff_schema, 2, function(x) {
					min_x = sum(x[x<0]) * 40;
					max_x = sum(x[x>0]) * 40;
					seq(floor(min_x), ceiling(max_x)); });

	disc = lapply(disc_seq, function(x) { cut(x, 5, disc_labels); });
#	print(disc_seq);

	outcome_seq = disc_seq[["outcome"]];
	gain_seq = disc_seq[["gain"]];
	loss_seq = disc_seq[["loss"]];
	outcome_disc = disc[["outcome"]];
	gain_disc = disc[["gain"]];
	loss_disc = disc[["loss"]];

	#	print(tail(pat[[1]]))
	##### TODO: make it more functional
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
	############## add decision #############
	# why lapply doesn't preserve names?
	# because names(pat) have no names!
	cache_names = names(pat);
	names(cache_names) = cache_names;
	pat = lapply(cache_names, function(x) { 
				 ret = cbind(pat[[x]], as.matrix(dat[x, ], ncol = 1))
				 colnames(ret) = c(colnames(pat[[x]]), "decision")
				 ret
})
	return(pat);
}

bn_analysis <- function() {
	data_file = './bn_data.dat';
	reload = F;
	if (file.exists(data_file) && reload) {
		print("loading bn data file ...");
		load(data_file)
	} else {
		dat = load_exec_data(igt_path);
		group_fname = "sub_exp_map";
		sub_exp_map = read.csv(paste(igt_path, group_fname, sep="/"));
		pep_data = prepare_data_for_bnlearn(dat);
		sub_names = names(pep_data);
		names(sub_names) = sub_names;
		black_list = data.frame(from = rep(c("gain", "loss", "outcome"), each = 3), 
								to = rep(c("gain", "loss", "outcome"), 3));
		#	print(blacklist);
		bns = lapply(sub_names, function(x) {
					 iamb(as.data.frame(pep_data[[x]]), blacklist = black_list) });

		#### group bn analysis ####
		groups = lapply(summary(sub_exp_map), function(x) { unlist(strsplit(x, ":"))[1] })
		names(groups) = unlist(groups);
		pep_data_gr = lapply(groups, function (gr_name) { 
							 sub_in_gr = which(sub_exp_map == gr_name);
							 abind(pep_data[sub_in_gr], along = 1) });
		bns_gr = lapply(groups, function(x) {
						iamb(as.data.frame(pep_data_gr[[x]]), blacklist = black_list) });

		#### cluster bn analysis ######

		save(file = data_file , list = ls());
	}
	# plot bns
	#	lapply(bns, function(x) {plot(x); par(ask = T) });
	png("/tmp/bns_gr.png", width = 800, height = 600);
	layout(matrix(seq(3), nrow = 1, byrow = T));
	lapply(groups, function(x) {plot(bns_gr[[x]], main = x); par(ask = T) });
	dev.off();

	############ cluster bn analysis  ############
	clusters = js_subject_clusters();

	bns_clust = lapply(clusters, function(f) {
							lapply(f, function(gr) {
								   lapply(gr, function(clus) {
										  sub_in_clus = rownames(clus);
										  clus_dat = abind(pep_data[sub_in_clus], along = 1);
										  iamb(as.data.frame(clus_dat), blacklist = black_list);
									 })
							 }) 
						});

	bns_clust_names = names(bns_clust)
	png("/tmp/bns.png", width = 3000, height = 2000);
	layout(matrix(seq(28), nrow = 4, byrow = T));
	par(cex = 1.2, lwd = 1);
	lapply(bns_clust_names, function(f) {
		   f_names = names(bns_clust[[f]]);
		   lapply(f_names, function(gr) {
				  gr_names = names(bns_clust[[f]][[gr]]);
				  lapply(gr_names, function(bn) {
#						 par(ask = T);
						 main = sprintf("%s, %s, %s", f, gr, bn); 
						 plot(bns_clust[[f]][[gr]][[bn]], main = main);
									 })
							 })
						});
	dev.off();
	return(bns_clust);
}
