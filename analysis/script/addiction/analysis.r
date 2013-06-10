library('dtw');
library('foreign');
library('mclust');


file_path <<- '/home/ali/doc/ms/thesis/all_in_one/data/hamed/IGT_85-86-9-13_modified_all.csv';

calc_ts <- function(trial, feature = "outcome") {
	payoff_schema = matrix(c(-250, 0.5, 1/2.013,
							 -250, 0.9, 1/15.625,
							 250, 0.5, 1/0.277,
							 250, 0.9, 1/0.625), nrow = 4, byrow = T);
	rownames(payoff_schema) = c("A", "B", "C", "D");
	colnames(payoff_schema) = c("outcome", "gain", "loss");

	# normalize payoff_schema
	payoff_schema = apply(payoff_schema, 2, function(x) { (x - mean(x)) / sd(x)});

	choice_payoff = sapply(trial, function(choice) { payoff_schema[toupper(choice), feature] });
	trial_ts = apply(as.matrix(choice_payoff, nrow = 1), 2, function(r) { cumsum(r) });
	t(trial_ts);
}

# TODO
# make me functional :D
# there is more that 40 card in some decks!!!!
calc_acc_reward <- function(trial) {
	rew_pun = read.octave('../pen.dat');
	len = length(trial);
	acc_rew = matrix(nrow = 1, ncol = len + 1);
	ind = matrix(c(1,1,1,1), nrow = 4, ncol = 1);
	acc_rew[1,1] = 2000;  # initial deposit!
	deck_index_map = list('A' = 1, 'B' = 2, 'C' = 3, 'D' = 4);
	for (i in seq(len)) {
		ch = deck_index_map[[trial[i]]];  # deck A is 97, convert to matrix index
		pin = ind[ch, 1];       # punishment index
		# print(sprintf("i: %d, pin: %d, ch: %d", i, pin, ch));
		acc_rew[1, i + 1] = acc_rew[1, i] + rew_pun$reward[ch, i] - rew_pun$punish[ch, pin];
		ind[ch, 1] = ind[ch, 1] + 1;
		ind[ch, 1] = min(ind[ch, 1], 40);		# more than 40 card in deck D, in dr ekhtiari data
	}
	return(acc_rew[, len + 1]);		# return the last
}

within_group_cluster <- function(subjects, num_of_clusters = 3, trial_range) {
	dis = dist(subjects[, trial_range],  method = "DTW");
	hc = hclust(dis, method = "ward");
	clusters = cutree(hc, k = num_of_clusters);
	clusters;
}

its_me <- function() {
	reload_file = './addict_without_random.dat';
	reload = T;
	if (reload) {
		load(reload_file);
	} else {
		# prepare data!
		dat = read.csv(file_path);
		sn = sapply(seq(100), function(x) { sprintf("S%d", x) });	# column names of subject selections
		# debug
		# dat = dat[127:133,];
		dat[,sn] = t(apply(dat[, sn], 1, function(x) {gsub("[0-9]+", "", x)}));		# remove digits from selections
		dat[["AccReward"]] = apply(dat[, sn], 1, function(x) { calc_acc_reward(x); });


		# get number of groups in current data
		groups = names(summary(dat[, "Group"]));
		names(groups) = groups;
		# apply retrun a matrix that need transpose
		# http://stackoverflow.com/questions/9521260/why-apply-returns-a-transposed-xts-matrix
		# cluster subjects in each group, based on each feature(outcome, gain, loss)
		features = c("outcome", "gain", "loss");
		names(features) = features;
		num_of_clusters = 3;
		clust_in_grp = lapply(groups, function(gr) {
							  sub_in_grp = dat[dat[["Group"]] == gr, ];

							  # add random subjects
							  random = data.frame(sub_in_grp);		# clone 
							  random[, "Group"] = "random";			# change group type
							  random[, "ID"] = sapply(random[, "ID"], function(r) { sprintf("rand_%s", r) });		# append rand_ to ID
							  # generate random choice sequence
							  random[, sn] = t(apply(random[, sn], 1, function(x) {
													 sapply(runif(100, min = 96, max = 100), function(x) { toupper(intToUtf8(ceiling(x))) }) }));
							  # bind to main subjects
							  #							  sub_in_grp_rnd = rbind(sub_in_grp, random);		# add random subjects
							  sub_in_grp_rnd = sub_in_grp;						# without random subjects

							  row.names(sub_in_grp_rnd) = sub_in_grp_rnd[, "ID"];		# set rownames to IDs
							  # calculate time series for each feature
							  sub_in_grp_feature_ts = lapply(features, function(f) {
															 sub_in_grp_rnd[, sn] = t(apply(sub_in_grp_rnd[, sn], 1, 'calc_ts', feature = f));
															 # cluster time series
															 # trial_range may be subset of trials
															 # e.g: trial_range = sn[3,60], trial 3 to 60
															 trial_range = sn;
															 sub_in_grp_rnd[["cluster"]] = within_group_cluster(sub_in_grp_rnd, num_of_clusters, trial_range);
															 sub_in_grp_rnd;
							 });
});
		save(file = reload_file, list = ls());
	}

	plot_ratio = 3;
	# plot number of optimal clusters for each group
	lapply(groups, function(gr) {
		   sub_in_grp = dat[dat[["Group"]] == gr, ];
		   row.names(sub_in_grp) = sub_in_grp[, "ID"];		# set rownames to IDs
		   sub_in_grp[, sn] = t(apply(sub_in_grp[, sn], 1, 'calc_ts', feature = "outcome"));
		   png(sprintf("/tmp/%s_outcome.optimal.clust.png", gr), width = 600 * plot_ratio, height = 800 * plot_ratio);
		   # TODO, there is some problem in Mclust plot
		   emfit = Mclust(sub_in_grp[, sn]);
		   print(summary(emfit));
		   dev.off();
		});

	# plot clusters
	lapply(features, function(f) {
		   png(sprintf("/tmp/%s.png", f), width = 600 * plot_ratio, height = 800 * plot_ratio);
		   sink(sprintf("/tmp/%s_stat.txt", f));

		   layout(matrix(seq(length(groups) * num_of_clusters), nrow = num_of_clusters));
		   lapply(groups, function(gr) {
				  curr_dat = clust_in_grp[[gr]][[f]];
				  num_of_sub_in_grp = sum(dat[["Group"]] == gr);
				  par(cex = 2);
				  sapply(seq(num_of_clusters), function(clus) { 	
						 curr_clust = curr_dat[curr_dat[["cluster"]] == clus, ];		# subjects that are in cluster clust
						 curr_clust = curr_clust[curr_clust[["Group"]] != "random", ];		# remove random subjects
						 print(sprintf("Group: %s, Cluster: %d, AccReward Mean: %f, AccReward SD: %f", 
									   gr, clus, mean(curr_clust[["AccReward"]]), sd(curr_clust[["AccReward"]])));
						 num_of_sub = dim(curr_clust)[1];
						 color = rainbow(num_of_sub, alpha = 1);
						 matplot(t(curr_clust[, sn]), type = 'l',
								 lwd = 4, ylab = sprintf("Acc. payoff based on %s", toupper(f)), 
								 col = color, xlab = "Trial", ylim = c(-20, 90),
								 main = sprintf("Group: %s, Cluster: %d", gr, clus)); 
#						 legend('topleft', curr_clust[, "ID"], 
#								col = color, fill = color);
						 legend('topleft', box.lwd = 0, legend = sprintf("%% of subjects: %.2f\nAccReward Mean: %.2f\nAccReward SD: %.2f", 
															   num_of_sub / num_of_sub_in_grp, 
															   mean(curr_clust[["AccReward"]]), 
															   sd(curr_clust[["AccReward"]])));
						});
				});
		   dev.off();
		   sink();
	});
	list(clust_in_grp, dat);
}

