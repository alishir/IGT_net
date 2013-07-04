library('dtw');
library('foreign');
library('mclust');
library('abind');
setwd(getwd());
source('./helper.r');

base_path <<- "/home/ali/doc/ms/thesis/all_in_one/data/payam/";
plot_path <<- "/tmp/";
plot_ratio <<- 3;

prepare_data <- function() {
	####### Data preperation
	dat = read.octave(paste(base_path, "all.dat", sep = "/"));
	sub_exp_map = read.csv(paste(base_path, "sub_exp_map", sep = "/"));
	sn <<- sapply(seq(100), function(x) { sprintf("S%d", x) });	# column names of subject selections
	dat = data.frame(dat);

	# for debug
	# dat = head(dat);

	colnames(dat) = sn;
	dat["ID"] = rownames(dat);
	sub_exp_map = data.frame(sub_exp_map);
	colnames(sub_exp_map) = c("Group");
	sub_exp_map["ID"] = rownames(sub_exp_map);
	dat = merge(dat, sub_exp_map, by = "ID");
	# convert keycodes to chars
	dat[, sn] = apply(dat[, sn], c(1,2), function(x) { toupper(intToUtf8(x)) });
	dat[["AccReward"]] = apply(dat[, sn], 1, function(x) { calc_acc_reward(x); });

	dat
}

method_simple_average_comparision <- function() {
	dat = prepare_data();
	# get number of groups in current data
	groups = names(summary(dat[, "Group"]));
	names(groups) = groups;

	ttest_results = lapply(groups, function(gr, control) {
						   tc = dat[dat["Group"] == control, "AccReward"];
						   if (gr != control) {
							   t1 = dat[dat["Group"] == gr, "AccReward"];
							   ttest = t.test(t1, tc);
							   # if statistics not in interval, reject H0
							   if (!(ttest$statistic >= ttest$conf.int[1] & ttest$statistic <= ttest$conf.int[2])) {
								   list("stat" = "H1", "test" = ttest);
							   }
							   else { list("stat" = "H0", "test" = ttest); }
						   }
}, "web");
	dump("ttest_results", file = "ttest.dump");

	####### box plot
	acc_reward = lapply(groups, function(gr) {
						dat[dat["Group"] == gr, "AccReward"]; });
	png('/tmp/var.png', height = 300 * plot_ratio, 
		width = 400 * plot_ratio, bg = "transparent");
	par(cex.axis = 2.5, cex.lab = 2.5, cex.main = 2.5,
		oma = c(1.5,4.5,1.5,2), mar = c(1.5,6,3,2));
	boxplot(acc_reward, 
			xlim = c(0,4), 
			ylim = c(500,3700), 
			ylab = "Total Reward", 
			names = names(acc_reward),
			main = "Total score of participants in three variants of IGT",
			col = rainbow(3),
			lwd = 5);
	dev.off();
}

lm_for_block <- function(trial, block) {
	blockSize = 20;
	payoff_scheme = matrix(0, nrow = 4, ncol = 3);
	colnames(payoff_scheme) = c('outcome', 'gain', 'loss');
	rownames(payoff_scheme) = c('A', 'B', 'C', 'D');
	payoff_scheme[,1] = c(-0.86, -0.86, 0.86, 0.86);
	payoff_scheme[,2] = c(-0.86, 0.86, -0.86, 0.86);
	payoff_scheme[,3] = c(-1.47, 0.34, 0.79, 0.34);
	# TODO; write me in functional
	choicePortion = matrix(0, nrow = 4, ncol = 1);
	rownames(choicePortion) = c('A', 'B', 'C', 'D');
	blockTrial = trial[((block - 1) * blockSize + 1):(block * blockSize)];
	choicePortion['A',] = sum(blockTrial == "A") / blockSize;
	shoicePortion['B',] = sum(blockTrial == "B") / blockSize;
	choicePortion['C',] = sum(blockTrial == "C") / blockSize;
	choicePortion['D',] = sum(blockTrial == "D") / blockSize;
	lm_result = lm(choicePortion ~ -1 + payoff_scheme);
	valueFeatureWeights = lm_result$coefficients;
	valueFeatureWeights = matrix(valueFeatureWeights, nrow = 1)
	#								  colnames(valueFeatureWeights) = c("outcome", "gain", "loss");		# it isn't work!
	valueFeatureWeights;
}

method_horstmann_over_blocks <- function() {
	dat = prepare_data();
	# get number of groups in current data
	groups = names(summary(dat[, "Group"]));
	names(groups) = groups;

	#### cluster base on median
	AccRewardMedian = median(unlist(dat["AccReward"]));
	dat[["MedianClust"]] = sapply(dat[, "AccReward"], function(x, accMedian) { if (x >= accMedian) as.factor("Good") else as.factor("Bad") }, AccRewardMedian);

	###

	blocks = sapply(seq(5), function(x) { sprintf("Block%d", x) });	# block names
	names(blocks) = blocks;
	sapply(seq(5), function(block) {
		   blockName = sprintf("Block%d", block);
		   # please attend to <<- operator, read ?assignOps
		   dat[[blockName]] <<- t(apply(dat[, sn], 1, lm_for_block, block));
			});

	medianClustNames = names(summary(dat[, "MedianClust"]));
	names(medianClustNames) = medianClustNames;

	features = c("outcome", "gain", "loss");
	names(features) = features;

	### plot data per groups and clusters
	png(paste(plot_path, 'horstmann_best_worst.png', sep = "/"),
		width = 900 * plot_ratio, height = 600 * plot_ratio);
	attach(mtcars);
	par(cex = 3, cex.axis = 3, cex.main = 3, cex.lab = 3, mar = c(5,5,5,5) + 1);
	layout(matrix(seq(6), nrow = 2, ncol = 3, byrow = FALSE));

	lapply(groups, function(gr) {
				sub_in_gr = subset(dat, Group == gr);
				lapply(sort(medianClustNames), function(clust) {
					   sub_in_clust = subset(sub_in_gr, MedianClust == clust);
					   featureMediansPerBlocks = lapply(features, function(f) {
														sapply(blocks, function(block) { 
															   featureIndex = grep(f, features);
															   median(sub_in_clust[, block][, featureIndex]);
});
});
					   prePlot = abind(featureMediansPerBlocks);
					   prePlot = matrix(prePlot, nrow = length(features));
					   cols = rainbow(length(features));
					   matplot(t(prePlot[,2:5]), type = 'l', lwd = 6, col = cols, ylim = c(-0.2, 0.3),
							   main = sprintf("Group: %s, Cluster: %s", gr, clust));
					   legend('bottomright', features, cex = 3, fill = cols);
});
		});
	dev.off();

	### plot data per groups
	png(paste(plot_path, 'horstmann_grp.png', sep = "/"),
		width = 900 * plot_ratio, height = 300 * plot_ratio);
	attach(mtcars);
	par(cex = 3, cex.axis = 3, cex.main = 3, cex.lab = 3, mar = c(5,5,5,5) + 1);
	layout(matrix(seq(3), nrow = 1, ncol = 3, byrow = FALSE));

	lapply(groups, function(gr) {
		   sub_in_gr = subset(dat, Group == gr);
		   featureMediansPerBlocks = lapply(features, function(f) {
											sapply(blocks, function(block) { 
												   featureIndex = grep(f, features);
												   median(sub_in_gr[, block][, featureIndex]);
							   });
});
		   prePlot = abind(featureMediansPerBlocks);
		   prePlot = matrix(prePlot, nrow = length(features));
		   cols = rainbow(length(features));
		   matplot(t(prePlot[,2:5]), type = 'l', lwd = 6, col = cols, ylim = c(-0.05, 0.1),
				   main = sprintf("Group: %s", gr));
		   legend('bottomright', features, cex = 3, fill = cols);
		});
	dev.off();
}
