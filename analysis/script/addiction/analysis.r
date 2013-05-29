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
its_me <- function() {
	# prepare data!
	dat = read.csv(file_path);
	sn = sapply(seq(100), function(x) { sprintf("S%d", x) });	# column names of subject selections
	dat[,sn] = t(apply(dat[, sn], 1, function(x) {gsub("[0-9]+", "", x)}));		# remove digits from selections

	# for test
	dat = dat[129:131, ];

	# get number of groups in current data
	groups = names(summary(dat[, "Group"]));
	names(groups) = groups;


	# apply retrun a matrix that need transpose
	# http://stackoverflow.com/questions/9521260/why-apply-returns-a-transposed-xts-matrix

	features = c("outcome", "gain", "loss");
	names(features) = features;
	# cluster subjects in each group, based on each feature(outcome, gain, loss)
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
						  sub_in_grp_rnd = rbind(sub_in_grp, random);
						  # calculate time series for each feature
						  sub_in_grp_feature_ts = lapply(features, function(f) {
														sub_in_grp_rnd[, sn] = t(apply(sub_in_grp_rnd[, sn], 1, 'calc_ts', feature = f));
														sub_in_grp_rnd
														});
						  # cluster time series

			});
	list(clust_in_grp, dat);
}

