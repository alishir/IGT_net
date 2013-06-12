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
