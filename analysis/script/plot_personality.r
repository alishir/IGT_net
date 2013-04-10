source('./igt_analysis.r');

plot_personality <- function(igt_path, sub_path = '')
{
	source('./plot_personality.r');
	if (reload)
	{
		load(paste(BASE_PATH, 'image.dat', sep = '/'));
	}
	else
	{
		dat = load_exec_data(igt_path);
		sub_mat = load_subjects(sub_path, rescale = F);
		group_fname = "sub_exp_map";
		sub_exp_map = read.csv(paste(igt_path, group_fname, sep = "/"));

		random_subjects = create_random_subjects(30);
		random_score = calc_block_score(sub_data = random_subjects);

		score_outcome = calc_block_score(sub_data = dat, group_map = sub_exp_map, 
										 score_type = 'raw', metric = 'outcome');
		best_worst_outcome = get_best_worst_block_score(score_outcome, rand_score = random_score);

		score_gain = calc_block_score(sub_data = dat, group_map = sub_exp_map, 
									  score_type = 'raw', metric = 'gain');
		best_worst_gain = get_best_worst_block_score(score_gain, rand_score = random_score);
	}

	web_outcome_best = rownames(best_worst_outcome[["web"]][["best"]]);
	sub_ind = grep("rand_*", web_outcome_best, invert = T); # remove random subjects
	web_outcome_best = web_outcome_best[sub_ind];

	com_outcome_best = rownames(best_worst_outcome[["com"]][["best"]]);
	sub_ind = grep("rand_*", com_outcome_best, invert = T); # remove random subjects
	com_outcome_best = com_outcome_best[sub_ind];

	com_outcome_random = rownames(best_worst_outcome[["com"]][["random"]]);
	sub_ind = grep("rand_*", com_outcome_random, invert = T); # remove random subjects
	com_outcome_random = com_outcome_random[sub_ind];

	com_outcome_worst = rownames(best_worst_outcome[["com"]][["worst"]]);
	sub_ind = grep("rand_*", com_outcome_worst, invert = T); # remove random subjects
	com_outcome_worst = com_outcome_worst[sub_ind];

	com_gain_best = rownames(best_worst_gain[["com"]][["best"]]);
	sub_ind = grep("rand_*", com_gain_best, invert = T); # remove random subjects
	com_gain_best = com_gain_best[sub_ind];

	pen_gain_best = rownames(best_worst_gain[["pen"]][["best"]]);
	sub_ind = grep("rand_*", pen_gain_best, invert = T); # remove random subjects
	pen_gain_best = pen_gain_best[sub_ind];

	pen_outcome_random = rownames(best_worst_outcome[["pen"]][["random"]]);
	sub_ind = grep("rand_*", pen_outcome_random, invert = T); # remove random subjects
	pen_outcome_random = pen_outcome_random[sub_ind];

	pen_outcome_worst = rownames(best_worst_outcome[["pen"]][["worst"]]);
	sub_ind = grep("rand_*", pen_outcome_worst, invert = T); # remove random subjects
	pen_outcome_worst = pen_outcome_worst[sub_ind];

	tot_reward = t(apply(dat, 1, calc_acc_reward))[,101];

	ratio = 2;
	png(paste(BASE_PATH, 'plot_personality.png', sep = '/'),
			  width = 1360 * ratio, height = 768 * ratio);
	layout(matrix(seq(4),  nrow = 2, ncol = 2));
	par(cex.axis = 3, cex.lab = 3, mar = c(6,6,6,6));
	xlim = c(floor(min(sub_mat[,"bis"])), ceiling(max(sub_mat[,"bis"])));
	ylim = c(floor(min(sub_mat[,"bas"])), ceiling(max(sub_mat[,"bas"])));

	plot(-1, -1, ylab = 'BAS', xlab = 'BIS', xlim = xlim, ylim = ylim);
	ratio = 10000;
	inches = F;

	col_size = 5
	col_plate_size = 10;
	cols = terrain.colors(col_plate_size, 0.7);
	cols = topo.colors(col_plate_size, 0.7);
	cols = cols[sample(seq(col_plate_size), size = col_size)];
#	cols = topo.colors(6, 0.7);
	subs = web_outcome_best;
	symbols(jitter(sub_mat[subs, "bis"]), jitter(sub_mat[subs, "bas"]),
			tot_reward[subs] / ratio,
			bg = cols[1], inches = inches, add = T);

# 	subs = pen_outcome_random;
# 	symbols(jitter(sub_mat[subs, "bis"]), jitter(sub_mat[subs, "bas"]),
# 			tot_reward[subs] / ratio,
# 			bg = cols[2], inches = inches, add = T);

# 	subs = pen_gain_best;
# 	symbols(jitter(sub_mat[subs, "bis"]), jitter(sub_mat[subs, "bas"]),
# 			tot_reward[subs] / ratio,
# 			bg = cols[3], inches = inches, add = T);

	subs = com_outcome_random;
	symbols(jitter(sub_mat[subs, "bis"]), jitter(sub_mat[subs, "bas"]),
			tot_reward[subs] / ratio,
			bg = cols[2], inches = inches, add = T);

# 	subs = com_outcome_best;
# 	symbols(jitter(sub_mat[subs, "bis"]), jitter(sub_mat[subs, "bas"]),
# 			tot_reward[subs] / ratio,
# 			bg = cols[5], inches = inches, add = T);

	subs = com_outcome_worst;
	symbols(jitter(sub_mat[subs, "bis"]), jitter(sub_mat[subs, "bas"]),
			tot_reward[subs] / ratio,
			bg = cols[3], inches = inches, add = T);

 	subs = com_gain_best;
 	symbols(jitter(sub_mat[subs, "bis"]), jitter(sub_mat[subs, "bas"]),
 			tot_reward[subs] / ratio,
 			bg = cols[4], inches = inches, add = T);


	legend('bottomright', c('web outcome best', 'com outcome random', 'com outcome worst', 'com gain best'), 
		   fill = cols[seq(1,4)], cex = 3);

	ylim = c(floor(min(sub_mat[,"bas_fs"])), ceiling(max(sub_mat[,"bas_fs"])));
	plot(-1, -1, xlab = 'BIS', ylab = 'BAS Fun Seeking', xlim = xlim, ylim = ylim);
	
	subs = web_outcome_best;
	symbols(jitter(sub_mat[subs, "bis"]), jitter(sub_mat[subs, "bas_fs"]),
			tot_reward[subs] / ratio,
			bg = cols[1], inches = inches, add = T);
	subs = com_outcome_random;
	symbols(jitter(sub_mat[subs, "bis"]), jitter(sub_mat[subs, "bas_fs"]),
			tot_reward[subs] / ratio,
			bg = cols[2], inches = inches, add = T);
	subs = com_outcome_worst;
	symbols(jitter(sub_mat[subs, "bis"]), jitter(sub_mat[subs, "bas_fs"]),
			tot_reward[subs] / ratio,
			bg = cols[3], inches = inches, add = T);

 	subs = com_gain_best;
 	symbols(jitter(sub_mat[subs, "bis"]), jitter(sub_mat[subs, "bas_fs"]),
 			tot_reward[subs] / ratio,
 			bg = cols[4], inches = inches, add = T);

	legend('bottomright', c('web outcome best', 'com outcome random', 'com outcome worst', 'com gain best'),
		   fill = cols[seq(1,4)], cex = 3);

	ylim = c(floor(min(sub_mat[,"bas_rr"])), ceiling(max(sub_mat[,"bas_rr"])));
	plot(-1, -1, ylab = 'BAS Reward Responsiveness', xlab = 'BIS', xlim = xlim, ylim = ylim);
	
	subs = web_outcome_best;
	symbols(jitter(sub_mat[subs, "bis"]), jitter(sub_mat[subs, "bas_rr"]),
			tot_reward[subs] / ratio,
			bg = cols[1], inches = inches, add = T);
	subs = com_outcome_random;
	symbols(jitter(sub_mat[subs, "bis"]), jitter(sub_mat[subs, "bas_rr"]),
			tot_reward[subs] / ratio,
			bg = cols[2], inches = inches, add = T);
	subs = com_outcome_worst;
	symbols(jitter(sub_mat[subs, "bis"]), jitter(sub_mat[subs, "bas_rr"]),
			tot_reward[subs] / ratio,
			bg = cols[3], inches = inches, add = T);

 	subs = com_gain_best;
 	symbols(jitter(sub_mat[subs, "bis"]), jitter(sub_mat[subs, "bas_rr"]),
 			tot_reward[subs] / ratio,
 			bg = cols[4], inches = inches, add = T);


	legend('bottomright', c('web outcome best', 'com outcome random', 'com outcome worst', 'com gain best'), 
		   fill = cols[seq(1,4)], cex = 3);

	ylim = c(floor(min(sub_mat[,"bas_d"])), ceiling(max(sub_mat[,"bas_d"])));
	plot(-1, -1, ylab = 'BAS Drive', xlab = 'BIS', xlim = xlim, ylim = ylim);
	
	subs = web_outcome_best;
	symbols(jitter(sub_mat[subs, "bis"]), jitter(sub_mat[subs, "bas_d"]),
			tot_reward[subs] / ratio,
			bg = cols[1], inches = inches, add = T);
	subs = com_outcome_random;
	symbols(jitter(sub_mat[subs, "bis"]), jitter(sub_mat[subs, "bas_d"]),
			tot_reward[subs] / ratio,
			bg = cols[2], inches = inches, add = T);
	subs = com_outcome_worst;
	symbols(jitter(sub_mat[subs, "bis"]), jitter(sub_mat[subs, "bas_d"]),
			tot_reward[subs] / ratio,
			bg = cols[3], inches = inches, add = T);

 	subs = com_gain_best;
 	symbols(jitter(sub_mat[subs, "bis"]), jitter(sub_mat[subs, "bas_d"]),
 			tot_reward[subs] / ratio,
 			bg = cols[4], inches = inches, add = T);


	legend('bottomright', c('web outcome best', 'com outcome random', 'com outcome worst', 'com gain best'), 
		   fill = cols[seq(1,4)], cex = 3);

	dev.off();
	save(file = paste(BASE_PATH, "image.dat", sep = "/"), 
		 list = ls());

}
