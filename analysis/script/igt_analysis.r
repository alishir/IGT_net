library('foreign');
library('plotrix');
library('pvclust');
library('calibrate');
library('dtw');
library('WRS');		# Wilcox Robust Methods
library('coin');	# Kruskal-Walls test
library('Hmisc');
source('./add_new_sub.r');
source('./Friedman-Test-with-Post-Hoc.r');
BASE_PATH <<- '~/doc/code/IGT_net/analysis/script/tmp';
load_exec_data <- function(igt_result_dir)
{
	if (file.exists(paste(igt_result_dir, "all.dat", sep = "/")))
	{
		da = read.octave(paste(igt_result_dir, "all.dat", sep = "/"));
		sub_deck_selection = da$b;
		rownames(sub_deck_selection) = as.vector(lapply(seq(nrow(sub_deck_selection)), 
														function(x){
															sprintf("%d", x);
														}));
		return (sub_deck_selection);
	}
	sub_list = list.files(igt_result_dir, pattern = "*.dat$");
	sub_size = length(sub_list);
	sub_deck_selection = matrix(data = NA, nrow = sub_size, ncol = 100);
	sub_ids = lapply(sub_list, function(e) {substr(e, 1, nchar(e) - 4);});
	rownames(sub_deck_selection) = as.vector(sub_ids);

	cat(sprintf("number of subjects: %d\n", sub_size));
	for (i in sub_list)
	{
		a = read.octave(paste(igt_result_dir, i, sep = "/"));
		b = a$game_seq[1, 1:100];
		d = as.matrix(b, nrow = 1, ncol = 100);
		sub_deck_selection[substr(i, 1, nchar(i) - 4), ] = d;
	}
	return(sub_deck_selection);
}

calc_20block_score_sub <- function(sub_trial, num_of_block, metric = 'outcome')
{
        # sub_trial is subject triala, 100 trial of igt game
        block_score = matrix(data = NA, nrow = 1, ncol = num_of_block);
		bs = floor(100 / num_of_block);
        for (i in seq(num_of_block))
        {
                block_trial = sub_trial[((i - 1) * bs + 1):(i * bs)];
                block_score[1, i] = calc_score_with_metric(block_trial, metric);
        }
        return(block_score);
}



#' calculate score of subject base on given metric and score type
#' @param score_type how calculate score, types are 'raw' and 'block', 
#' 		raw score is time serie of subject choices, and block score is subject
#' 		choices per block
#' @param metric metric to evaluate subject choices
#' @param sub_data matrix of subjects data, should have rownames
calc_block_score <- function(sub_data, group_map = NA, score_type = 'raw', num_of_block = 0, metric = 'outcome')
{
	ret = list();
	if (score_type == 'block')
	{
		score_len = num_of_block;
	}
	else if (score_type == 'raw')
	{
		score_len = dim(sub_data)[2] + 1;
	}

	if (is.na(group_map))
	{
		sub_in_group = rownames(sub_data);
		curr_score = matrix(data = NA, nrow = length(sub_in_group), ncol = score_len);
		rownames(curr_score) = as.vector(sub_in_group);

		for (s in sub_in_group)
		{
			if (s %in% rownames(sub_data))
			{
				if (score_type == 'block')
				{
					curr_score[as.character(s), ] = calc_20block_score_sub(sub_data[as.character(s), ], num_of_block, metric);
				}
				else if (score_type == 'raw')
				{
					curr_score[as.character(s), ] = calc_good_bad(sub_data[as.character(s), ], metric);
				}
			}
		} # subject in group
		return(curr_score);
	} # not exists group_map

	groups = c('com', 'web', 'pen');
	for (gr in groups)
	{
		sub_in_group = rownames(group_map)[which(group_map == gr)];
		curr_score = matrix(data = NA, nrow = length(sub_in_group), ncol = score_len);
		rownames(curr_score) = as.vector(sub_in_group);

		for (s in sub_in_group)
		{
			if (s %in% rownames(sub_data))
			{
				if (score_type == 'block')
				{
					curr_score[as.character(s), ] = calc_20block_score_sub(sub_data[as.character(s), ], num_of_block, metric);
				}
				else if (score_type == 'raw')
				{
					curr_score[as.character(s), ] = calc_good_bad(sub_data[as.character(s), ], metric);
				}
			}
		} # subject in group

		# remove NA lines
		curr_score = curr_score[rowSums(is.na(curr_score)) == 0, ];
		print(sprintf("size of %s group: %d\n", toupper(gr), nrow(curr_score)));
		ret[[gr]] = curr_score;

	} # list of groups
	return(ret);

}

plot_20block_score <- function(score)
{
	x = seq(1, 5);
	plot_col = c("red", "green", "blue", "white");
	pen_mean_score = colMeans(score$pen);
	web_mean_score = colMeans(score$web);
	com_mean_score = colMeans(score$com);

	pen_sd = apply(score$pen, 2, sd);
	web_sd = apply(score$web, 2, sd);
	com_sd = apply(score$com, 2, sd);

	# extract two cluster in each group
	pen_clus = within_group_cluster(score, "pen");
	com_clus = within_group_cluster(score, "com");
	web_clus = within_group_cluster(score, "web");


	pen_1st_clus_mean = colMeans(score$pen[pen_clus[[1]], ]);
	pen_2st_clus_mean = colMeans(score$pen[pen_clus[[2]], ]);
	pen_1st_sd = apply(score$pen[pen_clus[[1]], ], 2, sd);
	pen_2st_sd = apply(score$pen[pen_clus[[2]], ], 2, sd);



	com_1st_clus_mean = colMeans(score$com[com_clus[[1]], ]);
	com_2st_clus_mean = colMeans(score$com[com_clus[[2]], ]);
	com_1st_sd = apply(score$com[com_clus[[1]], ], 2, sd);
	com_2st_sd = apply(score$com[com_clus[[2]], ], 2, sd);


	#		web_1st_clus_mean = colMeans(score$web[web_clus[[1]], ]);
	#		web_2st_clus_mean = colMeans(score$web[web_clus[[2]], ]);

	plot(x - 0.01, pen_mean_score, ylim = range(-20, 20), type = "o", col = plot_col[4], xlab = "20 block", ylab = ("adv - dis)"));
	#		plotCI(x - 0.01, pen_mean_score, pen_sd, add = TRUE, scol = plot_col[1]);
	plotCI(x + 0.02, pen_1st_clus_mean, pen_1st_sd, add = TRUE, slty = 5, scol = plot_col[1]);
	lines(x, pen_1st_clus_mean, lty = 5, col = plot_col[1]);   
	plotCI(x - 0.02, pen_2st_clus_mean, pen_2st_sd, add = TRUE, slty = 3, scol = plot_col[1]);
	lines(x, pen_2st_clus_mean, lty = 3, col = plot_col[1]);

	#   lines(x + 0.01, com_mean_score, type = "o", col = plot_col[3]);
	#		plotCI(x + 0.01, com_mean_score, com_sd, add = TRUE, scol = plot_col[3]);
	plotCI(x + 0.02, com_1st_clus_mean, com_1st_sd, add = TRUE, slty = 5, scol = plot_col[3]);
	lines(x, com_1st_clus_mean, lty = 5, col = plot_col[3]);
	plotCI(x - 0.02, com_2st_clus_mean, com_2st_sd, add = TRUE, slty = 3, scol = plot_col[3]);
	lines(x, com_2st_clus_mean, lty = 3, col = plot_col[3]);

	#   lines(x + 0.02, web_mean_score, type = "o", col = plot_col[2]);
	#		plotCI(x + 0.02, web_mean_score, web_sd, add = TRUE, scol = plot_col[2]);
	#		lines(x, web_1st_clus_mean, lty = 5, col = plot_col[2]);
	#		lines(x, web_2st_clus_mean, lty = 3, col = plot_col[2]);

	legend("bottomright",  c("pen", "web", "com"), cex=0.8, col = plot_col, pch=21, lty=1);
	#   legend("bottomright",  c("pen", "com"), cex=0.8, col = c(plot_col[1], plot_col[3]), pch=21, lty=1);
}

# clus_cols: columns that used in clustering
# @param rand_score, random scores to filter random players
get_best_worst_block_score <- function(score, clus_cols, rand_score = NA)
{
	ret_val = list();
	nb = dim(score$pen)[2];
	groups = c('com', 'web', 'pen');
	num_of_clusters = 3;
	if (is.na(rand_score))		# if there is no random subjects
	{
		num_of_clusters = 2;
	}
	else
	{
		num_of_clusters = 3;
	}
	for (gr in groups)
	{
		curr_score = score[[gr]];
		if (!is.na(rand_score))
		{
			curr_score = rbind(curr_score, rand_score);
		}
		curr_mean_score = apply(curr_score, 2, mean);
		curr_sd = apply(curr_score, 2, sd);
		curr_clus = within_group_cluster(curr_score, num_of_clusters = num_of_clusters, clus_cols);

		curr_clus_medians = apply(curr_score[curr_clus[[1]], ], 2, median);
		curr_clus_sd = apply(curr_score[curr_clus[[1]], ], 2, sd);
		for (clus_ind in seq(2, num_of_clusters))
		{
			curr_clus_medians = rbind(curr_clus_medians, apply(curr_score[curr_clus[[clus_ind]], ], 2, median));
			curr_clus_sd = rbind(curr_clus_sd, apply(curr_score[curr_clus[[clus_ind]], ], 2, sd));
		}

		learned_portion = curr_clus_medians[, ceiling((0.41) * nb):nb];
		sum_learned_portion = apply(learned_portion, 1, sum);
		best_ind = which(sum_learned_portion == max(sum_learned_portion))[1];
		worst_ind = which(sum_learned_portion == min(sum_learned_portion))[1];

		ret_val[[gr]] = list();
		for (clus_ind in seq(num_of_clusters))
		{
			if (clus_ind == best_ind)
			{
				key = "best";
			}
			else if (clus_ind == worst_ind)
			{
				key = "worst";
			}
			else
			{
			#	key = paste("other", clus_ind, sep = "_");
				key = "random";
			}
			ret_val[[gr]][[key]] = curr_score[curr_clus[[clus_ind]], ];
		}
	}
	return(ret_val);
}

calc_score_with_metric <- function(trial, metric = 'outcome')
{
	if (metric == 'outcome')
	{
        adv_deck = c(99 , 100, 3, 4);         # c, d
        disadv_deck = c(97, 98, 1, 2);        # a, b
	}
	else if (metric == 'loss')
	{
        adv_deck = c(98, 99, 100, 1, 2, 4);         # c, b, d
        disadv_deck = c(97, 1);        				# a
	}
	else if (metric == 'gain')
	{
        adv_deck = c(98 , 100, 2, 4);         # b, d
        disadv_deck = c(97, 99, 1, 3);        # a, c
	}

	num_adv_selection = sum(trial %in% adv_deck);
	num_disadv_selection = sum(trial %in% disadv_deck);
	return (num_adv_selection - num_disadv_selection);
}


# custom distance between 20 block score of two subjects
# last blocks have higher weight in distance
block_dist <- function(x, y)
{
  #return(sum(sqrt((x - y) ^ 2 * t(seq(1,5)))));
  #return(sum(sqrt((x - y) ^ 2 * t(c(1,1,1,0,0)))));
  return(sum(sqrt((x - y) ^ 2 * t(c(0,0,0.5,1,2)))));
  #return(sum(sqrt((x - y) ^ 2 * t(c(1,1,1,1,1)))));
}


within_group_cluster <- function(score, num_of_clusters = 2, col_range)
{
	# cluster subjects with in group and ask user to select groups
	# score: a list of group scores e.g. score$com is the score of subjects in computer group
	# @return list of clusters each contain subjects id
	#	library('proxy');
	#	library('dtw');
	ret_val = list();
	if (exists("col_range"))
	{
		dis = dist(score[, col_range],  method = "DTW");
	}
	else
	{
		dis = dist(score,  method = "DTW");
	}
	hc = hclust(dis, method = "ward");
	plot(hc);
	clusters = cutree(hc, k = num_of_clusters);
	ind = rownames(as.matrix(clusters));
	
	for (clus_ind in seq(num_of_clusters))
	{
		ret_val[[clus_ind]] = ind[clusters %in% clus_ind];
		print(ret_val[[clus_ind]]);
	}
	return(ret_val);
}

calc_good_bad <- function(trial_choice, metric = 'outcome')
{
	len = length(trial_choice);
	good_bad = matrix(nrow = 1, ncol = len + 1);
	good_bad[1, 1] = 0;
	for (i in seq(1,len))
	{
		ch = intToUtf8(trial_choice[i]);
		ch = tolower(ch);
		if (metric == 'outcome')
		{
		#	ch = trial_choice[i] - 96;  # deck A is 97, convert to matrix index
			if (ch %in% c('a', 'b'))		# deck A, B
			{
				good_bad[1, i + 1] = good_bad[1, i]  - 0.86;
			}
			else if (ch %in% c('c', 'd'))		# deck C, D
			{
				good_bad[1, i + 1] = good_bad[1, i]  + 0.86;
			}
		}
		else if (metric == 'loss')
		{
			if (ch %in% c('a'))		# deck A
			{
				good_bad[1, i + 1] = good_bad[1, i]  - 1.47;
			}
			else if (ch %in% c('c'))		# deck C
			{
				good_bad[1, i + 1] = good_bad[1, i]  + 0.79;
			}
			else if (ch %in% c('b', 'd'))		# deck B, D
			{
				good_bad[1, i + 1] = good_bad[1, i]  + 0.34;
			}
		}
		else if (metric == 'gain')
		{
			if (ch %in% c('a', 'c'))		# deck A, B
			{
				good_bad[1, i + 1] = good_bad[1, i]  - 0.86;
			}
			else if (ch %in% c('b', 'd'))		# deck C, D
			{
				good_bad[1, i + 1] = good_bad[1, i]  + 0.86;
			}
		}	
	}
	return(good_bad);
}

calc_acc_reward <- function(trial_choice)
{
	rew_pun = read.octave('pen.dat');
	len = length(trial_choice);
	acc_rew = matrix(nrow = 1, ncol = len + 1);
	ind = matrix(c(1,1,1,1), nrow = 4, ncol = 1);
	acc_rew[1,1] = 2000;  # initial deposit!
	for (i in seq(len))
	{
		ch = trial_choice[i] - 96;  # deck A is 97, convert to matrix index
		pin = ind[ch, 1];       # punishment index
		#    print(sprintf("i: %d, pin: %d, ch: %d", i, pin, ch));
		acc_rew[1, i + 1] = acc_rew[1, i] + rew_pun$reward[ch, i] - rew_pun$punish[ch, pin];
		ind[ch, 1] = ind[ch, 1] + 1;
	}
	return(acc_rew);
}


corr_learning_personality <- function(sub_path, best_worst)
{
	print("Correlation Between Learned Subjects and Personalities");
	sub_mat = load_subjects(sub_path);
	com_best_subs = sub_mat[rownames(best_worst$com_best), ];
	pen_best_subs = sub_mat[rownames(best_worst$pen_best), ];
	web_best_subs = sub_mat[rownames(best_worst$web_best), ];
	com_worst_subs = sub_mat[rownames(best_worst$com_worst), ];
	pen_worst_subs = sub_mat[rownames(best_worst$pen_worst), ];
	web_worst_subs = sub_mat[rownames(best_worst$web_worst), ];

	best_subs = rbind(com_best_subs, pen_best_subs, web_best_subs);
	worst_subs = rbind(com_worst_subs, pen_worst_subs, web_worst_subs);

	best_type = 1;
	worst_type = 2;
	best_subs_aug = cbind(best_subs, matrix(best_type, nrow = nrow(best_subs), ncol = 1, dimnames = list(c(), c('learned'))));
	worst_subs_aug = cbind(worst_subs, matrix(worst_type, nrow = nrow(worst_subs), ncol = 1, dimnames = list(c(), c('learned'))));

	tdata = rbind(best_subs_aug, worst_subs_aug);

	# calc t-test for each parameter
	ci = 0.05;		# confidence intervall
	cl = 1 - ci;	# confidence level
	tbis = t.test(tdata[, "bis"] ~ tdata[, "learned"], data = tdata, conf.level = cl);
	dof = tbis$parameter;
	tval = tbis$statistic;
	if (abs(tval) > dt(1 - ci / 2, dof))
	{
		print("BIS::Learned => H0 doesn't rejected!");
	}
	tbas = t.test(tdata[, "bas"] ~ tdata[, "learned"], data = tdata, conf.level = cl);
	dof = tbas$parameter;
	tval = tbas$statistic;
	if (abs(tval) > dt(1 - ci / 2, dof))
	{
		print("BAS::Learned => H0 doesn't rejected!");
	}
	tbas_rr = t.test(tdata[, "bas_rr"] ~ tdata[, "learned"], data = tdata, conf.level = cl);
	dof = tbas_rr$parameter;
	tval = tbas_rr$statistic;
	if (abs(tval) > dt(1 - ci / 2, dof))
	{
		print("BAS_RR::Learned => H0 doesn't rejected!");
	}
	tbas_fs = t.test(tdata[, "bas_fs"] ~ tdata[, "learned"], data = tdata, conf.level = cl);
	dof = tbas_fs$parameter;
	tval = tbas_fs$statistic;
	if (abs(tval) > dt(1 - ci / 2, dof))
	{
		print("BAS_FS::Learned => H0 doesn't rejected!");
	}
	tbas_d = t.test(tdata[, "bas_d"] ~ tdata[, "learned"], data = tdata, conf.level = cl);
	dof = tbas_d$parameter;
	tval = tbas_d$statistic;
	if (abs(tval) > dt(1 - ci / 2, dof))
	{
		print("BAS_D::Learned => H0 doesn't rejected!");
	}
	print("End of Corr");
}

plot_bas_bis <- function(sub_path, best_worst, num_of_block)
{
	ratio = 2;
	par(cex.axis = 3, cex.main = 3, cex.lab = 3);
	png(paste(BASE_PATH, 'res.png', sep = "/"), 
		width = 1360 * ratio, height = 768 * ratio);
	attach(mtcars);
	# par(mfrow = c(2, 4), oma = c(2, 2, 2, 2));	 
	layout(matrix(c(1,2,3,4,5,5,7,9,6,6,8,8), nrow = 3, ncol = 4, byrow = TRUE));

	# plot sub bas/bis with groups color
	sub_mat = load_subjects(sub_path);
	sub_col = matrix(nrow = 1, ncol = nrow(sub_mat));
	sub_row = rownames(sub_mat);
	sub_col[sub_row %in% rownames(best_worst$com_best)] = 'red';
	sub_col[sub_row %in% rownames(best_worst$pen_best)] = 'orange';
	sub_col[sub_row %in% rownames(best_worst$web_best)] = 'yellow';
	sub_col[sub_row %in% rownames(best_worst$com_worst)] = 'blue';
	sub_col[sub_row %in% rownames(best_worst$pen_worst)] = 'green';
	sub_col[sub_row %in% rownames(best_worst$web_worst)] = 'lightgreen';

	com_best_subs = sub_mat[rownames(best_worst$com_best), ];
	pen_best_subs = sub_mat[rownames(best_worst$pen_best), ];
	web_best_subs = sub_mat[rownames(best_worst$web_best), ];
	com_worst_subs = sub_mat[rownames(best_worst$com_worst), ];
	pen_worst_subs = sub_mat[rownames(best_worst$pen_worst), ];
	web_worst_subs = sub_mat[rownames(best_worst$web_worst), ];

	cur_sub = rbind(com_best_subs, pen_best_subs, web_best_subs, com_worst_subs, pen_worst_subs, web_worst_subs);
	sub_col = matrix(nrow = 1, ncol = nrow(cur_sub));
	sub_row = rownames(cur_sub);
	sub_col[sub_row %in% rownames(best_worst$com_best)] = 'red';
	sub_col[sub_row %in% rownames(best_worst$pen_best)] = 'orange';
	sub_col[sub_row %in% rownames(best_worst$web_best)] = 'yellow';
	sub_col[sub_row %in% rownames(best_worst$com_worst)] = 'blue';
	sub_col[sub_row %in% rownames(best_worst$pen_worst)] = 'green';
	sub_col[sub_row %in% rownames(best_worst$web_worst)] = 'lightgreen';
	sub_mat = cur_sub;
#	print(cur_sub);

	#  x11();
	par(cex.axis = 2, cex.main = 3, cex.lab = 2);
	plot(sub_mat[, "bas"], sub_mat[, "bis"], col = sub_col, pch = 21, bg = sub_col, main = "Subjects BAS/BIS Dist.");
	legend('bottomright', c('COM Best', 'PEN Best', 'WEB Best', 'COM Worst', 'PEN Worst', 'WEB Worst'), 
		   fill = c('red', 'orange', 'yellow', 'blue', 'green', 'lightgreen'), cex = 2);
	textxy(sub_mat[, "bas"], sub_mat[, "bis"], labs = sub_row, cx = 2);
	#	readline("Press Enter for Next Plots: ");

	lm_com_best = lm(com_best_subs[, "bis"] ~ com_best_subs[, "bas"]);
	lm_pen_best = lm(pen_best_subs[, "bis"] ~ pen_best_subs[, "bas"]);
	lm_web_best = lm(web_best_subs[, "bis"] ~ web_best_subs[, "bas"]);
	lm_com_worst = lm(com_worst_subs[, "bis"] ~ com_worst_subs[, "bas"]);
	lm_pen_worst = lm(pen_worst_subs[, "bis"] ~ pen_worst_subs[, "bas"]);
	lm_web_worst = lm(web_worst_subs[, "bis"] ~ web_worst_subs[, "bas"]);
	# abline(lm_com_best, col = 'red');
	# abline(lm_pen_best, col = 'orange');
	# abline(lm_web_best, col = 'yellow');
	# abline(lm_com_worst, col = 'blue');
	# abline(lm_pen_worst, col = 'green');
	# abline(lm_web_worst, col = 'lightgreen');


	plot(sub_mat[, "bas_rr"], sub_mat[, "bis"], col = sub_col, pch = 21, bg = sub_col, main = "Subjects BAS RR/BIS Dist.");
	legend('bottomright', c('COM Best', 'PEN Best', 'WEB Best', 'COM Worst', 'PEN Worst', 'WEB Worst'), 
		   fill = c('red', 'orange', 'yellow', 'blue', 'green', 'lightgreen'), cex = 2);
	textxy(sub_mat[, "bas_rr"], sub_mat[, "bis"], labs = sub_row, cx = 2);
	lm_com_best = lm(com_best_subs[, "bis"] ~ com_best_subs[, "bas_rr"]);
	lm_pen_best = lm(pen_best_subs[, "bis"] ~ pen_best_subs[, "bas_rr"]);
	lm_web_best = lm(web_best_subs[, "bis"] ~ web_best_subs[, "bas_rr"]);
	lm_com_worst = lm(com_worst_subs[, "bis"] ~ com_worst_subs[, "bas_rr"]);
	lm_pen_worst = lm(pen_worst_subs[, "bis"] ~ pen_worst_subs[, "bas_rr"]);
	lm_web_worst = lm(web_worst_subs[, "bis"] ~ web_worst_subs[, "bas_rr"]);
	# abline(lm_com_best, col = 'red');
	# abline(lm_pen_best, col = 'orange');
	# abline(lm_web_best, col = 'yellow');
	# abline(lm_com_worst, col = 'blue');
	# abline(lm_pen_worst, col = 'green');
	# abline(lm_web_worst, col = 'lightgreen');


	plot(sub_mat[, "bas_fs"], sub_mat[, "bis"], col = sub_col, pch = 21, bg = sub_col, main = "Subjects BAS FS/BIS Dist.");
	legend('bottomright', c('COM Best', 'PEN Best', 'WEB Best', 'COM Worst', 'PEN Worst', 'WEB Worst'), 
		   fill = c('red', 'orange', 'yellow', 'blue', 'green', 'lightgreen'), cex = 2);
	textxy(sub_mat[, "bas_fs"], sub_mat[, "bis"], labs = sub_row, cx = 2);
	lm_com_best = lm(com_best_subs[, "bis"] ~ com_best_subs[, "bas_fs"]);
	lm_pen_best = lm(pen_best_subs[, "bis"] ~ pen_best_subs[, "bas_fs"]);
	lm_web_best = lm(web_best_subs[, "bis"] ~ web_best_subs[, "bas_fs"]);
	lm_com_worst = lm(com_worst_subs[, "bis"] ~ com_worst_subs[, "bas_fs"]);
	lm_pen_worst = lm(pen_worst_subs[, "bis"] ~ pen_worst_subs[, "bas_fs"]);
	lm_web_worst = lm(web_worst_subs[, "bis"] ~ web_worst_subs[, "bas_fs"]);
	# abline(lm_com_best, col = 'red');
	# abline(lm_pen_best, col = 'orange');
	# abline(lm_web_best, col = 'yellow');
	# abline(lm_com_worst, col = 'blue');
	# abline(lm_pen_worst, col = 'green');
	# abline(lm_web_worst, col = 'lightgreen');

	plot(sub_mat[, "bas_d"], sub_mat[, "bis"], col = sub_col, pch = 21, bg = sub_col, main = "Subjects BAS D/BIS Dist.");
	legend('bottomright', c('COM Best', 'PEN Best', 'WEB Best', 'COM Worst', 'PEN Worst', 'WEB Worst'), 
		   fill = c('red', 'orange', 'yellow', 'blue', 'green', 'lightgreen'), cex = 2);
	textxy(sub_mat[, "bas_d"], sub_mat[, "bis"], labs = sub_row, cx = 1);
	lm_com_best = lm(com_best_subs[, "bis"] ~ com_best_subs[, "bas_d"]);
	lm_pen_best = lm(pen_best_subs[, "bis"] ~ pen_best_subs[, "bas_d"]);
	lm_web_best = lm(web_best_subs[, "bis"] ~ web_best_subs[, "bas_d"]);
	lm_com_worst = lm(com_worst_subs[, "bis"] ~ com_worst_subs[, "bas_d"]);
	lm_pen_worst = lm(pen_worst_subs[, "bis"] ~ pen_worst_subs[, "bas_d"]);
	lm_web_worst = lm(web_worst_subs[, "bis"] ~ web_worst_subs[, "bas_d"]);
	# abline(lm_com_best, col = 'red');
	# abline(lm_pen_best, col = 'orange');
	# abline(lm_web_best, col = 'yellow');
	# abline(lm_com_worst, col = 'blue');
	# abline(lm_pen_worst, col = 'green');
	# abline(lm_web_worst, col = 'lightgreen');


	#	x11();
	plot(jitter(sub_mat[, "pnas_n"]), sub_mat[, "pnas_p"], col = sub_col, pch = 21, bg = sub_col, main = "Subjects PNAS Dist.");
	textxy(jitter(sub_mat[, "pnas_n"]), sub_mat[, "pnas_p"], labs = sub_row, cx = 2);
	lm_com_best = lm(com_best_subs[, "pnas_p"] ~ com_best_subs[, "pnas_n"]);
	lm_pen_best = lm(pen_best_subs[, "pnas_p"] ~ pen_best_subs[, "pnas_n"]);
	lm_web_best = lm(web_best_subs[, "pnas_p"] ~ web_best_subs[, "pnas_n"]);
	lm_com_worst = lm(com_worst_subs[, "pnas_p"] ~ com_worst_subs[, "pnas_n"]);
	lm_pen_worst = lm(pen_worst_subs[, "pnas_p"] ~ pen_worst_subs[, "pnas_n"]);
	lm_web_worst = lm(web_worst_subs[, "pnas_p"] ~ web_worst_subs[, "pnas_n"]);
	# abline(lm_com_best, col = 'red');
	# abline(lm_pen_best, col = 'orange');
	# abline(lm_web_best, col = 'yellow');
	# abline(lm_com_worst, col = 'blue');
	# abline(lm_pen_worst, col = 'green');
	# abline(lm_web_worst, col = 'lightgreen');
	#legend(-1, -1, c('COM Best', 'PEN Best', 'COM Worst', 'PEN Worst'), fill = c('red', 'orange', 'blue', 'green'), cex = 2);
	#  x11();
	#  scatterplot3d(sub_mat[, "bas"], sub_mat[, "bis"], sub_mat[, "pnas_n"]);


	boxplot(best_worst$com_best, ylim = c(-20, 20), at = 1:num_of_block, col = 'orange', boxwex = 0.15, main = "Best Clusters");
	boxplot(best_worst$pen_best, at = 1:num_of_block + 0.2, col = 'yellow', boxwex = 0.15, add = TRUE);
	boxplot(best_worst$web_best, at = 1:num_of_block - 0.2, col = 'red', boxwex = 0.15, add = TRUE);
	legend('bottomright', c(sprintf("COM #%d:{%s}", nrow(best_worst$com_best), 
									paste(as.character(rownames(best_worst$com_best)), collapse = ', ')), 
				sprintf("PEN #%d:{%s}", nrow(best_worst$pen_best), 
						paste(as.character(rownames(best_worst$pen_best)), collapse = ', ')),
				sprintf("WEB #%d:{%s}", nrow(best_worst$web_best), 
						paste(as.character(rownames(best_worst$web_best)), collapse = ', '))),
		   fill = c('orange', 'yellow', 'red'), cex = 2);
	num_com_best_female = length(which(com_best_subs[,"sex"] == 1));
	num_com_best_male = length(which(com_best_subs[,"sex"] == 0));
	com_best_sex_dis = rbind(num_com_best_male, num_com_best_female);
	num_pen_best_female = length(which(pen_best_subs[,"sex"] == 1));
	num_pen_best_male = length(which(pen_best_subs[,"sex"] == 0));
	pen_best_sex_dis = rbind(num_pen_best_male, num_pen_best_female);
	num_web_best_female = length(which(web_best_subs[,"sex"] == 1));
	num_web_best_male = length(which(web_best_subs[,"sex"] == 0));
	web_best_sex_dis = rbind(num_web_best_male, num_web_best_female);
	barplot(cbind(com_best_sex_dis, pen_best_sex_dis, web_best_sex_dis), 
			names.arg = c("COM", "PEN", "WEB"), col = c('purple', 'pink'));  # pink for female :D

	boxplot(best_worst$com_worst, ylim = c(-20, 20), at = 1:num_of_block, col = 'orange', boxwex = 0.15, main = "Worst Clusters");
	boxplot(best_worst$pen_worst, at = 1:num_of_block + 0.2, col = 'yellow', boxwex = 0.15, add = TRUE);
	boxplot(best_worst$web_worst, at = 1:num_of_block - 0.2, col = 'red', boxwex = 0.15, add = TRUE);
	legend('bottomright', c(sprintf("COM #%d:{%s}", nrow(best_worst$com_worst), 
									paste(as.character(rownames(best_worst$com_worst)), collapse = ', ')), 
				sprintf("PEN #%d:{%s}", nrow(best_worst$pen_worst), 
						paste(as.character(rownames(best_worst$pen_worst)), collapse = ', ')), 
				sprintf("PEN #%d:{%s}", nrow(best_worst$web_worst), 
						paste(as.character(rownames(best_worst$web_worst)), collapse = ', '))), 
		   fill = c('orange', 'yellow', 'red'), cex = 2);
	num_com_worst_female = length(which(com_worst_subs[,"sex"] == 1));
	num_com_worst_male = length(which(com_worst_subs[,"sex"] == 0));
	com_worst_sex_dis = rbind(num_com_worst_male, num_com_worst_female);
	num_pen_worst_female = length(which(pen_worst_subs[,"sex"] == 1));
	num_pen_worst_male = length(which(pen_worst_subs[,"sex"] == 0));
	pen_worst_sex_dis = rbind(num_pen_worst_male, num_pen_worst_female);
	num_web_worst_female = length(which(web_worst_subs[,"sex"] == 1));
	num_web_worst_male = length(which(web_worst_subs[,"sex"] == 0));
	web_worst_sex_dis = rbind(num_web_worst_male, num_web_worst_female);
	barplot(cbind(com_worst_sex_dis, pen_worst_sex_dis, web_worst_sex_dis), 
			names.arg = c("COM", "PEN", "WEB"), col = c('purple', 'pink'));  # pink for female :D

	dev.off();
}


plot_ts <- function(igt_data, best_worst)
{
	num_of_groups = length(names(best_worst));
	num_of_clusters_per_group = length(names(best_worst[[names(best_worst)[1]]]));

 	groups = names(best_worst);
 	ts_len = 101;   # trial length
# 	### PLOT Time Series ####
# 	ratio = 2;
# 	png(paste(BASE_PATH, 'acc_reward.png', sep = "/"), 
# 		width = 1360 * ratio, height = 768 * ratio);
# 	attach(mtcars);
# 	# par(mfrow = c(2, 4), oma = c(2, 2, 2, 2));	 
# 	layout(matrix(seq(num_of_clusters_per_group * num_of_groups), 
# 				  nrow = num_of_clusters_per_group, ncol = num_of_groups, byrow = FALSE));
# 
# 	for (gr in groups)
# 	{
# 		for (sg in sort(names(best_worst[[gr]])))
# 		{
# 			subs = rownames(best_worst[[gr]][[sg]]);
# 			data_ts = matrix(nrow = length(subs), ncol = ts_len);
# 			rownames(data_ts) = subs;
# 			for (sub_name in subs)
# 			{
# 				if (!(sub_name %in% rownames(data_ts)))
# 				{
# 					print(rownames(data_ts));
# 					print(sub_name);
# 				}
# 				if (!(sub_name %in% rownames(igt_data)))
# 				{
# 					print(rownames(igt_data));
# 					print(sub_name);
# 				}
# 				data_ts[sub_name, ] = calc_acc_reward(igt_data[sub_name, ]);
# 			}
# 			matplot(t(data_ts), type = 'o', pch = 1:nrow(data_ts),
# 					col = 1:nrow(data_ts), bg = 1:nrow(data_ts), main = paste(gr, sg, sep = "_"), 
# 					lwd = 3, cex.axis = 3, cex.main = 3);
# 			legend('topleft', rownames(data_ts), fill = 1:nrow(data_ts), cex = 3, ncol = 3);
# 		}
# 	}
# 	dev.off();


	### PLOT Good-BadTime Series ####
	metrics = c('outcome', 'gain', 'loss');
	for (metric in metrics)
	{
		ratio = 2;
		fn = paste(paste(BASE_PATH, 'good_bad', sep = "/"), 
				   paste(metric, 'png', sep = '.'), sep = '_');
	#	png(fn, width = 1360 * ratio, height = 768 * ratio);
		png(fn, width = 1360 * ratio, height = 1360 * ratio);
		attach(mtcars);
		# par(mfrow = c(2, 4), oma = c(2, 2, 2, 2));	 
		layout(matrix(seq(num_of_clusters_per_group * num_of_groups), 
					  nrow = num_of_clusters_per_group, ncol = num_of_groups, byrow = FALSE));
		# COM Best #
		ts_len = 101;   # trial length
		par(cex.axis = 3, cex.main = 3, cex.lab = 3, mar = c(6,6,6,6));


		for (gr in groups)
		{
			for (sg in sort(names(best_worst[[gr]])))
			{
				subs = rownames(best_worst[[gr]][[sg]]);
				data_ts = matrix(nrow = length(subs), ncol = ts_len);
				rownames(data_ts) = subs;
				for (sub_name in subs)
				{
					data_ts[sub_name, ] = calc_good_bad(igt_data[sub_name, ], metric);
				}
				matplot(t(data_ts), type = 'o', pch = 1:nrow(data_ts),
						col = 1:nrow(data_ts), bg = 1:nrow(data_ts), main = paste(gr, sg, sep = "_"),
					   	lwd = 3, cex.axis = 3, cex.main = 3);
				legend('topleft', rownames(data_ts), fill = 1:nrow(data_ts), cex = 3, ncol = 3);
			}
		}
		dev.off();
	}
}


total_clustering <- function(igt_data, score, num_of_clust = 3, col_range)
{
	all_data = rbind(score$web, score$pen, score$com);
	dis = dist(all_data[, col_range], method = "DTW");
	hc = hclust(dis, method = "ward");
	clusts = cutree(hc, k = num_of_clust);
	ind = rownames(as.matrix(clusts));
	
	ret_clusts = list();
	for (i in 1:num_of_clust)
	{
		key = sprintf("clust_%d", i);
		ret_clusts[[key]] = ind[clusts %in% i];			# cluster indexes
		print(length(ret_clusts[[key]]));
	}

	ratio = 2;
	png(paste(BASE_PATH, 'total_clusts.png', sep = "/"), 
		width = 1360 * ratio, height = 768 * ratio);
	attach(mtcars);
	# par(mfrow = c(2, 4), oma = c(2, 2, 2, 2));	 
	p_ncol = ceiling(num_of_clust / 2);
	layout(matrix(seq(1, p_ncol * 2), nrow = 2, ncol = p_ncol, byrow = FALSE));

	par(cex.axis = 3, cex.main = 3, cex.lab = 3, mar = c(4,4,4,4));
	ts_len = 101;
	for (key in names(ret_clusts))
	{
		print(key);

		g_nrow = length(ret_clusts[[key]]);
		group_ts = matrix(nrow = g_nrow, ncol = ts_len);
		rownames(group_ts) = c(ret_clusts[[key]]);
		for (sub_name in c(ret_clusts[[key]]))
		{
			group_ts[sub_name, ] = calc_good_bad(igt_data[sub_name, ]);
		}
		matplot(t(group_ts), type = 'o', pch = 1:g_nrow, col = 1:g_nrow, bg = 1:g_nrow, main = key, lwd = 3);
		legend('topleft', rownames(group_ts), pch = 1:g_nrow, fill = 1:g_nrow, cex = 3, ncol = 3);
	}
	dev.off();
}


plot_horstmann_result <- function(horstmann_result, best_worst, sub_exp_map)
{
	num_of_blocks = dim(horstmann_result)[2];
	w_outcome_median = matrix(apply(horstmann_result[,,'outcome'], 2, median), nrow = 1);
	rownames(w_outcome_median) = c('outcome');
	w_gain_median = matrix(apply(horstmann_result[,,'gain'], 2, median), nrow = 1);
	rownames(w_gain_median) = c('gain');
	w_loss_median = matrix(apply(horstmann_result[,,'loss'], 2, median), nrow = 1);
	rownames(w_loss_median) = c('loss');

	total_data = rbind(w_outcome_median, w_gain_median, w_loss_median);

	ratio = 2;
	png(paste(BASE_PATH, 'horstman_result.png', sep = "/"), 
		width = 1360 * ratio, height = 768 * ratio);
	attach(mtcars);
	# par(mfrow = c(2, 4), oma = c(2, 2, 2, 2));	 
	par(cex = 3, cex.axis = 3, cex.main = 3, cex.lab = 3, mar = c(5,5,5,5) + 1);
	layout(matrix(seq(1, 4), nrow = 2, ncol = 2, byrow = FALSE));

	# plot total
	matplot(t(total_data), type = 'o', pch = 1:3, col = 1:3, bg = 1:3, lwd = 6,
				main = "Total", ylab = "Median Weight", xlab = "Block");
	# calculate friedman test value for each block
	for (bk in seq(num_of_blocks))
	{
		# create data frame
		df_outcome = matrix(horstmann_result[, bk, 'outcome'], ncol = 1);
		df_gain = matrix(horstmann_result[, bk, 'gain'], ncol = 1);
		df_loss = matrix(horstmann_result[, bk, 'loss'], ncol = 1);
		df_pre = cbind(df_outcome, df_gain, df_loss);
		num_of_sub = dim(df_pre)[1];
		sig_tr = 0.05;
		df_frame = data.frame(Value = c(t(df_pre)), Group = factor(rep(c('outcome', 'gain', 'loss'), num_of_sub)), 
							  Subjects = factor(rep(rownames(horstmann_result), each = 3)));
		friedman_result = friedman.test.with.post.hoc(Value ~ Group | Subjects, data = df_frame, signif.P = sig_tr); 

		print(sprintf("Total Block %d\n", bk));
		print(friedman_result);

		if ("PostHoc.Test" %in% names(friedman_result))
		{
			print("<Post Hoc Analysis -- Total Data>");
			pvalue_mat = friedman_result$PostHoc.Test;
			for (pv_ind in seq(dim(pvalue_mat)[1]))
			{
				if (pvalue_mat[pv_ind] <= sig_tr)
				{
					pv_grps = strsplit(rownames(pvalue_mat)[pv_ind], ' - ');
					pv_grp1 = pv_grps[[1]][1];
					pv_grp2 = pv_grps[[1]][2];
					text(bk, total_data[pv_grp1, bk], '**', cex = 6);
					text(bk, total_data[pv_grp2, bk], '**', cex = 6);
				}
			}
			print("</Post Hoc Analysis -- Total Data>");
		} # Post Hoc
	} # block
	legend('bottomright', rownames(total_data), pch = 1:3, fill = 1:3, cex = 3);

	# plot by exp
	groups = c('pen', 'com', 'web');
	rn = rownames(sub_exp_map);		# get list of subjects that map to exp
	for (g in groups)
	{
		sub_in_ind = which(sub_exp_map == g);	# extract index of subjects in g exp
		sub_in_g = rn[sub_in_ind];			# extract sub names
		sub_in_g = intersect(rownames(horstmann_result), sub_in_g);
		curr_data = horstmann_result[sub_in_g,,];
		cd_outcome_median = matrix(apply(curr_data[,,'outcome'], 2, median), nrow = 1);
		cd_gain_median = matrix(apply(curr_data[,,'gain'], 2, median), nrow = 1);
		cd_loss_median = matrix(apply(curr_data[,,'loss'], 2, median), nrow = 1);
		rownames(cd_outcome_median) = c('outcome');
		rownames(cd_gain_median) = c('gain');
		rownames(cd_loss_median) = c('loss');
		cd_bind = rbind(cd_outcome_median, cd_gain_median, cd_loss_median);
		matplot(t(cd_bind), type = 'o', pch = 1:3, col = 1:3, bg = 1:3, lwd = 6, 
					main = g, ylab = "Median Weight", xlab = "Block");
		# calculate friedman test value for each block
		for (bk in seq(num_of_blocks))
		{
			# create data frame
			df_outcome = matrix(curr_data[, bk, 'outcome'], ncol = 1);
			df_gain = matrix(curr_data[, bk, 'gain'], ncol = 1);
			df_loss = matrix(curr_data[, bk, 'loss'], ncol = 1);
			df_pre = cbind(df_outcome, df_gain, df_loss);
			num_of_sub = dim(df_pre)[1];
			sig_tr = 0.05;
			df_frame = data.frame(Value = c(t(df_pre)), Group = factor(rep(c('outcome', 'gain', 'loss'), num_of_sub)), 
								  Subjects = factor(rep(sub_in_g, each = 3)));
			friedman_result = friedman.test.with.post.hoc(Value ~ Group | Subjects, data = df_frame, signif.P = sig_tr); 

			print(sprintf("Group: %s, Block %d\n", g, bk));
			print(friedman_result);

			if ("PostHoc.Test" %in% names(friedman_result))
			{
				print("<Post Hoc Analysis:>");
				pvalue_mat = friedman_result$PostHoc.Test;
				for (pv_ind in seq(dim(pvalue_mat)[1]))
				{
					if (pvalue_mat[pv_ind] <= sig_tr)
					{
						pv_grps = strsplit(rownames(pvalue_mat)[pv_ind], ' - ');
						pv_grp1 = pv_grps[[1]][1];
						pv_grp2 = pv_grps[[1]][2];
						text(bk, cd_bind[pv_grp1, bk], '**', cex = 6);
						text(bk, cd_bind[pv_grp2, bk], '**', cex = 6);
					}
				}
				print("</Post Hoc Analysis:>");
			} # within group, Post Hoc


		} # block
		legend('bottomright', rownames(cd_bind), pch = 1:3, fill = 1:3, cex = 3);
	} # group

	# between group, kruskal-wallis test
	sink('./between_group_kruskal.txt');
	sig_tr = 0.05;
	for (bk in seq(num_of_blocks))
	{
		for (metric in c('outcome', 'gain', 'loss'))
		{
			print(sprintf("Kruskal Test, Metric: %s, Block: %d", metric, bk));
			kw_res = kruskal_wallis_test(dat = horstmann_result[, bk, metric], sub_exp_map);
			for (pv_ind in seq(dim(kw_res)[1]))
			{
				if (kw_res[pv_ind] <= sig_tr)
				{
					pv_grps = strsplit(rownames(kw_res)[pv_ind], ' - ');
					pv_grp1 = pv_grps[[1]][1];
					pv_grp2 = pv_grps[[1]][2];
					print(sprintf("++++++ Kruskal +++++++ Metric: %s, Block: %d, groups(%s, %s)", 
								   metric, bk, pv_grp1, pv_grp2));
					# text(bk, cd_bind[pv_grp1, bk], '+++', cex = 6);
					# text(bk, cd_bind[pv_grp2, bk], '+++', cex = 6);
				}
			}
		}
	}
	sink();
	dev.off();

	print("#############################");

	ratio = 2;
	png(paste(BASE_PATH, 'horstmann_best_worst.png', sep = "/"),
		width = 1360 * ratio, height = 768 * ratio);
	attach(mtcars);
	# par(mfrow = c(2, 4), oma = c(2, 2, 2, 2));	 
	par(cex = 3, cex.axis = 3, cex.main = 3, cex.lab = 3, mar = c(5,5,5,5) + 1);
	layout(matrix(seq(6), nrow = 2, ncol = 3, byrow = FALSE));


	sink('./friedman_posthoc.txt');
	for (grp in c('pen', 'web', 'com'))
	{
		for (clus in c('best', 'worst'))
		{
			ind_name = paste(grp, clus, sep = '_');		# e.g. pen_worst
			sub_in = rownames(best_worst[[ind_name]]);		# get subjects in this cluster

			curr_data = horstmann_result[sub_in,,];
			cd_outcome_median = matrix(apply(curr_data[,,'outcome'], 2, median), nrow = 1);
			cd_gain_median = matrix(apply(curr_data[,,'gain'], 2, median), nrow = 1);
			cd_loss_median = matrix(apply(curr_data[,,'loss'], 2, median), nrow = 1);
			rownames(cd_outcome_median) = c('outcome');
			rownames(cd_gain_median) = c('gain');
			rownames(cd_loss_median) = c('loss');
			cd_bind = rbind(cd_outcome_median, cd_gain_median, cd_loss_median);
			matplot(t(cd_bind), type = 'o', pch = 1:3, col = 1:3, bg = 1:3, lwd = 6, 
						main = ind_name, ylab = "Median Weight", xlab = "Block");
			# calculate friedman test value for each block
			for (bk in seq(num_of_blocks))
			{
				# create data frame
				df_outcome = matrix(curr_data[, bk, 'outcome'], ncol = 1);
				df_gain = matrix(curr_data[, bk, 'gain'], ncol = 1);
				df_loss = matrix(curr_data[, bk, 'loss'], ncol = 1);
				df_pre = cbind(df_outcome, df_gain, df_loss);
				num_of_sub = dim(df_pre)[1];
				sig_tr = 0.05;
				df_frame = data.frame(Value = c(t(df_pre)), Group = factor(rep(c('outcome', 'gain', 'loss'), num_of_sub)), 
									  Subjects = factor(rep(sub_in, each = 3)));
				friedman_result = friedman.test.with.post.hoc(Value ~ Group | Subjects, data = df_frame, signif.P = sig_tr); 

				print(sprintf("Group: %s, Cluster %s, Block %d\n", grp, clus, bk));
				print(friedman_result);

				if ("PostHoc.Test" %in% names(friedman_result))
				{
					print("<Post Hoc Analysis:>");
					pvalue_mat = friedman_result$PostHoc.Test;
					for (pv_ind in seq(dim(pvalue_mat)[1]))
					{
						if (pvalue_mat[pv_ind] <= sig_tr)
						{
							pv_grps = strsplit(rownames(pvalue_mat)[pv_ind], ' - ');
							pv_grp1 = pv_grps[[1]][1];
							pv_grp2 = pv_grps[[1]][2];
							text(bk, cd_bind[pv_grp1, bk], '**', cex = 6);
							text(bk, cd_bind[pv_grp2, bk], '**', cex = 6);
						}
					}
					print("</Post Hoc Analysis:>");
				} # Post Hoc
			} # block
			legend('topleft', rownames(cd_bind), pch = 1:3, fill = 1:3, cex = 3);
		}
	} # group
	sink();
	dev.off();

	return;
}

kruskal_wallis_test <- function(dat, sub_exp_map)
{
	rn = rownames(sub_exp_map);		# get list of subjects
	pen_sub_ind = which(sub_exp_map == 'pen');	# extract index of subjects in g exp
	com_sub_ind = which(sub_exp_map == 'com');	# extract index of subjects in g exp
	web_sub_ind = which(sub_exp_map == 'web');	# extract index of subjects in g exp

	pen_subs = rn[pen_sub_ind];			# extract sub names
	com_subs = rn[com_sub_ind];			# extract sub names
	web_subs = rn[web_sub_ind];			# extract sub names

	pen_subs = intersect(rownames(as.matrix(dat)), pen_subs);
	com_subs = intersect(rownames(as.matrix(dat)), com_subs);
	web_subs = intersect(rownames(as.matrix(dat)), web_subs);

	pen_data = matrix(dat[pen_subs], ncol = 1);
	web_data = matrix(dat[web_subs], ncol = 1);
	com_data = matrix(dat[com_subs], ncol = 1);

	df_pre = rbind(pen_data, web_data, com_data);
	num_of_subs = dim(df_pre)[1];

	pre_data = data.frame(Value = c(t(df_pre)), Groups = factor(c(rep('pen', dim(pen_data)[1]),
																  rep('web', dim(web_data)[1]),
																  rep('com', dim(com_data)[1]))));

	kw = kruskal_test(Value ~ Groups, data = pre_data, 
					  distribution = approximate(B = 99999));

	### Nemenyi-Damico-Wolfe-Dunn test (joint ranking)
    ### Hollander & Wolfe (1999), page 244 
    ### (where Steel-Dwass results are given)
	if (require("multcomp")) 
	{
		NDWD = oneway_test(Value ~ Groups, data = pre_data,
							ytrafo = function(data) trafo(data, numeric_trafo = rank),
							xtrafo = function(data) trafo(data, factor_trafo = function(x)
														  model.matrix(~x - 1) %*% t(contrMat(table(x), "Tukey"))),
							teststat = "max", distribution = approximate(B = 90000))

		### global p-value
		print(pvalue(NDWD));

		ret_val = print(pvalue(NDWD, method = "single-step"));
	}
	
	return (ret_val);
}



horstmann_analysis <- function(raw_data, sub_exp_map, num_of_blocks)
{
	num_of_subs = nrow(raw_data);
	# define weight array w_i and selection portion b_i
	num_of_weights = 3;
	num_of_decks = 4;
	block_weight = array(0, dim = c(num_of_subs, num_of_blocks, num_of_weights),
						 dimnames = list(rownames(raw_data), NULL, c('outcome', 'gain', 'loss')));

	choice_portion = matrix(0, nrow = 4, ncol = 1);
	rownames(choice_portion) = c('a', 'b', 'c', 'd');

	bs = floor(100 / num_of_blocks);

	payoff_scheme = matrix(0, nrow = 4, ncol = 3);
	colnames(payoff_scheme) = c('outcome', 'gain', 'loss');
	rownames(payoff_scheme) = c('a', 'b', 'c', 'd');
	payoff_scheme[,1] = c(-0.86, -0.86, 0.86, 0.86);
	payoff_scheme[,2] = c(-0.86, 0.86, -0.86, 0.86);
	payoff_scheme[,3] = c(-1.47, 0.34, 0.79, 0.34);

	for (i in rownames(raw_data))
	{
		for (b in seq(num_of_blocks))
		{
			block_trial = raw_data[i,][((b - 1) * bs + 1):(b * bs)];
			choice_portion['a',] = sum(block_trial == 97) / bs;
			choice_portion['b',] = sum(block_trial == 98) / bs;
			choice_portion['c',] = sum(block_trial == 99) / bs;
			choice_portion['d',] = sum(block_trial == 100) / bs;
			
			lm_result = lm(choice_portion ~ -1 + payoff_scheme);
			block_weight[i,b,] = lm_result$coefficients;
		}
	}
	return(block_weight);
}


igt_doit <- function(igt_path, metric = 'outcome', sub_path = '')
{
	dat = load_exec_data(igt_path);
	group_fname = "sub_exp_map";
	sub_exp_map = read.csv(paste(igt_path, group_fname, sep = "/"));

	num_of_block = 5;
	score = calc_block_score(sub_data = dat, group_map = sub_exp_map, 
							 score_type = 'raw', metric = metric);


#	best_worst = get_best_worst_block_score(score, num_of_block, seq(3, num_of_block));
	best_worst = get_best_worst_block_score(score, dim(dat)[2]);

	# there is some problem with "exists" function
	if (nchar(sub_path) > 0)
	{
		plot_bas_bis(sub_path, best_worst, num_of_block);
		corr_learning_personality(sub_path, best_worst);
	}

	plot_ts(dat, best_worst);

	total_clustering(dat, score, num_of_clust = 6, seq(3, 5));

	return(list("total_score" = score, "best_worst_score" = best_worst));
}

do_horstmann_analysis <- function(igt_path, sub_path = '')
{
	dat = load_exec_data(igt_path);
	group_fname = "sub_exp_map";
	sub_exp_map = read.csv(paste(igt_path, group_fname, sep="/"));
	num_of_blocks = 5;
	hor_a = horstmann_analysis(raw_data = dat, sub_exp_map, num_of_blocks);

#	score = calc_block_score(dat, sub_exp_map, num_of_blocks);
	score = calc_block_score(sub_data = dat, group_map = sub_exp_map, 
							 score_type = 'block', num_of_block = num_of_blocks, metric = 'outcome');
	best_worst = get_best_worst_block_score(score, num_of_blocks, seq(3, num_of_blocks));

	plot_horstmann_result(hor_a, best_worst, sub_exp_map);
	return(hor_a);
}

create_random_subjects <- function(num_subs, prefix = "rand")
{
	num_rand_sub = num_subs;
	trial_len = 100;
	random_subjects = matrix(ceiling(runif(num_rand_sub * trial_len, min = 0, max = 4) + 96), 
							 nrow = num_rand_sub, ncol = trial_len);
	rownames(random_subjects) = as.vector(lapply(as.matrix(seq(num_rand_sub)), 
														   FUN = function(x) {
															   sprintf("%s_%02d", prefix, x);
														   }));
	return(random_subjects);
}

decision_strategy_analysis <- function(igt_path, sub_path = '', metric = 'outcome', random_salt = TRUE)
{
	dat = load_exec_data(igt_path);
	group_fname = "sub_exp_map";
	sub_exp_map = read.csv(paste(igt_path, group_fname, sep = "/"));

	score = calc_block_score(sub_data = dat, group_map = sub_exp_map, 
							 score_type = 'raw', metric = metric);

	if (random_salt)
	{
		random_subjects = create_random_subjects(30);
		random_score = calc_block_score(sub_data = random_subjects);

		best_worst = get_best_worst_block_score(score, rand_score = random_score);
		dat = rbind(dat, random_subjects);
	}
	else
	{
		best_worst = get_best_worst_block_score(score);
	}
	
	plot_ts(dat, best_worst);
	if (nchar(sub_path) > 0)
	{
	#	sink('./personality_corr.txt');
		sub_mat = load_subjects(sub_path);
		corr_analysis(best_worst, sub_mat);
		sink();
	}
	save(file = paste(BASE_PATH, "image.dat", sep = "/"), 
		 list = ls());
}

corr_analysis <- function(best_worst, sub_mat)
{
	# find the cluster which majority of subjects blongs to
	gr_max = list();
	nb = dim(best_worst[["com"]][["best"]])[2];
	sub_in_gr = list();
	for (gr in names(best_worst))
	{
		max_sub_in_sg = -1;
		max_sg = 'none';
		sub_in_gr[[gr]] = c();		# list of subjects in group gr
		for (sg in sort(names(best_worst[[gr]])))
		{
			subs = rownames(best_worst[[gr]][[sg]]);
			sub_ind = grep("rand_*", subs, invert = T); # remove random subjects
			subs_in_sg = subs[sub_ind];

			sub_in_gr[[gr]] = append(sub_in_gr[[gr]], subs_in_sg);

			if (length(sub_ind) > max_sub_in_sg)
			{
				max_sub_in_sg = length(sub_ind);
				max_sg = sg;
			}
		}
		gr_max[[gr]] = max_sg;
	}

	ratio = 1;
	png(paste(BASE_PATH, sprintf('personality.png') , sep = "/"),
		width = 1360 * ratio, height = 768 * ratio);
	plot(-10, -10, xlim = c(1, 10), ylim = c(0, 1), xlab = "BAS --- BIS");
	
	index = 2;
	bas_bis_pos = 5;
	for (gr in names(best_worst))
	{
		max_sg = gr_max[[gr]];
		nb = dim(best_worst[[gr]][[max_sg]])[2];
		subs_in_max = rownames(best_worst[[gr]][[max_sg]]);
		sub_ind = grep("rand_*", subs_in_max, invert = T); # remove random subjects
		subs_in_max = subs_in_max[sub_ind];
		subs_bas = sub_mat[subs_in_max, "bas"];
		subs_bis = sub_mat[subs_in_max, "bis"];

		total_sub_bas = sub_mat[sub_in_gr[[gr]], "bas"];
		total_sub_bis = sub_mat[sub_in_gr[[gr]], "bis"];

		bas_bis_b = list(total_sub_bas, total_sub_bis);
		bas_bis_p = c(index, index + bas_bis_pos);
		boxplot(bas_bis_b, at = bas_bis_p,
				col = index, boxwex = 0.4, main = "Personality Difference", add = T);
		
		bas_bis_b = list(subs_bas, subs_bis);
		boxplot(bas_bis_b, at = bas_bis_p + 0.5,
				col = index, boxwex = 0.4, main = "Personality Difference", add = T);

		index = index + 1;

		curr_score = best_worst[[gr]][[max_sg]][subs_in_max, ];
		learned_portion = curr_score[, ceiling((0.41) * nb):nb];
		sum_learned_portion = apply(learned_portion, 1, sum);

		if (length(subs_in_max) > 5)
		{
			rcorr(subs_bas, sum_learned_portion);
			rcorr(subs_bis, sum_learned_portion);
		}
		else
		{
			print(sprintf("not enough sub in group: gr: %s, sg: %s", gr, max_sg));
		}
	}
	legend("bottomright", names(best_worst), fill = 2:5, cex = 3);
	dev.off();
}

runner <- function(FUN, igt_path, metric = 'outcome', result_base_path = "/tmp", num_of_run = 30, run_it = TRUE)
{
	if (run_it)
	{
		for (run in seq(num_of_run))
		{
			print(sprintf("-=:: Run %02d ::=.", run));
			BASE_PATH <<- sprintf("%s/run_%02d", result_base_path, run);
			print(BASE_PATH);
			dir.create(BASE_PATH, recursive = TRUE);
			FUN(igt_path = igt_path, metric = metric)
		}
	}

	num_of_group = 3;
	cluster_per_group = 3;
	run_result = array(0, dim = c(num_of_run, num_of_group, cluster_per_group),
						 dimnames = list(NULL, c("pen", "web", "com"), c('best', 'worst', 'random')));

	random_salted_result = array(0, dim = c(num_of_run, num_of_group, cluster_per_group),
						 dimnames = list(NULL, c("pen", "web", "com"), c('best', 'worst', 'random')));
	for (run in seq(num_of_run))
	{
		zz = new.env();
		load(sprintf("%s/run_%02d/image.dat", result_base_path, run), 
			 envir = zz);
		best_worst = get("best_worst", envir = zz);
		for (gr in names(best_worst))
		{
			total_sub_in_gr = 0;
			for (sg in sort(names(best_worst[[gr]])))
			{
				subs = rownames(best_worst[[gr]][[sg]]);
				sub_ind = grep("rand_*", subs, invert = T); # remove random subjects
				random_sub_ind = grep("rand_*", subs); 		# get list of random subjects
				# TODO, debug this partition
				# sub_ind or sub[sub_ind] ???
				if (length(sub_ind) > 1)
				{
					best_worst[[gr]][[sg]] = best_worst[[gr]][[sg]][sub_ind, ];
					num_in_sg = dim(best_worst[[gr]][[sg]])[1];
				}
				else if (length(sub_ind) == 1)		# becuase of dim problem with one entry
				{
					best_worst[[gr]][[sg]] = best_worst[[gr]][[sg]][sub_ind, ];
					num_in_sg = 1;
				}

				total_sub_in_gr = total_sub_in_gr + num_in_sg;

				if (sg %in% c("best", "worst"))
				{
					print(sprintf("run %d, group: %s,  num of %s: %d", run, gr, sg, num_in_sg)); 
					run_result[run, gr, sg] = num_in_sg;
					print(sprintf("run %d, group: %s,  num of random in %s: %d", run, gr, sg, length(random_sub_ind))); 
					random_salted_result[run, gr, sg] = length(random_sub_ind);
				}
			} # sub group, best , worst
			#	print(sprintf("total sub in %s: %d", gr, total_sub_in_gr));
			run_result[run, gr, ] = run_result[run, gr, ] / total_sub_in_gr;
			random_salted_result[run, gr, ] = random_salted_result[run, gr, ] / 30;
		} # group
	} # run

	sink(sprintf('%s/random_friedman.txt', result_base_path));
	for (sub_group in c("best", "worst"))
	{
		print(sprintf("Stat Test in SubGroup: %s", sub_group));
		df_com = matrix(run_result[, "com", sub_group], ncol = 1);
		df_pen = matrix(run_result[, "pen", sub_group], ncol = 1);
		df_web = matrix(run_result[, "web", sub_group], ncol = 1);

		ratio = 1;
		png(paste(result_base_path, sprintf('runner_hist_%s.png', sub_group), sep = "/"),
			width = 1360 * ratio, height = 768 * ratio);
#		x11();
#		par(ask = T);
		ratio = 1;
		breaks = seq(0, 1, by = 0.2);
		nclass = length(breaks);
		p1 = hist(df_com, plot = F, nclass = nclass, breaks = breaks);
		p2 = hist(df_pen, plot = F, nclass = nclass, breaks = breaks);
		p3 = hist(df_web, plot = F, nclass = nclass, breaks = breaks);
		plot(p1, col = rgb(0,0,1,1/4), xlim = c(0,1));  # first histogram
		plot(p2, col = rgb(1,0,0,1/4), xlim = c(0,1), add = T);  # second
		plot(p3, col = rgb(0,1,0,1/4), xlim = c(0,1), add = T);  # and the last ...

		dev.off();

		dfr_com = matrix(random_salted_result[, "com", sub_group], ncol = 1);
		dfr_pen = matrix(random_salted_result[, "pen", sub_group], ncol = 1);
		dfr_web = matrix(random_salted_result[, "web", sub_group], ncol = 1);

		png(paste(result_base_path, sprintf('random_runner_hist_%s.png', sub_group), sep = "/"),
			width = 1360 * ratio, height = 768 * ratio);
		
#		x11();
#		par(ask = T);
		p1 = hist(dfr_com, plot = F, nclass = nclass, breaks = breaks);
		p2 = hist(dfr_pen, plot = F, nclass = nclass, breaks = breaks);
		p3 = hist(dfr_web, plot = F, nclass = nclass, breaks = breaks);
		plot(p1, col = rgb(0,0,1,1/4), xlim = c(0,1));  # first histogram
		plot(p2, col = rgb(1,0,0,1/4), xlim = c(0,1), add = T);  # second
		plot(p3, col = rgb(0,1,0,1/4), xlim = c(0,1), add = T);  # second

		dev.off();

		df_pre = cbind(df_com, df_pen, df_web);
		dfr_pre = cbind(dfr_com, dfr_pen, dfr_web);
		sig_tr = 0.05;
		df_frame = data.frame(Value = c(t(df_pre)), Group = factor(rep(c('com', 'pen', 'web'), num_of_run)), 
							  Runs = factor(rep(seq(num_of_run), each = 3)));
		dfr_frame = data.frame(Value = c(t(dfr_pre)), Group = factor(rep(c('com', 'pen', 'web'), num_of_run)), 
							   Runs = factor(rep(seq(num_of_run), each = 3)));
		print("Friedman Test, PERSON per Cluster");
		friedman_result = friedman.test.with.post.hoc(Value ~ Group | Runs, data = df_frame, signif.P = sig_tr); 
		if ("PostHoc.Test" %in% names(friedman_result))
		{
			print(friedman_result);
		}
		print("Friedman Test, RANDOM per Cluster");
		friedman_result = friedman.test.with.post.hoc(Value ~ Group | Runs, data = dfr_frame, signif.P = sig_tr); 
		if ("PostHoc.Test" %in% names(friedman_result))
		{
			print(friedman_result);
		}
	}
	sink();

	return(list("df_frame" = df_frame, "dfr_frame" = dfr_frame));
}
