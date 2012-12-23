library('foreign');
library('plotrix');
library('pvclust');
library('calibrate');
library('dtw');
load_exec_data <- function(igt_result_dir)
{
	if (file.exists(paste(igt_result_dir, "all.dat", sep = "/")))
	{
		da = read.octave(paste(igt_result_dir, "all.dat", sep = "/"));
		sub_deck_selection = da$b;
		rownames(sub_deck_selection) = seq(1,nrow(sub_deck_selection));
		return (sub_deck_selection);
	}
	sub_list = list.files(igt_result_dir, pattern = "*.dat$");
	sub_size = length(sub_list);
	sub_deck_selection = matrix(data = NA, nrow = sub_size, ncol = 100);
	sub_ids = lapply(sub_list, function(e) {substr(e, 1, nchar(e) - 4);});
	rownames(sub_deck_selection) <- as.vector(sub_ids);

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

calc_20block_score_sub <- function(sub_trial, num_of_block)
{
        # sub_trial is subject triala, 100 trial of igt game
        block_score = matrix(data = NA, nrow = 1, ncol = num_of_block);
		bs = floor(100 / num_of_block);
        for (i in seq(num_of_block))
        {
                block_trial = sub_trial[((i - 1) * bs + 1):(i * bs)];
                block_score[1, i] = adv_disadv_score(block_trial);
        }
        return(block_score);
}


calc_block_score <- function(sub_data, groups, num_of_block)
{
	num_sub = dim(sub_data)[1];
	ret = list();

	groups_row_names = rownames(groups);

	com_group = groups_row_names[which(groups == 'com')];
	pen_group = groups_row_names[which(groups == 'pen')];
	web_group = groups_row_names[which(groups == 'web')];

	com_result = matrix(data = NA, nrow = length(com_group), ncol = num_of_block);
	web_result = matrix(data = NA, nrow = length(web_group), ncol = num_of_block);
	pen_result = matrix(data = NA, nrow = length(pen_group), ncol = num_of_block);

	rownames(com_result) <- as.vector(com_group);
	rownames(web_result) <- as.vector(web_group);
	rownames(pen_result) <- as.vector(pen_group);

	for (s in com_group)
	{
		if (s %in% rownames(sub_data))
		{
			com_result[as.character(s), ] = calc_20block_score_sub(sub_data[as.character(s), ], num_of_block);
		}
	}
	for (s in web_group)
	{
		if (s %in% rownames(sub_data))
		{
			web_result[as.character(s), ] = calc_20block_score_sub(sub_data[as.character(s), ], num_of_block);
		}
	}
	for (s in pen_group)
	{
		if (s %in% rownames(sub_data))
		{
			pen_result[as.character(s), ] = calc_20block_score_sub(sub_data[as.character(s), ], num_of_block);
		}
	}

	# remove all NA rows
	com_result = com_result[rowSums(is.na(com_result)) == 0, ];
	web_result = web_result[rowSums(is.na(web_result)) == 0, ];
	pen_result = pen_result[rowSums(is.na(pen_result)) == 0, ];

	cat(sprintf("size of COM group: %d\n",nrow(com_result)));
	cat(sprintf("size of PEN group: %d\n",nrow(pen_result)));
	cat(sprintf("size of WEB group: %d\n",nrow(web_result)));

	return(list("com" = com_result, "pen" = pen_result, "web" = web_result));
}

plot_20block_score <- function(score)
{
	x <- seq(1, 5);
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
get_best_worst_block_score <- function(score, nb, clus_cols)
{
	x <- seq(1, 5);
	plot_col = c("red", "green", "blue", "white");
	pen_mean_score = colMeans(score$pen);
	web_mean_score = colMeans(score$web);
	com_mean_score = colMeans(score$com);

	pen_sd = apply(score$pen, 2, sd);
	web_sd = apply(score$web, 2, sd);
	com_sd = apply(score$com, 2, sd);

	# extract two cluster in each group
	pen_clus = within_group_cluster(score, "pen", clus_cols);
	com_clus = within_group_cluster(score, "com", clus_cols);
	web_clus = within_group_cluster(score, "web", clus_cols);


	pen_1st_clus_mean = colMeans(score$pen[pen_clus[[1]], ]);
	pen_2st_clus_mean = colMeans(score$pen[pen_clus[[2]], ]);
	pen_1st_sd = apply(score$pen[pen_clus[[1]], ], 2, sd);
	pen_2st_sd = apply(score$pen[pen_clus[[2]], ], 2, sd);


	if (pen_1st_clus_mean[nb] + pen_1st_clus_mean[nb - 1] > pen_2st_clus_mean[nb] + pen_2st_clus_mean[nb - 1])
	{
		pen_best_clus = score$pen[pen_clus[[1]], ];
		pen_wors_clus = score$pen[pen_clus[[2]], ];
	}
	else
	{
		pen_best_clus = score$pen[pen_clus[[2]], ];
		pen_wors_clus = score$pen[pen_clus[[1]], ];
	}



	com_1st_clus_mean = colMeans(score$com[com_clus[[1]], ]);
	com_2st_clus_mean = colMeans(score$com[com_clus[[2]], ]);
	com_1st_sd = apply(score$com[com_clus[[1]], ], 2, sd);
	com_2st_sd = apply(score$com[com_clus[[2]], ], 2, sd);

	if (com_1st_clus_mean[nb] + com_1st_clus_mean[nb - 1] > com_2st_clus_mean[nb] + com_2st_clus_mean[nb - 1])
	{
		com_best_clus = score$com[com_clus[[1]], ];
		com_wors_clus = score$com[com_clus[[2]], ];
	}
	else
	{
		com_best_clus = score$com[com_clus[[2]], ];
		com_wors_clus = score$com[com_clus[[1]], ];
	}

	web_1st_clus_mean = colMeans(score$web[web_clus[[1]], ]);
	web_1st_sd = apply(score$web[web_clus[[1]], ], 2, sd);
	web_2st_clus_mean = colMeans(score$web[web_clus[[2]], ]);
	web_2st_sd = apply(score$web[web_clus[[2]], ], 2, sd);

	if (web_1st_clus_mean[nb] + web_1st_clus_mean[nb - 1] > web_2st_clus_mean[nb] + web_2st_clus_mean[nb - 1])
	{
		web_best_clus = score$web[web_clus[[1]], ];
		web_wors_clus = score$web[web_clus[[2]], ];
	}
	else
	{
		web_best_clus = score$web[web_clus[[2]], ];
		web_wors_clus = score$web[web_clus[[1]], ];
	}

  return(list("com_best" = com_best_clus, "com_worst" = com_wors_clus, 
			  "pen_best" = pen_best_clus, "pen_worst" = pen_wors_clus, 
			  "web_best" = web_best_clus, "web_worst" = web_wors_clus));
}

adv_disadv_score <- function(trial)
{
        adv_deck = c(99 , 100, 3, 4);         # c, d
        disadv_deck = c(97, 98, 1, 2);        # a, b
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


within_group_cluster <- function(score, group_name, col_range)
{
	# cluster subjects with in group and ask user to select groups
	# score: a list of group scores e.g. score$com is the score of subjects in computer group
	# group_name: string name of group in score variable e.g. "com"
#	library('proxy');
#	library('dtw');
	#dis = dist(score[[group_name]], block_dist);
	if (exists("col_range"))
	{
		dis = dist(score[[group_name]][, col_range],  method = "DTW");
	}
	else
	{
		dis = dist(score[[group_name]],  method = "DTW");
	}
	hc = hclust(dis, method = "ward");
	plot(hc);
	clusters = cutree(hc, k = 2);
	ind = rownames(as.matrix(clusters));
	clu_1st = ind[clusters %in% 1];
	clu_2nd = ind[clusters %in% 2];
#	detach('package:dtw');
#	detach('package:proxy');
	print("In within_cluster:");
	print(group_name);
	print(clu_1st);
	print(clu_2nd);
	return(list("first" = clu_1st, "second" = clu_2nd));
}

calc_good_bad <- function(trial_choise)
{
  len = length(trial_choise);
  good_bad = matrix(nrow = 1, ncol = len + 1);
  good_bad[1, 1] = 0;
  for (i in seq(1,len))
  {
    ch = trial_choise[i] - 96;  # deck A is 97, convert to matrix index
    if (ch < 3)
	{
      good_bad[1, i + 1] = good_bad[1, i]  - 1;
	}
    else
	{
      good_bad[1, i + 1] = good_bad[1, i]  + 1;
	}
  }
  return(good_bad);
}

calc_acc_reward <- function(trial_choise)
{
  rew_pun = read.octave('pen.dat');
  len = length(trial_choise);
  acc_rew = matrix(nrow = 1, ncol = len + 1);
  ind = matrix(c(1,1,1,1), nrow = 4, ncol = 1);
  acc_rew[1,1] = 2000;  # initial deposit!
  for (i in seq(1,len))
  {
    ch = trial_choise[i] - 96;  # deck A is 97, convert to matrix index
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
	png('/tmp/res.png', width = 1360 * ratio, height = 768 * ratio);
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
	com_best_subs = matrix(rownames(best_worst$com_best));
	pen_best_subs = matrix(rownames(best_worst$pen_best));
	web_best_subs = matrix(rownames(best_worst$web_best));
	com_worst_subs = matrix(rownames(best_worst$com_worst));
	pen_worst_subs = matrix(rownames(best_worst$pen_worst));
	web_worst_subs = matrix(rownames(best_worst$web_worst));
	### PLOT Time Series ####
	ratio = 2;
	png('/tmp/acc_reward.png', width = 1360 * ratio, height = 768 * ratio);
	attach(mtcars);
	# par(mfrow = c(2, 4), oma = c(2, 2, 2, 2));	 
	layout(matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow = FALSE));

	# COM Best #
	ts_len = 101;   # trial length
	com_best_ts = matrix(nrow = nrow(com_best_subs), ncol = ts_len);
	rownames(com_best_ts) = c(com_best_subs);
	for (sub_name in com_best_subs)
	{
		print(sub_name);
		com_best_ts[sub_name, ] = calc_acc_reward(igt_data[sub_name, ]);
	}
	matplot(t(com_best_ts), type = 'o', pch=1:nrow(com_best_ts), 
			col=1:nrow(com_best_ts), bg=1:nrow(com_best_ts), main = "COM BEST", lwd = 3, cex.axis = 3, cex.main = 3);
	legend('topleft', rownames(com_best_ts), fill = 1:nrow(com_best_ts), cex = 3);

	com_worst_ts = matrix(nrow = nrow(com_worst_subs), ncol = ts_len);
	rownames(com_worst_ts) = c(com_worst_subs);
	for (sub_name in com_worst_subs)
	{
		print(sub_name);
		com_worst_ts[sub_name, ] = calc_acc_reward(igt_data[sub_name, ]);
	}
	matplot(t(com_worst_ts), type = 'o', pch=1:nrow(com_worst_ts), 
			col=1:nrow(com_worst_ts), bg=1:nrow(com_worst_ts), main = "COM WORST", lwd = 3, cex.axis = 3, cex.main = 3);
	legend('bottomleft', rownames(com_worst_ts), fill = 1:nrow(com_worst_ts), cex = 3);


	pen_best_ts = matrix(nrow = nrow(pen_best_subs), ncol = ts_len);
	rownames(pen_best_ts) = c(pen_best_subs);
	for (sub_name in pen_best_subs)
	{
		print(sub_name);
		pen_best_ts[sub_name, ] = calc_acc_reward(igt_data[sub_name, ]);
	}
	matplot(t(pen_best_ts), type = 'o', pch=1:nrow(pen_best_ts), 
			col=1:nrow(pen_best_ts), bg=1:nrow(pen_best_ts), main = "PEN BEST", lwd = 3, cex.axis = 3, cex.main = 3);
	legend('topleft', rownames(pen_best_ts), fill = 1:nrow(pen_best_ts), cex = 3);

	pen_worst_ts = matrix(nrow = nrow(pen_worst_subs), ncol = ts_len);
	rownames(pen_worst_ts) = c(pen_worst_subs);
	for (sub_name in c(pen_worst_subs))
	{
		print(sub_name);
		pen_worst_ts[sub_name, ] = calc_acc_reward(igt_data[sub_name, ]);
	}
	matplot(t(pen_worst_ts), type = 'o', pch=1:nrow(pen_worst_ts), 
			col=1:nrow(pen_worst_ts), bg=1:nrow(pen_worst_ts), main = "PEN WORST", lwd = 3, cex.axis = 3, cex.main = 3);
	legend('topleft', rownames(pen_worst_ts), fill = 1:nrow(pen_worst_ts), cex = 3);

	web_best_ts = matrix(nrow = nrow(web_best_subs), ncol = ts_len);
	rownames(web_best_ts) = c(web_best_subs);
	for (sub_name in c(web_best_subs))
	{
		print(sub_name);
		web_best_ts[sub_name, ] = calc_acc_reward(igt_data[sub_name, ]);
	}
	matplot(t(web_best_ts), type = 'o', pch=1:nrow(web_best_ts), 
			col=1:nrow(web_best_ts), bg=1:nrow(web_best_ts), main = "WEB BEST", lwd = 3, cex.axis = 3, cex.main = 3);
	legend('topleft', rownames(web_best_ts), fill = 1:nrow(web_best_ts), cex = 3);

	web_worst_ts = matrix(nrow = nrow(web_worst_subs), ncol = ts_len);
	rownames(web_worst_ts) = c(web_worst_subs);
	for (sub_name in c(web_worst_subs))
	{
		print(sub_name);
		web_worst_ts[sub_name, ] = calc_acc_reward(igt_data[sub_name, ]);
	}
	matplot(t(web_worst_ts), type = 'o', pch=1:nrow(web_worst_ts), 
			col=1:nrow(web_worst_ts), bg=1:nrow(web_worst_ts), main = "WEB WORST", lwd = 3, cex.axis = 3, cex.main = 3);
	legend('topleft', rownames(web_worst_ts), fill = 1:nrow(web_worst_ts), cex = 3);
	dev.off();


	### PLOT Good-BadTime Series ####
	ratio = 2;
	png('/tmp/good_bad.png', width = 1360 * ratio, height = 768 * ratio);
	attach(mtcars);
	# par(mfrow = c(2, 4), oma = c(2, 2, 2, 2));	 
	layout(matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow = FALSE));

	# COM Best #
	ts_len = 101;   # trial length
	par(cex.axis = 3, cex.main = 3, cex.lab = 3, mar = c(6,6,6,6));
	com_best_gb_ts = matrix(nrow = nrow(com_best_subs), ncol = ts_len);
	rownames(com_best_gb_ts) = c(com_best_subs);
	for (sub_name in c(com_best_subs))
	{
		com_best_gb_ts[sub_name, ] = calc_good_bad(igt_data[sub_name, ]);
	}
	matplot(t(com_best_gb_ts), type = 'o', pch=1:nrow(com_best_gb_ts), 
			col=1:nrow(com_best_gb_ts), bg=1:nrow(com_best_gb_ts), main = "COM BEST", lwd = 3, cex.axis = 3, cex.main = 3);
	legend('topleft', rownames(com_best_gb_ts), pch=1:nrow(com_best_gb_ts), fill = 1:nrow(com_best_gb_ts), cex = 3);

	com_worst_gb_ts = matrix(nrow = nrow(com_worst_subs), ncol = ts_len);
	rownames(com_worst_gb_ts) = c(com_worst_subs);
	for (sub_name in c(com_worst_subs))
	{
		com_worst_gb_ts[sub_name, ] = calc_good_bad(igt_data[sub_name, ]);
	}
	matplot(t(com_worst_gb_ts), type = 'o', pch=1:nrow(com_worst_gb_ts), 
			col=1:nrow(com_worst_gb_ts), bg=1:nrow(com_worst_gb_ts), main = "COM WORST", lwd = 3, cex.axis = 3, cex.main = 3);
	legend('bottomleft', rownames(com_worst_gb_ts), pch=1:nrow(com_worst_gb_ts), fill = 1:nrow(com_worst_gb_ts), cex = 3);

	pen_best_gb_ts = matrix(nrow = nrow(pen_best_subs), ncol = ts_len);
	rownames(pen_best_gb_ts) = c(pen_best_subs);
	for (sub_name in c(pen_best_subs))
	{
		pen_best_gb_ts[sub_name, ] = calc_good_bad(igt_data[sub_name, ]);
	}
	matplot(t(pen_best_gb_ts), type = 'o', pch=1:nrow(pen_best_gb_ts), 
			col=1:nrow(pen_best_gb_ts), bg=1:nrow(pen_best_gb_ts), main = "PEN BEST", lwd = 3, cex.axis = 3, cex.main = 3);
	legend('topleft', rownames(pen_best_gb_ts), pch=1:nrow(pen_best_gb_ts), fill = 1:nrow(pen_best_gb_ts), cex = 3);

	pen_worst_gb_ts = matrix(nrow = nrow(pen_worst_subs), ncol = ts_len);
	rownames(pen_worst_gb_ts) = c(pen_worst_subs);
	for (sub_name in c(pen_worst_subs))
	{
		pen_worst_gb_ts[sub_name, ] = calc_good_bad(igt_data[sub_name, ]);
	}
	matplot(t(pen_worst_gb_ts), type = 'o', pch=1:nrow(pen_worst_gb_ts), 
			col=1:nrow(pen_worst_gb_ts), bg=1:nrow(pen_worst_gb_ts), main = "PEN WORST", lwd = 3, cex.axis = 3, cex.main = 3);
	legend('bottomleft', rownames(pen_worst_gb_ts), pch=1:nrow(pen_worst_gb_ts), fill = 1:nrow(pen_worst_gb_ts), cex = 3);

	web_best_gb_ts = matrix(nrow = nrow(web_best_subs), ncol = ts_len);
	rownames(web_best_gb_ts) = c(web_best_subs);
	for (sub_name in c(web_best_subs))
	{
		web_best_gb_ts[sub_name, ] = calc_good_bad(igt_data[sub_name, ]);
	}
	matplot(t(web_best_gb_ts), type = 'o', pch=1:nrow(web_best_gb_ts), 
			col=1:nrow(web_best_gb_ts), bg=1:nrow(web_best_gb_ts), main = "WEB BEST", lwd = 3, cex.axis = 3, cex.main = 3);
	legend('topleft', rownames(web_best_gb_ts), pch=1:nrow(web_best_gb_ts), fill = 1:nrow(web_best_gb_ts), cex = 3);

	web_worst_gb_ts = matrix(nrow = nrow(web_worst_subs), ncol = ts_len);
	rownames(web_worst_gb_ts) = c(web_worst_subs);
	for (sub_name in c(web_worst_subs))
	{
		web_worst_gb_ts[sub_name, ] = calc_good_bad(igt_data[sub_name, ]);
	}
	matplot(t(web_worst_gb_ts), type = 'o', pch=1:nrow(web_worst_gb_ts), 
			col=1:nrow(web_worst_gb_ts), bg=1:nrow(web_worst_gb_ts), main = "WEB WORST", lwd = 3, cex.axis = 3, cex.main = 3);
	legend('bottomleft', rownames(web_worst_gb_ts), pch=1:nrow(web_worst_gb_ts), fill = 1:nrow(web_worst_gb_ts), cex = 3);
	dev.off();
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
	png('/tmp/total_clusts.png', width = 1360 * ratio, height = 768 * ratio);
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


plot_horstmann_result <- function(horstmann_result, sub_exp_map)
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
	png('/tmp/horstmann_result.png', width = 1360 * ratio, height = 768 * ratio);
	attach(mtcars);
	# par(mfrow = c(2, 4), oma = c(2, 2, 2, 2));	 
	par(cex = 3, cex.axis = 3, cex.main = 3, cex.lab = 3, mar = c(5,5,5,5));
	layout(matrix(seq(1, 4), nrow = 2, ncol = 2, byrow = FALSE));

	# plot total
	matplot(t(total_data), type = 'o', pch = 1:3, col = 1:3, bg = 1:3, lwd = 3, main = "Total");
	legend('bottomright', rownames(total_data), pch = 1:3, fill = 1:3, cex = 3);

	# plot by exp
	groups = c('pen', 'com', 'web');
	for (g in groups)
	{
		sub_in_g = which(sub_exp_map == g);
		curr_data = horstmann_result[sub_in_g,,];
		cd_outcome_median = matrix(apply(curr_data[,,'outcome'], 2, median), nrow = 1);
		rownames(cd_outcome_median) = c('outcome');
		cd_gain_median = matrix(apply(curr_data[,,'gain'], 2, median), nrow = 1);
		rownames(cd_gain_median) = c('gain');
		cd_loss_median = matrix(apply(curr_data[,,'loss'], 2, median), nrow = 1);
		rownames(cd_loss_median) = c('loss');
		cd_bind = rbind(cd_outcome_median, cd_gain_median, cd_loss_median);
		matplot(t(cd_bind), type = 'o', pch = 1:3, col = 1:3, bg = 1:3, lwd = 3, main = g);
		legend('bottomright', rownames(cd_bind), pch = 1:3, fill = 1:3, cex = 3);
	}
	dev.off();
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


igt_doit <- function(igt_path, sub_path = '')
{
	dat = load_exec_data(igt_path);
	group_fname = "sub_exp_map";
	sub_exp_map = read.csv(paste(igt_path, group_fname, sep="/"));

	num_of_block = 10;
	score = calc_block_score(dat, sub_exp_map, num_of_block);


	best_worst = get_best_worst_block_score(score, num_of_block, seq(6, num_of_block));

	# there is some problem with "exists" function
	if (nchar(sub_path) > 0)
	{
		plot_bas_bis(sub_path, best_worst, num_of_block);
		corr_learning_personality(sub_path, best_worst);
	}

#	plot_ts(dat, best_worst);

	total_clustering(dat, score, num_of_clust = 6, seq(3,10));

	return(list("total_score" = score, "best_worst_score" = best_worst));
}
