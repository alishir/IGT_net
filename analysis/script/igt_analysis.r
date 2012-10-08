library('foreign');
library('plotrix');
library('pvclust');
library('calibrate');
load_exec_data <- function(igt_result_dir)
{
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

calc_20block_score_sub <- function(sub_trial)
{
        # sub_trial is subject triala, 100 trial of igt game
        block_score = matrix(data = NA, nrow = 1, ncol = 5);
        for (i in c(1,2,3,4,5))
        {
                block_trial = sub_trial[(i * 20 - 19):(i * 20)];
                block_score[1, i] = adv_disadv_score(block_trial);
        }
        return(block_score);
}


calc_20block_score <- function(sub_data, groups)
{
	num_sub = dim(sub_data)[1];
	ret = list();

  groups_row_names = rownames(groups);
  
	com_group = groups_row_names[which(groups == 'com')];
	pen_group = groups_row_names[which(groups == 'pen')];
	web_group = groups_row_names[which(groups == 'web')];


  
	com_result = matrix(data = NA, nrow = length(com_group), ncol = 5);
	web_result = matrix(data = NA, nrow = length(web_group), ncol = 5);
	pen_result = matrix(data = NA, nrow = length(pen_group), ncol = 5);

	rownames(com_result) <- as.vector(com_group);
	rownames(web_result) <- as.vector(web_group);
	rownames(pen_result) <- as.vector(pen_group);

	for (s in com_group)
	{
		if (s %in% rownames(sub_data))
		{
			com_result[as.character(s), ] = calc_20block_score_sub(sub_data[as.character(s), ]);
		}
	}
	for (s in web_group)
	{
		if (s %in% rownames(sub_data))
		{
			web_result[as.character(s), ] = calc_20block_score_sub(sub_data[as.character(s), ]);
		}
	}
	for (s in pen_group)
	{
		if (s %in% rownames(sub_data))
		{
			pen_result[as.character(s), ] = calc_20block_score_sub(sub_data[as.character(s), ]);
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

get_best_worst_20block_score <- function(score)
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


	if (pen_1st_clus_mean[5] + pen_1st_clus_mean[4] > pen_2st_clus_mean[5] + pen_2st_clus_mean[4])
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

	if (com_1st_clus_mean[5] + com_1st_clus_mean[4] > com_2st_clus_mean[5] + com_2st_clus_mean[4])
	{
		com_best_clus = score$com[com_clus[[1]], ];
		com_wors_clus = score$com[com_clus[[2]], ];
	}
	else
	{
		com_best_clus = score$com[com_clus[[2]], ];
		com_wors_clus = score$com[com_clus[[1]], ];
	}

#	web_1st_clus_mean = colMeans(score$web[web_clus[[1]], ]);
#	web_2st_clus_mean = colMeans(score$web[web_clus[[2]], ]);
#	web_1st_sd = apply(score$web[web_clus[[1]], ], 2, sd);
#	web_2st_sd = apply(score$web[web_clus[[2]], ], 2, sd);

#	if (web_1st_clus_mean[5] > web_2st_clus_mean[5])
#	{
#		web_best_clus_sd = web_1st_sd;
#		web_wors_clus_sd = web_2st_sd;
#	}
#	else
#	{
#		web_best_clus_sd = web_2st_sd;
#		web_wors_clus_sd = web_1st_sd;
#	}

  return(list("com_best" = com_best_clus, "com_worst" = com_wors_clus, "pen_best" = pen_best_clus, "pen_worst" = pen_wors_clus));

}

adv_disadv_score <- function(trial)
{
        adv_deck = c(99 , 100);         # c, d
        disadv_deck = c(97, 98);        # a, b
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
  return(sum(sqrt((x - y) ^ 2 * t(c(0,0,0,1,1)))));
  #return(sum(sqrt((x - y) ^ 2 * t(c(1,1,1,1,1)))));
}


within_group_cluster <- function(score, group_name)
{
	# cluster subjects with in group and ask user to select groups
	# score: a list of group scores e.g. score$com is the score of subjects in computer group
	# group_name: string name of group in score variable e.g. "com"
	library('proxy');
	dis = dist(score[[group_name]], block_dist);
	hc = hclust(dis, method = "ward");
	plot(hc);
	clusters = cutree(hc, k = 2);
	ind = rownames(as.matrix(clusters));
	clu_1st = ind[clusters %in% 1];
	clu_2nd = ind[clusters %in% 2];
	detach('package:proxy');
	return(list("first" = clu_1st, "second" = clu_2nd));
}

igt_doit <- function(igt_path, sub_path)
{
	dat = load_exec_data(igt_path);
	group_fname = "sub_exp_map";
	sub_exp_map = read.csv(paste(igt_path, group_fname, sep="/"));
	
	score = calc_20block_score(dat, sub_exp_map);
  
  
	best_worst = get_best_worst_20block_score(score);
  
  
  ### PLOT Section ###
	# 1360x768
  ratio = 2;
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
	sub_col[sub_row %in% rownames(best_worst$com_worst)] = 'blue';
	sub_col[sub_row %in% rownames(best_worst$pen_worst)] = 'green';
	
	com_best_subs = sub_mat[rownames(best_worst$com_best), ];
	pen_best_subs = sub_mat[rownames(best_worst$pen_best), ];
	com_worst_subs = sub_mat[rownames(best_worst$com_worst), ];
	pen_worst_subs = sub_mat[rownames(best_worst$pen_worst), ];
	
	cur_sub = rbind(com_best_subs, pen_best_subs, com_worst_subs, pen_worst_subs);
	sub_col = matrix(nrow = 1, ncol = nrow(cur_sub));
	sub_row = rownames(cur_sub);
	sub_col[sub_row %in% rownames(best_worst$com_best)] = 'red';
	sub_col[sub_row %in% rownames(best_worst$pen_best)] = 'orange';
	sub_col[sub_row %in% rownames(best_worst$com_worst)] = 'blue';
	sub_col[sub_row %in% rownames(best_worst$pen_worst)] = 'green';
	sub_mat = cur_sub;
	print(cur_sub);
	
	#  x11();
	plot(sub_mat[, "bas"], sub_mat[, "bis"], col = sub_col, pch = 21, bg = sub_col, main = "Subjects BAS/BIS Dist.");
	legend('bottomright', c('COM Best', 'PEN Best', 'COM Worst', 'PEN Worst'), fill = c('red', 'orange', 'blue', 'green'), cex = 2);
	textxy(sub_mat[, "bas"], sub_mat[, "bis"], labs = sub_row, cx = 2);
	#	readline("Press Enter for Next Plots: ");
		
	lm_com_best = lm(com_best_subs[, "bis"] ~ com_best_subs[, "bas"]);
	lm_pen_best = lm(pen_best_subs[, "bis"] ~ pen_best_subs[, "bas"]);
	lm_com_worst = lm(com_worst_subs[, "bis"] ~ com_worst_subs[, "bas"]);
	lm_pen_worst = lm(pen_worst_subs[, "bis"] ~ pen_worst_subs[, "bas"]);
	abline(lm_com_best, col = 'red');
	abline(lm_pen_best, col = 'orange');
	abline(lm_com_worst, col = 'blue');
	abline(lm_pen_worst, col = 'green');
	
  
  
	plot(sub_mat[, "bas_rr"], sub_mat[, "bis"], col = sub_col, pch = 21, bg = sub_col, main = "Subjects BAS RR/BIS Dist.");
	legend('bottomright', c('COM Best', 'PEN Best', 'COM Worst', 'PEN Worst'), fill = c('red', 'orange', 'blue', 'green'), cex = 2);
	textxy(sub_mat[, "bas_rr"], sub_mat[, "bis"], labs = sub_row, cx = 2);
	lm_com_best = lm(com_best_subs[, "bis"] ~ com_best_subs[, "bas_rr"]);
	lm_pen_best = lm(pen_best_subs[, "bis"] ~ pen_best_subs[, "bas_rr"]);
	lm_com_worst = lm(com_worst_subs[, "bis"] ~ com_worst_subs[, "bas_rr"]);
	lm_pen_worst = lm(pen_worst_subs[, "bis"] ~ pen_worst_subs[, "bas_rr"]);
	abline(lm_com_best, col = 'red');
	abline(lm_pen_best, col = 'orange');
	abline(lm_com_worst, col = 'blue');
	abline(lm_pen_worst, col = 'green');
	
	
	plot(sub_mat[, "bas_fs"], sub_mat[, "bis"], col = sub_col, pch = 21, bg = sub_col, main = "Subjects BAS FS/BIS Dist.");
	legend('bottomright', c('COM Best', 'PEN Best', 'COM Worst', 'PEN Worst'), fill = c('red', 'orange', 'blue', 'green'), cex = 2);
	textxy(sub_mat[, "bas_fs"], sub_mat[, "bis"], labs = sub_row, cx = 2);
	lm_com_best = lm(com_best_subs[, "bis"] ~ com_best_subs[, "bas_fs"]);
	lm_pen_best = lm(pen_best_subs[, "bis"] ~ pen_best_subs[, "bas_fs"]);
	lm_com_worst = lm(com_worst_subs[, "bis"] ~ com_worst_subs[, "bas_fs"]);
	lm_pen_worst = lm(pen_worst_subs[, "bis"] ~ pen_worst_subs[, "bas_fs"]);
	abline(lm_com_best, col = 'red');
	abline(lm_pen_best, col = 'orange');
	abline(lm_com_worst, col = 'blue');
	abline(lm_pen_worst, col = 'green');
	
	plot(sub_mat[, "bas_d"], sub_mat[, "bis"], col = sub_col, pch = 21, bg = sub_col, main = "Subjects BAS D/BIS Dist.");
	legend('bottomright', c('COM Best', 'PEN Best', 'COM Worst', 'PEN Worst'), fill = c('red', 'orange', 'blue', 'green'), cex = 2);
	textxy(sub_mat[, "bas_d"], sub_mat[, "bis"], labs = sub_row, cx = 1);
	lm_com_best = lm(com_best_subs[, "bis"] ~ com_best_subs[, "bas_d"]);
	lm_pen_best = lm(pen_best_subs[, "bis"] ~ pen_best_subs[, "bas_d"]);
	lm_com_worst = lm(com_worst_subs[, "bis"] ~ com_worst_subs[, "bas_d"]);
	lm_pen_worst = lm(pen_worst_subs[, "bis"] ~ pen_worst_subs[, "bas_d"]);
	abline(lm_com_best, col = 'red');
	#abline(lm_pen_best, col = 'orange');
	abline(lm_com_worst, col = 'blue');
	abline(lm_pen_worst, col = 'green');
	
  
	#	x11();
	plot(jitter(sub_mat[, "pnas_n"]), sub_mat[, "pnas_p"], col = sub_col, pch = 21, bg = sub_col, main = "Subjects PNAS Dist.");
	textxy(jitter(sub_mat[, "pnas_n"]), sub_mat[, "pnas_p"], labs = sub_row, cx = 1);
	lm_com_best = lm(com_best_subs[, "pnas_p"] ~ com_best_subs[, "pnas_n"]);
	lm_pen_best = lm(pen_best_subs[, "pnas_p"] ~ pen_best_subs[, "pnas_n"]);
	lm_com_worst = lm(com_worst_subs[, "pnas_p"] ~ com_worst_subs[, "pnas_n"]);
	lm_pen_worst = lm(pen_worst_subs[, "pnas_p"] ~ pen_worst_subs[, "pnas_n"]);
	abline(lm_com_best, col = 'red');
	abline(lm_pen_best, col = 'orange');
	abline(lm_com_worst, col = 'blue');
	abline(lm_pen_worst, col = 'green');
	#legend(-1, -1, c('COM Best', 'PEN Best', 'COM Worst', 'PEN Worst'), fill = c('red', 'orange', 'blue', 'green'), cex = 2);
	#  x11();
	#  scatterplot3d(sub_mat[, "bas"], sub_mat[, "bis"], sub_mat[, "pnas_n"]);
	
    
	boxplot(best_worst$com_best, ylim = c(-20, 20), at = 1:5 - 0.1, col = 'orange', boxwex = 0.15, main = "Best Clusters");
	boxplot(best_worst$pen_best, at = 1:5 + 0.1, col = 'yellow', boxwex = 0.15, add = TRUE);
	legend('bottomright', c(sprintf("COM #%d:{%s}", nrow(best_worst$com_best), 
                                  paste(as.character(rownames(best_worst$com_best)), collapse = ', ')), 
                          sprintf("PEN #%d:{%s}", nrow(best_worst$pen_best), 
                                  paste(as.character(rownames(best_worst$pen_best)), collapse = ', '))), 
         fill = c('orange', 'yellow'), cex = 2);
  num_com_best_female = length(which(com_best_subs[,"sex"] == 1));
	num_com_best_male = length(which(com_best_subs[,"sex"] == 0));
  com_best_sex_dis = rbind(num_com_best_male, num_com_best_female);
	num_pen_best_female = length(which(pen_best_subs[,"sex"] == 1));
	num_pen_best_male = length(which(pen_best_subs[,"sex"] == 0));
	pen_best_sex_dis = rbind(num_pen_best_male, num_pen_best_female);
  barplot(cbind(com_best_sex_dis, pen_best_sex_dis), names.arg = c("COM", "PEN"), col = c('purple', 'pink'));  # pink for female :D
	
	boxplot(best_worst$com_worst, ylim = c(-20, 20), at = 1:5 - 0.1, col = 'orange', boxwex = 0.15, main = "Worst Clusters");
	boxplot(best_worst$pen_worst, at = 1:5 + 0.1, col = 'yellow', boxwex = 0.15, add = TRUE);
	legend('bottomright', c(sprintf("COM #%d:{%s}", nrow(best_worst$com_worst), 
	                                paste(as.character(rownames(best_worst$com_worst)), collapse = ', ')), 
                          sprintf("PEN #%d:{%s}", nrow(best_worst$pen_worst), 
                                  paste(as.character(rownames(best_worst$pen_worst)), collapse = ', '))), 
         fill = c('orange', 'yellow'), cex = 2);
	num_com_worst_female = length(which(com_worst_subs[,"sex"] == 1));
	num_com_worst_male = length(which(com_worst_subs[,"sex"] == 0));
	com_worst_sex_dis = rbind(num_com_worst_male, num_com_worst_female);
	num_pen_worst_female = length(which(pen_worst_subs[,"sex"] == 1));
	num_pen_worst_male = length(which(pen_worst_subs[,"sex"] == 0));
	pen_worst_sex_dis = rbind(num_pen_worst_male, num_pen_worst_female);
	barplot(cbind(com_worst_sex_dis, pen_worst_sex_dis), names.arg = c("COM", "PEN"), col = c('purple', 'pink'));  # pink for female :D
  
	dev.off();
	
  
#  readline("Press Enter for Next Plots: ");

  #plot_sub_clusters(sub_mat);
  
	return(list("total_score" = score, "best_worst_score" = best_worst));
}

plot_sub_clusters(sub_mat)
{
  sub_dis <- dist(sub_mat);
  hd <- hclust(sub_dis, method = "ward");
  clus_plot = plot(hd);
  # load previous group assignment
#  group_fname = "sub_exp_map";
#  sub_exp_map = read.csv(paste(sub_dir, group_fname, sep="/"));
  
  # colorify dendogram nodes
  local({
    exp_col_map = list("com" = "red", "pen" = "green", "web" = "blue");
    colLab <<- function(n) {
      if(is.leaf(n)) {
        a <- attributes(n);
        sub_name = a$label;
        if (sub_name %in% rownames(sub_exp_map))
        {
          color = exp_col_map[[sub_exp_map[sub_name, ]]];
          attr(n, "nodePar") <- c(a$nodePar, list(lab.col = color));
        }
      }
      return(n);
    }
  })
  dhc = as.dendrogram(sub_cluster);
  color_dhc <- dendrapply(dhc, colLab)
  #	op = par(mfrow=2:1);
  plot(color_dhc);
}