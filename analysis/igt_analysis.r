library('foreign');
library('plotrix');
library('pvclust');
load_exec_data <- function(igt_result_dir)
{
        sub_list = list.files(igt_result_dir, pattern = "*.dat");
        sub_size = length(sub_list);
        sub_deck_selection = matrix(data = NA, nrow = sub_size, ncol = 100);
        sub_ids = lapply(sub_list, function(e) {substr(e, 1, nchar(e) - 4);});
        rownames(sub_deck_selection) <- as.vector(sub_ids);

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


calc_20block_score <- function(sub_data)
{
	num_sub = dim(sub_data)[1];
	ret = list();

	com_group = c("5", "13", "2", "9", "3", "10", "11");
	web_group = c("0");
	pen_group = c("12", "8", "6", "4", "7", "14");

	com_result = matrix(data = NA, nrow = length(com_group), ncol = 5);
	web_result = matrix(data = NA, nrow = length(web_group), ncol = 5);
	pen_result = matrix(data = NA, nrow = length(pen_group), ncol = 5);

	rownames(com_result) <- as.vector(com_group);
	rownames(web_result) <- as.vector(web_group);
	rownames(pen_result) <- as.vector(pen_group);

	for (s in com_group)
	{
		com_result[s, ] = calc_20block_score_sub(sub_data[s, ]);
	}
	for (s in web_group)
	{
		web_result[s, ] = calc_20block_score_sub(sub_data[s, ]);
	}
	for (s in pen_group)
	{
		pen_result[s, ] = calc_20block_score_sub(sub_data[s, ]);
	}

	return(list("com" = com_result, "pen" = pen_result, "web" = web_result));
}

plot_20block_score <- function(score)
{
        x <- seq(1, 5);
        plot_col = c("red", "green", "blue");
        pen_mean_score = colMeans(score$pen);
#        web_mean_score = colMeans(score$web);
        com_mean_score = colMeans(score$com);

		pen_sd = apply(score$pen, 2, sd);
#		web_sd = apply(score$web, 2, sd);
		com_sd = apply(score$com, 2, sd);

		# extract two cluster in each group
		pen_clus = within_group_cluster(score, "pen");
		com_clus = within_group_cluster(score, "com");

		pen_1st_clus_mean = colMeans(score$pen[pen_clus[[1]], ]);
		pen_2st_clus_mean = colMeans(score$pen[pen_clus[[2]], ]);

		com_1st_clus_mean = colMeans(score$com[com_clus[[1]], ]);
		com_2st_clus_mean = colMeans(score$com[com_clus[[2]], ]);

        plot(x - 0.01, pen_mean_score, ylim = range(-20, 20), type = "o", col = plot_col[1], xlab = "20 block", ylab = ("adv - dis)"));
		plotCI(x - 0.01, pen_mean_score, pen_sd, add = TRUE, scol = plot_col[1]);
		lines(x, pen_1st_clus_mean, lty = 5, col = plot_col[1]);
		lines(x, pen_2st_clus_mean, lty = 3, col = plot_col[1]);
#        lines(x, web_mean_score, type = "o", col = plot_col[2]);
#		plotCI(x, web_mean_score, web_sd, add = TRUE, scol = plot_col[2]);
        lines(x + 0.01, com_mean_score, type = "o", col = plot_col[3]);
		plotCI(x + 0.01, com_mean_score, com_sd, add = TRUE, scol = plot_col[3]);
		lines(x, com_1st_clus_mean, lty = 5, col = plot_col[3]);
		lines(x, com_2st_clus_mean, lty = 3, col = plot_col[3]);
#        legend("bottomright",  c("pen", "web", "com"), cex=0.8, col = plot_col, pch=21, lty=1);
        legend("bottomright",  c("pen", "com"), cex=0.8, col = c(plot_col[1], plot_col[3]), pch=21, lty=1);
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
	return(sum(sqrt((x - y) ^ 2 * t(seq(1,5)))));
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

runme <- function(igt_path)
{
	dat = load_exec_data(igt_path);
	score = calc_20block_score(dat);
	plot_20block_score(score);
	return(score);
}
