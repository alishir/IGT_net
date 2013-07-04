source('./igt_analysis.r');
BASE_PATH <<- '/tmp/';

js_subject_clusters <- function(alpha = 0.3)
{
	data_file = './js_sub_clust.dat';
	if (! file.exists(data_file))
	{
		rm(list = ls());
		igt_path = '~/doc/ms/thesis/all_in_one/data/payam';
		dat = load_exec_data(igt_path);
		group_fname = "sub_exp_map";
		sub_exp_map = read.csv(paste(igt_path, group_fname, sep = "/"));

		random_subjects = create_random_subjects(30);

		# outcome
		random_score_o = calc_block_score(sub_data = random_subjects,
										  metric = 'outcome');

		score_o = calc_block_score(sub_data = dat, group_map = sub_exp_map, 
								   score_type = 'raw', metric = 'outcome');
		best_worst_o = get_best_worst_block_score(score_o, rand_score = random_score_o);

		# loss
		random_score_l = calc_block_score(sub_data = random_subjects,
										  metric = 'loss');

		score_l = calc_block_score(sub_data = dat, group_map = sub_exp_map, 
								   score_type = 'raw', metric = 'loss');
		best_worst_l = get_best_worst_block_score(score_l, rand_score = random_score_l);

		# gain
		random_score_g = calc_block_score(sub_data = random_subjects,
										  metric = 'gain');

		score_g = calc_block_score(sub_data = dat, group_map = sub_exp_map, 
								   score_type = 'raw', metric = 'gain');
		best_worst_g = get_best_worst_block_score(score_g, rand_score = random_score_g);
		save(file = data_file, list = ls());
	}
	else 
	{
		load(data_file);
	}

	best_worst_all = list('gain' = best_worst_g,
						  'outcome' = best_worst_o,
						  'loss' = best_worst_l);
	# fit line to last 60 trial to check if subjects learned the task
	# or not
	learn_or_not = list();
	for (metric in names(best_worst_all))
	{
		best_worst = best_worst_all[[metric]];
		learn_or_not[[metric]] = list();
		for (gr in names(best_worst))
		{
			learn_or_not[[metric]][[gr]] = list();
			learn_or_not[[metric]][[gr]][['learn']] = NULL;
			learn_or_not[[metric]][[gr]][['unlearn']] = NULL;
			sub_in = grep("rand_*", 
						  rownames(best_worst[[gr]][['random']]), 
						  invert = T);
			learn_or_not[[metric]][[gr]][['random']] = best_worst[[gr]][['random']][sub_in, ];
			for (sg in c('best', 'worst'))
			{
				lm_data = best_worst[[gr]][[sg]][,41:101];
				# remove random subjects
				sub_in = grep("rand_*", rownames(lm_data), invert = T);
				if (length(sub_in > 0))
				{
					lm_data = lm_data[sub_in,];

					lm_data2 = matrix(t(lm_data), nrow = 1);
					ln = lm(as.vector(lm_data2) ~ rep(seq(41,101), nrow(lm_data)));
					slope = ln[[1]][[2]];
#					print(sprintf("metric: %s, gr: %s, sg: %s, slope: %f, alpha: %f", 
#								  metric, gr, sg, slope, alpha));
					if (slope > alpha)
					{
						key = 'learn';
					}
					else
					{
						key = 'unlearn';
					}
					learn_or_not[[metric]][[gr]][[key]] = rbind(learn_or_not[[metric]][[gr]][[key]], best_worst[[gr]][[sg]][sub_in, ]);	
#					print(sprintf("New %s: %s", key, rownames(learn_or_not[[metric]][[gr]][[key]])));
				}
			}
		}
	}
	return(learn_or_not);
}

plot_sub_clusters <- function() {
	lon = js_sub_clust.dat();
	png('/tmp/outcome_com_random.png');
	par(cex.axis = 1.2, cex.lab = 1.2);
	matplot(t(lon$outcome$com$random), type = 'l', pch = 1:nrow(lon$outcome$com$random), lwd=4, ylab = "Performance(Long-term outcome)", xlab = "Trial");
	dev.off();

	png('/tmp/outcome_web_learn.png');
	matplot(t(lon$outcome$com$learn), type = 'l', pch = 1:nrow(lon$outcome$com$learn), lwd=4, ylab = "Performance(Long-term outcome)", xlab = "Trial");
	dev.off();

	png('/tmp/outcome_com_learn.png');
	matplot(t(lon$outcome$com$learn), type = 'l', pch = 1:nrow(lon$outcome$com$learn), lwd=4, ylab = "Performance(Long-term outcome)", xlab = "Trial");
	dev.off();

	png('/tmp/outcome_com_unlearn.png');
	matplot(t(lon$outcome$com$unlearn), type = 'l', pch = 1:nrow(lon$outcome$com$unlearn), lwd=4, ylab = "Performance(Long-term outcome)", xlab = "Trial");
	dev.off();
}
