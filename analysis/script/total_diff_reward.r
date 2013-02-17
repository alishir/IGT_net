source('./igt_analysis.r');

total_diff_analysis <- function(igt_path = '~/doc/ms/thesis/all_in_one/data/payam/')
{
	dat = load_exec_data(igt_result_dir = igt_path);
	tot_reward = t(apply(dat, 1, calc_acc_reward))[,101];

	group_fname = "sub_exp_map";
	sub_exp_map = read.csv(paste(igt_path, group_fname, sep = "/"));

	pen_sb = rownames(dat)[which(sub_exp_map == "pen")];
	com_sb = rownames(dat)[which(sub_exp_map == "com")];
	web_sb = rownames(dat)[which(sub_exp_map == "web")];

	bp_list = list(tot_reward[pen_sb], tot_reward[com_sb],
				   tot_reward[web_sb]);
# 	x11();
# 	hist(tot_reward[pen_sb], main = "PEN");
# 	qqnorm(tot_reward[pen_sb], main = "PEN");
# 	print(shapiro.test(tot_reward[pen_sb]));
# 	x11();
# 	hist(tot_reward[com_sb], main = "COM");
# 	qqnorm(tot_reward[com_sb], main = "COM");
# 	print(shapiro.test(tot_reward[com_sb]));
# 	x11();
# 	hist(tot_reward[web_sb], main = "WEB");
# 	qqnorm(tot_reward[web_sb], main = "WEB");
# 	print(shapiro.test(tot_reward[web_sb]));

	png('./total_diff_reward.png');
	bp_names = c("PEN", "COM", "WEB");
	boxplot(bp_list, names = bp_names);
	dev.off();

	pen_rw = matrix(tot_reward[pen_sb], ncol = 1);
	web_rw = matrix(tot_reward[web_sb], ncol = 1);
	com_rw = matrix(tot_reward[com_sb], ncol = 1);

	df_pre = rbind(pen_rw, com_rw, web_rw);
	pre_data = data.frame(Value = c(t(df_pre)), Groups = factor(c(rep('pen', length(tot_reward[pen_sb])),
																  rep('com', length(tot_reward[com_sb])),
																  rep('web', length(tot_reward[web_sb])))));

	kw = kruskal_test(Value ~ Groups, data = pre_data,
					  distribution = approximate(B = 99999));
	
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

	score = calc_block_score(sub_data = dat, group_map = sub_exp_map, 
							 score_type = 'raw', metric = 'outcome');

	total_score = rbind(score$pen, score$web, score$com)
	dis = dist(total_score, method = "DTW");
	hc = hclust(dis, method = "ward");
	ratio = 1;
	png('./tot_dendogram.png', width = 1360 * ratio, height = 768 * ratio);
	plot(hc, lwd = 4);
	dev.off();
	total_clustering(dat, score, num_of_clust = 4, seq(101));
}
