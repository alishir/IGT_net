source('./igt_analysis.r');

total_diff_analysis <- function(igt_path = 
								'~/doc/ms/thesis/all_in_one/data/igt_result/',
								sub_path = '~/doc/ms/thesis/all_in_one/data/sub_pool/'
								)
{
	dat = load_exec_data(igt_result_dir = igt_path);
	sub_mat = load_subjects(sub_path, rescale = F);

#	tot_reward = t(apply(dat, 1, calc_acc_reward))[,101];
	tot_reward = t(apply(dat, 1, calc_good_bad))[,101];

	group_fname = "sub_exp_map";
	sub_exp_map = read.csv(paste(igt_path, group_fname, sep = "/"));

	pen_sb = rownames(dat)[which(sub_exp_map == "pen")];
	com_sb = rownames(dat)[which(sub_exp_map == "com")];
	web_sb = rownames(dat)[which(sub_exp_map == "web")];

	bp_list = list('PEN' = tot_reward[pen_sb], 
				   'COM' = tot_reward[com_sb],
				   'WEB' =tot_reward[web_sb]);
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

	png(paste(BASE_PATH, 'total_diff_reward.png', sep = '/'));
	boxplot(bp_list, names = names(bp_list));
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
	png(paste(BASE_PATH, 'tot_dendogram.png', sep = '/'), 
		width = 1360 * ratio, height = 768 * ratio);
	plot(hc, lwd = 4);
	dev.off();

	ratio = 2;
	png(paste(BASE_PATH, 'personality_general.png', sep = '/'), 
		width = 1360 * ratio, height = 768 * ratio);
	num_clust = 4;
	layout(matrix(seq(2 * num_clust), nrow = 2, byrow = F));
	par(cex.axis = 3, cex.main = 3, cex.lab = 3, mar = c(6,6,6,6));
	clusts = cutree(hc, k = num_clust);
	ind = rownames(as.matrix(clusts));
	ret_clusts = list();
	col_plate_size = 4;
	cols = terrain.colors(col_plate_size, 0.7);
#	cols = topo.colors(col_plate_size, 0.7);
	for (i in 1:num_clust)
	{
		key = sprintf("clust_%d", i);
		ret_clusts[[key]] = ind[clusts %in% i];			# cluster indexes
		subs = ret_clusts[[key]];
		ts_len = 101;
		data_ts = matrix(nrow = length(subs), ncol = ts_len);
		rownames(data_ts) = subs;
		for (sub_name in subs)
		{
			data_ts[sub_name, ] = calc_good_bad(dat[sub_name, ], metric);
		}
		matplot(t(data_ts), type = 'o', pch = 1:nrow(data_ts),
				col = 1:nrow(data_ts), bg = 1:nrow(data_ts), main = key,
				lwd = 3, cex.axis = 3, cex.main = 3);
		legend('topleft', rownames(data_ts), fill = 1:nrow(data_ts), cex = 3, ncol = 3);
		radi = tot_reward[subs] + abs(min(tot_reward[subs])) + 1;
		radi = radi / 100;

		bg_cols = apply(as.matrix(subs), 1, function(sub_) {
						if (sub_exp_map[sub_, ] == 'web')
						{
							return(cols[1]);
						}
						else if (sub_exp_map[sub_, ] == 'pen')
						{
							return(cols[2]);
						}
						else if (sub_exp_map[sub_, ] == 'com')
						{
							return(cols[3]);
						}
				});

		symbols(sub_mat[subs, "bis"], sub_mat[subs, "bas"],
				radi, bg = bg_cols, inches = F);
		legend('topright', c('web', 'pen', 'com'), fill = cols, cex = 3)
	}

	dev.off();
#	total_clustering(dat, score, num_clust = 4, seq(101));
}
