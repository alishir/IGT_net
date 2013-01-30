source('./igt_analysis.r');
source('./dbhat.r');


iccs2013 <- function()
{
	payam_path = '~/doc/ms/thesis/all_in_one/data/payam/';
	my_path = '~/doc/ms/thesis/all_in_one/data/igt_result/';
	
	payam_data <<- load_exec_data(payam_path);
	my_data <<- load_exec_data(my_path);

	group_fname = "sub_exp_map";
	payam_map <<- read.csv(paste(payam_path, group_fname, sep = "/"));
	my_map <<- read.csv(paste(my_path, group_fname, sep = "/"));

	pa_acc_reward = apply(payam_data, 1, 
						  function(trial){
							  rew = calc_acc_reward(trial);
							  len = dim(rew)[2];
							  return(rew[len]);
						  });
	pa_acc_reward <<- array(pa_acc_reward);
	rownames(pa_acc_reward) <<- rownames(payam_data);
	my_acc_reward = apply(my_data, 1, 
						  function(trial){
							  rew = calc_acc_reward(trial);
							  len = dim(rew)[2];
							  return(rew[len]);
						  });
	my_acc_reward <<- array(my_acc_reward);
	rownames(my_acc_reward) <<- rownames(my_data);

	print("Payam Kruskal");
	pa_kw_result = kruskal_wallis_test(dat = pa_acc_reward, payam_map);
	print("My Kruskal");
	my_kw_result = kruskal_wallis_test(dat = my_acc_reward, my_map);
	
	pa_pen = which(payam_map == "pen");
	pa_com = which(payam_map == "com");
	pa_web = which(payam_map == "web");

	my_pen = which(my_map == "pen");
	my_com = which(my_map == "com");
	my_web = which(my_map == "web");

	print("Pen DB dist");
	print(DBhat(as.matrix(my_acc_reward[my_web]), as.matrix(pa_acc_reward[pa_pen])));
	print("COM DB dist");
	print(DBhat(as.matrix(my_acc_reward[my_web]), as.matrix(pa_acc_reward[pa_com])));
	print("WEB DB dist");
	print(DBhat(as.matrix(my_acc_reward[my_web]), as.matrix(pa_acc_reward[pa_web])));


	pa_score = calc_block_score(sub_data = payam_data, group_map = payam_map, 
							 score_type = 'raw');
	my_score = calc_block_score(sub_data = my_data, group_map = my_map, 
							 score_type = 'raw');

	random_subjects = create_random_subjects(30);
	random_score = calc_block_score(sub_data = random_subjects);

	pa_best_worst = get_best_worst_block_score(pa_score, rand_score = random_score);
	my_best_worst = get_best_worst_block_score(my_score, rand_score = random_score);


	my_web_best = rownames(my_best_worst[["web"]][["best"]]);
	my_web_best_ind = grep("rand_*", my_web_best, invert = T); # remove random subjects
	my_web_best = my_web_best[my_web_best_ind];

	pa_com_best = rownames(pa_best_worst[["com"]][["best"]]);
	pa_com_best_ind = grep("rand_*", pa_com_best, invert = T); # remove random subjects
	pa_com_best = pa_com_best[pa_com_best_ind];

	pa_pen_best = rownames(pa_best_worst[["pen"]][["best"]]);
	pa_pen_best_ind= grep("rand_*", pa_pen_best, invert = T); # remove random subjects
	pa_pen_best = pa_pen_best[pa_pen_best_ind];

	pa_web_best = rownames(pa_best_worst[["web"]][["best"]]);
	pa_web_best_ind= grep("rand_*", pa_web_best, invert = T); # remove random subjects
	pa_web_best = pa_web_best[pa_web_best_ind];


	print("Pen DB dist -- BEST");
	print(DBhat(as.matrix(my_acc_reward[my_web_best]), as.matrix(pa_acc_reward[pa_pen_best])));
	print("Com DB dist -- BEST");
	print(DBhat(as.matrix(my_acc_reward[my_web_best]), as.matrix(pa_acc_reward[pa_com_best])));
	print("Web DB dist -- BEST");
	print(DBhat(as.matrix(my_acc_reward[my_web_best]), as.matrix(pa_acc_reward[pa_web_best])));



}
