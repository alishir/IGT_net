
source('./igt_analysis.r');
poster <- function()
{
	payam_path = '~/doc/ms/thesis/all_in_one/data/payam/';
	my_path = '~/doc/ms/thesis/all_in_one/data/igt_result/';
	
	payam_data <<- load_exec_data(payam_path);

	group_fname = "sub_exp_map";
	payam_map <<- read.csv(paste(payam_path, group_fname, sep = "/"));


	pa_acc_reward = apply(payam_data, 1, 
						  function(trial){
							  rew = calc_acc_reward(trial);
							  len = dim(rew)[2];
							  return(rew[len]);
						  });
	pa_acc_reward <<- array(pa_acc_reward);
	rownames(pa_acc_reward) <<- rownames(payam_data);

	pa_pen = which(payam_map == "pen");
	pa_com = which(payam_map == "com");
	pa_web = which(payam_map == "web");

	bp_cols = gray.colors(3, 0.35);
	bp_cols = topo.colors(3, 0.35);


	boxplot_list = list('Real Card' = pa_acc_reward[pa_web],
						'Computerized' = pa_acc_reward[pa_com],
						'Pen & Paper' = pa_acc_reward[pa_pen]
						);
	rat = 3;
	png('/tmp/var.png', height = 300 * rat, width = 400 * rat, bg = "transparent");
	par(cex.axis = 2.5, cex.lab = 2.5, cex.main = 2.5,
		oma = c(1.5,4.5,1.5,2), mar = c(1.5,6,3,2));
	boxplot(boxplot_list, 
			xlim = c(0,4), 
			ylim = c(500,3500), 
			ylab = "Total Reward", 
			names = names(boxplot_list),
			main = "Total score of participants in three variants of IGT",
			col = bp_cols[1:3],
			lwd = 5);

	dev.off();
}
