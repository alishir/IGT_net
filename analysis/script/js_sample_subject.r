source('./igt_analysis.r');

draw_ts_of_sample_sub <- function(igt_path = '', sub_id = '29')
{
	dat = load_exec_data(igt_path);
	group_fname = "sub_exp_map";
	sub_exp_map = read.csv(paste(igt_path, group_fname, sep = "/"));

	score_o = calc_block_score(sub_data = dat, group_map = sub_exp_map, 
							 score_type = 'raw', metric = 'outcome');
	tot_o = rbind(score_o$pen, score_o$com, score_o$web);

	score_g = calc_block_score(sub_data = dat, group_map = sub_exp_map, 
							 score_type = 'raw', metric = 'gain');
	tot_g = rbind(score_g$pen, score_g$com, score_g$web);

	score_l = calc_block_score(sub_data = dat, group_map = sub_exp_map, 
							 score_type = 'raw', metric = 'loss');
	tot_l = rbind(score_l$pen, score_l$com, score_l$web);

	sub_tot = rbind(tot_o[sub_id, ], tot_g[sub_id, ], tot_l[sub_id, ]);
	
	png('/tmp/test.png')
	par(lwd = 4, cex=1.2, cex.lab=1.2, cex.axis=1.2);
	matplot(t(sub_tot), type = 'l', pch = 1:3, lwd = 4, xlab = 'Trial', ylab = 'Performance');
	legend('topleft', c('Long-term outcome', 'Gain frequency', 'Loss frequency'), fill = 1:3, lty=1:3, cex=1.2)
	dev.off();

}
