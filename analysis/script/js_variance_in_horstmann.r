source('./igt_analysis.r');

var_in_horstman <- function()
{
	igt_path = '~/doc/ms/thesis/all_in_one/data/payam/'
	dat = load_exec_data(igt_path);
	group_fname = "sub_exp_map";
	sub_exp_map = read.csv(paste(igt_path, group_fname, sep="/"));

	pen_id = which(sub_exp_map == 'pen');
	pen_g = rownames(sub_exp_map)[pen_id];

	com_id = which(sub_exp_map == 'com');
	com_g = rownames(sub_exp_map)[com_id];

	web_id = which(sub_exp_map == 'web');
	web_g = rownames(sub_exp_map)[web_id];

	num_of_blocks = 5;
	hor_a = horstmann_analysis(raw_data = dat, sub_exp_map, num_of_blocks);

	rat = 4;
	png('/tmp/var.png', height = 300 * rat, width = 200 * rat);
	layout(matrix(seq(1,4), byrow = T));
	par(cex.axis = 2.5, cex.lab = 2.5, 
		oma = c(1.5,4.5,1.5,2), mar = c(1.5,4.5,1.5,2));
	at_base = seq(1, 10, by = 2);
	bp_names = c('Block 1', 'Block 2', 'Block 3', 
				 'Block 4', 'Block 5');
	bp_cols = terrain.colors(4, 0.6);

	boxplot(hor_a[pen_g, ,'loss'], at = at_base, ylim = c(-0.6,0.6),
			axes = F, boxwex = 0.2, outline = F, col = bp_cols[1]);
	boxplot(hor_a[web_g, ,'loss'], at = at_base + 0.3, 
			names = bp_names, add = T, ylab = "Loss",
			boxwex = 0.2, outline = F, col = bp_cols[2]);
	boxplot(hor_a[com_g, ,'loss'], at = at_base + 0.6,
		   	axes = F, add = T, boxwex = 0.2, outline = F, col = bp_cols[3]);


	boxplot(hor_a[pen_g, ,'gain'], at = at_base, ylim = c(-0.6,0.6),
			axes = F, boxwex = 0.2, outline = F, col = bp_cols[1]);
	boxplot(hor_a[web_g, ,'gain'], at = at_base + 0.3, 
			names = bp_names, add = T, ylab = "Gain",
			boxwex = 0.2, outline = F, col = bp_cols[2]);
	boxplot(hor_a[com_g, ,'gain'], at = at_base + 0.6,
		   	axes = F, add = T, boxwex = 0.2, outline = F, col = bp_cols[3]);

	boxplot(hor_a[pen_g, ,'outcome'], at = at_base, ylim = c(-0.6,0.6),
			axes = F, boxwex = 0.2, outline = F, col = bp_cols[1]);
	boxplot(hor_a[web_g, ,'outcome'], at = at_base + 0.3, 
			names = bp_names, add = T, ylab = "Outcome",
			boxwex = 0.2, outline = F, col = bp_cols[2]);
	boxplot(hor_a[com_g, ,'outcome'], at = at_base + 0.6,
		   	axes = F, add = T, boxwex = 0.2, outline = F, col = bp_cols[3]);

	plot(1, type = "n", axes = FALSE, xlab = "", ylab = "");
	legend(x = "center", inset = 0,
		   legend = c("Pen & Paper", "Manual", "Computerized"), 
		   col = bp_cols[1:3], lwd = 5, cex = 2.6, horiz = TRUE);
	dev.off();
}
