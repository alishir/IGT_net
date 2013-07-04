setwd("/home/ali/doc/code/IGT_net/analysis/script");
source("./igt_analysis.r")

igt_path <<- '~/doc/ms/thesis/all_in_one/data/payam/'
dat_to_csv <- function(save_path = '/tmp/test.csv') {
	dat = load_exec_data(igt_path);
	group_fname = "sub_exp_map";
	sub_exp_map = read.csv(paste(igt_path, group_fname, sep="/"));

	choices = lapply(sub_names, function(sub_n) { 
					 data.frame(choices = paste0(lapply(dat[sub_n,], function(x) { intToUtf8(x) }), collapse = ''),
								group = sub_exp_map[sub_n,1],
								id = sub_n) });
	choices = do.call(rbind, choices);

	print(head(choices));
	write.csv(choices, row.names = F, file = save_path);
	return(choices);
}
# > pa  = unlist(lapply(da[3,], function(x) { if (x == "true") 1 else 0 }))

