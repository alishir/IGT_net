setwd("/home/ali/doc/code/IGT_net/analysis/script");
source("./igt_analysis.r");
library("abind");
library("dtw");

igt_path <<- '~/doc/ms/thesis/all_in_one/data/payam/'
bn_feature_visualizer <- function(load_path = '/tmp/outcome.csv') {
	dat = load_exec_data(igt_path);
	group_fname = "sub_exp_map";
	sub_exp_map = read.csv(paste(igt_path, group_fname, sep="/"));

	groups = lapply(summary(sub_exp_map), function(x) { unlist(strsplit(x, ":"))[1] });
	names(groups) = unlist(groups);
	feature_dat = read.csv(load_path);

	feature_ts_gr = lapply(groups, function(gr) { 
						   sub_in_gr = which(sub_exp_map == gr);
						   sub_names = rownames(sub_exp_map)[sub_in_gr];
						   # print(head(subset(feature_dat, id %in% sub_names))
						   subg = subset(feature_dat, id %in% sub_names);
						   # remove id column
						   subg = subg[, !names(subg) %in% c("id")];
						   apply(subg, c(1,2), function(x) { as.numeric(x) });
});
	return(feature_ts_gr);
}

run_bn_vis <- function() {
	features = c("outcome", "gain", "loss");
	names(features) = features;

	png("/tmp/bn.png", width = 800, height = 1000);
	layout(matrix(seq(3), nrow = 3));
	lapply(features, function(f) {
		   print(f);
		   fn = sprintf("%s%s.csv", igt_path, f);
		   feature_ts_gr = bn_feature_visualizer(fn);

		   colmeans_gr = lapply(groups, function(gr) { colMeans(feature_ts_gr[[gr]]) });
		   agr = abind(colmeans_gr, along = 2);
		   par(cex = 1.2);

		   matplot(agr, type = 'l', lwd = 4, ylab = sprintf("%s Based Behavior Avg", f));
		   legend("bottomleft", colnames(agr), fill = 1:3);

});
	dev.off();
}

ts_clustering <- function(ts_data, num_of_clust = 3) {
	dis = dist(ts_data, method = "DTW");
	hc = hclust(dis);
	return(hc);
}

bn_cluster_analysis <- function() {
	sf = "./bn_clust.dat";
	reload = TRUE;
	if (file.exists(sf) && reload) {
		load(sf);
		return(clust);
	}
	features = c("outcome", "gain", "loss");
	names(features) = features;
	clust = lapply(features, function(f) {
		   fn = sprintf("%s%s.csv", igt_path, f);
		   feature_ts_gr = bn_feature_visualizer(fn);
		   groups = names(feature_ts_gr);
		   names(groups) = groups;
		   lapply(groups, function(x) { 
				  ts_clustering(feature_ts_gr[[x]]) });
});
	save(file = sf, clust);
}

bn_subcluster_ext <- function() {
	clusters = bn_cluster_analysis();
	cut_h = 100; # max diff is 200
	lapply(clusters, function(f) {
		   lapply(f, function(gr) {
				  sub_cl = cutree(gr, h = cut_h);
				  ind = rownames(as.matrix(sub_cl));
				  number_of_clust = max(sub_cl);
				  ret_list = list()
				  for (clus_ind in seq(number_of_clust)) {
					   ret_val[[clus_ind]] = ind[sub_cl %in% clus_ind]
					}
				  ret_val
				  });
});
}

bn_vis_subclust <- function() {
	sub_clust = bn_subcluster_ext();
	features = names(sub_clust);
	names(features) = features;
	lapply(features, function(f) {
			fn = sprintf("%s%s.csv", igt_path, f);
			feature_ts_gr = bn_feature_visualizer(fn);
			groups = names(sub_clust[[f]]);
			names(groups) = groups;
			lapply(groups, function(gr) {
				   ifn = sprintf("/tmp/%s_%s.png", f, gr);
#				   png(ifn);
#				   layout(matrix(seq(length(names(sub_clust[[f]][[gr]]))), nrow = 1));
				   lapply(sub_clust[[f]][[gr]], function(sub_cl) {
#						  print(sub_cl);
#						  print(head(feature_ts_gr[[gr]][sub_cl, ]));
						  x11();
						  matplot(t(feature_ts_gr[[gr]][sub_cl, ]), type = 'l', main = sprintf("%s_%s", f, gr));
});
#				   dev.off();
				  });
});

}
