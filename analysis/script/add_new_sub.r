add_new_sub <- function(sub_dir)
{
	sub_list = sort(list.files(sub_dir, pattern = "*.sub$"), TRUE);
	print(sprintf("last subject is: %s", sub_list[1]));
	sub_id = readline("subject id: ");
	sub_age = readline("subject age: ");
	sub_sex = readline("subject sex(M/F): ");
	sub_contact = readline("subject contact: ");
	sub_bas_bis = readline("subject bas/bis answers: ");
	sub_bas_bis_score = calc_bas_bis_score(sub_bas_bis);
  sub_pnas = readline("subject PNAS: ");
  sub_pnas_score = calc_pnas_score(sub_pnas);
	file_name = paste(sub_id, "sub", sep=".");
	save_path = paste(sub_dir, file_name, sep="/");
	save(sub_id, sub_age, sub_sex, sub_contact, sub_bas_bis, sub_bas_bis_score, file=save_path);
	return(sub_id);
}

calc_pnas_score <- function(sub_pnas)
{
  if (nchar(sub_pnas != 20))
  {
    cat("Invalid PANAS");
    return(list("pnas_neg_score" = -10, "pnas_pos_score" = -10));
  }
  choice_value_map = list("a" = 5, "s" = 4, "d" = 3, "f" = 2, "g" = 1, " " = 3);
  return_score = list();
  positive_index = c(2, 4, 6, 7, 9, 11, 13, 15, 19, 20);
  negative_index = c(1, 3, 5, 8, 10, 12, 14, 16, 17, 18);
  
  positive_score = 0;
  negative_score = 0;
  
  for (i in positive_index)
  {
    positive_score = positive_score + as.numeric(choice_value_map[substr(sub_pnas, i, i)]);
  }
  for (i in negative_index)
  {
    negative_score = negative_score + as.numeric(choice_value_map[substr(sub_pnas, i, i)]);
  }
  return(list("pnas_neg_score" = negative_score, "pnas_pos_score" = positive_score));
}

calc_bas_bis_score <- function(sub_bas_bis)
{
	choice_value_map = list("a" = 4, "s" = 3, "d" = 2, "f" = 1, " " = 0)
	return_score = list();
	bis_index = c(1, 5, 9, 13, 14, 18, 20);
	bas_RR_index = c(2, 6, 10, 15, 19);
	bas_D_index = c(3, 7, 11, 16);
	bas_FS_index = c(4, 8, 12, 17);
	bis_score = 0;
	for (i in bis_index)
	{
		if (i == 20)
		{
			bis_score = bis_score + 5 - as.numeric(choice_value_map[substr(sub_bas_bis, i, i)])
		}
		else
		{
		  bis_score = bis_score + as.numeric(choice_value_map[substr(sub_bas_bis, i, i)])
		}
	}
	bas_rr_score = 0;
	for (i in bas_RR_index)
	{
		bas_rr_score = bas_rr_score + as.numeric(choice_value_map[substr(sub_bas_bis, i, i)])
	}
	bas_d_score = 0;
	for (i in bas_D_index)
	{
		bas_d_score = bas_d_score + as.numeric(choice_value_map[substr(sub_bas_bis, i, i)])
	}
	bas_fs_score = 0;
	for (i in bas_FS_index)
	{
		bas_fs_score = bas_fs_score + as.numeric(choice_value_map[substr(sub_bas_bis, i, i)])
	}
	return_score["bis_score_norm"] = (bis_score - 7) / (28 - 7);
	return_score["bas_rr_score"] = bas_rr_score;
	return_score["bas_d_score"] = bas_d_score;
	return_score["bas_fs_score"] = bas_fs_score;
	return_score["bas_score_norm"] = (bas_rr_score + bas_d_score + bas_fs_score - 13) / (52 - 13);
	return(return_score);
}

load_subjects <- function(sub_dir)
{
  sub_list = list.files(sub_dir, pattern = "*.sub$");
  sub_size = length(sub_list);
  sub_mat = matrix(data = NA, nrow = sub_size, ncol = 8);
  sub_ids = lapply(sub_list, function(e) {substr(e, 1, nchar(e) - 4);});
  rownames(sub_mat) <- as.vector(sub_ids);
  colnames(sub_mat) <- c("sex", "bas", "bas_rr", "bas_fs", "bas_d", "bis", "pnas_p", "pnas_n");
  sex_map = list("M" = 0, "F" = 1);
  for (i in sub_list)
  {
    print(i);
    load(paste(sub_dir, i, sep="/"));
    id = substr(i, 1, nchar(i) - 4);
    sub_mat[id, "sex"] = as.numeric(sex_map[sub_sex]);
    sub_mat[id, "bas"] = as.numeric(sub_bas_bis_score["bas_score_norm"]);
    sub_mat[id, "bas_rr"] = (as.numeric(sub_bas_bis_score["bas_rr_score"]) - 5) / (20 - 5);
    sub_mat[id, "bas_fs"] = (as.numeric(sub_bas_bis_score["bas_fs_score"]) - 4) / (16 - 4);
    sub_mat[id, "bas_d"] = (as.numeric(sub_bas_bis_score["bas_d_score"]) - 4) / (16 - 4);
    sub_mat[id, "bis"] = as.numeric(sub_bas_bis_score["bis_score_norm"]);
    sub_mat[id, "pnas_p"] = (as.numeric(sub_pnas_score["pnas_pos_score"]) - 10) / (50 - 10);
    sub_mat[id, "pnas_n"] = (as.numeric(sub_pnas_score["pnas_neg_score"]) - 10) / (50 - 10);
  }
  return(sub_mat);
}


cluster_subjects <- function(sub_mat)
{
	sub_dis <- dist(sub_mat);
	hd <- hclust(sub_dis, method = "ward");
	plot(hd);
	return(hd);
}

# get subject groups from experimenter
identify_groups <- function(sub_cluster, sub_dir)
{
	clus_plot = plot(sub_cluster);
	# load previous group assignment
	group_fname = "sub_exp_map";
	sub_exp_map = read.csv(paste(sub_dir, group_fname, sep="/"));

	# colorify dendogram nodes
	local({
		exp_col_map = list("com" = "red", "pen" = "green", "web" = "blue");
		colLab <<- function(n) {
			if(is.leaf(n)) {
				a <- attributes(n);
				sub_name = a$label;
				if (sub_name %in% rownames(sub_exp_map))
				{
					color = exp_col_map[[sub_exp_map[sub_name, ]]];
					attr(n, "nodePar") <- c(a$nodePar, list(lab.col = color));
				}
			}
			return(n);
		}
	})
	dhc = as.dendrogram(sub_cluster);
	color_dhc <- dendrapply(dhc, colLab)
#	op = par(mfrow=2:1);
	plot(color_dhc);
#	plot(dhc);
#	par(op);

	groups = identify(sub_cluster);
}

doit <- function(sub_path)
{
  if (nchar(sub_path) == 0)
  {
	  sub_path = readline("subject locateion: (e.g. /tmp/sub): ");
  }
  sub_mat = load_subjects(sub_path);
	hd = cluster_subjects(sub_mat);
	identify_groups(hd, sub_path);
}
