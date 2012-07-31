add_new_sub <- function(sub_dir)
{
	sub_list = sort(list.files(sub_dir), TRUE);
	print(sprintf("last subject is: %s", sub_list[1]));
	sub_id = readline("subject id: ");
	sub_age = readline("subject age: ");
	sub_bas_bis = readline("subject bas/bis answers: ");
	sub_bas_bis_score = calc_bas_bis_score(sub_bas_bis);
	file_name = paste(sub_id, "sub", sep=".");
	save_path = paste(sub_dir, file_name, sep="/");
	save(sub_id, sub_age, sub_bas_bis, sub_bas_bis_score, file=save_path);
	return(sub_id);
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


cluster_subjects <- function(sub_dir)
{
	sub_list = list.files(sub_dir);
	sub_size = length(sub_list);
	sub_mat = matrix(data = NA, nrow = sub_size, ncol = 2);
	rownames(sub_mat) <- as.vector(sub_list);
	colnames(sub_mat) <- c("bas", "bis");
	for (i in list.files(sub_dir))
	{
		load(paste(sub_dir, i, sep="/"));
		sub_mat[i, "bas"] = as.numeric(sub_bas_bis_score["bas_score_norm"]);
		sub_mat[i, "bis"] = as.numeric(sub_bas_bis_score["bis_score_norm"]);
	}
	print(sub_mat);
	sub_dis <- dist(sub_mat);
	hd <- hclust(sub_dis, method="ward");
	plot(hd);
	return(hd);
}
