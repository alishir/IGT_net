source('~/doc/ms/thesis/all_in_one/script/add_new_sub.r');
add_new_info_to_subjects <- function(sub_dir)
{
	sub_list = sort(list.files(sub_dir, pattern = "*.sub$"));
	for (i in sub_list)
	{
		print(i);
		zz = new.env();
		load(paste(sub_dir, i, sep = "/"), envir = zz);
		if (!("sub_sex" %in% ls(zz)))
		{
			assign("sub_sex", readline("subject sex(M/F): "), envir = zz);
		}
		if (!("sub_contact" %in% ls(zz)))
		{
			assign("sub_contact", readline("subject contact: "), envir = zz);
		}
		
    if (!("sub_pnas" %in% ls(zz)))
    {
      assign("sub_pnas", readline("subject PNAS: "), envir = zz);
    }
    else if (nchar(get("sub_pnas", envir = zz)) != 20)
    {
      cat(sprintf("Invalid PNAS value, subject %s\n", get("sub_id", envir = zz)));
      assign("sub_pnas", readline("subject PNAS: "), envir = zz);
    }
    
    if (!("sub_pnas_score" %in% ls(zz)))
    {
      if (nchar(get("sub_pnas", envir = zz)) == 20)
      {
        cat(sprintf("Calculating PNAS score ... subid: %s\n", get("sub_id", envir = zz)));
        assign("sub_pnas_score", calc_pnas_score(get("sub_pnas", envir = zz)), envir = zz);
      }
    }
    else
    {
      neg_score = get("sub_pnas_score", envir = zz);
      if (neg_score$pnas_neg_score == -10)
      {
        cat(sprintf("Neg Score: %d\n", neg_score$pnas_neg_score));
        cat(sprintf("Calculating PNAS score ... subid: %s\n", get("sub_id", envir = zz)));
        assign("sub_pnas_score", calc_pnas_score(get("sub_pnas", envir = zz)), envir = zz);
      }
    }

		save(list = ls(zz), file = paste(sub_dir, i, sep = "/"), envir = zz);
	}
}
