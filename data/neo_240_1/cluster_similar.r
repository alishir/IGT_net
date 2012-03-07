load_data <- function()
{
	neo <- c('n', 'e', 'o', 'a', 'c')
		cnames <- c()
		for (i in seq(1,6))
		{
			for (c in neo)
			{
				cnames <- c(cnames, paste(c, i, sep=""))
			}
		}
# print(cnames)

	data = read.csv('all_data.csv', header=FALSE)
		num_row = dim(data)[1]
		rnames <- c()
		for (i in seq(1, num_row))
		{
			rnames <- c(rnames, paste('sub', i, sep=""))
		}
# print(rnames)

	sub_data <- matrix(unlist(data), num_row, 30, byrow=FALSE, dimnames=(list(rnames, cnames)))

		sub_data
}

extract_feature <- function(sub_data, feature_list)
{
	new_data <- subset(sub_data, select = feature_list)
}

cluster_people <- function(num_cluster, feature)
{
	sub_data <- load_data()
#	selected_feature <- c("n5", "e5", "o3", "c5")
	new_data <- extract_feature(sub_data,  selected_feature)
	km <- kmeans(new_data, num_cluster, 10000000)
}
