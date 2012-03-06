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

print(sub_data)

km <- kmeans(sub_data, 6, 10000000)
print(km)
