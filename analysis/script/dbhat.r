DBhat <- function(X1,X2)
{
	# define means
	mX1 <- mean(X1)
	mX2 <- mean(X2)

	# define difference of means
	mDiff <- mX1 - mX2

	# define cov
	cvX1 <- cov(X1)
	cvX2 <- cov(X2)

	# define halfsum of cv's
	p <- (cvX1+cvX2)/2

	# the equation
	0.125 * t(mDiff) * p^(-1) * mDiff + 0.5 * log10( det(p) /
													sqrt( det(cvX1) * det(cvX2) ))
}
