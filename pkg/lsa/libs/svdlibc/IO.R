
### ceeboo 2008

## NOTES 1) the sparse readers cannot handle leading or trailing
##          spaces (and most likely are inefficient)
##       2) the dash in the outputfile is braindead (see below).

## write a dense matrix in sparse text format
write.D.ST <-
function(x, con = stdout(), sep = "\n") {
    if (!is.matrix(x))
        stop("'x' not a matrix")
    text <- paste(dim(x)[1], dim(x)[2], sum(x != 0))
    text <- c(text, apply(x, 2, function(x) {
        k <- which(x != 0)
        r <- as.character(length(k))
        if (length(k))
            r <- c(r, paste(k-1, x[k]))
        r
    }))
    writeLines(unlist(text, use.names = FALSE), con, sep)
}

## write a vector or dense matrix in dense text format
write.D.DT <-
function(x, file, ...) {
    if (is.character(file)) {
        file <- file(file, "w")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("Argument 'file' must be a character string or connection.")
    if (!isOpen(file)) {
        open(file, "w")
        on.exit(close(file))
    }
    if (is.vector(x))
        x <- array(x)
    if (is.null(dim(x)) || length(dim(x)) > 2)
        stop("can only write vector or matrix")
    ## write declaration
    cat(dim(x), file = file,
        sep = c(rep.int(" ", length(dim(x)) - 1), "\n"))
    ## write data
    x <- t(x)
    cat(as.character(x), file = file,
        sep = c(rep.int(" ", dim(x)[1] - 1), "\n"))
}

## read a file in sparse text format into a dense matrix
read.ST.D <-
function(con = stdin(), encoding = "unknown") {
    ## get data
    text <- readLines(con, encoding = encoding)
    data <- lapply(strsplit(text, "[[:space:]]+"), as.numeric)
    ## set up matrix
    x <- matrix(0, nrow = data[[1]][1], ncol = data[[1]][2])
    data <- data[-1]
    ## get row / column indexes
    len <- sapply(data, length)
    col <- sapply(data[len == 1], eval)
    col <- rep(seq_len(length(col)), col)
    row <- as.integer(sapply(data[len == 2], "[", 1)) + 1L
    ## populate
    x[cbind(row, col)] <- sapply(data[len == 2], "[", 2)
    x
}

## read file in dense format
read.DT <-
function(file, encoding = "unknown") {
    if (is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("Argument 'file' must be a character string or connection.")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    ## read first non-blank line
    dim <- scan(file, what = NA_integer_, nlines = 1L,
        quiet = TRUE, encoding = encoding)
    if (length(data) > 2)
        stop("invalid declaration")
    ## read remaining non-blank lines
    data <- scan(file, what = NA_real_, quiet = TRUE,
        encoding = encoding)
    ## Vector
    if (length(dim) == 1) {
        if (length(data) != dim)
            stop("data does not match declaration")
        return(data)
    }
    ## Matrix
    if (length(data) != prod(dim))
        stop("data does not match declaration")
    matrix(data, nrow = dim[1], byrow = TRUE)
}

## FIXME
svd.call <-
function(x, ndim = dim(x)[2], verbose = 1, tmpdir = tempdir())
{
    file <- file.path(tmpdir, "-SVD.txt")
    if (file.exists(tmpdir))
        on.exit(unlink(file.path(tmpdir, c("-SVD.txt","-S", "-Ut", "-Vt"))))
    else
        on.exit(unlink(tmpdir, recursive = TRUE))
    ## FIXME timing
    write.D.ST(x, con = file)
    ##
    system(paste("./svd", "-d", ndim, "-v", verbose, "-o",
        file.path(tmpdir, ""), file))
    list(d =   read.DT(file = file.path(tmpdir, "-S")),
         u = t(read.DT(file = file.path(tmpdir, "-Ut"))),
         v = t(read.DT(file = file.path(tmpdir, "-Vt"))))
}

## test sparse read / write (assume count data)
x <- matrix(ceiling(runif(12) * 100), nrow = 4)
x[sample(length(x), 4)] <- 0
print(x, digits = 2)

write.D.ST(x, con = "test.txt")

print(read.ST.D(con = "test.txt"), digits = 2)

## now check command line interfacing
## UNIX only, sorry.

system("./svd -d 2 -o ./ test.txt")

## not a matrix
##as.numeric(readLines("-S")[-1])
read.DT("-S")
read.DT("-Ut")
read.DT("-Vt")

## compare
system("cat -- -S -Ut -Vt")

## clean up the dash mess
system("rm -f -- -S -Ut -Vt")

system("rm -f test.txt")

## compare
r <- svd.call(x, verbose = 0)
r

r$u %*% diag(r$d) %*% t(r$v)

r <- svd(x)
r

r$u %*% diag(r$d) %*% t(r$v)

###
