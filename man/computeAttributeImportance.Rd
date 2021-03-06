\name{computeAttributeImportance}
\alias{computeAttributeImportance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{compute importances
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
computeAttributeImportance(dbname1, user1, password1, host1, post1)
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function () 
{
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = "JLI", user = "postgres", 
        password = "postgres", host = "localhost", port = "5432")
    entities_query <- "select distinct store_number entity_id, 'store' entity_type from stores where store_number=123"
    eq <- dbSendQuery(con, entities_query)
    eq_matrix <- as.data.frame(fetch(eq, n = -1))
    dbClearResult(eq)
    truncate_query <- "truncate table eba_results"
    tq <- dbSendQuery(con, truncate_query)
    dbClearResult(tq)
    computeImportance <- function(entity) {
        tryCatch({
            entity_id <- paste("'", entity[1], "'", sep = "")
            entity_type <- paste("'", entity[2], "'", sep = "")
            tradeoff_query <- paste("select attr1, attr2, attr3, attr4, attr5, attr6, attr7, attr8 from eba_matrix where entity_id=", 
                entity_id, "and entity_type=", entity_type, "order by attrs")
            tq <- dbSendQuery(con, tradeoff_query)
            eba_matrix <- as.data.frame(fetch(tq, n = -1))
            row.names(eba_matrix) <- c("attr1", "attr2", "attr3", 
                "attr4", "attr5", "attr6", "attr7", "attr8")
            btl <- eba(eba_matrix)
            importance <- as.data.frame(uscale(btl) * 100)
            names(importance)[1] <- "importance"
            entity_ids <- c(entity[1], entity[1], entity[1], 
                entity[1], entity[1], entity[1], entity[1], entity[1])
            entity_types <- c(entity[2], entity[2], entity[2], 
                entity[2], entity[2], entity[2], entity[2], entity[2])
            attrs <- c("attr1", "attr2", "attr3", "attr4", "attr5", 
                "attr6", "attr7", "attr8")
            table_prep <- as.data.frame(cbind(entity_ids, entity_types, 
                attrs, importance))
            dbWriteTable(con, "eba_results", table_prep, row.names = F, 
                append = T)
            dbClearResult(tq)
        }, error = function(cond) {
            message("can't process")
        })
    }
    apply(eq_matrix, 1, computeImportance)
    dbDisconnect(con)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
