\name{entities_query}
\alias{entities_query}
\docType{data}
\title{all entities within JLI
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("entities_query")}
\format{
  The format is:
 chr "select distinct store_number entity_id, 'store' entity_type from stores where store_number=123"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(entities_query)
## maybe str(entities_query) ; plot(entities_query) ...
}
\keyword{datasets}
