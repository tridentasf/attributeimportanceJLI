library("eba")
library("RPostgreSQL")

computeAttributeImportance <- function() {
  drv<-dbDriver("PostgreSQL")
  con<-dbConnect(drv, dbname="JLI", user="postgres", password="postgres", host="localhost", port="5432")
  #con<-dbConnect(drv, dbname="JLI", user="postgres", password="postgres", host="52.3.62.220", port="5432")
  #need to read out the list of stores and other entities from DB
  entities_query<-"select distinct store_number entity_id, 'store' entity_type from stores where store_number=123"
  eq<-dbSendQuery(con, entities_query)
  eq_matrix<-as.data.frame(fetch(eq, n=-1))
  dbClearResult(eq)
  
  #truncate the eba_results table before recalculating all importances
  truncate_query<-"truncate table eba_results"
  tq<-dbSendQuery(con, truncate_query)
  dbClearResult(tq)
  
  computeImportance<-function(entity) {
    tryCatch({ 
      #entity<-c("123","store")
      entity_id<-paste("'",entity[1],"'",sep="")
      entity_type<-paste("'",entity[2],"'",sep="")
      
      tradeoff_query<-paste("select attr1, attr2, attr3, attr4, attr5, attr6, attr7, attr8 from eba_matrix where entity_id=",entity_id,"and entity_type=",entity_type,"order by attrs")
      
      tq<-dbSendQuery(con, tradeoff_query)
      eba_matrix<-as.data.frame(fetch(tq, n=-1))
      row.names(eba_matrix)<-c("attr1","attr2", "attr3", "attr4", "attr5", "attr6", "attr7", "attr8")
      
      btl <- eba(eba_matrix)
      importance<-as.data.frame(uscale(btl)*100)
      names(importance)[1] <- "importance"
      
      entity_ids<-c(entity[1], entity[1], entity[1], entity[1], entity[1], entity[1], entity[1], entity[1])
      entity_types<-c(entity[2], entity[2], entity[2], entity[2], entity[2], entity[2], entity[2], entity[2])
      attrs<-c("attr1","attr2","attr3","attr4","attr5","attr6","attr7","attr8")
      
      table_prep<-as.data.frame(cbind(entity_ids, entity_types, attrs, importance))
      
      dbWriteTable(con, "eba_results", table_prep, row.names=F, append=T);
      
      dbClearResult(tq)
    },
    error = function(cond) {
      message("can't process")
    })
  }
  
  apply(eq_matrix, 1, computeImportance)
  
  dbDisconnect(con)
}