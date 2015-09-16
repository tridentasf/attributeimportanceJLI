library("eba")
library("RPostgreSQL")

computeAttributeImportance <- function(dbname1, user1, password1, host1, port1) {
  drv<-dbDriver("PostgreSQL")

  con<-dbConnect(drv, dbname="JLI", user="postgres", password="postgres", host="localhost", port="5432")
  #con<-dbConnect(drv, dbname=dbname1, user=user1, password=password1, host=host1, port=port1)
  
  #read out the list of stores from DB
  store_query<-"select distinct entity_group, entity, district, adi, store_type, store_number from eba_matrix_store"
  sq<-dbSendQuery(con, store_query)
  sq_matrix<-as.data.frame(fetch(sq, n=-1))
  dbClearResult(sq)
  
  #read out the list of storetypes from DB
  type_query<-"select distinct entity_group, entity, district, adi, store_type, store_number from eba_matrix_type"
  tq<-dbSendQuery(con, type_query)
  tq_matrix<-as.data.frame(fetch(tq, n=-1))
  dbClearResult(tq)
  
  #read out the list of adi from DB
  adi_query<-"select distinct entity_group, entity, district, adi, store_type, store_number from eba_matrix_adi"
  aq<-dbSendQuery(con, adi_query)
  aq_matrix<-as.data.frame(fetch(aq, n=-1))
  dbClearResult(aq)
  
  #read out the list of district from DB
  district_query<-"select distinct entity_group, entity, district, adi, store_type, store_number from eba_matrix_district"
  dq<-dbSendQuery(con, district_query)
  dq_matrix<-as.data.frame(fetch(dq, n=-1))
  dbClearResult(dq)
  
  #read out the list of entity from DB
  entity_query<-"select distinct entity_group, entity, district, adi, store_type, store_number from eba_matrix_entity"
  eq<-dbSendQuery(con, entity_query)
  eq_matrix<-as.data.frame(fetch(eq, n=-1))
  dbClearResult(eq)
  
  #read out the list of entity_group from DB
  entity_group_query<-"select distinct entity_group, entity, district, adi, store_type, store_number from eba_matrix_entity_group"
  egq<-dbSendQuery(con, entity_group_query)
  egq_matrix<-as.data.frame(fetch(egq, n=-1))
  dbClearResult(egq)
  
  #read out the list of pilot_types from DB
  pilot_type_query<-"select distinct entity_group, entity, district, adi, store_type, store_number from eba_matrix_pilot_type"
  ptq<-dbSendQuery(con, pilot_type_query)
  ptq_matrix<-as.data.frame(fetch(ptq, n=-1))
  dbClearResult(ptq)
  
  #read out the list of pilot from DB
  pilot_query<-"select distinct entity_group, entity, district, adi, store_type, store_number from eba_matrix_pilot"
  pq<-dbSendQuery(con, pilot_query)
  pq_matrix<-as.data.frame(fetch(pq, n=-1))
  dbClearResult(pq)
  
  #truncate the eba_results table before recalculating all importances
  truncate_query<-"truncate table eba_results"
  tq<-dbSendQuery(con, truncate_query)
  dbClearResult(tq)
  
  computeImportance<-function(record) {
    tryCatch({ 
      entity_group<-paste("'",record[1],"'",sep="")
      entity<-paste("'",record[2],"'",sep="")
      district<-paste("'",record[3],"'",sep="")
      adi<-paste("'",record[4],"'",sep="")
      store_type<-paste("'",record[5],"'",sep="")
      store_number<-paste("'",record[6],"'",sep="")
      
      level<-"eba_matrix_store";
      if (adi != "'N/A'" && store_type != "'N/A'" && store_number == "'N/A'")
        level<-"eba_matrix_type"
      else if (adi !="'N/A'" && store_type == "'N/A'" && store_number == "'N/A'")
        level<-"eba_matrix_adi"
      else if (district !="'N/A'" && adi =="'N/A'" && store_type == "'N/A'" && store_number == "'N/A'")
        level<-"eba_matrix_district"
      else if (entity != "'N/A'" && district =="'N/A'" && adi =="'N/A'" && store_type == "'N/A'" && store_number == "'N/A'")
        level<-"eba_matrix_entity"
      else if (entity_group != "'N/A'" && entity == "'N/A'" && district =="'N/A'" && adi =="'N/A'" && store_type == "'N/A'" && store_number == "'N/A'")
        level<-"eba_matrix_entity_group"
      else if (entity_group == "'N/A'" && entity == "'N/A'" && district =="'N/A'" && adi =="'N/A'" && store_type != "'N/A'" && store_number == "'N/A'")
        level<-"eba_matrix_pilot_type"
      else if (entity_group == "'N/A'" && entity == "'N/A'" && district =="'N/A'" && adi =="'N/A'" && store_type == "'N/A'" && store_number == "'N/A'")
        level<-"eba_matrix_pilot"
      
      tradeoff_query<-paste("select attr1, attr2, attr3, attr4, attr5, attr6, attr7, attr8 from",level,"where entity_group=",entity_group,"and entity=",entity,"and district=",district,"and adi=",adi,"and store_type=",store_type,"and store_number=",store_number,"order by attrs")
      
      tq<-dbSendQuery(con, tradeoff_query)
      eba_matrix<-as.data.frame(fetch(tq, n=-1))
      row.names(eba_matrix)<-c("attr1","attr2", "attr3", "attr4", "attr5", "attr6", "attr7", "attr8")
      
      btl <- eba(eba_matrix)
      importance<-as.data.frame(uscale(btl)*100)
      names(importance)[1] <- "importance"
      
      entity_group_c<-c(record[1], record[1], record[1], record[1], record[1], record[1], record[1], record[1])
      entity_c<-c(record[2], record[2], record[2], record[2], record[2], record[2], record[2], record[2])
      district_c<-c(record[3], record[3], record[3], record[3], record[3], record[3], record[3], record[3])
      adi_c<-c(record[4], record[4], record[4], record[4], record[4], record[4], record[4], record[4])
      store_type_c<-c(record[5], record[5], record[5], record[5], record[5], record[5], record[5], record[5])
      store_number_c<-c(record[6], record[6], record[6], record[6], record[6], record[6], record[6], record[6])
      attrs<-c("attr1","attr2","attr3","attr4","attr5","attr6","attr7","attr8")
      
      table_prep<-as.data.frame(cbind(entity_group_c, entity_c, district_c, adi_c, store_type_c, store_number_c, attrs, importance))
      
      dbWriteTable(con, "eba_results", table_prep, row.names=F, append=T);
      
      dbClearResult(tq);
    },
    error = function(cond) {
      message("can't process")
    })
  }
  
  apply(pq_matrix, 1, computeImportance)
  apply(ptq_matrix, 1, computeImportance)
  apply(egq_matrix, 1, computeImportance)
  apply(sq_matrix, 1, computeImportance)
  
  dbDisconnect(con)
}