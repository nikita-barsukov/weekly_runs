temporaryFile <- tempfile()
download.file("https://s3-eu-west-1.amazonaws.com/barsukov-assets/logs/clean_data.csv",
              destfile=temporaryFile, 
              method="curl")
ds <- read.csv(temporaryFile)

marathoners <- ds[(ds$distance > 40),]
marathoners$trainings <- sapply(marathoners$user_id, function(x){return(nrow(ds[ds$user_id == x,]))})
marathoners <- marathoners[marathoners$trainings > 10,]
users <- marathoners$user_id
  
dataset <- ds[ds$user_id %in% users,]
dataset <- droplevels.data.frame(dataset)
dataset$user_id <- as.factor(dataset$user_id)
dataset <- dataset[with(dataset, order(user_id, strptime(date, "%b %d, %Y %H:%M %p"))), ]
dataset$date <- strptime(dataset$date, "%b %d, %Y %H:%M %p")

cl <- by(dataset, dataset$user_id, function(x){
  m_date <- tail(x[x$distance > 40,"date"], n=1)
  x[x$date <= m_date & x$date >  m_date - 90 * 24 * 60 * 60,   ]
})
cl <- do.call(rbind, cl)

good_runners <- levels(cl$user_id)[table(cl$user_id)>1]

cl <- cl[cl$user_id %in% good_runners,]
cl <- droplevels.data.frame(cl)
