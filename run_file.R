library(xaringanBuilder)
library(intraday)
library(telegram)

reporting_process <- function(file_name="intraday_presentation.Rmd",the_date=lubridate::today()-1,audio_file_path="background_music/sample1.mp3",telegram_params){
  
  price_df_raw <- readr::read_csv(system.file("market-data/price_data.csv",package="intraday"),locale=readr::locale(tz="Turkey"))
  
  if(lubridate::as_date(price_df_raw$date[1],tz="Turkey") != the_date){
    simpleError("Wrong report date")
  }
  
  base_name <- tools::file_path_sans_ext(file_name)
  
  rmarkdown::render(normalizePath(file_name),params=list(data_date=the_date))
  
  build_png(paste0(base_name,".html"),slides="all",keep_intermediates=TRUE)
  
  dir.create(tmp <- tempfile())
  zip::unzip(paste0(base_name,".zip"),exdir=tmp)
  
  file_vec <- dir(tmp,full.names = TRUE)
  
  av::av_encode_video(input=file_vec[order(as.integer(gsub(paste0(".*",base_name,"_|\\.png+$"),"",file_vec)))],audio=audio_file_path,output=paste0(base_name,".mp4"),framerate=0.3,vfilter="scale=trunc(iw/2)*2:trunc(ih/2)*2")
  
  tbot <- TGBot$new(token = telegram_params$bot_token)
  
  print("Sending message to Telegram")
  
  video_duration <- av::av_media_info(paste0(base_name,".mp4"))$duration
  closeAllConnections()
  print(paste0("Video duration: ",video_duration))
  for(chat_id in telegram_params$chat_id){
    print(paste0("Chat ID: ",telegram_params$chat_id))
    try(tbot$sendMessage("Hello",chat_id = chat_id))
    
    try(tbot$sendDocument(normalizePath(paste0(base_name,".pdf")),chat_id = chat_id))
    try(tbot$sendVideo(normalizePath(paste0(base_name,".mp4")),duration=video_duration,caption=paste0("Gün İçi Piyasası Videosu (",the_date,")"),chat_id = chat_id))
  }
  
  print("End of sending message to Telegram")
  
  
  file.remove(paste0(base_name,".zip"))
  try(file.remove("index.html"))
  try(file.rename("intraday_presentation.html","index.html"))
  
}

send_msg_via_telegram <- function(the_message="Your message here.",api_key="",chat_id="",oops_type=""){
  telegram_url <- "https://api.telegram.org/bot"
  full_request <- paste0(telegram_url,api_key,"/sendMessage?chat_id=",chat_id,"&text=",the_message)
  tryCatch(
    httr::GET(full_request),
    error = function(e){
      print(paste0("OOPS. Could not process Telegram message. Type: ",oops_type," Error is ",e, "Message is: ",the_message))
    }
  )
}

send_document_via_telegram <- function(document_path,api_key="",chat_id="",oops_type=""){
  telegram_url <- "https://api.telegram.org/bot"
  the_document <- httr::upload_file(document_path,type="multipart/form-data")
  full_request <- paste0(telegram_url,api_key,"/sendDocument")
  the_handle <- handle('')
  tryCatch(
    httr::POST(full_request,encode ="multipart",body=list(chat_id=chat_id,document=the_document),handle=the_handle,timeout(60),config(http_version=1.1)),
    error = function(e){
      print(paste0("OOPS. Could not process Telegram message. Type: ",oops_type," Error is ",e))
    }
  )
  handle_reset(the_handle$url)
  rm(the_handle)
}



