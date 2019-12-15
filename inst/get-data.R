# Aim: get up-to-date data from upthat releases

pb_file_list = piggyback::pb_list()
for(i in 1:nrow(pb_file_list)) {
  message("Trying to get ", pb_file_list$file_name[i])
  piggyback::pb_download(pb_file_list$file_name[i])
}
