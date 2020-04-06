#' @include s3_service.R s3_operations.R
NULL


library(paws)
s3 <- paws::s3()
s3$list_buckets()

################################################################
#Upload Large Files
################################################################


###Parameters for Testing.
#bucket <- "name of bucket"
#file_name <- "local file path of file to be uploaded"
#key <- "file name on AWS"

#Function to Gather all Parts of the file into the format required of the complete_multipart_upload MultipartUpload Parameter.
parts_gather <- function(bucket,key,data,ID){
  multi_list <- list()
  for (i in seq_along(data)){
    multi <- s3$upload_part(Bucket = bucket, Key = key, Body = data[[i]], 
                            PartNumber = i, UploadId = ID)
    multi_list <- append(multi_list,list(list(ETag = multi$ETag,PartNumber = i)))
    i =+ 1
  }
  return(list(Parts = multi_list))
}


#Function to Break File into smaller chunks via binary.
list_gen <- function(file_name, max_file_size){
  #Break File into parts
    con <- file(file_name, "rb")
    pos <- 0
    file_size <- file.size(file_name)
    chunk_no <- 1
    data <- list()

  while(pos < file_size) #loops through and makes a 5GB chunk for each part
    {
      #Write parts to a list.
      seek(con, pos)
      data[[chunk_no]] <- readBin(con, "raw", max_file_size)
      pos  <- seek(con, 0)
      chunk_no <- chunk_no + 1
    }
  close(con)

return(data)
}



###Function to upload a 5GB+ file 
s3_uploadJT <- function(file_name,bucket_name, key, max_file_size = 5368709120){
  #browser()  
  max_file_size <- min(max_file_size, 5368709120) #File size limit as per AWS.
  if(file.size(file_name) > max_file_size){ #This function runs only if its more than 5GB
    #Break File into parts
    data <- list_gen(file_name, max_file_size)

    #Multipart
    MultipartUpload_list <- parts_gather(bucket,key,data,ID)
    
    #Complete Upload
    s3$complete_multipart_upload(Bucket = bucket, Key = key, UploadId = ID,
                                 MultipartUpload = MultipartUpload_list)
  }
  
  else{ 
    #File size is below AWS limit, proceed with put_object.
    body <- readBin(file_name, "raw", n = file.size(file_name))
    s3$put_object(
      Bucket = bucket_name,
      Body = body,
      Key = key
    )
  }
}
.s3$operations$uploadJT <- s3_uploadJT


s3_uploadJT(bucket,file_name, key)




################################################################
#Dowload Large Files
################################################################

s3_downloadJT <- function(bucket_name,key,file_name){
  #browser()  
  s3_download <- s3$get_object(
    Bucket = bucket_name,
    Key = key
  )
  s3_download_body <- s3_download$Body
  # Write output to file
  writeBin(s3_download_body, con = file_name)
}
.s3$operations$downloadJT <- s3_downloadJT

s3_downloadJT(bucket,key,file_name)