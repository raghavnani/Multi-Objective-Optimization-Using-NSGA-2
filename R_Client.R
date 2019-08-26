#'@description get observations of the functions returned
#'@author Case study team
#'@param input: give dataframe of observations to be passed to function
#'@param func: either 1 or 2. There are two functions that need to be optimised
#'@param endpoint: pass api-test2D, api-test3D for testing purposes. Api only for production environment
#'@return target variables for given observations 
apirequest = function(input, func, endpoint, base, token){
  #if(endpoint=="api"){
   # return("Access denied! :)")
   #}
  input_intermediate = 1:nrow(input)
  for(i in 1:nrow(input)){
    input_intermediate[i]=paste0(input[i,],collapse = ",")
  }

  data=paste0(input_intermediate, collapse =";")
  
  print(paste0("requesting ", nrow(input), " observations..."))
  call= paste(base,"/",endpoint,"/",func,"/",token,"/",data,sep="")
  
  #API-Request
  data_raw=GET(call)
  
  #extracting content from API response, convert to JSON
  data_json=content(data_raw,"text")
  

  #parsing the JSON format
  data_json_parsed = fromJSON(data_json, flatten = TRUE)
  print(paste0("only ", data_json_parsed[["remaining"]], " requests left"))
  
  #Check if error occured
  if (names(data_json_parsed)[1]!= "data") {
    print(data_json)
    return(data_json_parsed)
  }
  #converting to a data frame
  data_df = as.data.frame(data_json_parsed)
  
  #Convert data to string
  data_string=as.character(data_df$data)
  
  #Replace '[' and ']'
  data_string=gsub("\\[","",data_string)
  data_string=gsub("\\]","",data_string)
  
  #Seperate data
  split=strsplit(data_string,split=", ")
  split=unlist(split)
  
  #convert to double
  data=as.double(split)
  return (data)
}