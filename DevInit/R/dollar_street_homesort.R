wd <- "D:/Documents/DollarStreet/Homes"
setwd(wd)

files <- list.files(paste(wd,"Unsorted",sep="/"))
# 
# parseInc <- function(x){
#   return(as.integer(strsplit(x,"_")[[1]][1]))
# }
# 
# incomes <- sapply(files,parseInc)
# 
# medianIncome <- median(incomes)
# 
# for(image in files){
#   if(parseInc(image)<medianIncome){
#     #Copy to "Sorted/Below" folder
#     file.copy(paste(wd,"Unsorted",image,sep="/"),paste(wd,"Sorted","Below",image,sep="/"))
#   }else if(parseInc(image)>medianIncome){
#     #Copy to "Sorted/Above folder
#     file.copy(paste(wd,"Unsorted",image,sep="/"),paste(wd,"Sorted","Above",image,sep="/"))
#   }
# }

library('jpeg')

# load img data
setwd(paste(wd,"Unsorted",sep="/"))
file_list <- list.files(".",pattern="jpg")
data <- lapply(file_list, readJPEG)
# extract subject id + img nr from names
subject_ids <- lapply(file_list, function(file_name) as.numeric(unlist(strsplit(file_name, "_"))[1]))
# rename subject id's to c1 and c2 for more clear displaying of results
subject_ids[subject_ids==0]='c1'
subject_ids[subject_ids!='c1']='c2'
img_ids <- lapply(file_list, function(file_name) as.numeric(unlist(strsplit(unlist(strsplit(file_name, "_"))[2], "\\."))[1]))

# specify which data should be used as test and train by the img nrs
train_test_border <- 7
# split data into train and test, and bring into array form to feed to svm
train_in <- t(array(unlist(data[img_ids < train_test_border]), dim=c(length(unlist(data[1])),sum(img_ids < train_test_border))))
train_out <- unlist(subject_ids[img_ids < train_test_border])
test_in <- t(array(unlist(data[img_ids >= train_test_border]), dim=c(length(unlist(data[1])),sum(img_ids >= train_test_border))))
test_out <- unlist(subject_ids[img_ids >= train_test_border])

# train svm - try out different kernels + settings here
svm_model <- svm(train_in, train_out, type='C', kernel='linear')

# evaluate svm
p <- predict(svm_model, train_in)
print(p)
print(table(p, train_out))
p <- predict(svm_model, test_in)
print(p)
print(table(p, test_out))

print('svm demo done!')