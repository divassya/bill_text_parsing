import os
import gzip
import shutil

dir_name = "C:/Users/asus/Desktop/pdl/data/" #dir to data 
files = [name for name in os.listdir(dir_name) if os.path.isfile(os.path.join(dir_name, name))] #list of files in data

zip_file_path =  "C:/Users/asus/Desktop/pdl/sample/01/ARS20190301082039.bill" 
#zip_file_path2 = "C:/Users/asus/Desktop/pdl/sample/02/ARS20190301082039.gz" 
binary_file_path = "C:/Users/asus/Desktop/pdl/sample/03/ARS20190301082039unzipped4.bin"
with gzip.open(zip_file_path, 'rb') as f_in:
    with open(binary_file_path, 'wb') as f_out:
        shutil.copyfileobj(f_in, f_out)
print (".gz is unzipped")