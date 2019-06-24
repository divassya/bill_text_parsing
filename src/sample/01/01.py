import os
import gzip
import shutil
dir_name = "C:/Users/asus/Desktop/pdl/data/" #dir to data 
files = [name for name in os.listdir(dir_name) if os.path.isfile(os.path.join(dir_name, name))] #list of files in data

for file in files:
	hex_file = open(dir_name + file, "rb") #open binary file (this is a path)
	hex_file.seek(9) # skip the first 9 bytes
	archive = hex_file.read() # read rest
	zip_file_path = "C:/Users/asus/Desktop/pdl/sample/01/" + file
	with open(zip_file_path, 'wb') as f:
		f.write(archive)
		
	binary_file_path = "C:/Users/asus/Desktop/pdl/sample/03/ARS20190301082039unzipped4.bin"
	with gzip.open(zip_file_path, 'rb') as f_in:
		with open(binary_file_path, 'wb') as f_out:
			shutil.copyfileobj(f_in, f_out)
	print (".gz is unzipped")