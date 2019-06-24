import os
from zipfile import ZipFile, ZIP_DEFLATED

dir_name = "C:/Users/asus/Desktop/pdl/data/" #dir to data 
files = [name for name in os.listdir(dir_name) if os.path.isfile(os.path.join(dir_name, name))] #list of files in data

file_count =  len(files) #number of files in data
print("Number of Files:", file_count)

for file in files:
	hex_file = open(dir_name + file, "rb") #open binary file (this is a path)
	print( "in open line")
	hex_file.seek(9) # skip the first 9 bytes
	print( "in seek line")
	archive = hex_file.read() # read rest
	binary_file_path = dir_name + file+'.bin'
	with open(binary_file_path, 'wb') as f:
		f.write(archive)
	print( "in read line")
	print( hex_file)
	zip_file_path = file + '.zip'
	with ZipFile(zip_file_path, 'w', ZIP_DEFLATED) as zip_file:
		zip_file.write(file+'.bin')
		# printing all the contents of the zip file 
		zip_file.printdir() 
        # extracting all the files 
		print('Extracting all the files now...') 
		zip_file.extractall() 
		print('Done!') 