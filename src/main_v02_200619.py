import os
import gzip
import shutil
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
	tozip = gzip.compress(archive)
	binary_file_path = dir_name + file+'.bin'
	with open(binary_file_path, 'wb') as f:
		f.write(tozip)
	print( "in read line")
	print( hex_file)
	
	zip_file_path = file + '.gz'
	'''
	with open(binary_file_path, 'rb') as f_in:
		with gzip.open(zip_file_path, 'wb') as f_out:
			shutil.copyfileobj(f_in, f_out)
			'''
	
	with gzip.open(zip_file_path, 'rb') as f:
		file_content = f.read()
		decompressed = gzip.decompress(file_content)
		print (decompressed)
		

	binary_file_path2 = dir_name + file+'2.bin'
	with open(binary_file_path2, 'wb') as f:
		f.write(decompressed)