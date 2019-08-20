#this is to delete 9 bytes and decompress gzip file
import os
import gzip
import shutil
#dir_name = "C:/Users/asus/Desktop/pdl/" #dir to data
#dir_name = "C:/Users/Assiya.Karatay/Desktop/digital-summer-kcell/" #dir to data
def decompress(dir_name):
	files = [name for name in os.listdir(dir_name + "data/") if os.path.isfile(os.path.join(dir_name + "data/", name))] #list of files in data

	for file in files:
		hex_file = open(dir_name + "data/" + file, "rb") #open binary file (this is a path)
		hex_file.seek(9) # skip the first 9 bytes
		archive = hex_file.read() # read rest
		zip_file_path = dir_name + "data_zip/" + file
		with open(zip_file_path, 'wb') as f:
			f.write(archive)

		#os.remove(dir_name + "data/" + file)
		binary_file_path = dir_name + "data_done/" +  file
		with gzip.open(zip_file_path, 'rb') as f_in:
			with open(binary_file_path, 'wb') as f_out:
				shutil.copyfileobj(f_in, f_out)
		os.remove(zip_file_path)
		#print (".gz is unzipped")