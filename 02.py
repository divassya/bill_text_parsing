import os
import gzip
import shutil
dir_name = "C:/Users/asus/Desktop/pdl/data_done/" #dir to data 
files = [name for name in os.listdir(dir_name) if os.path.isfile(os.path.join(dir_name, name))] #list of files in data


for file in files:
	hex_file = open(dir_name + file, "rb") 
	# Seek a specific position in the file and read N bytes
	hex_file.seek(0, 0)	 # Go to beginning of the file
	offs = hex_file.read(60)
	print(offs)
	
	
# Seek can be called one of two ways:
#	x.seek(offset)
#	x.seek(offset, starting_point)

# starting_point can be 0, 1, or 2
# 0 - Default. Offset relative to beginning of file
# 1 - Start from the current position in the file
# 2 - Start from the end of a file (will require a negative offset)
