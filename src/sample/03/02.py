import os
import gzip
import shutil
import zlib
binary_file_path = "C:/Users/asus/Desktop/pdl/sample/01/ARS20190301082039.bill"
zip_file_path =	 "C:/Users/asus/Desktop/pdl/sample/02/ARS20190301082039.gz" 
'''
with open(binary_file_path, 'rb') as f_in:
	with gzip.open(zip_file_path, 'wb') as f_out:
		shutil.copyfileobj(f_in, f_out)'''



hex_file = open(binary_file_path, "rb")
content = hex_file.read()
with gzip.open(zip_file_path, 'wb') as f:
	#content2 = zlib.compressobj(content, wbits = 10)
	#compress = zlib.compressobj(zlib.Z_DEFAULT_COMPRESSION, zlib.DEFLATED, wbits = 10)  
	#compressed_data = compress.compress(content)
	compressed_data = zlib.compress(content)
	f.write(compressed_data)
print (".gz is created")

  

