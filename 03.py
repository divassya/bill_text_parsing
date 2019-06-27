hex_file = open("C:/Users/asus/Desktop/pdl/data_done/ARS20190301082039.bill.bin", "rb") 

info = [] #list of all bytes
chunk = hex_file.read(1)
while chunk:
	info.append(chunk)
	chunk = hex_file.read(1)
print(len(info))
info_cut = info[:65]
buff = []
nbuf = len(buff)

for tmp in info_cut:
	if nbuf<60:
		buff.append(tmp)		
		nbuf +=	1
	else:
		del buff[0]
		buff.append(tmp)
		print(buff)
	print(tmp)

