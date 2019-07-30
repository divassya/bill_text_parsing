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
						
'''


for file in files:
	hex_file = open(dir_name + file, "rb") 
	
	buff = []
	
	if len(buff)<60:
		hex_file.seek(1)	 # Go to the current position of the file
		tmp = hex_file.read(1)
		buff.append(tmp)
	else:
		for i in nbuf:
		if i<60:
			offset.remove(0)
			offset.append()
	print(offset)
	
	
# Seek can be called one of two ways:
#	x.seek(offset)
#	x.seek(offset, starting_point)
# Seek a specific position in the file and read N bytes
	
# starting_point can be 0, 1, or 2
# 0 - Default. Offset relative to beginning of file
# 1 - Start from the current position in the file
# 2 - Start from the end of a file (will require a negative offset)
'''
buf_txt = ""
for j in range(nbuf):
	buf_txt = buf_txt & str(buff(j)) #line = ser.readline().decode()
mdrk = ""
for j in range(nbuf, nbuf - 8, -1):
	if j = 0:
		Exit for
	mdr_hi = buff(j)\16
	mdr_lo = buff(j) - 16 * mdr_hi
	mdrk = hexbute(mdr_hi) & hexbute(mdr_lo) &mdrk
if (cid.readvalue() > 0 and vypiska.eadvalue() > 0 ) or (ns = 1):
	if mdrk = keydet_k:
		keydet_st = 1
		keydet_f = 0
		keydet_of = 4
		keydet_rb = 0
		keydet_ds = 0
		keydet_rd = 0
	elif (keydet_st = 1) and (keydet_f = 0) and keydet_rb = 0 :
		keydet_ns = buff(nbuf)
		keydet_rb = keydet_of + 1
		keydet_st = 0
		keydet_f = keydet_f + 1
		if keydet_f = 3:
			if Trim(keydet_rv.r02) = "1" or nfds = 1:
				#Debug.Print "Ok1"
				nfds = 1
			else:
				keydet_rv.r01 = ""
				keydet_rv.r02 = ""
				keydet_or = 0
				keydet_st = 0
				keydet_ds = 0
				keydet_f = 0
				keydet_of = 4
				'#GoTo exit_cycle
		#---
		keydet_rd = 0
		if keydet_ns = 0:
            keydet_rv.r01 = ""
            keydet_rb = 0
            keydet_f = keydet_f + 1
            #---
            if keydet_f = 3 or keydet_f = 11 or keydet_f = 13: 
				keydet_of = 4 #--5
	elif (keydet_st = 0) and (keydet_f > 0) and (keydet_f < keydet_nc + 1) and keydet_rb <= keydet_of:
		keydet_rb = keydet_rb + 1
		if keydet_rb = keydet_of + 1:
            if keydet_f > 1:
				keydet_ns = buff(nbuf)
				if keydet_f = 3 and keydet_ns < 10 and keydet_ns <> 0:
					keydet_ns = 10
					keydet_of = 5
					nofs = 1
			if keydet_ns = 0:
				select Case keydet_f
				Case 1
					keydet_rv.r01 = ""
				Case 2
					keydet_rv.r02 = ""
				Case 3
					keydet_rv.r03 = ""
				Case 4
					keydet_rv.r04 = ""
				Case 5
					keydet_rv.r05 = ""
				Case 6
					keydet_rv.r06 = ""
				Case 7
					keydet_rv.r07 = ""
				Case 8
					keydet_rv.r08 = ""
				Case 9
					keydet_rv.r09 = ""
				Case 10
					keydet_rv.r10 = ""
				Case 11
					keydet_rv.r11 = ""
				Case 12
					keydet_rv.r12 = ""
				Case 13
					keydet_rv.r13 = ""
				Case 14
					keydet_rv.r14 = ""
				end Select
				keydet_rb = 0
				keydet_f = keydet_f + 1
				#---
				if keydet_f = 3 or keydet_f = 11 or keydet_f = 13:
					keydet_of = 4 #--5
              
              #---
				if keydet_f = 3:
					if Trim(keydet_rv.r02) = "1" or nfds = 1:
                  #Debug.Print "Ok3"
						nfds = 1
					else:
						keydet_rv.r01 = ""
						keydet_rv.r02 = ""
						keydet_or = 0
						keydet_st = 0
						keydet_ds = 0
						keydet_f = 0
						keydet_of = 4
                  # GoTo exit_cycle

				keydet_rd = 0
    