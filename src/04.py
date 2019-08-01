from classiter import parskey

hex_file = open("C:/Users/asus/Documents/8 semester/digital-summer-kcell/src/data_done.txt", "rb") 
info = [] #list of all bytes
chunk = hex_file.read(1)
while chunk:
	info.append(chunk)
	chunk = hex_file.read(1)

buff = []
nbuf = len(buff)

for tmp in info:
	if nbuf<60:
		buff.append(tmp)		
		nbuf +=	1
		
	else:
		del buff[0]
		buff.append(tmp)
		break
	#print(buff)
	buf_txt = ""
	for j in range(1, nbuf+1):
		buf_txt = buf_txt + str(buff[j-1])
	print ('buf_txt' , buf_txt)
	
	mdrk = ""
	for j in range(nbuf, nbuf - 8, -1 ):
		if j == 0:
			break
		mdrk =	buff[j-1].hex() + mdrk
	print(mdrk)	


	for keys in parskey:
		#print(keys.Key + " is " + str(keys.n))
		#if keys.n == i :
		if keys.read == 0:
			mp = keys.n
			if keys.ST == 0 :
				print(keys.Key)
				print(str(tmp,"cp1252"))
				print(keys.Key.find(str(tmp,"cp1252")))
				if keys.Key.find(str(tmp,"cp1252")) == 0 :
					keys.ST = 1
					keys.keyr = str(tmp,"cp1252")
					if keys.keyr == keys.Key :
						keys.readvalue = 1
						#Active mobi
						if i == 12 :
						
							if buf_txt.find("mobi") > 0 :
								keym(12).ofs = keym(12).ofs + 5
								keym(12).ofsbuf = keym(12).ofsbuf + 5
							else:
								keym(12).ofs = keym(12).ofs + 7
								keym(12).ofsbuf = keym(12).ofsbuf + 7
						if keys.readvalue > 0 and i == 1 : #889
							ns = 0
							nbr = 5
				  
						if keys.ubuf == 1 :
							tr = 0
							for k in range(1, 61):
								if k > len(keys.Key) + keys.ofsbuf :
									if buff(61 - k) > 29 :
										keys.value = chr(buff(61 - k)) + keys.value
										tr = 1
									elif tr == 1 :
										keys.read = 2
										keys.value = (keys.value).split()
										break
										
				
						elif (keys.const == 0) and (i < 16) :
							if keym(i + 1).ST == 0 :
								if keym(i + 1).Key.find(str(tmp,"cp1252")) == 0:
									keym(i + 1).ST = 1
									keym(i + 1).keyr = str(tmp,"cp1252")
									if keym(i + 1).keyr == keym(i + 1).Key :
										keym(i + 1).readvalue = 1
										if keym(i + 1).readvalue > 0 and i == 1 :
											ns = 0
											nbr = 5
										if keym(i + 1).ubuf == 1 :
											tr = 0
											for k in range (1,61):
												if k > len(keym(i + 1).Key) + keym(i + 1).ofsbuf :
													if buff(61 - k) > 29 :
														keym(i + 1).value = chr(buff(61 - k)) + keym(i + 1).value
														tr = 1
													elif tr == 1 :
														keym(i + 1).read = 2
														keym(i + 1).value = Trim(keym(i + 1).value)
														keys.read = 2
														keys.value = "0"
														break
						   
							 
							elif keym(i + 1).ST != 0 :
								if keym(i + 1).Key.find(keym(i + 1).keyr + str(tmp,"cp1252")) == 0 and keym(i + 1).readvalue == 0 :
									keym(i + 1).keyr = keym(i + 1).keyr + str(tmp,"cp1252")
									if keym(i + 1).keyr == keym(i + 1).Key :
										keym(i + 1).readvalue = 1
										if keym(i + 1).ubuf == 1 :
											tr = 0
											for k in range(1,61):
												if k > len(keym(i + 1).Key) + keym(i + 1).ofsbuf :
													if buff(61 - k) > 29 :
														keym(i + 1).value = chr(buff(61 - k)) + keym(i + 1).value
														tr = 1
													elif tr == 1 :
														keym(i + 1).read = 2
														keym(i + 1).value = Trim(keym(i + 1).value)
														keys.read = 2
														keys.value = "0"
														break
								  
						break
					else:
						if keys.Key.find(keys.keyr + str(tmp,"cp1252")) == 1 and keys.readvalue == 0 :
							keys.keyr = keys.keyr + str(tmp,"cp1252")
						if keys.keyr == keys.Key :
							keys.readvalue = 1
							#Active mobi
							if i == 12 :
								if buf_txt.find("mobi") > 0 :
									keym(12).ofs = keym(12).ofs + 5
									keym(12).ofsbuf = keym(12).ofsbuf + 5
								else:
									keym(12).ofs = keym(12).ofs + 7
									keym(12).ofsbuf = keym(12).ofsbuf + 7
							 
							if keys.readvalue > 0 and i == 1 :
								ns = 0
								nbr = 5
								if keys.ubuf == 1 :
									tr = 0
								for k in range(1, 61):
									if k > len(keys.Key) + keys.ofsbuf :
										if buff(61 - k) > 29 :
											keys.value = chr(buff(61 - k)) + keys.value
											tr = 1
										elif tr == 1 :
											keys.read = 2
											keys.value = Trim(keys.value)
											break
						
						elif keys.readvalue == 0 :
							keys.keyr = ""
							keys.ST = 0
							break
						else:
							if keys.readnb == 0 and str(tmp,"cp1252") == "*" :
								keys.readnb = 0
							else:
								keys.readnb = keys.readnb + 1
							if keys.readnb > keys.ofs :
								if tmp > 29 :
									keys.value = keys.value + str(tmp)
									break
								else:
									keys.read = 2
									keys.value = (keys.value).strip()
									if i == 16 :
										for kk in parskey:
											if kk.value.find(",") > 0 :
												kk.value = kk.value[0:(kk.value.find(",") + 2)]
												kk.value = kk.value.replace(";", " ")
										keym(2).value = (keym(2).value[0: len(keym(2).value) - 2])
						
										ncf = ncf + 1
										ncc = ncc + 1
										f4.write( '{0}; {1}; {2}; {3}; {4}; {5}; {6}; {7}; {8}; {9}; {10}; {11}; {12}; {13}; {14}; {15}; {16}; {17}; {18}; {19};'.format(\
										file, ncf, keym(1).value, keym(2).value, keym(3).value, keym(4).value, \
										keym(5).value, keym(6).value, keym(7).value, keym(8).value, keym(9).value, \
										keym(10).value, keym(11).value, keym(12).value, keym(13).value, keym(14).value, \
										keym(15).value, keym(16).value 
											))	
										if ncc == 100 :
											ncc = 0
											f4.close()
											f4 = open(resA, 'w+') 
								
							   
								for kk in range(1, 17):
									kk.read = 0
									kk.ST = 0
									kk.keyr = ""
									#keymb(kk.n) = kk.value
									kk.value = ""
									kk.readvalue = 0
									kk.readnb = 0
								
								keym(12).ofs = 5
								keym(12).ofsbuf = 5
								mp = 0
								
								ns = 1
							break
							
						break
	'''#def exit_cycle():#	continue
	hex_file.close()
	f6.close()
	f5.close()
	f4.close()
	pars_data4 = ncf'''