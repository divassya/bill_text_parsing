from classiter import parskey, row_det
from  select_cases import select_case_1, add_tmp, select_case_3, select_case_4
from copy import deepcopy
from decompress import decompress
import os

dir_name = "C:/Users/Assiya.Karatay/Desktop/digital-summer-kcell/" #dir to data
#hex_file = open("C:/Users/Assiya.Karatay/Desktop/digital-summer-kcell/data_done/ARS20190502035519.BILL.bin", "rb")
decompress(dir_name)
files = [name for name in os.listdir(dir_name + "data_done/") if os.path.isfile(os.path.join(dir_name + "data_done/", name))] #list of files in data_done

for current_file in files:

	#current_file is the name of file, file is the opened io reader
	a = dir_name + "data_done/" + current_file
	file = open(a , "rb")
	#keydet
	keydet_k = "068403010003030000" #"062003050003030000"
	keydet_kr = ""
	keydet_st = 0
	keydet_nc = 14
	keydet_of = 4 # 5
	keydet_f = 0
	keydet_ds = 0
	keydet_or = 0
	#keydets
	keydets_k = "068403010003030000" #"062003040003030000"
	keydets_kr = ""
	keydets_k2 = "Итого в учетном периоде начислено:"
	keydets_k2r = ""
	keydets_st = 0
	keydets_nc = 10
	keydets_of = 5
	keydets_r = 0
	keydets_f = 0
	keydets_or = 0
	keydets_ds = 0
	keydets_ns = 0
	keydet_rd = 0
	#
	ncf = 0
	ncc = 0
	ncca = 0
	nccb = 0
	nbr = 0
	mask = ""
	pim = 1
	mskp = ""
	mskn = 0
	mp = 0

	mdrn = 0
	keydet_ns = 0
	res = dir_name + "data_res/" +	current_file+ ".txt"
	resA = dir_name + "data_res/" +	 current_file + "A.txt"
	resB = dir_name + "data_res/" +	 current_file + "B.txt"
	f4 = open(res, 'w+')
	f5 = open(resA, 'w+')
	f6 = open(resB, 'w+')
	info = [] #list of all bytes
	chunk = file.read(1)
	while chunk:
		info.append(chunk)
		chunk = file.read(1)

	buff = [b'\0'] * 60
	keymb = ['']*17
	nbuf=0
	ns = 0
	nfds = 0
	nofs = 0


	itr = iter(info)
	keym = [key for key in parskey]
	row_dets = [row for row in row_det]
	mdri = row_dets[0]
	keydet_rv =  row_dets[1]
	keydets_rv =  row_dets[2]
	mdr = [mdri]*1
	for tmp in info:

		if nbuf < 60:
			buff[nbuf] = tmp
			nbuf +=	1
		else:
			del buff[0]
			buff.append(tmp)

		buf_txt = ""
		for j in range(nbuf):
			buf_txt = buf_txt + str(buff[j], "cp1251",'ignore')

		mdrk = ""
		for j in range(nbuf-1, nbuf -10, -1 ):
			#print(j)
			if j == 0:
				break
			mdrk =	buff[j].hex() + mdrk
		mdrk = mdrk.upper()
		#print(mdrk) ""323130393334303120""
		if (keym[1].readvalue > 0 and keym[6].readvalue > 0) or (ns == 1):
			if mdrk == keydet_k:
				keydet_st = 1
				keydet_f = 0
				keydet_of = 4
				keydet_rb = 0
				keydet_ds = 0
				keydet_rd = 0
			elif (keydet_st == 1) and (keydet_f == 0) and keydet_rb == 0:
				keydet_ns = ord(buff[nbuf-1])
				keydet_rb = keydet_of + 1
				keydet_st = 0
				keydet_f = keydet_f + 1
				if keydet_f == 3:
					if keydet_rv.r02.split() == "1" or nfds == 1:
						# Debug.Print "Ok1"
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
				# ---
				keydet_rd = 0
				if keydet_ns == 0:
					keydet_rv.r01 = ""
					keydet_rb = 0
					keydet_f = keydet_f + 1
					# ---
					if keydet_f == 3 or keydet_f == 11 or keydet_f == 13:
						keydet_of = 4  # --5
			elif (keydet_st == 0) and (keydet_f > 0) and (keydet_f < keydet_nc + 1) and keydet_rb <= keydet_of:
				keydet_rb = keydet_rb + 1
				if keydet_rb == keydet_of + 1:
					if keydet_f > 1:
						keydet_ns = ord(buff[nbuf-1])
						if keydet_f == 3 and keydet_ns < 10 and keydet_ns != 0:
							keydet_ns = 10
							keydet_of = 5
							nofs = 1
					if keydet_ns == 0:
						select_case_1(keydet_f)

						keydet_rb = 0
						keydet_f = keydet_f + 1
						# ---
						if keydet_f == 3 or keydet_f == 11 or keydet_f == 13:
							keydet_of = 4  # --5

						# ---
						if keydet_f == 3:
							if (keydet_rv.r02).split() == "1" or nfds == 1:
								# Debug.Print "Ok3"
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
			elif (keydet_st == 0) and (keydet_f > 0) and (keydet_f < keydet_nc + 1) and (keydet_rb >= keydet_of) and (keydet_rd < keydet_ns):
				keydet_rd = keydet_rd + 1
				add_tmp(keydet_rv, keydet_f, tmp) #470

				if keydet_rd == keydet_ns:
					keydet_rb = 0
					keydet_f = keydet_f + 1
					keydet_of = 4
					# ---
					if (keydet_f == 3 or keydet_f == 11 or keydet_f == 13) and nofs == 1:
						keydet_of = 5  # --5--01

					# ---IsNumeric
					if keydet_f == 3:  # 509line
						if keydet_rv.r02.strip() == "1" \
								or (nfds == 1 and (keydet_rv.r02.strip()).isnumeric()):
							# Debug.Print "Ok2"
							nfds = 1
						else:
							keydet_rv.r01 = ""
							keydet_rv.r02 = ""
							keydet_rv.r03 = ""
							keydet_rv.r04 = ""
							keydet_rv.r05 = ""
							keydet_rv.r06 = ""
							keydet_rv.r07 = ""
							keydet_rv.r08 = ""
							keydet_rv.r09 = ""
							keydet_rv.r10 = ""
							keydet_rv.r11 = ""
							keydet_rv.r12 = ""
							keydet_rv.r13 = ""
							keydet_rv.r14 = ""
							keydet_or = 0
							keydet_st = 0
							keydet_ds = 0
							keydet_f = 0
							keydet_of = 4
					keydet_rd = 0

			elif (keydet_f == keydet_nc + 1):
				mdrn = mdrn + 1
				# ReDim Preserve mdr(mdrn)

				i = deepcopy(keydet_rv)
				mdr.append(i)
				# keydet_rv.r14 = Left(keydet_rv.r14, keydet_ns)
				#mdr[mdrn] = keydet_rv


				keydet_rv.r01 = ""
				keydet_rv.r02 = ""
				keydet_rv.r03 = ""
				keydet_rv.r04 = ""
				keydet_rv.r05 = ""
				keydet_rv.r06 = ""
				keydet_rv.r07 = ""
				keydet_rv.r08 = ""
				keydet_rv.r09 = ""
				keydet_rv.r10 = ""
				keydet_rv.r11 = ""
				keydet_rv.r12 = ""
				keydet_rv.r13 = ""
				keydet_rv.r14 = ""
				keydet_or = 0
				keydet_st = 0
				keydet_ds = 0
				keydet_f = 0
				keydet_of = 4

		# -----
		if nfds == 1 and (keym[1].readvalue > 0 and keym[6].readvalue > 0 ):
			nfds = 0
			nofs = 0
		if (keym[1].readvalue > 0 and keym[6].readvalue > 0) or (ns == 1):
			if (mdrk == keydets_k) and (keydets_st == 0) and (nfds == 1):
				keydets_st = 1  # ???

			elif (keydets_st == 1) and (mdrk == keydet_k) and keym[1].readvalue > 0:
				keydets_st = 0
			if (keydets_st == 1):
				if (keydets_k2.find(str(tmp, "cp1251", 'ignore'), 0) == 0) and (keydets_k2r == "") and keydets_r == 0:
					keydets_k2r = str(tmp, "cp1251", 'ignore')
				elif (keydets_k2.find(keydets_k2r + str(tmp, "cp1251", 'ignore'), 0) == 0) and (keydets_k2r != "") and keydets_r == 0:
					keydets_k2r = keydets_k2r + str(tmp, "cp1251", 'ignore')
					if keydets_k2r == keydets_k2 and keydets_r == 0:
						keydets_r = 1
						# keydets_kr = "OK"
						keydets_ds = 1
						keydets_k2r = ""
						for k in range(1, mdrn+1):
							ncca = ncca + 1
							mdr[k].r01 = mdr[k].r01.replace(";", " ")
							mdr[k].r02 = mdr[k].r02.replace(";", " ")
							mdr[k].r03 = mdr[k].r03.replace(";", " ")
							mdr[k].r04 = mdr[k].r04.replace(";", " ")
							mdr[k].r05 = mdr[k].r05.replace(";", " ")
							mdr[k].r06 = mdr[k].r06.replace(";", " ")
							mdr[k].r07 = mdr[k].r07.replace(";", " ")
							mdr[k].r08 = mdr[k].r08.replace(";", " ")
							mdr[k].r09 = mdr[k].r09.replace(";", " ")
							mdr[k].r10 = mdr[k].r10.replace(";", " ")
							mdr[k].r11 = mdr[k].r11.replace(";", " ")
							mdr[k].r12 = mdr[k].r12.replace(";", " ")
							mdr[k].r13 = mdr[k].r13.replace(";", " ")
							mdr[k].r14 = mdr[k].r14.replace(";", " ")

							f5.write('{0};{1};{2};{3};{4};{5};{6};{7};{8};{9};{10};{11};{12};{13};{14};{15};{16};{17};{18};{19};\n'.format(
								current_file, ncf, keymb[2], keymb[3], keymb[4], keymb[5],
								mdr[k].r01, mdr[k].r02, mdr[k].r03, mdr[k].r04, mdr[k].r05,
								mdr[k].r06,	mdr[k].r07, mdr[k].r08, mdr[k].r09, mdr[k].r10,
								mdr[k].r11, mdr[k].r12, mdr[k].r13, mdr[k].r14))

							if ncca == 100:
								ncca = 0
								f5.close()
								f5 = open(resA, 'a+')



						mdrn = 0
						mdr = [mdri] * 1
						#mdr = []
						#mdr.append(mdri)
				elif (keydets_k2.find(keydets_k2r + str(tmp, "cp1251", 'ignore'), 1) != 0) and (keydets_k2r != "") and keydets_r == 0:
						keydets_k2r = ""
				elif (keydets_r == 1) and (keydets_f < keydets_nc):
					if ord(tmp) > 29 and keydets_ds == 0 and keydets_or == 0:
						keydets_ds = 1
						select_case_3(keydets_rv, keydets_f, keydets_ns) #630
						keydets_ns = ord(buff[nbuf-1])
						keydets_f = keydets_f + 1
						select_case_4(keydets_rv, keydets_f, tmp) #654
					elif ord(tmp) > 29 and keydets_ds == 1 and keydets_or == 0:
						add_tmp(keydets_rv, keydets_f, tmp)
					elif (ord(tmp) < 30) and (keydets_ds == 1) and (keydets_or == 0):
						keydets_or = 1
						keydets_ds = 0
					elif (keydets_ds == 0) and (keydets_or > 0) and (keydets_or < keydets_of):
						keydets_or = keydets_or + 1
						if keydets_or == keydets_of:
							if keydets_f == 1:
								keydets_rv.r01 = keydets_rv.r01[0:keydets_ns - 1]

							keydets_ns = ord(tmp)
							keydets_or = 0
							keydets_ds = 0

				elif (keydets_f == keydets_nc) and (keydets_or == 0) and (ord(tmp) > 29) and (keydets_ds == 1):
					add_tmp(keydets_rv, keydets_f, tmp)
				elif (keydets_f == keydets_nc) and (keydets_or == 0) and (ord(tmp)< 30):
					keydets_or = 1
				elif (keydets_f == keydets_nc) and (keydets_or < keydets_of) and (keydets_or > 0):
					keydets_or = keydets_or + 1
				elif (keydets_f == keydets_nc) and (keydets_or == keydets_of):
					keydets_rv.r10 = keydets_rv.r10[0: keydets_ns - 1]
					nccb = nccb + 1
					keydets_rv.r01 = keydets_rv.r01.replace(";", " ")
					keydets_rv.r02 = keydets_rv.r02.replace(";", " ")
					keydets_rv.r03 = keydets_rv.r03.replace(";", " ")
					keydets_rv.r04 = keydets_rv.r04.replace(";", " ")
					keydets_rv.r05 = keydets_rv.r05.replace(";", " ")
					keydets_rv.r06 = keydets_rv.r06.replace(";", " ")
					keydets_rv.r07 = keydets_rv.r07.replace(";", " ")
					keydets_rv.r08 = keydets_rv.r08.replace(";", " ")
					keydets_rv.r09 = keydets_rv.r09.replace(";", " ")
					keydets_rv.r10 = keydets_rv.r10.replace(";", " ")
					f6.write('{0};{1};{2};{3};{4};{5};{6};{7};{8};{9};{10};{11};{12};{13};{14};{15};\n'.format(	current_file, ncf, keymb[2], keymb[3], keymb[4], keymb[5],keydets_rv.r01, keydets_rv.r02, keydets_rv.r03, keydets_rv.r04, keydets_rv.r05,keydets_rv.r06, keydets_rv.r07, keydets_rv.r08, keydets_rv.r09, keydets_rv.r10))
					if nccb == 100:
						nccb = 0
						f6.close()
						f6 = open(resB, 'a+')
					keydet_rv.r01 = ""
					keydet_rv.r02 = ""
					keydet_rv.r03 = ""
					keydet_rv.r04 = ""
					keydet_rv.r05 = ""
					keydet_rv.r06 = ""
					keydet_rv.r07 = ""
					keydet_rv.r08 = ""
					keydet_rv.r09 = ""
					keydet_rv.r10 = ""
					keydet_rv.r11 = ""
					keydet_rv.r12 = ""
					keydet_rv.r13 = ""
					keydet_rv.r14 = ""
					keydet_or = 0
					keydet_st = 0
					keydet_ds = 0
					keydet_f = 0
					keydet_of = 4
						#
					keydets_or = 0
					keydets_st = 0
					keydets_ds = 0
					keydets_f = 0
					keydets_r = 0
					keydets_kr = "OK"
					keydets_rv.r01 = ""
					keydets_rv.r02 = ""
					keydets_rv.r03 = ""
					keydets_rv.r04 = ""
					keydets_rv.r05 = ""
					keydets_rv.r06 = ""
					keydets_rv.r07 = ""
					keydets_rv.r08 = ""
					keydets_rv.r09 = ""
					keydets_rv.r10 = ""
			if keydets_kr == "OK":
				keydets_kr = ""

		for i in range(1,17):

			if keym[i].n == i:
				if keym[i].read == 0:
					mp = i
					if keym[i].ST == 0 :
						if (keym[i].Key).find(str(tmp,"cp1251",'ignore')) == 0 :
							keym[i].ST = 1
							keym[i].keyr = str(tmp, "cp1251", 'ignore')
							if keym[i].keyr == keym[i].Key :
								keym[i].readvalue = 1
								#Active mobi
								if i == 12 :
									if buf_txt.find("mobi") > 0 :
										keym[12].ofs = keym[12].ofs + 5
										keym[12].ofsbuf = keym[12].ofsbuf + 5
									else:
										keym[12].ofs = keym[12].ofs + 7
										keym[12].ofsbuf = keym[12].ofsbuf + 7
								if keym[i].readvalue > 0 and i == 1 : #889
									ns = 0
									nbr = 5

								if keym[i].ubuf == 1 :
									tr = 0
									for k in range(1, 61):
										if k > len(keym[i].Key) + keym[i].ofsbuf :
											if ord(buff[61 - k]) > 29 :
												keym[i].value = str(buff[61 - k],"cp1252",'ignore')+ keym[i].value
												tr = 1
											elif tr == 1 :
												keym[i].read = 2
												keym[i].value = (keym[i].value).split()
												break


						elif (keym[i].const == 0) and (i < 16) :
							if keym[i + 1].ST == 0:
								if keym[i + 1].Key.find(str(tmp,"cp1251", 'ignore')) == 0:
									keym[i + 1].ST = 1
									keym[i + 1].keyr = str(tmp,"cp1251",'ignore')
									if keym[i + 1].keyr == keym[i + 1].Key :
										keym[i + 1].readvalue = 1
										if keym[i + 1].readvalue > 0 and i == 1 :
											ns = 0
											nbr = 5
										if keym[i + 1].ubuf == 1 :
											tr = 0
											for k in range (1,61):
												if k > len(keym[i + 1].Key) + keym[i + 1].ofsbuf :
													if ord(buff[61 - k]) > 29 :
														keym[i + 1].value = str(buff[61 - k],"cp1251",'ignore')  + keym[i + 1].value
														tr = 1
													elif tr == 1 :
														keym[i + 1].read = 2
														keym[i + 1].value = keym[i + 1].value.split()
														keym[i].read = 2
														keym[i].value = "0"
														break


							elif keym[i + 1].ST != 0 :
								if keym[i + 1].Key.find(keym[i + 1].keyr + str(tmp,"cp1251",'ignore')) == 0 and keym[i + 1].readvalue == 0 :
									keym[i + 1].keyr = keym[i + 1].keyr + str(tmp,"cp1251",'ignore')
									if keym[i + 1].keyr == keym[i + 1].Key :
										keym[i + 1].readvalue = 1
										if keym[i + 1].ubuf == 1 :
											tr = 0
											for k in range(1,61):
												if k > len(keym[i + 1].Key) + keym[i + 1].ofsbuf :
													if ord(buff[61 - k]) > 29 :
														keym[i + 1].value = str(buff[61 - k],"cp1251",'ignore')+ keym[i + 1].value
														tr = 1
													elif tr == 1 :
														keym[i + 1].read = 2
														keym[i + 1].value = keym[i + 1].value.split()
														keym[i].read = 2
														keym[i].value = "0"
														break

						break
					else:
						if keym[i].Key.find(keym[i].keyr + str(tmp,"cp1251",'ignore')) == 0 and keym[i].readvalue == 0 :
							keym[i].keyr = keym[i].keyr + str(tmp,"cp1251",'ignore')
							if keym[i].keyr == keym[i].Key :
								keym[i].readvalue = 1
								#Active mobi
								if i == 12 :
									if buf_txt.find("mobi") > 0 :
										keym[12].ofs = keym[12].ofs + 5
										keym[12].ofsbuf = keym[12].ofsbuf + 5
									else:
										keym[12].ofs = keym[12].ofs + 7
										keym[12].ofsbuf = keym[12].ofsbuf + 7

								if keym[i].readvalue > 0 and i == 1 :
									ns = 0
									nbr = 5
								if keym[i].ubuf == 1 :
									tr = 0
									for k in range(1, 61):
										if k > (len(keym[i].Key) + keym[i].ofsbuf) :
											if ord(buff[61 - k]) > 29 :
												keym[i].value = str(buff[61 - k],"cp1251",'ignore') + keym[i].value
												tr = 1
											elif tr == 1 :
												keym[i].read = 2
												keym[i].value = keym[i].value.strip()
												break

						elif keym[i].readvalue == 0 :
							keym[i].keyr = ""
							keym[i].ST = 0
							break
						else:
							if keym[i].readnb == 0 and str(tmp,"cp1251",'ignore') == "*" :
								keym[i].readnb = 0
							else:
								keym[i].readnb = keym[i].readnb + 1
							if keym[i].readnb > keym[i].ofs :
								if ord(tmp) > 29 :
									keym[i].value = keym[i].value + str(tmp, "cp1251", "ignore")
									break
								else:
									keym[i].read = 2
									keym[i].value = (keym[i].value).strip()
									if i == 4:
										keym[i].value = str(buff[nbuf - len(keym[i].value)-1], "cp1251", "ignore") + keym[i].value
									if i == 16 :

										for k in range(1,17):
											if type(keym[k].value) == list:
												keym[k].value = ''.join((keym[k].value))
											if keym[k].value.find(",") > 0 :
												keym[k].value = keym[k].value[0:(keym[k].value.find(",") + 3)]
												keym[k].value = keym[k].value.replace(";", " ")

										keym[2].value = (keym[2].value[0: len(keym[2].value) - 1])

										ncf = ncf + 1
										ncc = ncc + 1
										f4.write('{0};{1};{2};{3};{4};{5};{6};{7};{8};{9};{10};{11};{12};{13};{14};{15};{16};{17};\n'.format( \
											current_file, ncf, keym[1].value, keym[2].value, keym[3].value, \
											keym[4].value, keym[5].value, keym[6].value, keym[7].value, keym[8].value, \
											keym[9].value, keym[10].value, keym[11].value, keym[12].value, \
											keym[13].value, keym[14].value, keym[15].value, keym[16].value))

										if ncc == 100 :
											ncc = 0
											f4.close()
											f4 = open(res, 'a+')


										for k in range(1, 17):
											keym[k].read = 0
											keym[k].ST = 0
											keym[k].keyr = ""
											keymb[k] = keym[k].value
											keym[k].value = ""
											keym[k].readvalue = 0
											keym[k].readnb = 0

										keym[12].ofs = 5
										keym[12].ofsbuf = 5
										mp = 0
										ns = 1
									break
					break


	file.close()
	f6.close()
	f5.close()
	f4.close()

