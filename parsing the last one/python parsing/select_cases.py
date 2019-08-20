def select_case_1(i):
	if i == 1:
		keydet_rv.r01 = ""
	elif i == 2:
		keydet_rv.r02 = ""
	elif i == 3:
		keydet_rv.r03 = ""
	elif i == 4:
		keydet_rv.r04 = ""
	elif i == 5:
		keydet_rv.r05 = ""
	elif i == 6:
		keydet_rv.r06 = ""
	elif i == 7:
		keydet_rv.r07 = ""
	elif i == 8:
		keydet_rv.r08 = ""
	elif i == 9:
		keydet_rv.r09 = ""
	elif i == 10:
		keydet_rv.r10 = ""
	elif i == 11:
		keydet_rv.r11 = ""
	elif i == 12:
		keydet_rv.r12 = ""
	elif i == 13:
		keydet_rv.r13 = ""
	elif i == 14:
		keydet_rv.r14 = ""
def add_tmp(keydet_rv, i, tmp):
	if i == 1:
		keydet_rv.r01 = keydet_rv.r01 + str(tmp,"cp1251",'ignore')
	elif i == 2:
		keydet_rv.r02 = keydet_rv.r02 + str(tmp,"cp1251",'ignore')
	elif i == 3:
		keydet_rv.r03 = keydet_rv.r03 + str(tmp,"cp1251",'ignore')
	elif i == 4:
		keydet_rv.r04 = keydet_rv.r04 + str(tmp,"cp1251",'ignore')
	elif i == 5:
		keydet_rv.r05 = keydet_rv.r05 + str(tmp,"cp1251",'ignore')
	elif i == 6:
		keydet_rv.r06 = keydet_rv.r06 + str(tmp,"cp1251",'ignore')
	elif i == 7:
		keydet_rv.r07 = keydet_rv.r07 + str(tmp,"cp1251",'ignore')
	elif i == 8:
		keydet_rv.r08 = keydet_rv.r08 + str(tmp,"cp1251",'ignore')
	elif i == 9:
		keydet_rv.r09 = keydet_rv.r09 + str(tmp,"cp1251",'ignore')
	elif i == 10:
		keydet_rv.r10 = keydet_rv.r10  + str(tmp,"cp1251",'ignore')
	elif i == 11:
		keydet_rv.r11 = keydet_rv.r11  + str(tmp,"cp1251",'ignore')
	elif i == 12:
		keydet_rv.r12 = keydet_rv.r12  + str(tmp,"cp1251",'ignore')
	elif i == 13:
		keydet_rv.r13 = keydet_rv.r13  + str(tmp,"cp1251",'ignore')
	elif i == 14:
		keydet_rv.r14 = keydet_rv.r14  + str(tmp,"cp1251",'ignore')


def select_case_3(keydets_rv, i, keydets_ns): #630
	#1: keydets_rv.r01 = keydets_rv.r01[0: keydets_ns],
	if i == 2:
		keydets_rv.r02 = keydets_rv.r02[0: keydets_ns]
	elif i == 3:
		keydets_rv.r03 = keydets_rv.r03[0: keydets_ns]
	elif i == 4:
		keydets_rv.r04 = keydets_rv.r04[0: keydets_ns]
	elif i == 5:
		keydets_rv.r05 = keydets_rv.r05[0: keydets_ns]
	elif i == 6:
		keydets_rv.r06 = keydets_rv.r06[0: keydets_ns]
	elif i == 7:
		keydets_rv.r07 = keydets_rv.r07[0: keydets_ns]
	elif i == 8:
		keydets_rv.r08 = keydets_rv.r08[0: keydets_ns]
	elif i == 9:
		keydets_rv.r09 = keydets_rv.r09[0: keydets_ns]
	elif i == 10:
		keydets_rv.r10 = keydets_rv.r10[0: keydets_ns]

def select_case_4(keydets_rv, i, tmp): #654
	if i == 1:
		keydets_rv.r01 = str(tmp,"cp1251",'ignore')
	elif i == 2:	
		keydets_rv.r02 =  str(tmp,"cp1251",'ignore')
	elif i == 3:
		keydets_rv.r03 = str(tmp,"cp1251",'ignore')
	elif i == 4:
		keydets_rv.r04 = str(tmp,"cp1251",'ignore')
	elif i == 5:
		keydets_rv.r05 = str(tmp,"cp1251",'ignore')
	elif i == 6:
		keydets_rv.r06 = str(tmp,"cp1251",'ignore')
	elif i == 7:
		keydets_rv.r07 = str(tmp,"cp1251",'ignore')
	elif i == 8:
		keydets_rv.r08 = str(tmp,"cp1251",'ignore')
	elif i == 9:
		keydets_rv.r09 = str(tmp,"cp1251",'ignore')
	elif i == 10:
		keydets_rv.r10 = str(tmp,"cp1251",'ignore')

