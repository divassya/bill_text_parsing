import os
import gzip
import shutil
from parskey import parskey 
dir_name = "C:/Users/asus/Desktop/pdl/data_done/" #dir to data 
files = [name for name in os.listdir(dir_name) if os.path.isfile(os.path.join(dir_name, name))] #list of files in data
def select_case_1(i):
	switcher={
		1: keydet_rv.r01 = "",
		2: keydet_rv.r02 = "",
		3: keydet_rv.r03 = "",
		4: keydet_rv.r04 = "",
		5: keydet_rv.r05 = "",
		6: keydet_rv.r06 = "",
		7: keydet_rv.r07 = "",
		8: keydet_rv.r08 = "",
		9: keydet_rv.r09 = "",
		10:keydet_rv.r10 = "",
		11:keydet_rv.r11 = "",
		12:keydet_rv.r12 = "",
		13:keydet_rv.r13 = "",
		14:keydet_rv.r14 = ""
			
	}
	return switcher.get(i,"Invalid keydet_f")
def select_case_2(i):
	switcher={
		1: keydet_rv.r01 = keydet_rv.r01 & chr(tmp),
		2: keydet_rv.r02 = keydet_rv.r02 & chr(tmp),
		3: keydet_rv.r03 = keydet_rv.r03 & chr(tmp),
		4: keydet_rv.r04 = keydet_rv.r04 & chr(tmp),
		5: keydet_rv.r05 = keydet_rv.r05 & chr(tmp),
		6: keydet_rv.r06 = keydet_rv.r06 & chr(tmp),
		7: keydet_rv.r07 = keydet_rv.r07 & chr(tmp),
		8: keydet_rv.r08 = keydet_rv.r08 & chr(tmp),
		9: keydet_rv.r09 = keydet_rv.r09 & chr(tmp),
		10:keydet_rv.r10 = keydet_rv.r10 & chr(tmp),
		11:keydet_rv.r11 = keydet_rv.r11 & chr(tmp),
		12:keydet_rv.r12 = keydet_rv.r12 & chr(tmp),
		13:keydet_rv.r13 = keydet_rv.r13 & chr(tmp),
		14:keydet_rv.r14 = keydet_rv.r14 & chr(tmp)
	
	}
	return switcher.get(i,"Invalid keydet_f")  
def select_case_3(i):
	switcher={
		#1: keydets_rv.r01 = keydets_rv.r01[0: keydets_ns],
		
		2: keydets_rv.r02 = keydets_rv.r02[0: keydets_ns],
		3: keydets_rv.r03 = keydets_rv.r03[0: keydets_ns],
		4: keydets_rv.r04 = keydets_rv.r04[0: keydets_ns],
		5: keydets_rv.r05 = keydets_rv.r05[0: keydets_ns],
		6: keydets_rv.r06 = keydets_rv.r06[0: keydets_ns],
		7: keydets_rv.r07 = keydets_rv.r07[0: keydets_ns],
		8: keydets_rv.r08 = keydets_rv.r08[0: keydets_ns],
		9: keydets_rv.r09 = keydets_rv.r09[0: keydets_ns],
		10:keydets_rv.r10 = keydets_rv.r10[0: keydets_ns]
	}
	return switcher.get(i,"Invalid keydet_f")
def select_case_4(i):
	switcher={
		1: keydets_rv.r01 = chr(tmp)
		2: keydets_rv.r02 = chr(tmp)
		3: keydets_rv.r03 = chr(tmp)
		4: keydets_rv.r04 = chr(tmp)
		5: keydets_rv.r05 = chr(tmp)
		6: keydets_rv.r06 = chr(tmp)
		7: keydets_rv.r07 = chr(tmp)
		8: keydets_rv.r08 = chr(tmp)
		9: keydets_rv.r09 = chr(tmp)
		10: keydets_rv.r10 = chr(tmp)
		
		}
	return switcher.get(i,"Invalid keydet_f")
def select_case_5(i):
	switcher={
		1: keydets_rv.r01 = keydets_rv.r01 & chr(tmp)
		2: keydets_rv.r02 = keydets_rv.r02 & chr(tmp)
		3: keydets_rv.r03 = keydets_rv.r03 & chr(tmp)
		4: keydets_rv.r04 = keydets_rv.r04 & chr(tmp)
		5: keydets_rv.r05 = keydets_rv.r05 & chr(tmp)
		6: keydets_rv.r06 = keydets_rv.r06 & chr(tmp)
		7: keydets_rv.r07 = keydets_rv.r07 & chr(tmp)
		8: keydets_rv.r08 = keydets_rv.r08 & chr(tmp)
		9: keydets_rv.r09 = keydets_rv.r09 & chr(tmp)
		10: keydets_rv.r10 = keydets_rv.r10 & chr(tmp)
		
		}
	return switcher.get(i,"Invalid keydet_f")
	
		
class row_det(): #запись детализации

	def _init_(self):
		pass
	def	r01(cls, r01):
		cls.r01 = r01
	def	r02(cls, r02):
		cls.r02 = r02
	def	r03(cls, r03):
		cls.r03 = r03
	def	r04(cls, r04):
		cls.r04 = r04
	def	r05(cls, r05):
		cls.r05 = r05
	def	r06(cls, r06):
		cls.r06 = r06
	def	r07(cls, r07):
		cls.r07 = r07
	def	r08(cls, r08):
		cls.r08 = r08
	def	r09(cls, r09):
		cls.r09 = r09
	def	r10(cls, r10):
		cls.r10 = r10
	def	r11(cls, r11):
		cls.r11 = r11
	def	r12(cls, r12):
		cls.r12 = r12
	def	r13(cls, r13):
		cls.r13 = r13
	def	r14(cls, r14):
		cls.r14 = r14
mdr = row_det()
mdr.r01()
mdr.r02()
mdr.r03()
mdr.r04()
mdr.r05()
mdr.r06()
mdr.r07()
mdr.r08()
mdr.r09)
mdr.r10()
mdr.r11()
mdr.r12()
mdr.r13()
mdr.r14()

keydet_rv = row_det()
keydet_rv.r01()
keydet_rv.r02()
keydet_rv.r03()
keydet_rv.r04()
keydet_rv.r05()
keydet_rv.r06()
keydet_rv.r07()
keydet_rv.r08()
keydet_rv.r09()
keydet_rv.r10()
keydet_rv.r11()
keydet_rv.r12()
keydet_rv.r13()
keydet_rv.r14()

keydets_rv = row_det()
keydets_rv.r01()
keydets_rv.r02()
keydets_rv.r03()
keydets_rv.r04()
keydets_rv.r05()
keydets_rv.r06()
keydets_rv.r07()
keydets_rv.r08()
keydets_rv.r09()
keydets_rv.r10()
keydets_rv.r11()
keydets_rv.r12()
keydets_rv.r13()
keydets_rv.r14()

cid = parskey(1)
cid.Key("CID") 
cid.n(1)
cid.read(0)
cid.ST(0)
cid.keyr("")
cid.ofs(2)
cid.value("")
cid.readvalue(0)
cid.readnb(0)
cid.ubuf(0)
cid.const(1)
cid.ofsbuf(0)
print(cid.__dict__)
#BANUMBER
banumber = parskey(2)
banumber.Key("BANUMBER")
banumber.n(2)
banumber.read(0)
banumber.ST(0)
banumber.keyr("")
banumber.ofs(1)
banumber.value("")
banumber.readvalue(0)
banumber.readnb(0)
banumber.ubuf(0)
banumber.const(1)
banumber.ofsbuf(0)
print(banumber.__dict__)
#DOCSEQNUM
docseqnum = parskey(3)
docseqnum.Key("DOCSEQNUM")
docseqnum.n(3)
docseqnum.read(0)
docseqnum.ST(0)
docseqnum.keyr("")
docseqnum.ofs(1)
docseqnum.value("")
docseqnum.readvalue(0)
docseqnum.readnb(0)
docseqnum.ubuf(0)
docseqnum.const(1)
docseqnum.ofsbuf(0)
print(docseqnum.__dict__)
#Счет-фактура №
factura = parskey(4)
factura.Key("Счет-фактура №") 
factura.n(4)
factura.read(0)
factura.ST(0)
factura.keyr("")
factura.ofs(1)
factura.value("")
factura.readvalue(0)
factura.readnb(0)
factura.ubuf(0)
factura.const(1)
factura.ofsbuf(0)
print(factura.__dict__)
#от
ot = parskey(5)
ot.Key("от") 
ot.n(5)
ot.read(0)
ot.ST(0)
ot.keyr("")
ot.ofs(5)
ot.value("")
ot.readvalue(0)
ot.readnb(0)
ot.ubuf(0)
ot.const(1)
ot.ofsbuf(0)
print(ot.__dict__)
#Выписка по балансу лицевого счета за учетный период
vypiska = parskey(6)
vypiska.Key("Выписка по балансу лицевого счета за учетный период") 
vypiska.n(6)
vypiska.read(0)
vypiska.ST(0)
vypiska.keyr("")
vypiska.ofs(1)
vypiska.value("")
vypiska.readvalue(0)
vypiska.readnb(0)
vypiska.ubuf(0)
vypiska.const(1)
vypiska.ofsbuf(0)
print(vypiska.__dict__)
#-
defis = parskey(7)
defis.Key("-") 
defis.n(7)
defis.read(0)
defis.ST(0)
defis.keyr("")
defis.ofs(1)
defis.value("")
defis.readvalue(0)
defis.readnb(0)
defis.ubuf(0)
defis.const(1)
defis.ofsbuf(0)
print(defis.__dict__)
#Баланс на начало учетного периода (тенге):
nachalo = parskey(8)
nachalo.Key("Баланс на начало учетного периода (тенге):") 
nachalo.n(8)
nachalo.read(0)
nachalo.ST(0)
nachalo.keyr("")
nachalo.ofs(5)
nachalo.value("")
nachalo.readvalue(0)
nachalo.readnb(0)
nachalo.ubuf(0)
nachalo.const(1)
nachalo.ofsbuf(0)
print(nachalo.__dict__)

#Поступления средств (тенге):
postupleniya = parskey(9)
postupleniya.Key("Поступления средств (тенге):") 
postupleniya.n(9)
postupleniya.read(0)
postupleniya.ST(0)
postupleniya.keyr("")
postupleniya.ofs(5)
postupleniya.value("")
postupleniya.readvalue(0)
postupleniya.readnb(0)
postupleniya.ubuf(0)
postupleniya.const(1)
postupleniya.ofsbuf(0)
print(postupleniya.__dict__)
#Перенос баланса (тенге):
perenos = parskey(10)
perenos.Key("Перенос баланса (тенге):") 
perenos.n(10)
perenos.read(0) #0 нет 1 да
perenos.ST(0) #0 нет 1 да
perenos.keyr("")
perenos.ofs(5) #15
perenos.value("")
perenos.readvalue(0)
perenos.readnb(0)
perenos.ubuf(1)
perenos.const(1)
perenos.ofsbuf(5)
print(perenos.__dict__)
#Корректировки (тенге):
korrekt = parskey(11)
korrekt.Key("Корректировки (тенге):") 
korrekt.n(11)
korrekt.read(0) #0 нет 1 да
korrekt.ST(0) #0 нет 1 да
korrekt.keyr("")
korrekt.ofs(5) #14
korrekt.value("")
korrekt.readvalue(0)
korrekt.readnb(0)
korrekt.ubuf(1)
korrekt.const(1)
korrekt.ofsbuf(5)
print(korrekt.__dict__)
#mobi TV по договору поручения
#TV по договору поручения
mobi = parskey(12)
mobi.Key("mobi TV и/или благотворительность") 
mobi.n(12)
mobi.read(0) #0 нет 1 да
mobi.ST(0) #0 нет 1 да
mobi.keyr("")
mobi.ofs(5) #14
mobi.value("")
mobi.readvalue(0)
mobi.readnb(0)
mobi.ubuf(1)
mobi.const(0)
mobi.ofsbuf(0)
print(mobi.__dict__)
#Начислено за услуги связи (тенге):
uslugi = parskey(13)
uslugi.Key("Начислено за услуги связи (тенге):") 
uslugi.n(13)
uslugi.read(0) #0 нет 1 да
uslugi.ST(0) #0 нет 1 да
uslugi.keyr("")
uslugi.ofs(5) #8
uslugi.value("")
uslugi.readvalue(0)
uslugi.readnb(0)
uslugi.ubuf(1)
uslugi.const(1)
uslugi.ofsbuf(5)
print(uslugi.__dict__)
#Начислено за оборудование (тенге):
oborud = parskey(14)
oborud.Key("Начислено за оборудование (тенге):") 
oborud.n(14)
oborud.read(0) #0 нет 1 да
oborud.ST(0) #0 нет 1 да
oborud.keyr("")
oborud.ofs(5) #8
oborud.value("")
oborud.readvalue(0)
oborud.readnb(0)
oborud.ubuf(0)
oborud.const(1)
oborud.ofsbuf(0)
print(oborud.__dict__)
#Баланс на конец учетного периода (тенге):
konec = parskey(15)
konec.Key("Баланс на конец учетного периода (тенге):") 
konec.n(15)
konec.read(0) #0 нет 1 да
konec.ST(0) #0 нет 1 да
konec.keyr("")
konec.ofs(5) #8
konec.value("")
konec.readvalue(0)
konec.readnb(0)
konec.ubuf(0)
konec.const(1)
konec.ofsbuf(0)
print(konec.__dict__)
#Сумма к оплате (тенге):
sum = parskey(16)
sum.Key("Сумма к оплате (тенге):") 
sum.n(16)
sum.read(0) #0 нет 1 да
sum.ST(0) #0 нет 1 да
sum.keyr("")
sum.ofs(5) #8
sum.value("")
sum.readvalue(0)
sum.readnb(0)
sum.ubuf(0)
sum.const(1)
sum.ofsbuf(0)
print(sum.__dict__)
#keydet
keydet_k = "068403010003030000" #"062003050003030000"
keydet_kr = ""
keydet_st = 0
keydet_nc = 14
keydet_of = 4 ' 5
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
res = dir_name + "data_res/" +	file + ".txt"
resA = dir_name + "data_res/" +	 file + "A.txt"
resB = dir_name + "data_res/" +	 file + "B.txt"
#with open(res, 'wb') as f4:
#with open(resA, 'wb') as f5:
#with open(resB, 'wb') as f6:
f4 = open(res, 'wb')
f5 = open(resA, 'wb')
f6 = open(resB, 'wb')
#
nbuf = 0
ns = 0
nfds = 0
nofs = 0
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
	
buf_txt = ""
for j in range(nbuf):
	buf_txt = buf_txt & str(buff(j)) #line = ser.readline().decode()
mdrk = ""
for j in range(nbuf, nbuf - 8, -1):
	if j = 0:
		break
	mdr_hi = buff(j)\16
	mdr_lo = buff(j) - 16 * mdr_hi
	mdrk = hexbute(mdr_hi) & hexbute(mdr_lo) &mdrk

'''
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
			if (keydet_rv.r02).split() = "1" or nfds = 1:
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
				#GoTo exit_cycle
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
				select_case_1(keydet_f)
				
				keydet_rb = 0
				keydet_f = keydet_f + 1
				#---
				if keydet_f = 3 or keydet_f = 11 or keydet_f = 13:
					keydet_of = 4 #--5
			  
			  #---
				if keydet_f = 3:
					if (keydet_rv.r02).split() = "1" or nfds = 1:
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
	elif (keydet_st = 0) and (keydet_f > 0) and (keydet_f < keydet_nc + 1) and (keydet_rb >= keydet_of) and (keydet_rd < keydet_ns):
		keydet_rd = keydet_rd + 1
		select_case_2(keydet_f)
		
		if keydet_rd = keydet_ns:
			keydet_rb = 0
			keydet_f = keydet_f + 1
			keydet_of = 4
			#---
			if (keydet_f = 3 or keydet_f = 11 or keydet_f = 13) and nofs = 1:
				keydet_of = 5 #--5--01
			
			#---IsNumeric
			if keydet_f = 3: #509line
				if keydet_rv.r02.strip() = "1" \
					or (nfds = 1 and (keydet_rv.r02.strip()).isnumeric()):
					#Debug.Print "Ok2"
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
		
	elif (keydet_f = keydet_nc + 1):
		mdrn = mdrn + 1
		#ReDim Preserve mdr(mdrn)
		mdr = []
		mdr = mdr.append(mdrn)
		#keydet_rv.r14 = Left(keydet_rv.r14, keydet_ns)
		mdr(mdrn) = keydet_rv
		#Debug.Print keydet_rv.r01 & " " & keydet_rv.r02 & " " & keydet_rv.r03 & " " & keydet_rv.r04 & " " & keydet_rv.r05 & " " & keydet_rv.r06 & " " _
		#& keydet_rv.r07 & " " & keydet_rv.r08 & " " & keydet_rv.r09 & " " & keydet_rv.r10 & " " & keydet_rv.r11 & " " & keydet_rv.r12 & " " _
		#& keydet_rv.r13 & " " & keydet_rv.r14
		
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
		
	  
#-----
if nfds = 1 and (cid.readvalue() > 0 and vypiska.readvalue() > 0 ):
	nfds = 0
	nofs = 0
if (cid.readvalue() > 0 and vypiska.readvalue() > 0) or (ns = 1) :
	if (mdrk = keydets_k) and (keydets_st = 0) and (nfds = 1) :
		keydets_st = 1 #???
		
		elif (keydets_st = 1) and (mdrk = keydet_k) and cid.readvalue() > 0 :
			keydets_st = 0
		if (keydets_st = 1):
			if(keydets_k2.find(chr(tmp), 0) = 1) and (keydets_k2r = "") and keydets_r = 0:
				keydets_k2r = chr(tmp)
			elif (keydets_k2.find(keydets_k2r & chr(tmp), 1) = 1) and (keydets_k2r != "") and keydets_r = 0 :
				keydets_k2r = keydets_k2r & chr(tmp)
				if keydets_k2r = keydets_k2 and keydets_r = 0 :
					keydets_r = 1
					#keydets_kr = "OK"
					keydets_ds = 1
					keydets_k2r = ""
					for k in range(1,mdrn) :
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
						
						f5.write( '{0}; {1}; {2}; {3}; {4}; {5};{6}; {7}; {8}; {9}; {10}; {11}; {12}; {13}; {14}; {15}; {16}; {17}; {18}; {19};'.format(\
							file, ncf, keymb(2), keymb(3), keymb(4), keymb(5)\
							mdr[k].r01, mdr[k].r02, mdr[k].r03, mdr[k].r04, mdr[k].r05, mdr[k].r06, \
							mdr[k].r07, mdr[k].r08, mdr[k].r09, mdr[k].r10, mdr[k].r11, mdr[k].r12,\
							mdr[k].r13, mdr[k].r14
							)) 
						
					if ncca = 100:
						ncca = 0
						f5.close()
						f5 = open(resA, 'w+')
			   
				k+=1
				mdrn = 0
				mdr = mdr.append(mdrn)
			elif(keydets_k2.find(keydets_k2r & chr(tmp), 1) != 1) and (keydets_k2r != "") and keydets_r = 0 :
				keydets_k2r = ""
			elif (keydets_r = 1) and (keydets_f < keydets_nc) :
				if tmp > 29 and keydets_ds = 0 and keydets_or = 0 :
					keydets_ds = 1
					select_case_3(keydets_f)
					keydets_ns = buff(nbuf - 1)
					keydets_f = keydets_f + 1
					select_case_4(keydets_f)
				elif tmp > 29 and keydets_ds = 1 and keydets_or = 0:
					select_case_5(keydets_f)
				elif (tmp < 30) and (keydets_ds = 1) and (keydets_or = 0):
					keydets_or = 1
					keydets_ds = 0
				elif (keydets_ds = 0) and (keydets_or > 0) and (keydets_or < keydets_of):
					keydets_or = keydets_or + 1
					if keydets_or = keydets_of:
						if keydets_f = 1:
							keydets_rv.r01 = keydets_rv.r01[0:keydets_ns-1]
							
				
						keydets_ns = tmp
						keydets_or = 0
						keydets_ds = 0
		   
			elif (keydets_f = keydets_nc) and (keydets_or = 0) and (tmp > 29) and (keydets_ds = 1):
				select_case_5(keydets_f)
			elif (keydets_f = keydets_nc) and (keydets_or = 0) and (tmp < 30) :
				keydets_or = 1
			elif (keydets_f = keydets_nc) and (keydets_or < keydets_of) and (keydets_or > 0) :
				keydets_or = keydets_or + 1
			elif (keydets_f = keydets_nc) and (keydets_or = keydets_of) :
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
				f6.write( '{0}; {1}; {2}; {3}; {4}; {5};{6}; {7}; {8}; {9}; {10}; {11}; {12}; {13}; {14}; {15}; '.format(\
					file, ncf, keymb(2), keymb(3), keymb(4), keymb(5)\
					keydets_rv.r01, keydets_rv.r02, keydets_rv.r03, keydets_rv.r04, keydets_rv.r05 \
					keydets_rv.r06, keydets_rv.r07, keydets_rv.r08, keydets_rv.r09, keydets_rv.r10 \
					)) 
				if nccb = 100:
					nccb = 0
					f6.close()
					f6 = open(resA, 'w+')
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
		if keydets_kr = "OK":
			keydets_kr = ""
'''
		
#867
		#indentation is in question 
keym = [parskey(i) for i in range(1,16)]
for i in range(1,16): #867
	if keym(i).n = i :
		if keym(i).read = 0:
			mp = i
			if keym(i).ST = 0 :
				if keym(i).Key.find(chr(tmp)) = 0 :
					keym(i).ST = 1
					keym(i).keyr = chr(tmp)
					if keym(i).keyr = keym(i).Key :
						keym(i).readvalue = 1
						#Active mobi
						if i = 12 :
						
							if buf_txt.find("mobi") > 0 :
								keym(12).ofs = keym(12).ofs + 5
								keym(12).ofsbuf = keym(12).ofsbuf + 5
							else:
								keym(12).ofs = keym(12).ofs + 7
								keym(12).ofsbuf = keym(12).ofsbuf + 7
						if keym(i).readvalue > 0 and i = 1 : #889
							ns = 0
							nbr = 5
				  
						if keym(i).ubuf = 1 :
							tr = 0
							for k in range(1, 61):
								if k > len(keym(i).Key) + keym(i).ofsbuf :
									if buff(61 - k) > 29 :
										keym(i).value = chr(buff(61 - k)) & keym(i).value
										tr = 1
									elif tr = 1 :
										keym(i).read = 2
										keym(i).value = (keym(i).value).split()
										break
										
				
						elif (keym(i).const = 0) and (i < 16) :
							if keym(i + 1).ST = 0 :
								if keym(i + 1).Key.find(chr(tmp)) = 0:
									keym(i + 1).ST = 1
									keym(i + 1).keyr = chr(tmp)
									if keym(i + 1).keyr = keym(i + 1).Key :
										keym(i + 1).readvalue = 1
										if keym(i + 1).readvalue > 0 and i = 1 :
											ns = 0
											nbr = 5
										if keym(i + 1).ubuf = 1 :
											tr = 0
											for k in range (1,61):
												if k > len(keym(i + 1).Key) + keym(i + 1).ofsbuf :
													if buff(61 - k) > 29 :
														keym(i + 1).value = chr(buff(61 - k)) & keym(i + 1).value
														tr = 1
													elif tr = 1 :
														keym(i + 1).read = 2
														keym(i + 1).value = Trim(keym(i + 1).value)
														keym(i).read = 2
														keym(i).value = "0"
														break
							   
							 
							elif keym(i + 1).ST <> 0 :
								if keym(i + 1).Key.find(keym(i + 1).keyr & chr(tmp)) = 0 and keym(i + 1).readvalue = 0 :
									keym(i + 1).keyr = keym(i + 1).keyr & chr(tmp)
									if keym(i + 1).keyr = keym(i + 1).Key :
										keym(i + 1).readvalue = 1
										if keym(i + 1).ubuf = 1 :
											tr = 0
											for k in range(1,61):
												if k > len(keym(i + 1).Key) + keym(i + 1).ofsbuf :
													if buff(61 - k) > 29 :
														keym(i + 1).value = chr(buff(61 - k)) & keym(i + 1).value
														tr = 1
													elif tr = 1 :
														keym(i + 1).read = 2
														keym(i + 1).value = Trim(keym(i + 1).value)
														keym(i).read = 2
														keym(i).value = "0"
														break
								  
						break
				else:
					if keym(i).Key.find(keym(i).keyr & chr(tmp)) = 1 and keym(i).readvalue = 0 :
						keym(i).keyr = keym(i).keyr & chr(tmp)
						if keym(i).keyr = keym(i).Key :
							keym(i).readvalue = 1
							#Active mobi
							if i = 12 :
								if buf_txt.find("mobi") > 0 :
									keym(12).ofs = keym(12).ofs + 5
									keym(12).ofsbuf = keym(12).ofsbuf + 5
								else:
									keym(12).ofs = keym(12).ofs + 7
									keym(12).ofsbuf = keym(12).ofsbuf + 7
							 
							if keym(i).readvalue > 0 and i = 1 :
								ns = 0
								nbr = 5
							if keym(i).ubuf = 1 :
								tr = 0
								for k in range(1, 61):
									if k > len(keym(i).Key) + keym(i).ofsbuf :
										if buff(61 - k) > 29 :
											keym(i).value = chr(buff(61 - k)) & keym(i).value
											tr = 1
										elif tr = 1 :
											keym(i).read = 2
											keym(i).value = Trim(keym(i).value)
											break
					
					elif keym(i).readvalue = 0 :
						keym(i).keyr = ""
						keym(i).ST = 0
						break
					else:
						if keym(i).readnb = 0 and chr(tmp) = "*" :
							keym(i).readnb = 0
						else:
							keym(i).readnb = keym(i).readnb + 1
				  
						if keym(i).readnb > keym(i).ofs :
							if tmp > 29 :
								keym(i).value = keym(i).value & chr(tmp)
								break
							else:
								keym(i).read = 2
								keym(i).value = (keym(i).value).strip()
							if i = 16 :
								for k in range(1,17):
									if keym(k).value.find(",") > 0 :
										keym(k).value = keym(k).value[0:(keym(k).value.find(",") + 2)]
										keym(k).value = keym(k).value.replace(";", " ")
								keym(2).value = (keym(2).value[0: len(keym(2).value) - 2)
						
								ncf = ncf + 1
								ncc = ncc + 1
								f4.write( '{0}; {1}; {2}; {3}; {4}; {5}; {6}; {7}; {8}; {9}; {10}; {11}; {12}; {13}; {14}; {15}; {16}; {17}; {18}; {19};'.format(\
									file, ncf, keym(1).value, keym(2).value, keym(3).value, keym(4).value, \
									keym(5).value, keym(6).value, keym(7).value, keym(8).value, keym(9).value, \
									keym(10).value, keym(11).value, keym(12).value, keym(13).value, keym(14).value, \
									keym(15).value, keym(16).value 
									))	
								if ncc = 100 :
									ncc = 0
									f4.close()
									f4 = open(resA, 'w+') 
								
							   
								for k in range(1, 17):
									keym(k).read = 0
									keym(k).ST = 0
									keym(k).keyr = ""
									keymb(k) = keym(k).value
									keym(k).value = ""
									keym(k).readvalue = 0
									keym(k).readnb = 0
								
								keym(12).ofs = 5
								keym(12).ofsbuf = 5
								mp = 0
								
								ns = 1
							break
							
					break
def exit_cycle():
	continue

hex_file.close()
f6.close()
f5.close()
f4.close()
pars_data4 = ncf

	

				
		
					   
							