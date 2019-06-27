import os
import gzip
import shutil
dir_name = "C:/Users/asus/Desktop/pdl/data_done/" #dir to data 
files = [name for name in os.listdir(dir_name) if os.path.isfile(os.path.join(dir_name, name))] #list of files in data

class parskey(): #структура ключа
	def _init_(self):
		pass
	'''key = "Default key", n , read, ST, keyr, ofs, value, readvalue, readnb, ubuf, const, ofsbuf):
		self.key = key
		self.n = n
		self.read = read
		self.ST = ST
		self.keyr = keyr
		self.ofs = ofs
		self.value = value
		self.readvalue = readvalue
		self.readnb = readnb
		self.ubuf = ubuf
		self.const = const
		self.ofsbuf = ofsbuf
	'''
	#class methods
	def Key(cls, Key): #ключ As String 
		cls.Key = Key
	def n(cls, n): #порядок As Integer 
		cls.n = n
	def read(cls, read): # As Integer  прочитан
		cls.read = read
	def ST(cls, ST):  #As Integer начато определение
		cls.ST = ST
	def keyr(cls, keyr): # As String ' прочитанный ключ
		cls.keyr = keyr
	def ofs(cls, ofs): #As Integer ' смещение до начала данных
		cls.ofs = ofs
	def value(cls, value): #As String ' значение ключа
		cls.value = value
	def readvalue (cls, readvalue): #As Integer ' начато чтение значений
		cls. readvalue = readvalue
	def readnb (cls, readnb):
		cls.readnb = readnb
	def ubuf (cls, ubuf):
		cls.ubuf = ubuf
	def const(cls, const):
		cls.const = const
	def ofsbuf (cls, ofsbuf):
		cls.ofsbuf = ofsbuf
		
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
		
cid = parskey()
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
banumber = parskey()
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
docseqnum = parskey()
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
factura = parskey()
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
ot = parskey()
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
vypiska = parskey()
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
defis = parskey()
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
nachalo = parskey()
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
postupleniya = parskey()
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
perenos = parskey()
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
korrekt = parskey()
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
mobi = parskey()
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
uslugi = parskey()
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
oborud = parskey()
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
konec = parskey()
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
sum = parskey()
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
res = dir_name + "data_res/" +  file + ".txt"
resA = dir_name + "data_res/" +  file + "A.txt"
resB = dir_name + "data_res/" +  file + "B.txt"
#with open(res, 'wb') as f4:
#with open(resA, 'wb') as f5:
#with open(resB, 'wb') as f6:
#
nbuf = 0
ns = 0
nfds = 0
nofs = 0
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