class parskey(): #структура ключа
	def _init_(self,index):
		self.index = index
		pass
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