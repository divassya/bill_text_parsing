class IterKeys(type):
	def __iter__(cls):
		return iter(cls._allKeys)

class parskey(metaclass=IterKeys):
	_allKeys = []
	def __init__(self, Key, n, read, ST, keyr, ofs, value, readvalue, readnb, ubuf, const,  ofsbuf ):
		self._allKeys.append(self)

		self.Key = Key
		self.n = n
		self.read = read
		self.ST = ST
		self.keyr = keyr
		self.ofs = ofs
		self.value = value #As String ' значение ключа
		self. readvalue = readvalue #As Integer ' начато чтение значений
		self.readnb = readnb
		self.ubuf = ubuf
		self.const = const
		self.ofsbuf = ofsbuf
		

cid = parskey("CID", 1, 0,0, "", 2,"",0,0,0,1,0)
banumber = parskey("BANUMBER", 2, 0,0, "", 1,"",0,0,0,1,0)
docseqnum = parskey("DOCSEQNUM", 3, 0,0, "", 1,"",0,0,0,1,0)
factura = parskey("Счет-фактура №", 4, 0,0, "", 2,"",0,0,0,1,0)
ot = parskey("от", 5, 0,0, "", 5,"",0,0,0,1,0)
vypiska = parskey("Выписка по балансу лицевого счета за учетный период", 6, 0,0, "", 1,"",0,0,0,1,0)
defis = parskey("-", 7, 0,0, "", 1,"",0,0,0,1,0)
nachalo = parskey("Баланс на начало учетного периода (тенге):", 8, 0,0, "", 5,"",0,0,0,1,0)
postupleniya = parskey("Поступления средств (тенге):", 9, 0,0, "", 5,"",0,0,0,1,0)
perenos = parskey("Перенос баланса (тенге):", 10, 0,0, "", 5,"",0,0,1,1,5)
korrekt = parskey("Корректировки (тенге):", 11, 0,0, "", 5,"",0,0,1,1,5)
mobi = parskey("mobi TV и/или благотворительность", 12, 0,0, "", 5,"",0,0,1,0,0)
uslugi = parskey("Начислено за услуги связи (тенге):", 13, 0,0, "", 5,"",0,0,1,1,5)
oborud = parskey("Начислено за оборудование (тенге):", 14, 0,0, "", 5,"",0,0,0,1,0)
konec = parskey("Баланс на конец учетного периода (тенге):", 15, 0,0, "", 5,"",0,0,0,1,0)
sum = parskey("Сумма к оплате (тенге):", 16, 0,0, "", 5,"",0,0,0,1,0)
'''
print (parskey._allKeys)

for keys in parskey:
	print(keys.Key + " is " + str(keys.n))
	'''	