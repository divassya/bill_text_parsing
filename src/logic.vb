Function pars_data4(file As String, data_res As String, stdf As Double, stdt As Double) As Integer
  Dim SF As Long, i As Integer, j As Integer, k As Integer, tr As Integer
  Dim keym(16) As parskey, tmp As Byte, buff(60) As Byte, nbuf As Integer, ncf As Integer, ncc As Integer, keydets_rv As row_det, _
  pim As Integer, mskn As Integer, ns As Integer, mdr() As row_det, mdrn As Long, keydet_rv As row_det, keydet_ds As Integer, _
  keydet_or As Integer, _
  keydet_ns As Integer, keydets_or As Integer, keydets_ds As Integer, keydets_ns As Integer, ncca As Integer, nccb As Integer, _
  keymb(16) As String
  '
  Dim nbr As Long, mask  As String, mskp  As String, ric  As String, _
  keydet_k As String, keydet_kr As String, keydet_st As Integer, keydet_nc As Integer, keydet_of As Integer, keydet_f As Integer, _
  keydets_k As String, keydets_kr As String, keydets_k2 As String, keydets_k2r As String, keydets_st As Integer, keydets_nc As Integer, _
  keydets_of  As Integer, keydets_r  As Integer, buf_txt As String, mp As Integer, mdrk As String, mdr_hi As Integer, mdr_lo As Integer, _
  keydets_f As Integer, keydet_rb As Integer, keydet_rd As Integer, nfds As Integer, nofs As Integer
  'CID
  'parskey
  'Type parskey ' структура ключа
   ' Key As String ' ключ
    'n As Integer ' порядок
    'read As Integer ' прочитан
    'ST As Integer ' начато определение
    'keyr As String ' прочитанный ключ
    'ofs As Integer ' смещение до начала данных
    'value As String ' значение ключа
    'readvalue As Integer ' начато чтение значений
    'readnb As Integer ' прочитано байт значения
    'ubuf As Integer 'читать буфер
    'const As Integer 'может отсутствовать
    'ofsbuf As Integer 'буфер смещение до начала данных
'End Type
  'row_det
  'Type row_det ' Тип, запись детализации
   ' r01 As String 'поле 01
    'r02 As String 'поле 02
    'r03 As String 'поле 03
    'r04 As String 'поле 04
    'r05 As String 'поле 05
    'r06 As String 'поле 06
    'r07 As String 'поле 07
    'r08 As String 'поле 08
    'r09 As String 'поле 09
    'r10 As String 'поле 10
    'r11 As String 'поле 11
    'r12 As String 'поле 12
    'r13 As String 'поле 13
    'r14 As String 'поле 14
'End Type
  '
  keym(1).Key = "CID"
  keym(1).n = 1
  keym(1).read = 0 ' 0 нет 1 да
  keym(1).ST = 0 ' 0 нет 1 да
  keym(1).keyr = ""
  keym(1).ofs = 2
  keym(1).value = ""
  keym(1).readvalue = 0
  keym(1).readnb = 0
  keym(1).ubuf = 0
  keym(1).const = 1
  keym(1).ofsbuf = 0
  'BANUMBER
  keym(2).Key = "BANUMBER"
  keym(2).n = 2
  keym(2).read = 0 ' 0 нет 1 да
  keym(2).ST = 0 ' 0 нет 1 да
  keym(2).keyr = ""
  keym(2).ofs = 1
  keym(2).value = ""
  keym(2).readvalue = 0
  keym(2).readnb = 0
  keym(2).ubuf = 0
  keym(2).const = 1
  keym(2).ofsbuf = 0
  'DOCSEQNUM
  keym(3).Key = "DOCSEQNUM"
  keym(3).n = 3
  keym(3).read = 0 ' 0 нет 1 да
  keym(3).ST = 0 ' 0 нет 1 да
  keym(3).keyr = ""
  keym(3).ofs = 1
  keym(3).value = ""
  keym(3).readvalue = 0
  keym(3).readnb = 0
  keym(3).ubuf = 0
  keym(3).const = 1
  keym(3).ofsbuf = 0
  'Счет-фактура №
  keym(4).Key = "Счет-фактура №"
  keym(4).n = 4
  keym(4).read = 0 ' 0 нет 1 да
  keym(4).ST = 0 ' 0 нет 1 да
  keym(4).keyr = ""
  keym(4).ofs = 1
  keym(4).value = ""
  keym(4).readvalue = 0
  keym(4).readnb = 0
  keym(4).ubuf = 0
  keym(4).const = 1
  keym(4).ofsbuf = 0
  'от
  keym(5).Key = "от"
  keym(5).n = 5
  keym(5).read = 0 ' 0 нет 1 да
  keym(5).ST = 0 ' 0 нет 1 да
  keym(5).keyr = ""
  keym(5).ofs = 5 '4
  keym(5).value = ""
  keym(5).readvalue = 0
  keym(5).readnb = 0
  keym(5).ubuf = 0
  keym(5).const = 1
  keym(5).ofsbuf = 0
  'Выписка по балансу лицевого счета за учетный период
  keym(6).Key = "Выписка по балансу лицевого счета за учетный период"
  keym(6).n = 6
  keym(6).read = 0 ' 0 нет 1 да
  keym(6).ST = 0 ' 0 нет 1 да
  keym(6).keyr = ""
  keym(6).ofs = 1
  keym(6).value = ""
  keym(6).readvalue = 0
  keym(6).readnb = 0
  keym(6).ubuf = 0
  keym(6).const = 1
  keym(6).ofsbuf = 0
  '-
  keym(7).Key = "-"
  keym(7).n = 7
  keym(7).read = 0 ' 0 нет 1 да
  keym(7).ST = 0 ' 0 нет 1 да
  keym(7).keyr = ""
  keym(7).ofs = 1
  keym(7).value = ""
  keym(7).readvalue = 0
  keym(7).readnb = 0
  keym(7).ubuf = 0
  keym(7).const = 1
  keym(7).ofsbuf = 0
  'Баланс на начало учетного периода (тенге):
  keym(8).Key = "Баланс на начало учетного периода (тенге):"
  keym(8).n = 8
  keym(8).read = 0 ' 0 нет 1 да
  keym(8).ST = 0 ' 0 нет 1 да
  keym(8).keyr = ""
  keym(8).ofs = 5
  keym(8).value = ""
  keym(8).readvalue = 0
  keym(8).readnb = 0
  keym(8).ubuf = 0
  keym(8).const = 1
  keym(8).ofsbuf = 0
  'Поступления средств (тенге):
  keym(9).Key = "Поступления средств (тенге):"
  keym(9).n = 9
  keym(9).read = 0 ' 0 нет 1 да
  keym(9).ST = 0 ' 0 нет 1 да
  keym(9).keyr = ""
  keym(9).ofs = 5 '6
  keym(9).value = ""
  keym(9).readvalue = 0
  keym(9).readnb = 0
  keym(9).ubuf = 0
  keym(9).const = 1
  keym(9).ofsbuf = 0
  'Перенос баланса (тенге):
  keym(10).Key = "Перенос баланса (тенге):"
  keym(10).n = 10
  keym(10).read = 0 ' 0 нет 1 да
  keym(10).ST = 0 ' 0 нет 1 да
  keym(10).keyr = ""
  keym(10).ofs = 5 '15
  keym(10).value = ""
  keym(10).readvalue = 0
  keym(10).readnb = 0
  keym(10).ubuf = 1
  keym(10).const = 1
  keym(10).ofsbuf = 5
  'Корректировки (тенге):
  keym(11).Key = "Корректировки (тенге):"
  keym(11).n = 11
  keym(11).read = 0 ' 0 нет 1 да
  keym(11).ST = 0 ' 0 нет 1 да
  keym(11).keyr = ""
  keym(11).ofs = 5 '14
  keym(11).value = ""
  keym(11).readvalue = 0
  keym(11).readnb = 0
  keym(11).ubuf = 1
  keym(11).const = 1
  keym(11).ofsbuf = 5
  'mobi TV по договору поручения
  'TV по договору поручения
  keym(12).Key = "mobi TV и/или благотворительность"
  keym(12).n = 12
  keym(12).read = 0 ' 0 нет 1 да
  keym(12).ST = 0 ' 0 нет 1 да
  keym(12).keyr = ""
  keym(12).ofs = 5 '14
  keym(12).value = ""
  keym(12).readvalue = 0
  keym(12).readnb = 0
  keym(12).ubuf = 1
  keym(12).const = 0
  keym(12).ofsbuf = 0
  'Начислено за услуги связи (тенге):
  keym(13).Key = "Начислено за услуги связи (тенге):"
  keym(13).n = 13
  keym(13).read = 0 ' 0 нет 1 да
  keym(13).ST = 0 ' 0 нет 1 да
  keym(13).keyr = ""
  keym(13).ofs = 5 '8
  keym(13).value = ""
  keym(13).readvalue = 0
  keym(13).readnb = 0
  keym(13).ubuf = 1
  keym(13).const = 1
  keym(13).ofsbuf = 5
  'Начислено за оборудование (тенге):
  keym(14).Key = "Начислено за оборудование (тенге):"
  keym(14).n = 14
  keym(14).read = 0 ' 0 нет 1 да
  keym(14).ST = 0 ' 0 нет 1 да
  keym(14).keyr = ""
  keym(14).ofs = 5
  keym(14).value = ""
  keym(14).readvalue = 0
  keym(14).readnb = 0
  keym(14).ubuf = 0
  keym(14).const = 1
  keym(14).ofsbuf = 0
  'Баланс на конец учетного периода (тенге):
  keym(15).Key = "Баланс на конец учетного периода (тенге):"
  keym(15).n = 15
  keym(15).read = 0 ' 0 нет 1 да
  keym(15).ST = 0 ' 0 нет 1 да
  keym(15).keyr = ""
  keym(15).ofs = 5
  keym(15).value = ""
  keym(15).readvalue = 0
  keym(15).readnb = 0
  keym(15).ubuf = 0
  keym(15).const = 1
  keym(15).ofsbuf = 0
  'Сумма к оплате (тенге):
  keym(16).Key = "Сумма к оплате (тенге):"
  keym(16).n = 16
  keym(16).read = 0 ' 0 нет 1 да
  keym(16).ST = 0 ' 0 нет 1 да
  keym(16).keyr = ""
  keym(16).ofs = 5
  keym(16).value = ""
  keym(16).readvalue = 0
  keym(16).readnb = 0
  keym(16).ubuf = 0
  keym(16).const = 1
  keym(16).ofsbuf = 0
  'If stdf > 20200201 Or stdt > 20200201 Then
  '  pars_data4 = 0
  '  Exit Function
  'End If
'Type parskey ' структура ключа
'    key As String ' ключ
'    n As Integer ' порядок
'    read As Integer ' прочитан
'    st As Integer ' начато определение
'    keyr As String ' прочитанный ключ
'    ofs As Integer ' смещение до начала данных
'    value As String ' значение ключа
'    readvalue As Integer ' начато чтение значений
'    readnb As Integer ' прочитано байт значения
'End Type
  'keydet
  keydet_k = "068403010003030000" '"062003050003030000"
  keydet_kr = ""
  keydet_st = 0
  keydet_nc = 14
  keydet_of = 4 ' 5
  keydet_f = 0
  keydet_ds = 0
  keydet_or = 0
  'keydets
  keydets_k = "068403010003030000" '"062003040003030000"
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
  '
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
  Open data_res & file & ".txt" For Output As #4
  Open data_res & file & "A.txt" For Output As #5
  Open data_res & file & "B.txt" For Output As #6
  '
  nbuf = 0
  ns = 0
  nfds = 0
  nofs = 0
  SF = FreeFile
  Open Workbooks("amdocs_pdl.xlsm").path & "\" & "TEMPOR2.TMP" For Binary As #SF
  'ARS20170121113235_MOBITV_40634973.BILL.txt ARS20170406181023TEMPOR0_ ARS20170406181023TMP debug_null --res2_1  --TEMPOR0_R2
  'ARS20170121113235_MBTVDBG TEMPOR0_3_DEBUG moby_tv_DAT_txt2.txt TEMPOR0_DBG.TXT 'TEMPOR2.TMP 'ARS20170123220008.DAT moby_tv_DAT_txt
    Do Until EOF(SF)
      Get #SF, , tmp
      'tmp = tmp Xor 127
      'Debug.Print Chr(TMP)
      If nbuf < 60 Then
        nbuf = nbuf + 1
        buff(nbuf) = tmp
      Else
        For j = 1 To nbuf
          If j < 60 Then
            buff(j) = buff(j + 1)
          Else
            buff(j) = tmp
          End If
        Next j
      End If
      '--- отладочный блок
      'Debug.Assert InStr(1, buf_txt, "mobi") = 0
      buf_txt = ""
      For j = 1 To nbuf
        buf_txt = buf_txt & Chr(buff(j))
      Next j
      mdrk = ""
      For j = nbuf To nbuf - 8 Step -1
        If j = 0 Then
          Exit For
        End If
        mdr_hi = buff(j) \ 16
        mdr_lo = buff(j) - 16 * mdr_hi
        mdrk = hexbute(mdr_hi) & hexbute(mdr_lo) & mdrk
      Next j
      'Debug.Assert keym(1).value <> "63869444"
      '----- keydet_rv
      '-----
      ' start
      '-----
      If (keym(1).readvalue > 0 And keym(6).readvalue > 0) Or (ns = 1) Then
        'Debug.Assert keym(1).readvalue > 0 And keym(6).readvalue > 0 '
        If mdrk = keydet_k Then
          keydet_st = 1
          keydet_f = 0
          keydet_of = 4
          keydet_rb = 0
          keydet_ds = 0
          keydet_rd = 0
        ElseIf (keydet_st = 1) And (keydet_f = 0) And keydet_rb = 0 Then
          keydet_ns = buff(nbuf)
          keydet_rb = keydet_of + 1
          keydet_st = 0
          keydet_f = keydet_f + 1
          '---
          'If keydet_f = 3 Or keydet_f = 11 Or keydet_f = 13 Then
          '  keydet_of = 5
          'End If
          '---
          If keydet_f = 3 Then
            If Trim(keydet_rv.r02) = "1" Or nfds = 1 Then
              'Debug.Print "Ok1"
              nfds = 1
            Else
              keydet_rv.r01 = ""
              keydet_rv.r02 = ""
              keydet_or = 0
              keydet_st = 0
              keydet_ds = 0
              keydet_f = 0
              keydet_of = 4
              ' GoTo exit_cycle
            End If
          End If
          '---
          keydet_rd = 0
          If keydet_ns = 0 Then
            keydet_rv.r01 = ""
            keydet_rb = 0
            keydet_f = keydet_f + 1
            '---
            If keydet_f = 3 Or keydet_f = 11 Or keydet_f = 13 Then
              keydet_of = 4 '--5
            End If
            '---
          End If
        ElseIf (keydet_st = 0) And (keydet_f > 0) And (keydet_f < keydet_nc + 1) And keydet_rb <= keydet_of Then
          keydet_rb = keydet_rb + 1
          If keydet_rb = keydet_of + 1 Then
            If keydet_f > 1 Then
              keydet_ns = buff(nbuf)
              If keydet_f = 3 And keydet_ns < 10 And keydet_ns <> 0 Then
                keydet_ns = 10
                keydet_of = 5
                nofs = 1
              End If
            End If
            If keydet_ns = 0 Then
              Select Case keydet_f
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
              End Select
              keydet_rb = 0
              keydet_f = keydet_f + 1
              '---
              If keydet_f = 3 Or keydet_f = 11 Or keydet_f = 13 Then
                keydet_of = 4 '--5
              End If
              '---
              If keydet_f = 3 Then
                If Trim(keydet_rv.r02) = "1" Or nfds = 1 Then
                  'Debug.Print "Ok3"
                  nfds = 1
                Else
                  keydet_rv.r01 = ""
                  keydet_rv.r02 = ""
                  keydet_or = 0
                  keydet_st = 0
                  keydet_ds = 0
                  keydet_f = 0
                  keydet_of = 4
                  ' GoTo exit_cycle
                End If
              End If
              keydet_rd = 0
            End If
          End If
        ElseIf (keydet_st = 0) And (keydet_f > 0) And (keydet_f < keydet_nc + 1) And (keydet_rb >= keydet_of) And (keydet_rd < keydet_ns) Then
          keydet_rd = keydet_rd + 1
          Select Case keydet_f
            Case 1
              keydet_rv.r01 = keydet_rv.r01 & Chr(tmp)
            Case 2
              keydet_rv.r02 = keydet_rv.r02 & Chr(tmp)
            Case 3
              keydet_rv.r03 = keydet_rv.r03 & Chr(tmp)
            Case 4
              keydet_rv.r04 = keydet_rv.r04 & Chr(tmp)
            Case 5
              keydet_rv.r05 = keydet_rv.r05 & Chr(tmp)
            Case 6
              keydet_rv.r06 = keydet_rv.r06 & Chr(tmp)
            Case 7
              keydet_rv.r07 = keydet_rv.r07 & Chr(tmp)
            Case 8
              keydet_rv.r08 = keydet_rv.r08 & Chr(tmp)
            Case 9
              keydet_rv.r09 = keydet_rv.r09 & Chr(tmp)
            Case 10
              keydet_rv.r10 = keydet_rv.r10 & Chr(tmp)
            Case 11
              keydet_rv.r11 = keydet_rv.r11 & Chr(tmp)
            Case 12
              keydet_rv.r12 = keydet_rv.r12 & Chr(tmp)
            Case 13
              keydet_rv.r13 = keydet_rv.r13 & Chr(tmp)
            Case 14
              keydet_rv.r14 = keydet_rv.r14 & Chr(tmp)
          End Select
          If keydet_rd = keydet_ns Then
            keydet_rb = 0
            keydet_f = keydet_f + 1
            keydet_of = 4
            '---
            If (keydet_f = 3 Or keydet_f = 11 Or keydet_f = 13) And nofs = 1 Then
              keydet_of = 5 '--5--01
            End If
            '---IsNumeric
            If keydet_f = 3 Then
              If Trim(keydet_rv.r02) = "1" _
              Or (nfds = 1 And IsNumeric(Trim(keydet_rv.r02))) Then
                'Debug.Print "Ok2"
                nfds = 1
              Else
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
                ' GoTo exit_cycle
              End If
            End If
            '---
            keydet_rd = 0
          End If
        ElseIf (keydet_f = keydet_nc + 1) Then
          mdrn = mdrn + 1
          ReDim Preserve mdr(mdrn)
          'keydet_rv.r14 = Left(keydet_rv.r14, keydet_ns)
          mdr(mdrn) = keydet_rv
          'Debug.Print keydet_rv.r01 & " " & keydet_rv.r02 & " " & keydet_rv.r03 & " " & keydet_rv.r04 & " " & keydet_rv.r05 & " " & keydet_rv.r06 & " " _
          '& keydet_rv.r07 & " " & keydet_rv.r08 & " " & keydet_rv.r09 & " " & keydet_rv.r10 & " " & keydet_rv.r11 & " " & keydet_rv.r12 & " " _
          '& keydet_rv.r13 & " " & keydet_rv.r14
          ''
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
        End If
      End If
      '-----
      ' end
      '-----
      If nfds = 1 And keym(1).readvalue > 0 And keym(6).readvalue > 0 Then
          nfds = 0
          nofs = 0
      End If
      '
      If (keym(1).readvalue > 0 And keym(6).readvalue > 0) Or (ns = 1) Then
        If (mdrk = keydets_k) And (keydets_st = 0) And (nfds = 1) Then
            keydets_st = 1 '???
        'End If
        ElseIf (keydets_st = 1) And (mdrk = keydet_k) And keym(1).readvalue > 0 Then
          keydets_st = 0
        End If
        If (keydets_st = 1) Then
          If (InStr(1, keydets_k2, Chr(tmp)) = 1) And (keydets_k2r = "") And keydets_r = 0 Then
            keydets_k2r = Chr(tmp)
          ElseIf (InStr(1, keydets_k2, keydets_k2r & Chr(tmp)) = 1) And (keydets_k2r <> "") And keydets_r = 0 Then
            keydets_k2r = keydets_k2r & Chr(tmp)
            If keydets_k2r = keydets_k2 And keydets_r = 0 Then
              keydets_r = 1
              'keydets_kr = "OK"
              keydets_ds = 1
              keydets_k2r = ""
              For k = 1 To mdrn
                ncca = ncca + 1
                mdr(k).r01 = Replace(mdr(k).r01, ";", " ")
                mdr(k).r02 = Replace(mdr(k).r02, ";", " ")
                mdr(k).r03 = Replace(mdr(k).r03, ";", " ")
                mdr(k).r04 = Replace(mdr(k).r04, ";", " ")
                mdr(k).r05 = Replace(mdr(k).r05, ";", " ")
                mdr(k).r06 = Replace(mdr(k).r06, ";", " ")
                mdr(k).r07 = Replace(mdr(k).r07, ";", " ")
                mdr(k).r08 = Replace(mdr(k).r08, ";", " ")
                mdr(k).r09 = Replace(mdr(k).r09, ";", " ")
                mdr(k).r10 = Replace(mdr(k).r10, ";", " ")
                mdr(k).r11 = Replace(mdr(k).r11, ";", " ")
                mdr(k).r12 = Replace(mdr(k).r12, ";", " ")
                mdr(k).r13 = Replace(mdr(k).r13, ";", " ")
                mdr(k).r14 = Replace(mdr(k).r14, ";", " ")
                ' & keym(2).value & ";" & keym(3).value & ";" & keym(4).value & ";" & keym(5).value
                Print #5, file & ";" & ncf & ";" & keymb(2) & ";" & keymb(3) & ";" & keymb(4) & ";" & keymb(5) _
                  & ";" & mdr(k).r01 & ";" & mdr(k).r02 & ";" & mdr(k).r03 & ";" & mdr(k).r04 & ";" & mdr(k).r05 & ";" & mdr(k).r06 & ";" _
                  & mdr(k).r07 & ";" & mdr(k).r08 & ";" & mdr(k).r09 & ";" & mdr(k).r10 & ";" & mdr(k).r11 & ";" & mdr(k).r12 & ";" _
                  & mdr(k).r13 & ";" & mdr(k).r14 & ";"
                If ncca = 100 Then
                  ncca = 0
                  Close #5
                  Open data_res & file & "A.txt" For Append As #5
                End If
              Next k
              mdrn = 0
              ReDim mdr(mdrn)
            End If
          ElseIf (InStr(1, keydets_k2, keydets_k2r & Chr(tmp)) <> 1) And (keydets_k2r <> "") And keydets_r = 0 Then
            keydets_k2r = ""
          ElseIf (keydets_r = 1) And (keydets_f < keydets_nc) Then
            If tmp > 29 And keydets_ds = 0 And keydets_or = 0 Then
              keydets_ds = 1
              Select Case keydets_f
                'Case 1
                '  keydets_rv.r01 = Left(keydet_rv.r01, keydet_ns)
              Case 2
                keydets_rv.r02 = Left(keydets_rv.r02, keydets_ns)
              Case 3
                keydets_rv.r03 = Left(keydets_rv.r03, keydets_ns)
              Case 4
                keydets_rv.r04 = Left(keydets_rv.r04, keydets_ns)
              Case 5
                keydets_rv.r05 = Left(keydets_rv.r05, keydets_ns)
              Case 6
                keydets_rv.r06 = Left(keydets_rv.r06, keydets_ns)
              Case 7
                keydets_rv.r07 = Left(keydets_rv.r07, keydets_ns)
              Case 8
                keydets_rv.r08 = Left(keydets_rv.r08, keydets_ns)
              Case 9
                keydets_rv.r09 = Left(keydets_rv.r09, keydets_ns)
              Case 10
                keydets_rv.r10 = Left(keydets_rv.r10, keydets_ns)
              End Select
              keydets_ns = buff(nbuf - 1)
              keydets_f = keydets_f + 1
              Select Case keydets_f
              Case 1
                keydets_rv.r01 = Chr(tmp)
              Case 2
                keydets_rv.r02 = Chr(tmp)
              Case 3
                keydets_rv.r03 = Chr(tmp)
              Case 4
                keydets_rv.r04 = Chr(tmp)
              Case 5
                keydets_rv.r05 = Chr(tmp)
              Case 6
                keydets_rv.r06 = Chr(tmp)
              Case 7
                keydets_rv.r07 = Chr(tmp)
              Case 8
                keydets_rv.r08 = Chr(tmp)
              Case 9
                keydets_rv.r09 = Chr(tmp)
              Case 10
                keydets_rv.r10 = Chr(tmp)
              End Select
            ElseIf tmp > 29 And keydets_ds = 1 And keydets_or = 0 Then
              Select Case keydets_f
              Case 1
                keydets_rv.r01 = keydets_rv.r01 & Chr(tmp)
              Case 2
                keydets_rv.r02 = keydets_rv.r02 & Chr(tmp)
              Case 3
                keydets_rv.r03 = keydets_rv.r03 & Chr(tmp)
              Case 4
                keydets_rv.r04 = keydets_rv.r04 & Chr(tmp)
              Case 5
                keydets_rv.r05 = keydets_rv.r05 & Chr(tmp)
              Case 6
                keydets_rv.r06 = keydets_rv.r06 & Chr(tmp)
              Case 7
                keydets_rv.r07 = keydets_rv.r07 & Chr(tmp)
              Case 8
                keydets_rv.r08 = keydets_rv.r08 & Chr(tmp)
              Case 9
                keydets_rv.r09 = keydets_rv.r09 & Chr(tmp)
              Case 10
                keydets_rv.r10 = keydets_rv.r10 & Chr(tmp)
              End Select
            ElseIf (tmp < 30) And (keydets_ds = 1) And (keydets_or = 0) Then
              keydets_or = 1
              keydets_ds = 0
            ElseIf (keydets_ds = 0) And (keydets_or > 0) And (keydets_or < keydets_of) Then
              keydets_or = keydets_or + 1
              If keydets_or = keydets_of Then
                If keydets_f = 1 Then
                  keydets_rv.r01 = Left(keydets_rv.r01, keydets_ns)
                End If
                keydets_ns = tmp
                keydets_or = 0
                keydets_ds = 0
              End If
            End If
          ElseIf (keydets_f = keydets_nc) And (keydets_or = 0) And (tmp > 29) And (keydets_ds = 1) Then
              Select Case keydets_f
              Case 1
                keydets_rv.r01 = keydets_rv.r01 & Chr(tmp)
              Case 2
                keydets_rv.r02 = keydets_rv.r02 & Chr(tmp)
              Case 3
                keydets_rv.r03 = keydets_rv.r03 & Chr(tmp)
              Case 4
                keydets_rv.r04 = keydets_rv.r04 & Chr(tmp)
              Case 5
                keydets_rv.r05 = keydets_rv.r05 & Chr(tmp)
              Case 6
                keydets_rv.r06 = keydets_rv.r06 & Chr(tmp)
              Case 7
                keydets_rv.r07 = keydets_rv.r07 & Chr(tmp)
              Case 8
                keydets_rv.r08 = keydets_rv.r08 & Chr(tmp)
              Case 9
                keydets_rv.r09 = keydets_rv.r09 & Chr(tmp)
              Case 10
                keydets_rv.r10 = keydets_rv.r10 & Chr(tmp)
              End Select
          ElseIf (keydets_f = keydets_nc) And (keydets_or = 0) And (tmp < 30) Then
            keydets_or = 1
          ElseIf (keydets_f = keydets_nc) And (keydets_or < keydets_of) And (keydets_or > 0) Then
            keydets_or = keydets_or + 1
          ElseIf (keydets_f = keydets_nc) And (keydets_or = keydets_of) Then
            keydets_rv.r10 = Left(keydets_rv.r10, keydets_ns)
            'Debug.Print keydets_rv.r01 & " " & keydets_rv.r02 & " " & keydets_rv.r03 & " " & keydets_rv.r04 & " " & keydets_rv.r05 & " " _
            '& keydets_rv.r06 & " " & keydets_rv.r07 & " " & keydets_rv.r08 & " " & keydets_rv.r09 & " " & keydets_rv.r10
            '
            nccb = nccb + 1
            '& keym(2).value & ";" & keym(3).value & ";" & keym(4).value & ";" & keym(5).value
            keydets_rv.r01 = Replace(keydets_rv.r01, ";", " ")
            keydets_rv.r02 = Replace(keydets_rv.r02, ";", " ")
            keydets_rv.r03 = Replace(keydets_rv.r03, ";", " ")
            keydets_rv.r04 = Replace(keydets_rv.r04, ";", " ")
            keydets_rv.r05 = Replace(keydets_rv.r05, ";", " ")
            keydets_rv.r06 = Replace(keydets_rv.r06, ";", " ")
            keydets_rv.r07 = Replace(keydets_rv.r07, ";", " ")
            keydets_rv.r08 = Replace(keydets_rv.r08, ";", " ")
            keydets_rv.r09 = Replace(keydets_rv.r09, ";", " ")
            keydets_rv.r10 = Replace(keydets_rv.r10, ";", " ")
            Print #6, file & ";" & ncf & ";" & keymb(2) & ";" & keymb(3) & ";" & keymb(4) & ";" & keymb(5) _
                  & ";" & keydets_rv.r01 & ";" & keydets_rv.r02 & ";" & keydets_rv.r03 & ";" & keydets_rv.r04 & ";" & keydets_rv.r05 _
                  & ";" & keydets_rv.r06 & ";" & keydets_rv.r07 & ";" & keydets_rv.r08 & ";" & keydets_rv.r09 & ";" & keydets_rv.r10 & ";"
            If nccb = 100 Then
              nccb = 0
              Close #6
              Open data_res & file & "B.txt" For Append As #6
            End If
            'For k = 1 To 16
            '  keym(k).read = 0
            '  keym(k).ST = 0
            '  keym(k).keyr = ""
            '  keym(k).value = ""
            '  keym(k).readvalue = 0
            '  keym(k).readnb = 0
            'Next k
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
            '
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
          End If
        End If
        If keydets_kr = "OK" Then
          'Debug.Print "OK"
          keydets_kr = ""
        End If
  '----keydets
  'keydets_k = "062003040003030000"
  'keydets_kr = ""
  'keydets_k2 = "Итого в учетном периоде начислено:"
  'keydets_k2r = ""
  'keydets_st = 0
  'keydets_nc = 10
  'keydets_of = 5
  'keydets_r = 0
  'keydets_f = 0
  'keydet_rv2
      '-----
      End If
        'Debug.Print TMP, Chr(TMP)
        'цикл по ключам отладочный блок
        'If keym(1).readvalue > 0 Or ns = 1 Then
        '  nbr = nbr + 1
        '  ric = is_command(tmp)
        '  If pim = 1 And ric = "" Then
        '    If mskn > 2 Then
        '      mask = mask & mskp & "(" & mskn & ")" & "."
        '    ElseIf mskn = 2 Then
        '      mask = mask & mskp & mskp & "."
        '    Else
        '      mask = mask & mskp & "."
        '    End If
        '    pim = 0
        '    mskp = ""
        '    mskn = 0
        '  ElseIf ric <> "" Then
        '    If mskp = ric Then
        '      mskn = mskn + 1
        '    Else
        '      If (mskn > 2) And (mskp <> ric) Then
        '        mask = mask & mskp & "(" & mskn & ")" '& ric
        '        mskn = 1
        '      ElseIf mskn = 2 Then
        '        mask = mask & mskp & mskp '& ric
        '        mskn = 1
        '      Else
        '        If mskp <> "" Then
        '          mask = mask & mskp '& ric
        '        End If
        '        mskn = 1
        '      End If
        '    End If
        '    pim = 1
        '    mskp = ric
        '  End If
        'End If
        '
        'Debug.Assert InStr(buf_txt, "57776680") = 0
        For i = 1 To 16
          If keym(i).n = i Then
            If keym(i).read = 0 Then
              'Debug.Assert mp <> 14
              mp = i
              If keym(i).ST = 0 Then
                If InStr(1, keym(i).Key, Chr(tmp)) = 1 Then
                  keym(i).ST = 1
                  keym(i).keyr = Chr(tmp)
                  If keym(i).keyr = keym(i).Key Then
                    keym(i).readvalue = 1
                    'Active mobi
                    If i = 12 Then
                      If InStr(1, buf_txt, "mobi") > 0 Then
                        keym(12).ofs = keym(12).ofs + 5
                        keym(12).ofsbuf = keym(12).ofsbuf + 5
                      Else
                        keym(12).ofs = keym(12).ofs + 7
                        keym(12).ofsbuf = keym(12).ofsbuf + 7
                      End If
                    End If
                    '
                    If keym(i).readvalue > 0 And i = 1 Then
                      ns = 0
                      nbr = 5
                      ' Print #4, mask
                      'mask = is_command(buff(nbuf - 4)) & is_command(buff(nbuf - 3)) & is_command(buff(nbuf - 2)) _
                      '  & is_command(buff(nbuf - 1)) & is_command(buff(nbuf))
                    End If
                    If keym(i).ubuf = 1 Then
                      tr = 0
                      For k = 1 To 60
                        If k > Len(keym(i).Key) + keym(i).ofsbuf Then
                          If buff(61 - k) > 29 Then
                            keym(i).value = Chr(buff(61 - k)) & keym(i).value
                            tr = 1
                          ElseIf tr = 1 Then
                            keym(i).read = 2
                            keym(i).value = Trim(keym(i).value)
                            GoTo exit_cycle
                          End If
                        End If
                      Next k
                    End If
                  End If
                ElseIf (keym(i).const = 0) And (i < 16) Then
                  If keym(i + 1).ST = 0 Then
                    If InStr(1, keym(i + 1).Key, Chr(tmp)) = 1 Then
                      keym(i + 1).ST = 1
                      keym(i + 1).keyr = Chr(tmp)
                      If keym(i + 1).keyr = keym(i + 1).Key Then
                        keym(i + 1).readvalue = 1
                        If keym(i + 1).readvalue > 0 And i = 1 Then
                          ns = 0
                          nbr = 5
                          ' Print #4, mask
                          'mask = is_command(buff(nbuf - 4)) & is_command(buff(nbuf - 3)) & is_command(buff(nbuf - 2)) _
                          '  & is_command(buff(nbuf - 1)) & is_command(buff(nbuf))
                        End If
                        If keym(i + 1).ubuf = 1 Then
                          tr = 0
                          For k = 1 To 60
                            If k > Len(keym(i + 1).Key) + keym(i + 1).ofsbuf Then
                              If buff(61 - k) > 29 Then
                                keym(i + 1).value = Chr(buff(61 - k)) & keym(i + 1).value
                                tr = 1
                              ElseIf tr = 1 Then
                                keym(i + 1).read = 2
                                keym(i + 1).value = Trim(keym(i + 1).value)
                                keym(i).read = 2
                                keym(i).value = "0"
                                GoTo exit_cycle
                              End If
                            End If
                          Next k
                        End If
                      End If
                    End If
                  ElseIf keym(i + 1).ST <> 0 Then
                    If InStr(1, keym(i + 1).Key, keym(i + 1).keyr & Chr(tmp)) = 1 And keym(i + 1).readvalue = 0 Then
                      keym(i + 1).keyr = keym(i + 1).keyr & Chr(tmp)
                      If keym(i + 1).keyr = keym(i + 1).Key Then
                        keym(i + 1).readvalue = 1
                        If keym(i + 1).ubuf = 1 Then
                          tr = 0
                          For k = 1 To 60
                            If k > Len(keym(i + 1).Key) + keym(i + 1).ofsbuf Then
                              If buff(61 - k) > 29 Then
                                keym(i + 1).value = Chr(buff(61 - k)) & keym(i + 1).value
                                tr = 1
                              ElseIf tr = 1 Then
                                keym(i + 1).read = 2
                                keym(i + 1).value = Trim(keym(i + 1).value)
                                keym(i).read = 2
                                keym(i).value = "0"
                                GoTo exit_cycle
                              End If
                            End If
                          Next k
                        End If
                      End If
                    End If
                  End If
                End If
                GoTo exit_cycle
              Else
                If InStr(1, keym(i).Key, keym(i).keyr & Chr(tmp)) = 1 And keym(i).readvalue = 0 Then
                  keym(i).keyr = keym(i).keyr & Chr(tmp)
                  If keym(i).keyr = keym(i).Key Then
                    keym(i).readvalue = 1
                    'Active mobi
                    If i = 12 Then
                      If InStr(1, buf_txt, "mobi") > 0 Then
                        keym(12).ofs = keym(12).ofs + 5
                        keym(12).ofsbuf = keym(12).ofsbuf + 5
                      Else
                        keym(12).ofs = keym(12).ofs + 7
                        keym(12).ofsbuf = keym(12).ofsbuf + 7
                      End If
                    End If
                    If keym(i).readvalue > 0 And i = 1 Then
                      ns = 0
                      nbr = 5
                      If ncf > 0 Then
                        ' Print #4, mask
                      End If
                      'mask = is_command(buff(nbuf - 4)) & is_command(buff(nbuf - 3)) & is_command(buff(nbuf - 2)) _
                      '  & is_command(buff(nbuf - 1)) & is_command(buff(nbuf))
                    End If
                    If keym(i).ubuf = 1 Then
                      tr = 0
                      For k = 1 To 60
                        If k > Len(keym(i).Key) + keym(i).ofsbuf Then
                          If buff(61 - k) > 29 Then
                            keym(i).value = Chr(buff(61 - k)) & keym(i).value
                            tr = 1
                          ElseIf tr = 1 Then
                            keym(i).read = 2
                            keym(i).value = Trim(keym(i).value)
                            GoTo exit_cycle
                          End If
                        End If
                      Next k
                    End If
                  End If
                ElseIf keym(i).readvalue = 0 Then
                  keym(i).keyr = ""
                  keym(i).ST = 0
                  GoTo exit_cycle
                Else
                  If keym(i).readnb = 0 And Chr(tmp) = "*" Then
                    keym(i).readnb = 0
                  Else
                    keym(i).readnb = keym(i).readnb + 1
                  End If
                  If keym(i).readnb > keym(i).ofs Then
                    If tmp > 29 Then
                      keym(i).value = keym(i).value & Chr(tmp)
                      GoTo exit_cycle
                    Else
                      keym(i).read = 2
                      keym(i).value = Trim(keym(i).value)
                      If i = 16 Then
                        For k = 1 To 16
                          If InStr(keym(k).value, ",") > 0 Then
                            keym(k).value = Left(keym(k).value, InStr(keym(k).value, ",") + 2)
                            keym(k).value = Replace(keym(k).value, ";", " ")
                          End If
                        Next k
                        keym(2).value = Left(keym(2).value, Len(keym(2).value) - 1)
                        'keym(5).value = Left(keym(5).value, Len(keym(5).value) - 1)
                        'keym(8).value = Left(keym(8).value, Len(keym(8).value) - 1)
                        'keym(9).value = Left(keym(9).value, Len(keym(9).value) - 1)
                        'keym(13).value = Left(keym(13).value, Len(keym(13).value) - 1)
                        'keym(14).value = Left(keym(14).value, Len(keym(14).value) - 1)
                        ncf = ncf + 1
                        'Debug.Assert ncf <> 4
                        ncc = ncc + 1
                        '
                        'mask = Replace(mask, "0", "")
                        '
                        Print #4, file & ";" & ncf & ";" & keym(1).value & ";" & keym(2).value & ";" & keym(3).value & ";" & keym(4).value _
                        & ";" & keym(5).value & ";" & keym(6).value & ";" & keym(7).value & ";" & keym(8).value & ";" & keym(9).value _
                        & ";" & keym(10).value & ";" & keym(11).value & ";" & keym(12).value & ";" & keym(13).value & ";" & keym(14).value _
                        & ";" & keym(15).value & ";" & keym(16).value & ";" '& nbr & ";" & mask & ";"
                        If ncc = 100 Then
                          ncc = 0
                          Close #4
                          Open data_res & file & ".txt" For Append As #4
                        End If
                        'If ncf = 5000 Then
                        '  GoTo exit_tst
                        'End If
                        'Debug.Print
                        For k = 1 To 16
                          keym(k).read = 0
                          keym(k).ST = 0
                          keym(k).keyr = ""
                          keymb(k) = keym(k).value
                          keym(k).value = ""
                          keym(k).readvalue = 0
                          keym(k).readnb = 0
                        Next k
                        keym(12).ofs = 5
                        keym(12).ofsbuf = 5
                        mp = 0
                        'nbr = 0
                        'mask = ""
                        ns = 1
                      End If
                      GoTo exit_cycle
                    End If
                  End If
                End If
              End If
              GoTo exit_cycle
            End If
          End If
        Next i
exit_cycle:
      '
    Loop
exit_tst:
  If ns = 1 Then
    'Print #4, mask
  End If
  Close #SF
  Close #6
  Close #5
  Close #4
  pars_data4 = ncf
End Function