Attribute VB_Name = "AMDOCS_pdl"
Option Explicit

Type parskey ' структура ключа
    Key As String ' ключ
    n As Integer ' порядок
    read As Integer ' прочитан
    ST As Integer ' начато определение
    keyr As String ' прочитанный ключ
    ofs As Integer ' смещение до начала данных
    value As String ' значение ключа
    readvalue As Integer ' начато чтение значений
    readnb As Integer ' прочитано байт значения
    ubuf As Integer 'читать буфер
    const As Integer 'может отсутствовать
    ofsbuf As Integer 'буфер смещение до начала данных
End Type

Sub tst_do()
  SetFileAttributes Workbooks("amdocs_pdl.xlsm").path & "\TEMPOR0.TMP", 7
End Sub

Sub tst_do33()
  Dim DF As Long, SF As Long, nb As Integer, tmp  As Byte, tmpr As Byte
  SF = FreeFile
  Open Workbooks("amdocs_pdl.xlsm").path & "\ARS20170123220008.DAT2.TXT" For Binary As #SF
  DF = FreeFile
  nb = 1
  Open Workbooks("amdocs_pdl.xlsm").path & "\ARS20170123220008.DAT2_.TXT" For Binary As #DF
  Do Until EOF(SF) '
     Get #SF, , tmp '
     tmpr = tmp
     'hi = in_byte \ 16
     'lo = in_byte - 16 * hi
     Put #DF, , tmp '
     nb = nb + 1
     If nb > 80 And tmp = 32 Then
       nb = 0
       Put #DF, , 10 '
       'Put #DF, , 13 '
     End If
  Loop
  Close #SF '
  Close #DF '
End Sub

Sub split_file2()
  Dim tmp(1000000) As Byte, n As Long, pth As String, fln As String, fn As Long
  Dim oFile As New clsFile, sFile As New clsFile, redb As Long, nr As Long
  Dim nn As Integer, s(10) As String
     
  s(1) = "201_d-04.5-1zakr.iso" '"181_gold2.tskr.1z.iso" '"131_Corset-Ac.dec1.iso" '"101_Corset-Ac1-20-1.iso" '"051_Corset-Acad.kry.iso"
  s(2) = "202_d-04.5-2zakr.iso" '"182_gold2.tskr.2z.iso" '"132_Corset-Ac.col2.iso" '"102_Corset-Ac1-20-2.iso" '"061_Corset-Ac.noa2.iso"
  s(3) = "211_Corset-Ac.3-2-1z.iso" '"191_Cor-Ac.d3-3.1zak.iso" '"141_Corset-Academy3.iso" '"111_AKpopul.1-1zakr.iso" '"071_Corset-A.plux1.1.iso"
  s(4) = "212_Corset-Ac.3-2-2z.iso" '192_Cor-Ac.d3-3.2zak.iso" '"151_Corset-Acad.biz.iso" '"112_AKpopul.1-2zakr.iso" '"072_Corset-A.plux2.2.iso"
  s(5) = "221_Cor.Ak.sloz.ub1.iso" '"161_Corset-Ac.d4-4.1.iso" '"121_Corset-Acad.4-1.iso" '"081_Corset-Ak.color.iso"
  s(6) = "" '"162_Corset-Ac.d4-4.2.iso" '"122_Corset-Acad.4-2.iso" '"091_AKpopul.2-1w-m.iso"
  s(7) = "" '"171_Corset-Ac.2-2.1.iso" '"131_Corset-Ac.dec1.iso" '"092_AKpopul.2-2w-m.iso"
  s(8) = "" '"172_Corset-Ac.2-2.2.iso"
     
  For nn = 5 To 5
     
  nr = 0
  fn = 0
  pth = "C:\dsk\01" '"121_Corset-Acad.4-1.iso" '"121_Corset-Acad.4-1.iso" '"TEST_BYTE.TXT" 121_Corset-Acad.4-1.jpg
  fln = s(nn)
  '"042_Corset-Ac.4-3-2.iso" '"041_Corset-Ac.4-3-1.iso"
  '"031_Corset-Acad.osk.iso"'"021_Corset-Ac.noa1.iso"'"012_Corset-Acad.4-2.iso" '"011_Corset-Ac.4-2-1.iso"

  oFile.OpenFile pth & "\" & fln
  sFile.OpenFile pth & "\" & fln & "_" & fn, FILE_GENERIC_WRITE
      
  ' Debug.Print oFile.Length
  
  oFile.SeekFile 0, so_Begin
  redb = oFile.ReadBytes(tmp, 0, 1000000)
  'sFile.SeekFile 0, so_Begin
  
  Do While redb > 0
  
  If redb > 0 Then
    sFile.WriteBytes tmp, 0, redb
    'sFile.SeekFile redb, so_Current
    nr = nr + 1
    'Debug.Print nr
    
    If nr = 1750 Then
      
      nr = 0
      fn = fn + 1
      sFile.CloseFile
      sFile.OpenFile pth & "\" & fln & "_" & fn, FILE_GENERIC_WRITE
      
    End If
    
  End If
  
  'oFile.SeekFile redb, so_Current
  redb = oFile.ReadBytes(tmp, 0, 1000000)
    
  Loop
  
  'oFile.Flush
  'sFile.Flush
   
  oFile.CloseFile
  sFile.CloseFile
    
  Next nn
  
End Sub

Sub split_file() '2GB
  Dim DF As Long, SF As Long, tmp(1000000) As Byte, n As Long, pth As String, fln As String, fn As Long
  fn = 0
  pth = "C:\dsk"
  fln = "121_Corset-Acad.4-1.iso"
  n = 0
  SF = FreeFile
  Open pth & "\" & fln For Binary As #SF
  DF = FreeFile
  Open pth & "\" & fln & "_" & fn For Binary As #DF
  Do Until EOF(SF)
    Get #SF, , tmp
    Put #DF, , tmp
    n = n + 1
    'Debug.Print n
    If n > 1500 Then
      Close #DF
      n = 0
      fn = fn + 1
      DF = FreeFile
      Open pth & "\" & fln & "_" & fn For Binary As #DF
    End If
  Loop
  Close #SF
  Close #DF
End Sub



Sub tst_do2()
  Dim n As Long
  n = pars_data4("ARS20170123220008.DAT", "C:\vitaliy\z\PDL\APP\data_res\") 'ARS20170123220008.DAT'TEMPOR0_DBG.TXT
End Sub

Function do_file(ByVal file As String, ByVal data As String, ByVal data_done As String, ByVal data_res As String, _
stdf As Double, stdt As Double) As Integer
  Dim e, n, es, ps As Integer, d As Date
  cpb data & file
  d = Now
  e = exb
  es = 0
  ps = 0
  If e = 1 Then
    prd
    es = Int(86400 * (Now - d))
    d = Now
    n = pars_data4(file, data_res, stdf, stdt)
    Sleep 100
    DeleteFile Workbooks("amdocs_pdl.xlsm").path & "\TEMPOR2.TMP"
    Sleep 100
    CopyFile data & file, data_done & file, 0
    Sleep 100
    DeleteFile data & file
    'Debug.Print file, e, n
    do_file = n
    ps = Int(86400 * (Now - d))
  Else
    Sleep 100
    DeleteFile Workbooks("amdocs_pdl.xlsm").path & "\TEMPOR0.TMP"
    do_file = 0
  End If
  Open Workbooks("amdocs_pdl.xlsm").path & "\TEMPOR0.LOG" For Append As #1
    Print #1, " start : " & d & "; " & file & "; ext: " & e & " (" & es & "); row: " & n & " (" & ps & ");"
  Close #1
End Function

Function cpb(file As String) As Integer
  Dim SF As Long, DF As Long, DestinationFile As String, n As Long
  Dim tmp As Byte
  DestinationFile = Workbooks("amdocs_pdl.xlsm").path & "\TEMPOR0.TMP"
  DeleteFile DestinationFile
  Sleep 100
  SF = FreeFile
  Open file For Binary As #SF
  Sleep 100
  DF = FreeFile
  Open DestinationFile For Binary As #DF
  n = 0
  Do Until EOF(SF) '
     Get #SF, , tmp '
     n = n + 1
     If n > 9 Then
       Put #DF, , tmp '
     End If
  Loop
  Close #SF '
  Close #DF '
  Sleep 100
  SetFileAttributes DestinationFile, 1 Or 2 Or 4
End Function

Function exb() As Integer
  Dim readstr As String, ps As Integer
  Sleep 100
  Open Workbooks("amdocs_pdl.xlsm").path & "\TEMPOR0.BAT" For Output As #1
      Print #1, "cd " & Workbooks("amdocs_pdl.xlsm").path & "\"
      Print #1, Workbooks("amdocs_pdl.xlsm").path & "\tmp.dll  e -y " & Workbooks("amdocs_pdl.xlsm").path & _
      "\TEMPOR0.TMP > " & Workbooks("amdocs_pdl.xlsm").path & "\TEMPOR1.TMP"
  Close #1
  Sleep 100
  SetFileAttributes Workbooks("amdocs_pdl.xlsm").path & "\TEMPOR0.BAT", 1 Or 2 Or 4
  Sleep 100
  ShellAndWait2 Workbooks("amdocs_pdl.xlsm").path & "\" & "TEMPOR0.BAT", 4000000, vbHide, IgnoreBreak
  Sleep 100
  DeleteFile Workbooks("amdocs_pdl.xlsm").path & "\TEMPOR0.BAT"
  Sleep 100
  SetFileAttributes Workbooks("amdocs_pdl.xlsm").path & "\TEMPOR1.TMP", 1 Or 2 Or 4
  Sleep 100
  DeleteFile Workbooks("amdocs_pdl.xlsm").path & "\TEMPOR0.TMP"
  Sleep 100
  Open Workbooks("amdocs_pdl.xlsm").path & "\TEMPOR1.TMP" For Input As #1
    Do While Not EOF(1)
      Line Input #1, readstr
      'Debug.Print readstr
      If ps = 0 Then
        ps = InStr(1, readstr, "Everything is Ok")
      End If
    Loop
  Close #1
  Sleep 100
  DeleteFile Workbooks("amdocs_pdl.xlsm").path & "\TEMPOR1.TMP"
  exb = ps
End Function

Function prd() As Integer
  Dim SF As Long, DF As Long
  Dim tmp As Byte
  SetFileAttributes Workbooks("amdocs_pdl.xlsm").path & "\TEMPOR0", 7
  DeleteFile Workbooks("amdocs_pdl.xlsm").path & "\TEMPOR2.TMP"
  Sleep 100
  SF = FreeFile
  Open Workbooks("amdocs_pdl.xlsm").path & "\TEMPOR0" For Binary As #SF
  Sleep 100
  DF = FreeFile
  Open Workbooks("amdocs_pdl.xlsm").path & "\TEMPOR2.TMP" For Binary As #DF
  Do Until EOF(SF) '
     Get #SF, , tmp '
     tmp = tmp Xor 127
     Put #DF, , tmp '
  Loop
  Close #SF '
  Close #DF '
  Sleep 100
  DeleteFile Workbooks("amdocs_pdl.xlsm").path & "\TEMPOR0"
  Sleep 100
  SetFileAttributes Workbooks("amdocs_pdl.xlsm").path & "\TEMPOR2.TMP", 7
End Function

Sub run_parse()
Attribute run_parse.VB_Description = "Start parse"
Attribute run_parse.VB_ProcData.VB_Invoke_Func = "R\n14"
  Dim data  As String, data_done As String, data_res As String, SearchStr  As String, stdt As Double
  '
  Dim FileName As String   ' Walking filename variable...
  Dim DirName As String    ' SubDirectory Name
  Dim dirNames() As String ' Buffer for directory name entries
  Dim nDir As Long   ' Number of directories in this path
  Dim i As Long      ' For-loop counter...
  Dim hSearch As Long   ' Search Handle
  Dim WFD As WIN32_FIND_DATA
  Dim Cont As Integer
  Dim FT As FILETIME
  Dim ST As SYSTEMTIME
  Dim fr As recfile
  Dim pt As String, stdf As Double
  '
  stdt = Year(Now) * 10000 + Month(Now) * 100 + Day(Now)
  SearchStr = "*.BILL"
  data = Workbooks("amdocs_pdl.xlsm").path & "\" & Workbooks("amdocs_pdl.xlsm").Worksheets("Sheet1").Cells(1, 2)
  data_done = Workbooks("amdocs_pdl.xlsm").path & "\" & Workbooks("amdocs_pdl.xlsm").Worksheets("Sheet1").Cells(2, 2)
  data_res = Workbooks("amdocs_pdl.xlsm").path & "\" & Workbooks("amdocs_pdl.xlsm").Worksheets("Sheet1").Cells(3, 2)
  ' Debug.Print data, data_done, data_res '--Row, Column
  ' цикл по файлам
'---------------------------------------------------------------------------
'код выполняющий поиск файлов
'---------------------------------------------------------------------------
'   Function FindFilesAPI(path As String, SearchStr As String)
   If Right(data, 1) <> "\" Then data = data & "\"
   If Right(data_done, 1) <> "\" Then data_done = data_done & "\"
   If Right(data_res, 1) <> "\" Then data_res = data_res & "\"
   ' Search for subdirectories.
   nDir = 0
   ReDim dirNames(nDir)
   Cont = True
   hSearch = FindFirstFile(data & "*.BILL", WFD)
   If hSearch <> INVALID_HANDLE_VALUE Then
      Do While Cont
         DirName = StripNulls(WFD.cFileName)
         ' Ignore the current and encompassing directories.
         If (DirName <> ".") And (DirName <> "..") Then
            ' Check for directory with bitwise comparison.
            If GetFileAttributes(data & DirName) And _
             FILE_ATTRIBUTE_DIRECTORY Then
               dirNames(nDir) = DirName
               nDir = nDir + 1
               ReDim Preserve dirNames(nDir)
               ' Uncomment the next line to list directories
               ' List1.AddItem DirName
            End If
         End If
         Cont = FindNextFile(hSearch, WFD)  ' Get next subdirectory.
      Loop
      Cont = FindClose(hSearch)
   End If
   ' Walk through this directory and sum file sizes.
   hSearch = FindFirstFile(data & SearchStr, WFD)
   Cont = True
   
   If hSearch <> INVALID_HANDLE_VALUE Then
      While Cont
         FileName = StripNulls(WFD.cFileName)
            If (FileName <> ".") And (FileName <> "..") And _
              ((GetFileAttributes(data & FileName) And _
               FILE_ATTRIBUTE_DIRECTORY) <> FILE_ATTRIBUTE_DIRECTORY) Then
            ' To list files w/o dates, uncomment the next line
            ' and remove or Comment the lines down to End If
            FileTimeToLocalFileTime WFD.ftCreationTime, FT
            FileTimeToSystemTime FT, ST
            stdf = (ST.wYear + 0.00001) * 10000 + ST.wMonth * 100 + ST.wDay
            'stdf = (ST.wYear + 0.00001) * 10000
            'stdf = stdf + ST.wMonth * 100 + ST.wDay
            '
            do_file FileName, data, data_done, data_res, stdf, stdt
            'Exit Sub
          End If
         Cont = FindNextFile(hSearch, WFD)  ' Get next file
      Wend
      Cont = FindClose(hSearch)
   End If

   ' If there are sub-directories...
    If nDir > 0 Then
      ' Recursively walk into them...
      For i = 0 To nDir - 1
        FindFilesAPI data & dirNames(i) _
        & "\", SearchStr
      Next i
   End If
End Sub

Sub BASE2()
  Dim SF As Long, DF As Long, SourceFile, DestinationFile, readstr As String, n, ps As Long
  Dim tmp As Byte
  ' open *.bill
  ' update header
  SourceFile = Workbooks("amdocs_pdl.xlsm").Worksheets("Sheet1").Cells(1, 1)
  DestinationFile = "ars_debug" & ".tmp"
  'SF = FreeFile
  Open SourceFile For Binary As #1 '#SF
  'DF = FreeFile
  'Open DestinationFile For Binary As #2 '#DF
  n = 0
  Do Until EOF(1) 'SF
     Get #1, , tmp 'SF
     n = n + 1
     If n > 9 Then
       'Put #2, , TMP 'DF
     End If
  Loop
  Close #1 'SF
  'Close #2 'DF
End Sub

Function GetRegistry(Key, path, ByVal ValueName As String)
    Dim hKey As Long
    Dim lValueType As Long
    Dim sResult As String
    Dim lResultLen As Long
    Dim ResultLen As Long
    Dim x, TheKey As Long

    
    TheKey = -99
    
    Select Case UCase(Key)
        Case "HKEY_CLASSES_ROOT": TheKey = &H80000000
        Case "HKEY_CURRENT_USER": TheKey = &H80000001
        Case "HKEY_LOCAL_MACHINE": TheKey = &H80000002
        Case "HKEY_USERS": TheKey = &H80000003
        Case "HKEY_CURRENT_CONFIG": TheKey = &H80000004
        Case "HKEY_DYN_DATA": TheKey = &H80000005
    End Select
    
    If TheKey = -99 Then
        GetRegistry = "Not Found"
        Exit Function
    End If
    
    If RegOpenKeyA(TheKey, path, hKey) <> 0 Then _
        x = RegCreateKeyA(TheKey, path, hKey)
    
    sResult = Space(100)
    lResultLen = 100
    
    x = RegQueryValueExA(hKey, ValueName, 0, lValueType, _
    sResult, lResultLen)
        
    Select Case x
        Case 0: GetRegistry = Left(sResult, lResultLen - 1)
        Case Else: GetRegistry = "Not Found"
    End Select
    
    RegCloseKey hKey
End Function

Private Function WriteRegistry(ByVal Key As String, _
    ByVal path As String, ByVal entry As String, _
    ByVal value As String)
    
    Dim hKey As Long
    Dim lValueType As Long
    Dim sResult As String
    Dim lResultLen As Long
    Dim TheKey As Long
    Dim x As Long
   
    TheKey = -99
    Select Case UCase(Key)
        Case "HKEY_CLASSES_ROOT": TheKey = &H80000000
        Case "HKEY_CURRENT_USER": TheKey = &H80000001
        Case "HKEY_LOCAL_MACHINE": TheKey = &H80000002
        Case "HKEY_USERS": TheKey = &H80000003
        Case "HKEY_CURRENT_CONFIG": TheKey = &H80000004
        Case "HKEY_DYN_DATA": TheKey = &H80000005
    End Select
    
    If TheKey = -99 Then
        WriteRegistry = False
        Exit Function
    End If

    If RegOpenKeyA(TheKey, path, hKey) <> 0 Then
        x = RegCreateKeyA(TheKey, path, hKey)
    End If

    x = RegSetValueExA(hKey, entry, 0, 1, value, Len(value) + 1)
    If x = 0 Then WriteRegistry = True Else WriteRegistry = False
End Function


Sub UpdateRegistryWithTime()
    Dim RootKey As String, path As String, RegEntry As String, RegVal As Date, LastTime As String
    RootKey = "hkey_current_user"
    path = "software\microsoft\office\9.0\excel\LastStarted"
    RegEntry = "DateTime"
    RegVal = Now()
    LastTime = GetRegistry(RootKey, path, RegEntry)
    Debug.Print LastTime
    
    Call WriteRegistry(RootKey, path, RegEntry, RegVal)
End Sub

Sub TestIt()
    Dim RootKey As String, path As String, RegEntry As String
    RootKey = "hkey_current_user"
    path = "software\microsoft\office\9.0\common\autocorrect"
    RegEntry = "path"
    MsgBox GetRegistry(RootKey, path, RegEntry), vbInformation, _
        path & "\RegEntry"
End Sub


Sub BASEA()
  Dim a, b, C As Byte
  a = 37
  b = 255
  C = a Xor b
  Debug.Print Chr(a)
  Debug.Print Chr(b)
  Debug.Print Chr(C)
  C = C Xor b
  Debug.Print Chr(C)
End Sub

Sub BASE()
Attribute BASE.VB_Description = "a"
Attribute BASE.VB_ProcData.VB_Invoke_Func = " \n14"
  Dim SF As Long, DF As Long, SourceFile, DestinationFile, readstr As String, n, ps As Long
  Dim tmp As Byte
  ' open *.bill
  ' update header
  Debug.Print Workbooks("amdocs_pdl.xlsm").path
  SourceFile = Workbooks("amdocs_pdl.xlsm").path & "\" & Workbooks("amdocs_pdl.xlsm").Worksheets("Sheet1").Cells(1, 1)
  DestinationFile = Workbooks("amdocs_pdl.xlsm").path & "\" & "ars_debug2" & ".tmp"
  SF = FreeFile
  Open SourceFile For Binary As #SF
  DF = FreeFile
  Open DestinationFile For Binary As #DF
  n = 0
  Do Until EOF(SF) '
     Get #SF, , tmp '
     n = n + 1
     If n > 9 Then
       Put #DF, , tmp '
     End If
  Loop
  Close #SF '
  Close #DF '
  ' extract file
  ShellAndWait Workbooks("amdocs_pdl.xlsm").path & "\" & "ars2.bat", 4000000, vbHide, IgnoreBreak
  'ShellAndWait "C:\Program Files\7-Zip\7z.exe" e -y ars_debug.tmp > ars_debug.tmp.ext, 4000000, vbHide, IgnoreBreak
  'check extract
  ps = 0
  Open "ars_debug2.tmp.ext" For Input As #3
    Do While Not EOF(3)
      Line Input #3, readstr
      Debug.Print readstr
      If ps = 0 Then
        ps = InStr(1, readstr, "Everything is Ok")
      End If
    Loop
  Close #3
  'Debug.Print ps
  'Everything is Ok
  
  'Debug.Print "read file"
  ' read file
  'Debug.Print "read file"
End Sub

Function hexbute(ByVal in_byte As Byte) As String
  Select Case in_byte
    Case 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
      hexbute = "" & in_byte
    Case 10
      hexbute = "A"
    Case 11
      hexbute = "B"
    Case 12
      hexbute = "C"
    Case 13
      hexbute = "D"
    Case 14
      hexbute = "E"
    Case 15
      hexbute = "F"
  End Select
End Function

Sub test_bh()
  Debug.Print is_command(11)
  Debug.Print is_command(255)
End Sub

Function is_command(in_byte As Byte) As String
  Dim hi, lo As Integer
  hi = in_byte \ 16
  lo = in_byte - 16 * hi
  is_command = hexbute(hi) & hexbute(lo)
  If (in_byte < 30) Or (in_byte = 127) Or (in_byte > 127 And in_byte < 160) Then  '00-1f 7f 80-9f
    Exit Function
  End If
  is_command = ""
End Function
Sub tst_do4()
  Dim wb As Workbook
  Dim xPro As VBIDE.VBProject
  Set wb = Workbooks("amdocs_pdl.xlsm")
  'wb.VBProject.FileName
  'Debug.Print Workbooks("amdocs_pdl.xlsm ").VBProject.VBComponents.Count"
  Debug.Print wb.VBProject.FileName
End Sub

Sub tst_do5()
  Debug.Print Workbooks("amdocs_pdl.xlsm").CustomDocumentProperties.count, Workbooks("amdocs_pdl.xlsm").CustomDocumentProperties.Item("Test")
Dim docprops As DocumentProperties
Dim docprop As DocumentProperty

Set docprops = ThisWorkbook.CustomDocumentProperties
'Set docprop = docprops.Add(name:="test", LinkToContent:=False, type:=msoPropertyTypeString, value:="xyz")
'ThisWorkbook.Save
  'Dim p As Object
  'For Each p In ActiveWorkbook.CustomDocumentProperties
  '  Debug.Print p.name, p.value
  'Next
  'Debug.Print Application.VBE.VBProjects.Count
  'VBE.VBProjects("amdocs_pdl.xlsm").FileName
End Sub

Sub AddSht_AddCode()
    Dim wb As Workbook
    Dim xPro As VBIDE.VBProject
    Dim xCom As VBIDE.VBComponent
    Dim xMod As VBIDE.CodeModule
    Dim xLine As Long

    Set wb = Workbooks.Add

    With wb
        Set xPro = .VBProject
        Set xCom = xPro.VBComponents("Sheet1")
        Set xMod = xCom.CodeModule

        With xMod
            xLine = .CreateEventProc("Change", "Worksheet")
            xLine = xLine + 1
            .InsertLines xLine, "  Cells.Columns.AutoFit"
        End With
    End With

End Sub


Sub tst_do3()
  Dim n As Long
  n = pars_data4("test_1713_nd7.txt", "C:\vitaliy\z\PDL\APP\data_res\", Year(Now) * 10000 + Month(Now) * 100 + Day(Now), _
  20180301) 'ARS20170123220008.DAT'TEMPOR0_DBG.TXT moby_tv_DAT_txt ARS20170406181023TEMPOR0_
End Sub

Function pars_data4(file As String, data_res As String, stdf As Double, stdt As Double) As Integer
  Dim SF As Long, i As Integer, j As Integer, k As Integer, tr As Integer
  Dim keym(16) As parskey, tmp As Byte, buff(60) As Byte, nbuf As Integer, ncf As Integer, ncc As Integer, keydets_rv As row_det, _
  pim As Integer, mskn As Integer, ns As Integer, mdr() As row_det, mdrn As Long, keydet_rv As row_det, keydet_ds As Integer, keydet_or As Integer, _
  keydet_ns As Integer, keydets_or As Integer, keydets_ds As Integer, keydets_ns As Integer, ncca As Integer, nccb As Integer, keymb(16) As String
  '
  Dim nbr As Long, mask  As String, mskp  As String, ric  As String, _
  keydet_k As String, keydet_kr As String, keydet_st As Integer, keydet_nc As Integer, keydet_of As Integer, keydet_f As Integer, _
  keydets_k As String, keydets_kr As String, keydets_k2 As String, keydets_k2r As String, keydets_st As Integer, keydets_nc As Integer, _
  keydets_of  As Integer, keydets_r  As Integer, buf_txt As String, mp As Integer, mdrk As String, mdr_hi As Integer, mdr_lo As Integer, _
  keydets_f As Integer, keydet_rb As Integer, keydet_rd As Integer, nfds As Integer, nofs As Integer
  'CID
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
  If stdf > 20200201 Or stdt > 20200201 Then
    pars_data4 = 0
    Exit Function
  End If
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
      tmp = tmp Xor 127
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


Sub pars_data3()
  Dim SF As Long, i, j, k, tr As Integer
  Dim keym(15) As parskey, tmp As Byte, buff(60) As Byte, nbuf, ncf, ncc As Integer
  
  'CID
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
  keym(8).ofsbuf = 0
  'Поступления средств (тенге):
  keym(9).Key = "Поступления средств (тенге):"
  keym(9).n = 9
  keym(9).read = 0 ' 0 нет 1 да
  keym(9).ST = 0 ' 0 нет 1 да
  keym(9).keyr = ""
  keym(9).ofs = 6
  keym(9).value = ""
  keym(9).readvalue = 0
  keym(9).readnb = 0
  keym(9).ubuf = 0
  keym(9).ofsbuf = 0
  'Перенос баланса (тенге):
  keym(10).Key = "Перенос баланса (тенге):"
  keym(10).n = 10
  keym(10).read = 0 ' 0 нет 1 да
  keym(10).ST = 0 ' 0 нет 1 да
  keym(10).keyr = ""
  keym(10).ofs = 15
  keym(10).value = ""
  keym(10).readvalue = 0
  keym(10).readnb = 0
  keym(10).ubuf = 1
  keym(10).ofsbuf = 5
  'Корректировки (тенге):
  keym(11).Key = "Корректировки (тенге):"
  keym(11).n = 11
  keym(11).read = 0 ' 0 нет 1 да
  keym(11).ST = 0 ' 0 нет 1 да
  keym(11).keyr = ""
  keym(11).ofs = 14
  keym(11).value = ""
  keym(11).readvalue = 0
  keym(11).readnb = 0
  keym(11).ubuf = 1
  keym(11).ofsbuf = 5
  'Начислено за услуги связи (тенге):
  keym(12).Key = "Начислено за услуги связи (тенге):"
  keym(12).n = 12
  keym(12).read = 0 ' 0 нет 1 да
  keym(12).ST = 0 ' 0 нет 1 да
  keym(12).keyr = ""
  keym(12).ofs = 8
  keym(12).value = ""
  keym(12).readvalue = 0
  keym(12).readnb = 0
  keym(12).ubuf = 1
  keym(12).ofsbuf = 5
  'Начислено за оборудование (тенге):
  keym(13).Key = "Начислено за оборудование (тенге):"
  keym(13).n = 13
  keym(13).read = 0 ' 0 нет 1 да
  keym(13).ST = 0 ' 0 нет 1 да
  keym(13).keyr = ""
  keym(13).ofs = 6
  keym(13).value = ""
  keym(13).readvalue = 0
  keym(13).readnb = 0
  keym(13).ubuf = 0
  keym(13).ofsbuf = 0
  'Баланс на конец учетного периода (тенге):
  keym(14).Key = "Баланс на конец учетного периода (тенге):"
  keym(14).n = 14
  keym(14).read = 0 ' 0 нет 1 да
  keym(14).ST = 0 ' 0 нет 1 да
  keym(14).keyr = ""
  keym(14).ofs = 6
  keym(14).value = ""
  keym(14).readvalue = 0
  keym(14).readnb = 0
  keym(14).ubuf = 0
  keym(14).ofsbuf = 0
  'Сумма к оплате (тенге):
  keym(15).Key = "Сумма к оплате (тенге):"
  keym(15).n = 15
  keym(15).read = 0 ' 0 нет 1 да
  keym(15).ST = 0 ' 0 нет 1 да
  keym(15).keyr = ""
  keym(15).ofs = 6
  keym(15).value = ""
  keym(15).readvalue = 0
  keym(15).readnb = 0
  keym(15).ubuf = 0
  keym(15).ofsbuf = 0
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
  '
  ncf = 0
  ncc = 0
  Open Workbooks("amdocs_pdl.xlsm").path & "\" & "ars_debug_resS3.txt" For Output As #4
  '
  nbuf = 0
  SF = FreeFile
  Open Workbooks("amdocs_pdl.xlsm").path & "\" & "ars_debug2" For Binary As #SF 'ars_debug  'one_data.txt
    Do Until EOF(SF)
      Get #SF, , tmp
      If nbuf < 60 Then
        nbuf = nbuf + 1
      End If
      For j = 1 To nbuf
        If j = nbuf Then
          buff(j) = tmp
        Else
          buff(j) = buff(j + 1)
        End If
      Next j
        'Debug.Print TMP, Chr(TMP)
        'цикл по ключам
        For i = 1 To 15
          If keym(i).n = i Then
            If keym(i).read = 0 Then
              If keym(i).ST = 0 Then
                If InStr(1, keym(i).Key, Chr(tmp)) = 1 Then
                  keym(i).ST = 1
                  keym(i).keyr = Chr(tmp)
                  If keym(i).keyr = keym(i).Key Then
                    keym(i).readvalue = 1
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
                End If
                GoTo exit_cycle
              Else
                If InStr(1, keym(i).Key, keym(i).keyr & Chr(tmp)) = 1 And keym(i).readvalue = 0 Then
                  keym(i).keyr = keym(i).keyr & Chr(tmp)
                  If keym(i).keyr = keym(i).Key Then
                    keym(i).readvalue = 1
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
                  keym(i).readnb = keym(i).readnb + 1
                  If keym(i).readnb > keym(i).ofs Then
                    If tmp > 29 Then
                      keym(i).value = keym(i).value & Chr(tmp)
                      GoTo exit_cycle
                    Else
                      keym(i).read = 2
                      keym(i).value = Trim(keym(i).value)
                      If i = 15 Then
                      keym(2).value = Left(keym(2).value, Len(keym(2).value) - 1)
                      keym(5).value = Left(keym(5).value, Len(keym(5).value) - 1)
                      keym(8).value = Left(keym(8).value, Len(keym(8).value) - 1)
                      keym(9).value = Left(keym(9).value, Len(keym(9).value) - 1)
                      keym(13).value = Left(keym(13).value, Len(keym(13).value) - 1)
                      keym(14).value = Left(keym(14).value, Len(keym(14).value) - 1)
                      ncf = ncf + 1
                      ncc = ncc + 1
                      Print #4, ncf & ";" & keym(1).value & ";" & keym(2).value & ";" & keym(3).value & ";" & keym(4).value _
                      & ";" & keym(5).value & ";" & keym(6).value & ";" & keym(7).value & ";" & keym(8).value & ";" & keym(9).value _
                      & ";" & keym(10).value & ";" & keym(11).value & ";" & keym(12).value & ";" & keym(13).value & ";" & keym(14).value _
                      & ";" & keym(15).value
                      If ncc = 100 Then
                        ncc = 0
                        Close #4
                        Open Workbooks("amdocs_pdl.xlsm").path & "\" & "ars_debug_resS3.txt" For Append As #4
                      End If
                      If ncf = 5000 Then
                        GoTo exit_tst
                      End If
                      'Debug.Print
                      For k = 1 To 15
                          keym(k).read = 0
                          keym(k).ST = 0
                          keym(k).keyr = ""
                          keym(k).value = ""
                          keym(k).readvalue = 0
                          keym(k).readnb = 0
                        Next k
                      End If
                      GoTo exit_cycle
                    End If
                  End If
                End If
              End If
            End If
          End If
        Next i
exit_cycle:
      '
    Loop
exit_tst:
  Close #SF
  Close #4
End Sub



Sub pars_data2()
  Dim SF As Long, i, j, k, tr As Integer
  Dim keym(15) As parskey, tmp As Byte, buff(60) As Byte, nbuf, ncf As Integer
  
  'CID
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
  keym(8).ofsbuf = 0
  'Поступления средств (тенге):
  keym(9).Key = "Поступления средств (тенге):"
  keym(9).n = 9
  keym(9).read = 0 ' 0 нет 1 да
  keym(9).ST = 0 ' 0 нет 1 да
  keym(9).keyr = ""
  keym(9).ofs = 6
  keym(9).value = ""
  keym(9).readvalue = 0
  keym(9).readnb = 0
  keym(9).ubuf = 0
  keym(9).ofsbuf = 0
  'Перенос баланса (тенге):
  keym(10).Key = "Перенос баланса (тенге):"
  keym(10).n = 10
  keym(10).read = 0 ' 0 нет 1 да
  keym(10).ST = 0 ' 0 нет 1 да
  keym(10).keyr = ""
  keym(10).ofs = 15
  keym(10).value = ""
  keym(10).readvalue = 0
  keym(10).readnb = 0
  keym(10).ubuf = 1
  keym(10).ofsbuf = 5
  'Корректировки (тенге):
  keym(11).Key = "Корректировки (тенге):"
  keym(11).n = 11
  keym(11).read = 0 ' 0 нет 1 да
  keym(11).ST = 0 ' 0 нет 1 да
  keym(11).keyr = ""
  keym(11).ofs = 14
  keym(11).value = ""
  keym(11).readvalue = 0
  keym(11).readnb = 0
  keym(11).ubuf = 1
  keym(11).ofsbuf = 5
  'Начислено за услуги связи (тенге):
  keym(12).Key = "Начислено за услуги связи (тенге):"
  keym(12).n = 12
  keym(12).read = 0 ' 0 нет 1 да
  keym(12).ST = 0 ' 0 нет 1 да
  keym(12).keyr = ""
  keym(12).ofs = 8
  keym(12).value = ""
  keym(12).readvalue = 0
  keym(12).readnb = 0
  keym(12).ubuf = 1
  keym(12).ofsbuf = 5
  'Начислено за оборудование (тенге):
  keym(13).Key = "Начислено за оборудование (тенге):"
  keym(13).n = 13
  keym(13).read = 0 ' 0 нет 1 да
  keym(13).ST = 0 ' 0 нет 1 да
  keym(13).keyr = ""
  keym(13).ofs = 6
  keym(13).value = ""
  keym(13).readvalue = 0
  keym(13).readnb = 0
  keym(13).ubuf = 0
  keym(13).ofsbuf = 0
  'Баланс на конец учетного периода (тенге):
  keym(14).Key = "Баланс на конец учетного периода (тенге):"
  keym(14).n = 14
  keym(14).read = 0 ' 0 нет 1 да
  keym(14).ST = 0 ' 0 нет 1 да
  keym(14).keyr = ""
  keym(14).ofs = 6
  keym(14).value = ""
  keym(14).readvalue = 0
  keym(14).readnb = 0
  keym(14).ubuf = 0
  keym(14).ofsbuf = 0
  'Сумма к оплате (тенге):
  keym(15).Key = "Сумма к оплате (тенге):"
  keym(15).n = 15
  keym(15).read = 0 ' 0 нет 1 да
  keym(15).ST = 0 ' 0 нет 1 да
  keym(15).keyr = ""
  keym(15).ofs = 6
  keym(15).value = ""
  keym(15).readvalue = 0
  keym(15).readnb = 0
  keym(15).ubuf = 0
  keym(15).ofsbuf = 0
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
  '
  ncf = 0
  Open "ars_debug_res.txt" For Output As #4
  '
  nbuf = 0
  SF = FreeFile
  Open "ars_debug" For Binary As #SF 'ars_debug  'one_data.txt
    Do Until EOF(SF)
      Get #SF, , tmp
      If nbuf < 60 Then
        nbuf = nbuf + 1
      End If
      For j = 1 To nbuf
        If j = nbuf Then
          buff(j) = tmp
        Else
          buff(j) = buff(j + 1)
        End If
      Next j
        'Debug.Print TMP, Chr(TMP)
        'цикл по ключам
        For i = 1 To 15
          If keym(i).n = i Then
            If keym(i).read = 0 Then
              If keym(i).ST = 0 Then
                If InStr(1, keym(i).Key, Chr(tmp)) = 1 Then
                  keym(i).ST = 1
                  keym(i).keyr = Chr(tmp)
                  If keym(i).keyr = keym(i).Key Then
                    keym(i).readvalue = 1
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
                End If
                GoTo exit_cycle
              Else
                If InStr(1, keym(i).Key, keym(i).keyr & Chr(tmp)) = 1 And keym(i).readvalue = 0 Then
                  keym(i).keyr = keym(i).keyr & Chr(tmp)
                  If keym(i).keyr = keym(i).Key Then
                    keym(i).readvalue = 1
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
                  keym(i).readnb = keym(i).readnb + 1
                  If keym(i).readnb > keym(i).ofs Then
                    If tmp > 29 Then
                      keym(i).value = keym(i).value & Chr(tmp)
                      GoTo exit_cycle
                    Else
                      keym(i).read = 2
                      keym(i).value = Trim(keym(i).value)
                      If i = 15 Then
                      ncf = ncf + 1
        Print #4, ncf & "-------" ' Debug.Print
        Print #4, keym(1).Key, keym(1).value
        Print #4, keym(2).Key, keym(2).value
        Print #4, keym(3).Key, keym(3).value
        Print #4, keym(4).Key, keym(4).value
        Print #4, keym(5).Key, keym(5).value
        Print #4, keym(6).Key, keym(6).value
        Print #4, keym(7).Key, keym(7).value
        Print #4, keym(8).Key, keym(8).value
        Print #4, keym(9).Key, keym(9).value
        Print #4, keym(10).Key, keym(10).value
        Print #4, keym(11).Key, keym(11).value
        Print #4, keym(12).Key, keym(12).value
        Print #4, keym(13).Key, keym(13).value
        Print #4, keym(14).Key, keym(14).value
        Print #4, keym(15).Key, keym(15).value
                      For k = 1 To 15
                          keym(k).read = 0
                          keym(k).ST = 0
                          keym(k).keyr = ""
                          keym(k).value = ""
                          keym(k).readvalue = 0
                          keym(k).readnb = 0
                        Next k
                      End If
                      GoTo exit_cycle
                    End If
                  End If
                End If
              End If
            End If
          End If
        Next i
exit_cycle:
      '
    Loop
  Close #SF
  Close #4
End Sub

Sub tst()
  Dim SF As Long, tmp As Byte
  SF = FreeFile
  Open "C:\vitaliy\z\PDL\one_data.txt" For Binary As #SF
    Do Until EOF(SF)
      Get #SF, , tmp
    Loop
  Close #SF
End Sub


Sub pars_data()
  Dim SF As Long, i, j, k, tr As Integer
  Dim keym(15) As parskey, tmp As Byte, buff(60) As Byte, nbuf As Integer
  
  'CID
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
  keym(8).ofsbuf = 0
  'Поступления средств (тенге):
  keym(9).Key = "Поступления средств (тенге):"
  keym(9).n = 9
  keym(9).read = 0 ' 0 нет 1 да
  keym(9).ST = 0 ' 0 нет 1 да
  keym(9).keyr = ""
  keym(9).ofs = 6
  keym(9).value = ""
  keym(9).readvalue = 0
  keym(9).readnb = 0
  keym(9).ubuf = 0
  keym(9).ofsbuf = 0
  'Перенос баланса (тенге):
  keym(10).Key = "Перенос баланса (тенге):"
  keym(10).n = 10
  keym(10).read = 0 ' 0 нет 1 да
  keym(10).ST = 0 ' 0 нет 1 да
  keym(10).keyr = ""
  keym(10).ofs = 15
  keym(10).value = ""
  keym(10).readvalue = 0
  keym(10).readnb = 0
  keym(10).ubuf = 1
  keym(10).ofsbuf = 5
  'Корректировки (тенге):
  keym(11).Key = "Корректировки (тенге):"
  keym(11).n = 11
  keym(11).read = 0 ' 0 нет 1 да
  keym(11).ST = 0 ' 0 нет 1 да
  keym(11).keyr = ""
  keym(11).ofs = 14
  keym(11).value = ""
  keym(11).readvalue = 0
  keym(11).readnb = 0
  keym(11).ubuf = 1
  keym(11).ofsbuf = 5
  'Начислено за услуги связи (тенге):
  keym(12).Key = "Начислено за услуги связи (тенге):"
  keym(12).n = 12
  keym(12).read = 0 ' 0 нет 1 да
  keym(12).ST = 0 ' 0 нет 1 да
  keym(12).keyr = ""
  keym(12).ofs = 8
  keym(12).value = ""
  keym(12).readvalue = 0
  keym(12).readnb = 0
  keym(12).ubuf = 1
  keym(12).ofsbuf = 5
  'Начислено за оборудование (тенге):
  keym(13).Key = "Начислено за оборудование (тенге):"
  keym(13).n = 13
  keym(13).read = 0 ' 0 нет 1 да
  keym(13).ST = 0 ' 0 нет 1 да
  keym(13).keyr = ""
  keym(13).ofs = 6
  keym(13).value = ""
  keym(13).readvalue = 0
  keym(13).readnb = 0
  keym(13).ubuf = 0
  keym(13).ofsbuf = 0
  'Баланс на конец учетного периода (тенге):
  keym(14).Key = "Баланс на конец учетного периода (тенге):"
  keym(14).n = 14
  keym(14).read = 0 ' 0 нет 1 да
  keym(14).ST = 0 ' 0 нет 1 да
  keym(14).keyr = ""
  keym(14).ofs = 6
  keym(14).value = ""
  keym(14).readvalue = 0
  keym(14).readnb = 0
  keym(14).ubuf = 0
  keym(14).ofsbuf = 0
  'Сумма к оплате (тенге):
  keym(15).Key = "Сумма к оплате (тенге):"
  keym(15).n = 15
  keym(15).read = 0 ' 0 нет 1 да
  keym(15).ST = 0 ' 0 нет 1 да
  keym(15).keyr = ""
  keym(15).ofs = 6
  keym(15).value = ""
  keym(15).readvalue = 0
  keym(15).readnb = 0
  keym(15).ubuf = 0
  keym(15).ofsbuf = 0
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
  '
  nbuf = 0
  SF = FreeFile
  Open "one_data.txt" For Binary As #SF
    Do Until EOF(SF)
      Get #SF, , tmp
      If nbuf < 60 Then
        nbuf = nbuf + 1
      End If
      For j = 1 To nbuf
        If j = nbuf Then
          buff(j) = tmp
        Else
          buff(j) = buff(j + 1)
        End If
      Next j
        'Debug.Print TMP, Chr(TMP)
        'цикл по ключам
        For i = 1 To 15
          If keym(i).n = i Then
            If keym(i).read = 0 Then
              If keym(i).ST = 0 Then
                If InStr(1, keym(i).Key, Chr(tmp)) = 1 Then
                  keym(i).ST = 1
                  keym(i).keyr = Chr(tmp)
                  If keym(i).keyr = keym(i).Key Then
                    keym(i).readvalue = 1
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
                End If
                GoTo exit_cycle
              Else
                If InStr(1, keym(i).Key, keym(i).keyr & Chr(tmp)) = 1 And keym(i).readvalue = 0 Then
                  keym(i).keyr = keym(i).keyr & Chr(tmp)
                  If keym(i).keyr = keym(i).Key Then
                    keym(i).readvalue = 1
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
                  keym(i).readnb = keym(i).readnb + 1
                  If keym(i).readnb > keym(i).ofs Then
                    If tmp > 29 Then
                      keym(i).value = keym(i).value & Chr(tmp)
                      GoTo exit_cycle
                    Else
                      keym(i).read = 2
                      keym(i).value = Trim(keym(i).value)
                      GoTo exit_cycle
                    End If
                  End If
                End If
              End If
            End If
          End If
        Next i
exit_cycle:
      '
    Loop
  Close #SF
  Debug.Print keym(1).Key, keym(1).value
  Debug.Print keym(2).Key, keym(2).value
  Debug.Print keym(3).Key, keym(3).value
  Debug.Print keym(4).Key, keym(4).value
  Debug.Print keym(5).Key, keym(5).value
  Debug.Print keym(6).Key, keym(6).value
  Debug.Print keym(7).Key, keym(7).value
  Debug.Print keym(8).Key, keym(8).value
  Debug.Print keym(9).Key, keym(9).value
  Debug.Print keym(10).Key, keym(10).value
  Debug.Print keym(11).Key, keym(11).value
  Debug.Print keym(12).Key, keym(12).value
  Debug.Print keym(13).Key, keym(13).value
  Debug.Print keym(14).Key, keym(14).value
  Debug.Print keym(15).Key, keym(15).value
End Sub

  'Поступления средств (тенге):
  'keym(9).key = "Поступления средств (тенге):"
  'keym(9).n = 9
  'keym(9).read = 0 ' 0 нет 1 да
  'keym(9).st = 0 ' 0 нет 1 да
  'keym(9).keyr = ""
  'keym(9).ofs = 35
  'keym(9).value = ""
  'keym(9).readvalue = 0
  'keym(9).readnb = 0
  'Перенос баланса (тенге):
  'keym(10).key = "Перенос баланса (тенге):"
  'keym(10).n = 10
  'keym(10).read = 0 ' 0 нет 1 да
  'keym(10).st = 0 ' 0 нет 1 да
  'keym(10).keyr = ""
  'keym(10).ofs = 15
  'keym(10).value = ""
  'keym(10).readvalue = 0
  'keym(10).readnb = 0
  'Корректировки (тенге):
  'keym(11).key = "Корректировки (тенге):"
  'keym(11).n = 11
  'keym(11).read = 0 ' 0 нет 1 да
  'keym(11).st = 0 ' 0 нет 1 да
  'keym(11).keyr = ""
  'keym(11).ofs = 14
  'keym(11).value = ""
  'keym(11).readvalue = 0
  'keym(11).readnb = 0
  'Начислено за услуги связи (тенге):
  'keym(12).key = "Начислено за услуги связи (тенге):"
  'keym(12).n = 12
  'keym(12).read = 0 ' 0 нет 1 да
  'keym(12).st = 0 ' 0 нет 1 да
  'keym(12).keyr = ""
  'keym(12).ofs = 8
  'keym(12).value = ""
  'keym(12).readvalue = 0
  'keym(12).readnb = 0
  'Начислено за оборудование (тенге):
  'keym(13).key = "Начислено за оборудование (тенге):"
  'keym(13).n = 13
  'keym(13).read = 0 ' 0 нет 1 да
  'keym(13).st = 0 ' 0 нет 1 да
  'keym(13).keyr = ""
  'keym(13).ofs = 6
  'keym(13).value = ""
  'keym(13).readvalue = 0
  'keym(13).readnb = 0
  'Баланс на конец учетного периода (тенге):
  'keym(14).key = "Баланс на конец учетного периода (тенге):"
  'keym(14).n = 14
  'keym(14).read = 0 ' 0 нет 1 да
  'keym(14).st = 0 ' 0 нет 1 да
  'keym(14).keyr = ""
  'keym(14).ofs = 6
  'keym(14).value = ""
  'keym(14).readvalue = 0
  'keym(14).readnb = 0
  'Сумма к оплате (тенге):
  'keym(15).key = "Сумма к оплате (тенге):"
  'keym(15).n = 15
  'keym(15).read = 0 ' 0 нет 1 да
  'keym(15).st = 0 ' 0 нет 1 да
  'keym(15).keyr = ""
  'keym(15).ofs = 6
  'keym(15).value = ""
  'keym(15).readvalue = 0
  'keym(15).readnb = 0

