Attribute VB_Name = "list_file"
Option Explicit


Public path_s As String

Type recfile ' “ип, запись
    name As String 'им€ файла
    size As String ' размер файла  в байтах
    path As String ' путь к файлу
    disk As String ' диск
    desk As String ' расширение
    datecr As String ' дата создани€ файла
    datemd As String ' дата изменени€ файла
    dateac As String ' дата доступа файла
    type As String ' тип файла
End Type

Type row_det ' “ип, запись детализации
    r01 As String 'поле 01
    r02 As String 'поле 02
    r03 As String 'поле 03
    r04 As String 'поле 04
    r05 As String 'поле 05
    r06 As String 'поле 06
    r07 As String 'поле 07
    r08 As String 'поле 08
    r09 As String 'поле 09
    r10 As String 'поле 10
    r11 As String 'поле 11
    r12 As String 'поле 12
    r13 As String 'поле 13
    r14 As String 'поле 14
End Type


'0 - нет описани€
'1 - описание по папке
'2 - описание по расширению
'3 - описание по папке и расширению
'---------------------------------------------------------------------------
'--------------------------------------
'код из MSDN
'--------------------------------------
'объ€вление функций и структур
'--------------------------------------
   Declare Function FindFirstFile Lib "kernel32" Alias _
   "FindFirstFileA" (ByVal lpFileName As String, lpFindFileData _
   As WIN32_FIND_DATA) As Long

   Declare Function FindNextFile Lib "kernel32" Alias "FindNextFileA" _
   (ByVal hFindFile As Long, lpFindFileData As WIN32_FIND_DATA) As Long

   Declare Function GetFileAttributes Lib "kernel32" Alias _
   "GetFileAttributesA" (ByVal lpFileName As String) As Long
   
   Declare Function SetFileAttributes Lib "kernel32" Alias _
   "SetFileAttributesA" (ByVal lpFileName As String, dwFileAttributes As Long) As Long
   
   Declare Function DeleteFile Lib "kernel32" Alias _
   "DeleteFileA" (ByVal lpFileName As String) As Long
   
   Declare Function CopyFile Lib "kernel32" Alias _
   "CopyFileA" (ByVal lpExistingFileName As String, ByVal lpNewFileName As String, bFailIfExists As Long) As Long

   Declare Function FindClose Lib "kernel32" (ByVal hFindFile As Long) _
   As Long

   Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Long)
      
   Declare Function RegOpenKeyA Lib "ADVAPI32.DLL" (ByVal hKey As Long, ByVal sSubKey As String, _
   ByRef hkeyResult As Long) As Long
   
   Declare Function RegCloseKey Lib "ADVAPI32.DLL" (ByVal hKey As Long) As Long

   Declare Function RegSetValueExA Lib "ADVAPI32.DLL" (ByVal hKey As Long, ByVal sValueName As String, _
   ByVal dwReserved As Long, ByVal dwType As Long, ByVal sValue As String, ByVal dwSize As Long) As Long

   Declare Function RegCreateKeyA Lib "ADVAPI32.DLL" (ByVal hKey As Long, ByVal sSubKey As String, _
   ByRef hkeyResult As Long) As Long

   'Private
   Declare Function RegQueryValueExA Lib "ADVAPI32.DLL" (ByVal hKey As Long, ByVal sValueName As String, _
   ByVal dwReserved As Long, ByRef lValueType As Long, ByVal sValue As String, ByRef lResultLen As Long) As Long
   
   'Declare Function GetFileSize

   Declare Function FileTimeToLocalFileTime Lib "kernel32" _
   (lpFileTime As FILETIME, lpLocalFileTime As FILETIME) As Long
     
   Declare Function FileTimeToSystemTime Lib "kernel32" _
   (lpFileTime As FILETIME, lpSystemTime As SYSTEMTIME) As Long

   Public Const MAX_PATH = 260
   Public Const MAXDWORD = &HFFFF
   Public Const INVALID_HANDLE_VALUE = -1
   Public Const FILE_ATTRIBUTE_ARCHIVE = &H20
   Public Const FILE_ATTRIBUTE_DIRECTORY = &H10
   Public Const FILE_ATTRIBUTE_HIDDEN = &H2
   Public Const FILE_ATTRIBUTE_NORMAL = &H80
   Public Const FILE_ATTRIBUTE_READONLY = &H1
   Public Const FILE_ATTRIBUTE_SYSTEM = &H4
   Public Const FILE_ATTRIBUTE_TEMPORARY = &H100

   Type FILETIME
     dwLowDateTime As Long
     dwHighDateTime As Long
   End Type

   Type WIN32_FIND_DATA
     dwFileAttributes As Long
     ftCreationTime As FILETIME
     ftLastAccessTime As FILETIME
     ftLastWriteTime As FILETIME
     nFileSizeHigh As Long
     nFileSizeLow As Long
     dwReserved0 As Long
     dwReserved1 As Long
     cFileName As String * MAX_PATH
     cAlternate As String * 14
   End Type

   Type SYSTEMTIME
     wYear As Integer
     wMonth As Integer
     wDayOfWeek As Integer
     wDay As Integer
     wHour As Integer
     wMinute As Integer
     wSecond As Integer
     wMilliseconds As Integer
   End Type
   
   
Private Declare Function CreateFile Lib "kernel32" Alias "CreateFileA" _
    (ByVal lpFileName As String, ByVal dwDesiredAccess As Long, _
     ByVal dwShareMode As Long, lpSecurityAttributes As Long, _
     ByVal dwCreationDisposition As Long, ByVal dwFlagsAndAttributes As Long, _
     ByVal hTemplateFile As Long) As Long
      
Private Declare Function CloseHandle Lib "kernel32" _
    (ByVal hObject As Long) As Long
    
Private Declare Function GetFileSizeEx Lib "kernel32" _
    (ByVal hFile As Long, lpFileSize As Currency) As Boolean

Private Const OPEN_EXISTING = 3
Private Const FILE_SHARE_READ = &H1
Private Const GENERIC_READ = &H80000000

Function File_size(in_file As String) As String
  Dim fhandle As Long
  Dim sz As Currency
  Dim fname As String
  fhandle = CreateFile(in_file, GENERIC_READ, FILE_SHARE_READ, _
                    ByVal 0&, OPEN_EXISTING, 0, 0)
  GetFileSizeEx fhandle, sz
  sz = sz * 10000
  File_size = sz
  Call CloseHandle(fhandle)
End Function
   
   

   Public Function StripNulls(OriginalStr As String) As String
      If (InStr(OriginalStr, Chr(0)) > 0) Then
         OriginalStr = Left(OriginalStr, _
          InStr(OriginalStr, Chr(0)) - 1)
      End If
      StripNulls = OriginalStr
   End Function
'---------------------------------------------------------------------------
'код выполн€ющий поиск файлов
'---------------------------------------------------------------------------
   Function FindFilesAPI(path As String, SearchStr As String)
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
   Dim pt As String
    
   pt = Application.ActiveWorkbook.FullName
   pt = Left(pt, InStrRev(pt, "\"))
     
   If Right(path, 1) <> "\" Then path = path & "\"
   ' Search for subdirectories.
   nDir = 0
   ReDim dirNames(nDir)
   Cont = True
   hSearch = FindFirstFile(path & "*", WFD)
   If hSearch <> INVALID_HANDLE_VALUE Then
      Do While Cont
         DirName = StripNulls(WFD.cFileName)
         ' Ignore the current and encompassing directories.
         If (DirName <> ".") And (DirName <> "..") Then
            ' Check for directory with bitwise comparison.
            If GetFileAttributes(path & DirName) And _
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
   hSearch = FindFirstFile(path & SearchStr, WFD)
   Cont = True
   
   If hSearch <> INVALID_HANDLE_VALUE Then
      While Cont
         FileName = StripNulls(WFD.cFileName)
            If (FileName <> ".") And (FileName <> "..") And _
              ((GetFileAttributes(path & FileName) And _
               FILE_ATTRIBUTE_DIRECTORY) <> FILE_ATTRIBUTE_DIRECTORY) Then
            FindFilesAPI = FindFilesAPI + (WFD.nFileSizeHigh * _
             MAXDWORD) + WFD.nFileSizeLow
            ' To list files w/o dates, uncomment the next line
            ' and remove or Comment the lines down to End If
            'List1.AddItem path & FileName
            
           ' Include Creation date...
           FileTimeToLocalFileTime WFD.ftCreationTime, FT
           FileTimeToSystemTime FT, ST
           fr.datecr = GetFileDate(ST.wYear, ST.wMonth, ST.wDay, _
             ST.wHour, ST.wMinute, ST.wSecond)
           ' and Last Modified Date
           FileTimeToLocalFileTime WFD.ftLastWriteTime, FT
           FileTimeToSystemTime FT, ST
           fr.datemd = GetFileDate(ST.wYear, ST.wMonth, ST.wDay, _
             ST.wHour, ST.wMinute, ST.wSecond)
           'LastAccessTime
           FileTimeToLocalFileTime WFD.ftLastAccessTime, FT
           FileTimeToSystemTime FT, ST
           fr.dateac = GetFileDate(ST.wYear, ST.wMonth, ST.wDay, _
             ST.wHour, ST.wMinute, ST.wSecond)
           fr.desk = GetFileDesc(FileName)
           fr.disk = GetFileDisk(path)
           fr.name = FileName
           fr.path = path
           '2,147,483,647
           If WFD.nFileSizeLow < 0 Then
             fr.size = File_size(fr.path & fr.name)
           Else
             fr.size = WFD.nFileSizeHigh * MAXDWORD + WFD.nFileSizeLow
           End If
           fr.type = GetFileType(WFD.dwFileAttributes)
           '
           'Debug.Print fr.datecr, fr.datemd, fr.desk, fr.disk, _
           fr.name, fr.path, fr.size, fr.type
           WriteFile pt & "RESW_.IN", fr
           'List1.AddItem path & FileName & vbTab & _
              Format(DateCStr, "mm/dd/yyyy hh:nn:ss") _
              & vbTab & Format(DateMStr, "mm/dd/yyyy hh:nn:ss")
          End If
         Cont = FindNextFile(hSearch, WFD)  ' Get next file
      Wend
      Cont = FindClose(hSearch)
   End If

   ' If there are sub-directories...
    If nDir > 0 Then
      ' Recursively walk into them...
      For i = 0 To nDir - 1
        FindFilesAPI path & dirNames(i) _
        & "\", SearchStr
      Next i
   End If
     FindFilesAPI = 0
   End Function
'-----------------------------------------------------
'получаю дату
'-----------------------------------------------------
Function GetFileDate(wYear As Integer, wMonth As Integer, _
wDay As Integer, wHour As Integer, wMinute As Integer, _
wSecond As Integer) As String
  ' формат дд.мм.гггг чч:ми:сс
  Dim Ret As String
  If wDay < 10 Then
    Ret = "0" & CStr(wDay) & "."
  Else
    Ret = CStr(wDay) & "."
  End If
  If wMonth < 10 Then
    Ret = Ret & "0" & CStr(wMonth) & "."
  Else
    Ret = Ret & CStr(wMonth) & "."
  End If
  Ret = Ret & CStr(wYear) & " "
  If wHour < 10 Then
    Ret = Ret & "0" & CStr(wHour) & ":"
  Else
    Ret = Ret & CStr(wHour) & ":"
  End If
  If wMinute < 10 Then
    Ret = Ret & "0" & CStr(wMinute) & ":"
  Else
    Ret = Ret & CStr(wMinute) & ":"
  End If
  If wSecond < 10 Then
    Ret = Ret & "0" & CStr(wSecond)
  Else
    Ret = Ret & CStr(wSecond)
  End If
  GetFileDate = Ret
End Function
'-----------------------------------------------------
'получаю диск
'-----------------------------------------------------
Function GetFileDisk(file_name As String) As String
  'определ€ю первый символ в имени файла
  GetFileDisk = Left(file_name, 1)
End Function
'-----------------------------------------------------
' 1  Public Const FILE_ATTRIBUTE_ARCHIVE = &H20
' 2  Public Const FILE_ATTRIBUTE_DIRECTORY = &H10
' 4  Public Const FILE_ATTRIBUTE_HIDDEN = &H2
' 8  Public Const FILE_ATTRIBUTE_NORMAL = &H80
' 16  Public Const FILE_ATTRIBUTE_READONLY = &H1
' 32  Public Const FILE_ATTRIBUTE_SYSTEM = &H4
' 64  Public Const FILE_ATTRIBUTE_TEMPORARY = &H100
'-----------------------------------------------------
'тип файла
'-----------------------------------------------------
Function GetFileType(dwFileAttributes As Long) As String
  'определ€ю первый символ в имени файла
  Dim Ret As Integer
  If dwFileAttributes And FILE_ATTRIBUTE_ARCHIVE Then
    Ret = Ret + 1
  End If
  If dwFileAttributes And FILE_ATTRIBUTE_DIRECTORY Then
    Ret = Ret + 2
  End If
  If dwFileAttributes And FILE_ATTRIBUTE_HIDDEN Then
    Ret = Ret + 4
  End If
  If dwFileAttributes And FILE_ATTRIBUTE_NORMAL Then
    Ret = Ret + 8
  End If
  If dwFileAttributes And FILE_ATTRIBUTE_READONLY Then
    Ret = Ret + 16
  End If
  If dwFileAttributes And FILE_ATTRIBUTE_SYSTEM Then
    Ret = Ret + 32
  End If
  If dwFileAttributes And FILE_ATTRIBUTE_TEMPORARY Then
    Ret = Ret + 64
  End If
  GetFileType = CStr(Ret)
End Function
'-----------------------------------------------------
'получаю расширение файла
'-----------------------------------------------------
Function GetFileDesc(file_name As String) As String
  'определ€ю последнее вхождение символа . в имени файла
  Dim pos_c, pos_n As Integer
  pos_c = 1
  pos_n = 0
  While pos_c > 0
    pos_c = InStr(pos_c + 1, file_name, ".", 1)
    If pos_c > 0 Then pos_n = pos_c
  Wend
  If pos_n > 0 Then
    GetFileDesc = Right(file_name, Len(file_name) - pos_n)
  Else
    GetFileDesc = ""
  End If
End Function
'-----------------------------------------------------
'записываю результат в файл
'-----------------------------------------------------
Sub WriteFile(file As String, fr As recfile)
  Dim res As String ''& Chr$(9) & test_md5v3(fr.path & fr.name)
  res = fr.datecr & Chr$(9) & fr.datemd & Chr$(9) & fr.dateac & _
  Chr$(9) & fr.desk & Chr$(9) & fr.disk _
  & Chr$(9) & fr.name & Chr$(9) & test_md5v3(fr.path & fr.name) & _
  Chr$(9) & fr.path & Chr$(9) & fr.size & Chr$(9) & fr.type & Chr$(9)
  Open file For Append As #2
  Print #2, res
  Close #2
  'Debug.Print test_md5v3(fr.path & fr.name)
End Sub

Function GetValue()
  Dim SearchPath As String, FindStr As String
  FindStr = "*.*"
  'Screen.MousePointer = vbHourglass
  SearchPath = "G:\201107\ћультфильмы\" 'path_s d:\RESW_c.INF
  FindFilesAPI SearchPath, FindStr
  'SearchPath = "D:" 'path_s
  'FindFilesAPI SearchPath, FindStr
  'SearchPath = "G:" 'path_s
  'FindFilesAPI SearchPath, FindStr
End Function

Sub test_md5()
  Dim CObject1 As New MD5Hash 'Class1
  Set CObject1 = New MD5Hash
  'MD5File(FileName As String)
  'Debug.Print 'CObject1.MD5File("f:\VB6STKIT.DLL") 'RESW_T")
  Debug.Print Shell("f:\pc\md5\md5.exe f:\VB6STKIT.DLL")
'5   c1fa559076b9a55bd65e814e670403
End Sub

Function test_md5v3(in_file As String) As String
  Dim ex, res, strLine, pt As String, _
  ByteBuffer(32) As Byte, i As Integer
  
  pt = Application.ActiveWorkbook.FullName
  pt = Left(pt, InStrRev(pt, "\"))
  
  
  ex = "cmd /c " & pt & "md5.exe """
  res = """ > " & pt & "tmp.txt"
  ShellAndWait ex & in_file & res, 4000000, vbHide, IgnoreBreak
  
  'Shell ex & in_file & res, 0
  'Sleep (1000)
  Open pt & "tmp.txt" For Binary Access Read As #3
    Get #3, , ByteBuffer
  'Do While Not EOF(FN)
  'Loop
  'Open "f:\tmp.txt" For Input As #3
  'Line Input #3, strLine
  Close #3
  test_md5v3 = ""
  For i = 0 To 31
    test_md5v3 = test_md5v3 & Chr(ByteBuffer(i))
  Next i
End Function

Sub test_md52()
  'Dim f As Integer
  Dim s As String
  'Shell "cmd /c f:\md5.exe """ & _
  '"f:\VB6STKIT.DLL"" > f:\tmp.txt"
  'f = FreeFile
  'Open "f:\tmp.txt" For Input As #3
  'Line Input #3, strLine
  'Close #3
  'Application.ActiveWorkbook.
  'Debug.Print
  s = Application.ActiveWorkbook.FullName
  's = "C:\0list\file_info.xls"
  'instr(
  Debug.Print Left(s, InStrRev(s, "\"))
  'test_md5v3("f:\VB6STKIT.DLL")
  'Kill CurrentProject.Path& "\temp.txt"
  ' strLine should now contain the MD5 value
End Sub


