!Module to read in name/value pairs from a file, with each line of the form line 'name = value'
!Should correctly interpret FITS headers
!Antony Lewis (http://cosmologist.info/). Released to the public domain.
!This version March 2005.

module IniFile
 implicit none
 public
  integer, parameter :: Ini_max_name_len = 128
  integer, parameter :: Ini_max_string_len = 1024
  logical :: Ini_fail_on_not_found = .false.
  logical :: Ini_Echo_Read = .false.
  type TNameValue
   !no known way to make character string pointers..
    character(Ini_max_name_len)  :: Name
    character(Ini_max_string_len):: Value
  end type TNameValue

  type TNameValue_pointer
     Type(TNameValue), pointer :: P
  end type TNameValue_pointer

  Type TNameValueList
    integer Count
    integer Delta
    integer Capacity
    type(TNameValue_pointer), dimension(:), pointer :: Items
  end Type TNameValueList

  Type TIniFile
   logical SlashComments
   Type (TNameValueList) :: L, ReadValues
  end Type TIniFile

  Type(TIniFile) :: DefIni

contains

   subroutine TNameValueList_Init(L)
    Type (TNameValueList) :: L

     L%Count = 0
     L%Capacity = 0
     L%Delta = 128
     nullify(L%Items)

   end subroutine TNameValueList_Init

   subroutine TNameValueList_Clear(L)
    Type (TNameValueList) :: L
    integer i, status

    do i=L%count,1,-1
     deallocate (L%Items(i)%P, stat = status)
    end do
    deallocate (L%Items, stat = status)
    call TNameValueList_Init(L)

   end subroutine TNameValueList_Clear

   subroutine TNameValueList_ValueOf(L, AName, AValue)
     Type (TNameValueList) :: L
     character(LEN=*), intent(in) :: AName
     CHARACTER(LEN=*) :: AValue
     integer i

     do i=1, L%Count
       if (L%Items(i)%P%Name == AName) then
          AValue = L%Items(i)%P%Value
          return
       end if
     end do
     AValue = ''

   end subroutine TNameValueList_ValueOf

   subroutine TNameValueList_Add(L, AName, AValue)
    Type (TNameValueList) :: L
    character(LEN=*), intent(in) :: AName, AValue

    if (L%Count == L%Capacity) call TNameValueList_SetCapacity(L, L%Capacity + L%Delta)
    L%Count = L%Count + 1
    allocate(L%Items(L%Count)%P)
    L%Items(L%Count)%P%Name = AName
    L%Items(L%Count)%P%Value = AValue

   end subroutine TNameValueList_Add

   subroutine TNameValueList_SetCapacity(L, C)
    Type (TNameValueList) :: L
    integer C
    type(TNameValue_pointer), dimension(:), pointer :: TmpItems

    if (L%Count > 0) then
      if (C < L%Count) stop 'TNameValueList_SetCapacity: smaller than Count'
      allocate(TmpItems(L%Count))
      TmpItems = L%Items(1:L%Count)
      deallocate(L%Items)
      allocate(L%Items(C))
      L%Items(1:L%Count) = TmpItems
      deallocate(TmpItems)
    else
     allocate(L%Items(C))
    end if
    L%Capacity = C

   end subroutine TNameValueList_SetCapacity

   subroutine TNameValueList_Delete(L, i)
    Type (TNameValueList) :: L
    integer, intent(in) :: i

     deallocate(L%Items(i)%P)
     if (L%Count > 1) L%Items(i:L%Count-1) = L%Items(i+1:L%Count)
     L%Count = L%Count -1

   end subroutine TNameValueList_Delete

  subroutine Ini_NameValue_Add(Ini,AInLine) !200 puntos para la estadistica
    Type(TIniFile) :: Ini
    character (LEN=*), intent(IN) :: AInLine
    integer EqPos, slashpos, lastpos
    character (LEN=len(AInLine)) :: AName, S, InLine

      InLine=trim(adjustl(AInLine))
      EqPos = scan(InLine,'=')
      if (EqPos/=0 .and. InLine(1:1)/='#' .and. InLine(1:7) /= 'COMMENT' ) then

         AName = trim(InLine(1:EqPos-1))

         S = adjustl(InLine(EqPos+1:))
           if (Ini%SlashComments) then
           slashpos=scan(S,'/')
           if (slashpos /= 0) then
              S  = S(1:slashpos-1)
           end if
         end if
         lastpos=len_trim(S)
         if (lastpos>1) then
          if (S(1:1)=='''' .and. S(lastpos:lastpos)=='''') then
           S = S(2:lastpos-1)
          end if
         end if
         call TNameValueList_Add(Ini%L, AName, S)

      end if

  end subroutine Ini_NameValue_Add


  subroutine Ini_Open(filename, unit_id,  error, slash_comments)
     character (LEN=*), intent(IN) :: filename
     integer, intent(IN) :: unit_id
     logical, optional, intent(OUT) :: error
     logical, optional, intent(IN) :: slash_comments
     logical aerror

     call TNameValueList_Clear(DefIni%L)
     call TNameValueList_Clear(DefIni%ReadValues)

     if (present(slash_comments)) then
      call Ini_Open_File(DefIni,filename,unit_id,aerror,slash_comments)
     else
      call Ini_Open_File(DefIni,filename,unit_id,aerror)
     end if

     if (present(error)) then
       error = aerror
     else
      if (aerror) then
        write (*,*) 'Ini_Open: Error opening file ' // trim(filename)
        stop
      end if
     end if

  end subroutine Ini_Open


  subroutine Ini_Open_File(Ini, filename, unit_id,  error, slash_comments)
     Type(TIniFile) :: Ini
     character (LEN=*), intent(IN) :: filename
     integer, intent(IN) :: unit_id
     logical, intent(OUT) :: error
     logical, optional, intent(IN) :: slash_comments
     character (LEN=120) :: InLine

    call TNameValueList_Init(Ini%L)
    call TNameValueList_Init(Ini%ReadValues)

    if (present(slash_comments)) then
     Ini%SlashComments = slash_comments
    else
     Ini%SlashComments = .false.
    end if

    open(unit=unit_id,file=filename,form='formatted',status='old', err=500)

    do
      read (unit_id,'(a)',end=400) InLine
      if (InLine == 'END') exit;
      if (InLine /= '') call Ini_NameValue_Add(Ini,InLine)
    end do

400 close(unit_id)
    error=.false.
    return

500 error=.true.

  end subroutine Ini_Open_File

  subroutine Ini_Open_Fromlines(Ini, Lines, NumLines, slash_comments)
    Type(TIniFile) :: Ini
    integer, intent(IN) :: NumLines
    character (LEN=*), dimension(NumLines), intent(IN) :: Lines
    logical, intent(IN) :: slash_comments
    integer i

    call TNameValueList_Init(Ini%L)
    Ini%SlashComments = slash_comments

    do i=1,NumLines
       call Ini_NameValue_Add(Ini,Lines(i))
    end do

  end  subroutine Ini_Open_Fromlines

  subroutine Ini_Close

    call Ini_close_File(DefIni)

  end subroutine Ini_Close

  function Strip_As(Line,offset)
    character (LEN=150) :: Strip_As, NewLine, RestLine,Stripped
    character (LEN=*), intent(IN) :: Line
    character (LEN=5) :: snumber
    integer :: Apos, dim, Blank, number
    integer, intent(IN) :: offset

    dim=len(Line)
    NewLine = ''
    RestLine = trim(Line)//' '
    Strip_As = ''
    Stripped = ''

    do
       Apos = scan(trim(RestLine), 'A')
       if (Apos > 0) then
          NewLine = RestLine(1:Apos-1)
          RestLine = RestLine(Apos+1:dim)//' '
          Blank = scan(RestLine,' ')
          if (Blank > 0) then
             snumber = Restline(1:Blank)
             Restline = Restline(Blank:dim)
          else
             stop 'Something wrong here'
          end if
          read(snumber, *) number
          number = number+offset-2
          write(snumber,'(I5)') number
          Stripped = trim(Stripped)//trim(NewLine)//trim(snumber)
       else
          exit
    end if
 end do
    Strip_As =   trim(Stripped)//trim(RestLine)
  end function Strip_As

  function Ini_Read_String_A(Key, num, NotFoundFail)
   character (LEN=150) Ini_Read_String_A, TmpLine
   logical, optional, intent(IN) :: NotFoundFail
   character (LEN=*), intent(IN) :: Key
   integer, intent (IN) :: num

   if (present(NotFoundFail)) then
         TmpLine = Ini_Read_String(Key, NotFoundFail)
      else
         TmpLine = Ini_Read_String(Key)
      end if
   Ini_Read_String_A = Strip_As(TmpLine, num)

  end function Ini_Read_String_A


  subroutine Ini_Close_File(Ini)
    Type(TIniFile) :: Ini

    call TNameValueList_Clear(Ini%L)
    call TNameValueList_Clear(Ini%ReadValues)

  end  subroutine Ini_Close_File


  function Ini_Read_String(Key, NotFoundFail) result(AValue)
   character (LEN=*), intent(IN) :: Key
   logical, optional, intent(IN) :: NotFoundFail
   character(LEN=Ini_max_string_len) :: AValue

     if (present(NotFoundFail)) then
      AValue = Ini_Read_String_File(DefIni, Key, NotFoundFail)
     else
      AValue = Ini_Read_String_File(DefIni, Key)
     end if

  end function Ini_Read_String



  function Ini_Read_String_File(Ini, Key, NotFoundFail) result(AValue)
   Type(TIniFile) :: Ini
   character (LEN=*), intent(IN) :: Key
   logical, optional, intent(IN) :: NotFoundFail
   character(LEN=Ini_max_string_len) :: AValue

   call TNameValueList_ValueOf(Ini%L, Key, AValue)

   if (AValue/='') then
    call  TNameValueList_Add(Ini%ReadValues, Key, AValue)
    if (Ini_Echo_Read) write (*,*) trim(Key)//' = ',trim(AValue)
    return
   end if
   if (Ini_fail_on_not_found) then
      write(*,*) 'key not found : '//Key
      stop
   end if
   if (present(NotFoundFail)) then
      if (NotFoundFail) then
         write(*,*) 'key not found : '//Key
         stop
      end if
   end if

  end function Ini_Read_String_File


    function Ini_Read_Int(Key, Default)
     integer, optional, intent(IN) :: Default
     character (LEN=*), intent(IN) :: Key
     integer Ini_Read_Int

     if (present(Default)) then
      Ini_Read_Int = Ini_Read_Int_File(DefIni, Key, Default)
     else
      Ini_Read_Int = Ini_Read_Int_File(DefIni, Key)
     end if
    end function Ini_Read_Int


  function Ini_Read_Int_File(Ini, Key, Default)
   Type(TIniFile) :: Ini
   integer Ini_Read_Int_File
   integer, optional, intent(IN) :: Default
   character  (LEN=*), intent(IN) :: Key
  character(LEN=Ini_max_string_len) :: S

   S = Ini_Read_String_File(Ini, Key,.not. present(Default))
   if (S == '') then
      if (.not. present(Default)) then
        write(*,*) 'no value for key: '//Key
        stop
      end if
      Ini_Read_Int_File = Default
      write (S,*) Default
      call  TNameValueList_Add(Ini%ReadValues, Key, S)
   else
    if (verify(trim(S),'-+0123456789') /= 0) goto 10
    read (S,*, err = 10) Ini_Read_Int_File
   end if

  return

10 write (*,*) 'error reading integer for key: '//Key
   stop

  end function Ini_Read_Int_File

    function Ini_Read_Double(Key, Default)
     double precision, optional, intent(IN) :: Default
     character (LEN=*), intent(IN) :: Key
     double precision Ini_Read_Double

     if (present(Default)) then
      Ini_Read_Double = Ini_Read_Double_File(DefIni, Key, Default)
     else
      Ini_Read_Double = Ini_Read_Double_File(DefIni, Key)
     end if
    end function Ini_Read_Double


  function Ini_Read_Double_File(Ini,Key, Default)
   Type(TIniFile) :: Ini
   double precision Ini_Read_Double_File
   double precision, optional, intent(IN) :: Default
   character (LEN=*), intent(IN) :: Key
   character(LEN=Ini_max_string_len) :: S

   S = Ini_Read_String_File(Ini,Key,.not. present(Default))
   if (S == '') then
      if (.not. present(Default)) then
        write(*,*) 'no value for key: '//Key
        stop
      end if
      Ini_Read_Double_File = Default
      write (S,*) Default
      call  TNameValueList_Add(Ini%ReadValues, Key, S)
   else
    read (S,*, err=10) Ini_Read_Double_File
   end if

  return

10 write (*,*) 'error reading double for key: '//Key
   stop

  end function Ini_Read_Double_File


    function Ini_Read_Real(Key, Default)
     real, optional, intent(IN) :: Default
     character (LEN=*), intent(IN) :: Key
     real Ini_Read_Real

     if (present(Default)) then
      Ini_Read_Real = Ini_Read_Real_File(DefIni, Key, Default)
     else
      Ini_Read_Real = Ini_Read_Real_File(DefIni, Key)
     end if
    end function Ini_Read_Real

    function Ini_Read_Real_File(Ini,Key, Default)
    Type(TIniFile) :: Ini
    real Ini_Read_Real_File
    real, optional, intent(IN) :: Default
    character (LEN=*), intent(IN) :: Key
    character(LEN=Ini_max_string_len) :: S

   S = Ini_Read_String_File(Ini,Key,.not. present(Default))
   if (S == '') then
      if (.not. present(Default)) then
        write(*,*) 'no value for key: '//Key
        stop
      end if
      Ini_Read_Real_File = Default
      write (S,*) Default
      call  TNameValueList_Add(Ini%ReadValues, Key, S)

   else
    read (S,*, err=10) Ini_Read_Real_File
   end if

  return

10 write (*,*) 'error reading double for key: '//Key
   stop

  end function Ini_Read_Real_File


    function Ini_Read_Logical(Key, Default)
     Logical, optional, intent(IN) :: Default
     character (LEN=*), intent(IN) :: Key
    logical Ini_Read_Logical

     if (present(Default)) then
      Ini_Read_Logical = Ini_Read_Logical_File(DefIni, Key, Default)
     else
      Ini_Read_Logical = Ini_Read_Logical_File(DefIni, Key)
     end if
    end function Ini_Read_Logical

  function Ini_Read_Logical_File(Ini, Key, Default)
   Type(TIniFile) :: Ini
   logical Ini_Read_Logical_File
   logical, optional, intent(IN) :: Default
   character  (LEN=*), intent(IN) :: Key

   character(LEN=Ini_max_string_len) :: S

   S = Ini_Read_String_File(Ini,Key,.not. present(Default))
   if (S == '') then
      if (.not. present(Default)) then
        write(*,*) 'no value for key: '//Key
        stop
      end if
      Ini_Read_Logical_File = Default
      write (S,*) Default
      call  TNameValueList_Add(Ini%ReadValues, Key, S)
   else
    if (verify(trim(S),'10TF') /= 0) goto 10
    read (S,*, err = 10) Ini_Read_Logical_File
   end if

  return

10 write (*,*) 'error reading logical for key: '//Key
   stop
  end function Ini_Read_Logical_File


  subroutine Ini_SaveReadValues(afile,unit_id)
   character(LEN=*)  :: afile
   integer, intent(in) :: unit_id

   call Ini_SaveReadValues_File(DefIni, afile, unit_id)

  end subroutine Ini_SaveReadValues


  subroutine Ini_SaveReadValues_File(Ini, afile, unit_id)
   Type(TIniFile) :: Ini
   character(LEN=*), intent(in) :: afile
   integer, intent(in) :: unit_id
   integer i

   open(unit=unit_id,file=afile,form='formatted',status='replace', err=500)

   do i=1, Ini%ReadValues%Count
    write (unit_id,'(a)') trim(Ini%ReadValues%Items(i)%P%Name) // ' = ' &
                        //trim(Ini%ReadValues%Items(i)%P%Value)

   end do

   close(unit_id)
   return

500 write(*,*) 'Ini_SaveReadValues_File: Error creating '//trim(afile)

  end subroutine Ini_SaveReadValues_File

end module IniFile
