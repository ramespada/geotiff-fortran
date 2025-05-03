module GTIFF
! program:        Fortran GeoTIFF Library
! description:    Module with procedures for reading and writing GeoTIFF files.
! programmed by:  Ramiro A. Espada
! date:           May, 2024.
!
  use zlib

  implicit none
  !save
  !private
  !public TIFF_Open
  !public TIFF_Close
  !public TIFF_GET_TAG_VALUE
  !public TIFF_GET_IMAGE
  !public GTIFF_GET_KEY_VALUE
  !public TIFF_FILE,TIFF_IFD,TIFF_TAG
 
  integer, parameter :: MAX_IFD=15
  !include 'tags.ext'
  !List of supported TIFF Tags (and it's TagID)
  integer :: TIFF_NewSubfileType          = 254  !
  integer :: TIFF_SubfileType             = 255  !
  integer :: TIFF_ImageWidth              = 256  !
  integer :: TIFF_ImageLength             = 257  !
  integer :: TIFF_BitsPerSample           = 258  ! 4,8,16
  integer :: TIFF_Compression             = 259  ! 1=uncompressed; 2=CCITT; 32773=PackBits;
  integer :: TIFF_PhotometricInt          = 262  ! 0=whiteIs0; 1= blackIs0; 2=RGB; 3=palleteColor; 4=transparencyMask
  integer :: TIFF_Threshholding           = 263
  integer :: TIFF_CellWidth               = 264  !
  integer :: TIFF_CellLength              = 265  !
  integer :: TIFF_FillOrder               = 266  !
  integer :: TIFF_ImageDescription        = 270  !
  integer :: TIFF_Model                   = 270  !
  integer :: TIFF_StripOffsets            = 273  ! Striped Images
  integer :: TIFF_Orientation             = 274  ! orientation - default value is 1 (0,0 = northwest corner)
  integer :: TIFF_SamplesPerPixel         = 277  ! 1=grayscale; 3= RGB
  integer :: TIFF_RowsPerStrip            = 278  ! Striped Images
  integer :: TIFF_StripByteCounts         = 279  ! Striped Images
  integer :: TIFF_MinSampleValue          = 280
  integer :: TIFF_MaxSampleValue          = 281
  integer :: TIFF_XResolution             = 282  !
  integer :: TIFF_YResolution             = 283  !
  integer :: TIFF_PlanarConfiguration     = 284  ! 1=chunky (default), 2=Planar (not recomended!)
  integer :: TIFF_FreeOffsets             = 288  !
  integer :: TIFF_FreeByteCounts          = 289  !
  integer :: TIFF_GrayResponseUn          = 290
  integer :: TIFF_GrayResponseCu          = 291
  integer :: TIFF_ResolutionUnit          = 296
  integer :: TIFF_Software                = 305
  integer :: TIFF_DateTime                = 306  !YYYY:MM:DD HH:MM:SS
  integer :: TIFF_Artist                  = 315  !
  integer :: TIFF_HostComputer            = 316
  integer :: TIFF_ColorMap                = 320
  integer :: TIFF_TileWidth               = 322  !Tiled Images  
  integer :: TIFF_TileLength              = 323  !Tiled Images
  integer :: TIFF_TileOffsets             = 324  !Tiled Images
  integer :: TIFF_TileByteCounts          = 325  !Tiled Images
  integer :: TIFF_ExtraSamples            = 338
  integer :: TIFF_SampleFormat            = 339  !1=unsigned int, 2=int, 3=IEEE float, 4=undefined
  integer :: TIFF_Copyright               = 33432! 

  !GeoTIFF Tags
  integer :: GTIFF_GeoKeyDirectoryTag     = 34735 !(mandatory)
  integer :: GTIFF_GeoDoubleParamsTag     = 34736 !(optional)  !where (which byte) double precision parameters are stored.
  integer :: GTIFF_GeoAsciiParamsTag      = 34737 !(optional)  !where (which byte) ascii            parameters are stored.  
                                                                 !Conditional tags shall follow the following rules:
  integer :: GTIFF_ModelPixelScaleTag     = 33550 !(optional)    !- One of ModelTiepointTag or ModelTransformationTag SHALL be included in an Image File Directory (IFD)  
  integer :: GTIFF_ModelTiepointTag       = 33922 !(conditional) !- If the ModelPixelScaleTag is included in an IFD, then a ModelTiepointTag SHALL also be included.     
  integer :: GTIFF_ModelTransformationTag = 34264 !(conditional) !- If the ModelTransformationTag is included in an IFD, then a ModelPixelScaleTag SHALL NOT be included  
  !GeoTIFF Keys
  integer :: GKey_GTModelType             = 1024  ! 1=projection, 2=Geographic, 3=Geocentric (cartesian 3D)
  integer :: GKey_GTRasterType            = 1025  ! 1=pixel is Area, 2=pixel is point, 0=undefined, 32767=user defined
  integer :: GKey_GTCitation              = 1026 
  ! Geodetic params:
  integer :: GKey_GeodeticCRSGeoKey       = 2048 
  integer :: GKey_GeographicType          = 2048 
  integer :: GKey_GeogCitation            = 2049 
  integer :: GKey_GeogGeodeticDatum       = 2050 
  integer :: GKey_GeogPrimeMeridian       = 2051 
  integer :: GKey_GeogLinearUnits         = 2052 
  integer :: GKey_GeogLinearUnitSize      = 2053 
  integer :: GKey_GeogAngularUnits        = 2054 
  integer :: GKey_GeogAngularUnitSize     = 2055 
  integer :: GKey_GeogEllipsoid           = 2056 
  integer :: GKey_GeogSemiMajorAxis       = 2057 
  integer :: GKey_GeogSemiMinorAxis       = 2058 
  integer :: GKey_GeogInvFlattening       = 2059 
  integer :: GKey_GeogAzimuthUnits        = 2060 
  integer :: GKey_GeogPrimeMeridianLong   = 2061 
  ! Projected params:
  integer :: GKey_ProjectedCSType         = 3072  !EPSG! 0=undefined, 32767=user defined
  integer :: GKey_PCSCitation             = 3073 
  integer :: GKey_Projection              = 3074 
  integer :: GKey_ProjCoordTrans          = 3075 
  integer :: GKey_ProjLinearUnits         = 3076 
  integer :: GKey_ProjLinearUnitSize      = 3077 
  integer :: GKey_ProjStdParallel1        = 3078 
  integer :: GKey_ProjStdParallel2        = 3079 
  integer :: GKey_ProjNatOriginLong       = 3080 
  integer :: GKey_ProjNatOriginLat        = 3081 
  integer :: GKey_ProjFalseEasting        = 3082 
  integer :: GKey_ProjFalseNorthing       = 3083 
  integer :: GKey_ProjFalseOriginLong     = 3084 
  integer :: GKey_ProjFalseOriginLat      = 3085 
  integer :: GKey_ProjFalseOriginEasting  = 3086 
  integer :: GKey_ProjFalseOriginNorthing = 3087 
  integer :: GKey_ProjCenterLong          = 3088 
  integer :: GKey_ProjCenterLat           = 3089 
  integer :: GKey_ProjCenterEasting       = 3090 
  integer :: GKey_ProjCenterNorthing      = 3091 
  integer :: GKey_ProjScaleAtNatOrigin    = 3092 
  integer :: GKey_ProjScaleAtCenter       = 3093 
  integer :: GKey_ProjAzimuthAngle        = 3094 
  integer :: GKey_ProjStraightVertPoleLong= 3095 
  ! Vertical params:
  integer :: GKey_VerticalCSType          = 4096 
  integer :: GKey_VerticalCitation        = 4097 
  integer :: GKey_VerticalDatum           = 4096 
  integer :: GKey_VerticalUnits           = 4099 
  !GDAL Tags:
  integer :: GDAL_METADATA  =42112 
  integer :: GDAL_NODATA    =42113 

  interface TIFF_GET_TAG_VALUE
     module procedure get_tag_value_int, get_tag_values_int, get_tag_value_float, get_tag_values_float,get_tag_value_double, get_tag_values_double, get_tag_values_char
  end interface TIFF_GET_TAG_VALUE

  interface GTIFF_GET_KEY_VALUE
     module procedure get_key_value_short, get_key_values_ascii, get_key_value_double, get_key_values_double
  end interface GTIFF_GET_KEY_VALUE

  character (len=9) :: tagTypeName(12)
  integer           :: typeSize(12)
  data tagTypeName( 1), typeSize( 1)  / 'byte     ' ,  1 / 
  data tagTypeName( 2), typeSize( 2)  / 'ascii    ' ,  1 / 
  data tagTypeName( 3), typeSize( 3)  / 'short    ' ,  2 / 
  data tagTypeName( 4), typeSize( 4)  / 'long     ' ,  4 / 
  data tagTypeName( 5), typeSize( 5)  / 'rational ' ,  8 / 
  data tagTypeName( 6), typeSize( 6)  / 'sbyte    ' ,  1 / 
  data tagTypeName( 7), typeSize( 7)  / 'undefined' ,  1 / 
  data tagTypeName( 8), typeSize( 8)  / 'sshort   ' ,  2 / 
  data tagTypeName( 9), typeSize( 9)  / 'slong    ' ,  4 / 
  data tagTypeName(10), typeSize(10)  / 'srational' ,  8 / 
  data tagTypeName(11), typeSize(11)  / 'float    ' ,  4 / 
  data tagTypeName(12), typeSize(12)  / 'double   ' ,  8 / 

  integer (kind=8), parameter :: intAdj8 = 4294967296_8  ! adjustment for 8-byte unsigned integers
  integer (kind=4), parameter :: intAdj4 = 65536_4       ! adjustment for 4-byte unsigned integers
  integer (kind=2), parameter :: intAdj2 = 256_2         ! adjustment for 2-byte unsigned integers
  integer (kind=1), parameter :: intAdj1 = 8_1           ! adjustment for 1-byte unsigned integers

  !GeoTIFF Dir and Keys:  --------------------
  type GEO_KEY
      integer                    :: Id,Typ,Cnt,Offset  !8-bytes (2,2,2,2) key 
  end type  
  type GEO_DIR
      integer                    :: n_Keys,version,revision,minor_revision 
      type(GEO_KEY), allocatable :: Key(:)  
      integer                    :: offsetAscii,offsetFloat
  end type
  !Image File Dir and Tags: ------------------
  type TIFF_TAG
      integer         :: Id,Typ,Cnt,Offset  !12-bytes (2,2,4,4) tag
  end type  
  type TIFF_IFD                    !Image File Directory
     integer          :: n_tags    ! # of Tags in this IFD            !2-bytes
     type(TIFF_TAG), allocatable :: Tag(:) 
     integer          :: offset    ! next IFD offset or 0 (end IFD)   !4-bytes
  end type
  !-------------------------------------------
  type TIFF_FILE                            !Representation of TIFF file
     integer(kind=4)    :: iUnit            ! id of file (for open and close commands)
     character(256)      :: path            ! path to file
     !Header
     character(len=2)   :: byteOrder        ! II/MM
     integer            :: magic_num        ! 42
     integer            :: offset           ! offset (location from begining in bytes) of 1st IFD (IMPORTANT: first byte = 0!)
     type (TIFF_IFD)    :: IFD(MAX_IFD)     ! Image File Directory (IFD) list
     type (GEO_DIR)     :: gDir             ! GeoDir with GeoKeys
     integer            :: n_imgs           ! total number of ifds
     !Important extra parameters:
     integer            :: nx,ny            !nx, ny
     logical            :: swapByte=.false.    
     character(5)       :: ImgType='strip'  !'strip' / 'tile'
     integer            :: compression=1    !1=None, 32773=PackBits, 2:Huffman, 5:LZW, 8:DEFLATE (zlib).
     integer            :: planarConf =1    !1=chunky format, 2=planar format
     integer            :: orientation=1    !
     integer            :: samplesPerPixel=1!1=[0,0]
     integer            :: sampleFormat=1   !1=unsigned integer, 2=two component signed integer, 3=IEEE float, 4=undefined
     !GeoTIFF parameters:
     double precision ::  trans(16) =0.0    !transformation matrix indices -> coordinates
     double precision ::  tiePt(6)  =0.0    ![i,j,k,x,y,z] values
     double precision ::  scale(3)  =0.0    ![dx,dy,dz]
  end type
  !-------------------------------------------
contains

subroutine TIFF_Open(iUnit,inpFile,action,tiff,iost)
   implicit none
   type(TIFF_FILE), intent(inout) :: tiff
   integer        , intent(inout) :: iost
   integer        , intent(in)    :: iUnit
   character(*)   , intent(in)    :: inpFile
   character(1)   , intent(in)    :: action  !'r' or 'w'

   tiff%path=inpFile
   tiff%iUnit=iUnit
   select case (action)
     case ('r','R','read','READ','Read')
        print'("Opening tiff file: ",A20)',inpFile
        open(unit=tiff%iUnit, file=trim(tiff%path), form='UNFORMATTED', &
             action='READ',status='OLD',access='DIRECT', recl=1,iostat=iost)

        ![OK] Read Header
        call TIFF_READ_HEADER(tiff)  

        ![OK] Read all IFDs
        call TIFF_READ_IFDS(tiff)
      
        !save some important tag values
        call TIFF_GET_TAG_VALUE(tiff, 1, TIFF_ImageWidth         , tiff%nx             )
        call TIFF_GET_TAG_VALUE(tiff, 1, TIFF_ImageLength        , tiff%ny             )
        call TIFF_GET_TAG_VALUE(tiff, 1, TIFF_Compression        , tiff%compression    )
        call TIFF_GET_TAG_VALUE(tiff, 1, TIFF_PlanarConfiguration, tiff%planarConf     )
        call TIFF_GET_TAG_VALUE(tiff, 1, TIFF_Orientation        , tiff%orientation    ) 
        call TIFF_GET_TAG_VALUE(tiff, 1, TIFF_SamplesPerPixel    , tiff%samplesPerPixel) ! multi-band!

        if (hasTag(tiff, 1, TIFF_SampleFormat)) then
             call TIFF_GET_TAG_VALUE(tiff, 1, TIFF_SampleFormat  , tiff%sampleFormat) ! data representation
        else
            print '("SampleFormat not specified. Asuming unsigned integer.")'
            tiff%sampleFormat=1
        end if

        ![  ] CHECKS =====================!
        if ( tiff%planarConf  /= 1 ) stop 'Planar configuration not supported.'
        if ( tiff%compression == 2 ) stop 'Huffman bilevel compression not supported!.'
        if ( tiff%compression == 5 ) stop 'LZW compression not supported (yet)!.'
        !=================================!
        
        ![  ] Read GeoKeys from GeoDir
        if ( hasTag(tiff, 1, GTIFF_GeoKeyDirectoryTag) ) then 
            print*, "GeoTIFF File!"
            call GTIFF_READ_GDIR(tiff)

            !Raster coordinates and scale parameters:
            if ( hasTag(tiff,1, GTIFF_ModelTransformationTag) ) then
               call TIFF_GET_TAG_VALUE(tiff, 1, GTIFF_ModelTransformationTag, tiff%trans )
            else
               call TIFF_GET_TAG_VALUE(tiff, 1, GTIFF_ModelTiePointTag      , tiff%tiePt)
               call TIFF_GET_TAG_VALUE(tiff, 1, GTIFF_ModelPixelScaleTag    , tiff%scale)
            end if
        else
            print*, "WARNING: GeoKeyDirectory Tag not found."
        endif

        if      ( hasTag(tiff,1, TIFF_TileOffsets ) ) then 
           print*, " Tiled type!"; tiff%ImgType='tile'
        else if ( hasTag(tiff, 1, TIFF_StripOffsets) ) then
           print*, " Strip type!"; tiff%ImgType='strip'
        else
           stop "ERROR. Image type not identified!"
        end if

     case ('w','W','write','WRITE','Write')
        stop "Write option not supported yet!"
        
     case default
        stop "Not a valid action!"
   end select
end subroutine

subroutine TIFF_Close(tiff)
   implicit none
   type(TIFF_FILE), intent(inout) :: tiff
   close(tiff%iUnit)
endsubroutine

subroutine TIFF_READ_HEADER(tiff)
   implicit none
   type(TIFF_FILE), intent(inout) :: tiff
   character(len=1) :: cha_1(2)
   integer (kind=1) :: magic_1(2), offset_1(4)

   print*,'  Reading 8-byte header..'
   read(unit=tiff%iUnit, rec=1) cha_1(1)     ! endianness 
   read(unit=tiff%iUnit, rec=2) cha_1(2)     ! endianness 
   tiff%byteOrder=transfer([cha_1(1),cha_1(2)],tiff%byteOrder)
   if ( tiff%byteOrder /= cpuEnd() ) tiff%swapByte=.true. !If byteOrder is diffrent to CPU byte-order change the way of reading the tiff file:

   read(unit=tiff%iUnit, rec=3) magic_1(1)   ! magic number (42) 
   read(unit=tiff%iUnit, rec=4) magic_1(2)   ! magic number (42) 

   read(unit=tiff%iUnit, rec=5) offset_1(1)  ! IFD offset   (1st byte) 
   read(unit=tiff%iUnit, rec=6) offset_1(2)  ! IFD offset   (2nd byte) 
   read(unit=tiff%iUnit, rec=7) offset_1(3)  ! IFD offset   (3rd byte) 
   read(unit=tiff%iUnit, rec=8) offset_1(4)  ! IFD offset   (4th byte)  
   
   if ( tiff%swapByte ) then
      tiff%magic_num = transfer([magic_1(2),   magic_1(1),  0_1       , 0_1        ], tiff%magic_num) 
      tiff%offset    = transfer([offset_1(4), offset_1(3), offset_1(2), offset_1(1)], int(1) )
   else
      tiff%magic_num = transfer([magic_1(1),   magic_1(2),  0_1       , 0_1        ], tiff%magic_num) 
      tiff%offset    = transfer([offset_1(1), offset_1(2), offset_1(3), offset_1(4)], int(1) )
   end if
   !CHECKS ==========================!
   if ( tiff%magic_num /= 42 ) stop 'Fatal ERROR: Not a TIFF file..' !Check magic-number (TIFF format identifier)
   if ( tiff%offset     < 0  ) tiff%offset = tiff%offset + intAdj4   !Adjustment for 4-byte unsigned integers
   !=================================!
   print '("   Header: ",A3, I4, I12)',tiff%byteOrder,tiff%magic_num,tiff%offset
end subroutine 

subroutine TIFF_READ_IFDs(tiff)
  implicit none
  type(TIFF_FILE), intent(inout) :: tiff
  integer(kind=1) :: ntags_1(2), id_1(2), typ_1(2)
  integer(kind=1) :: cnt_1(4), off_1(4), ioff_1(4)
  integer         :: i,t,tmp_offset
  
  tmp_offset=tiff%offset
  i=1
  do while (tmp_offset /= 0) !
     print '("   Image File Directory (IFD): ",I6)', i

     if (i > size(tiff%IFD) ) stop 'Too many IFDs. Change max number of IFDs admitted for TIFF files in the source code.'

     tiff%IFD(i)%offset=tmp_offset 

     read(unit=tiff%iUnit, rec=tmp_offset+1) ntags_1(1) ! # tags (1)
     read(unit=tiff%iUnit, rec=tmp_offset+2) ntags_1(2) ! # tags (2)

     if (tiff%swapByte) then
        tiff%IFD(i)%n_tags = transfer([ntags_1(2:1:-1),0_1,0_1], int(1))
     else
        tiff%IFD(i)%n_tags = transfer([ntags_1        ,0_1,0_1], int(1))
     end if

     print '("    # of Tags in IFD: ",I12)', tiff%IFD(i)%n_Tags 
     if ( .not. allocated(tiff%IFD(i)%tag) ) allocate( tiff%IFD(i)%tag( tiff%IFD(i)%n_Tags ))

     do t=1,tiff%IFD(i)%n_tags

        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+1 )  id_1(1) !TagId   (1)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+2 )  id_1(2) !TagId   (2)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+3 ) typ_1(1) !TagType (1)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+4 ) typ_1(2) !TagType (2)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+5 ) cnt_1(1) !Count   (1)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+6 ) cnt_1(2) !Count   (2)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+7 ) cnt_1(3) !Count   (3)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+8 ) cnt_1(4) !Count   (4)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+9 ) off_1(1) !Offset  (1)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+10) off_1(2) !Offset  (2)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+11) off_1(3) !Offset  (3)
        read(unit=tiff%iUnit, rec=tmp_offset+2+(t-1)*12+12) off_1(4) !Offset  (4)

        if ( tiff%swapByte ) then
            tiff%IFD(i)%tag(t)%Id     = transfer([ id_1(2:1:-1),0_1,0_1], int(1))
            tiff%IFD(i)%tag(t)%Typ    = transfer([typ_1(2:1:-1),0_1,0_1], int(1))
            tiff%IFD(i)%tag(t)%Cnt    = transfer(cnt_1(4:1:-1), int(1))
            tiff%IFD(i)%tag(t)%Offset = transfer(off_1(4:1:-1), int(1))
        else 
            tiff%IFD(i)%tag(t)%Id     = transfer([ id_1,0_1,0_1], int(1))
            tiff%IFD(i)%tag(t)%Typ    = transfer([typ_1,0_1,0_1], int(1))
            tiff%IFD(i)%tag(t)%Cnt    = transfer(cnt_1          , int(1))
            tiff%IFD(i)%tag(t)%Offset = transfer(off_1          , int(1))
        end if

        if ( tiff%IFD(i)%tag(t)%Cnt    < 0 )  tiff%IFD(i)%tag(t)%Cnt   =tiff%IFD(i)%tag(t)%Cnt    +  intAdj4 
        if ( tiff%IFD(i)%tag(t)%Offset < 0 )  tiff%IFD(i)%tag(t)%Offset=tiff%IFD(i)%tag(t)%Offset +  intAdj4 

        call print_tag(t,tiff%IFD(i)%tag(t))
     end do

     read(unit=tiff%iUnit, rec=tmp_offset+2+tiff%IFD(i)%n_Tags*12+1) ioff_1(1) !IFD Offset  (1)
     read(unit=tiff%iUnit, rec=tmp_offset+2+tiff%IFD(i)%n_Tags*12+2) ioff_1(2) !IFD Offset  (2)
     read(unit=tiff%iUnit, rec=tmp_offset+2+tiff%IFD(i)%n_Tags*12+3) ioff_1(3) !IFD Offset  (3)
     read(unit=tiff%iUnit, rec=tmp_offset+2+tiff%IFD(i)%n_Tags*12+4) ioff_1(4) !IFD Offset  (4)

     if (tiff%swapByte) then
        tmp_offset    = transfer(ioff_1(4:1:-1), tmp_offset)
     else
        tmp_offset    = transfer(ioff_1        , tmp_offset)
     end if
     print '("   IFD offset: ",i5)',tmp_offset
     i=i+1
  end do
  tiff%n_imgs=i-1

end subroutine

subroutine GTIFF_READ_GDIR(tiff)
      implicit none
      type(TIFF_FILE),intent(inout) :: tiff
      integer         :: gDirOffset,k,typ,cnt
      logical         :: found
      integer(kind=1) :: v_1(2),  r_1(2), mr_1(2), nk_1(2)  !Head(8)
      integer(kind=1) ::id_1(2),typ_1(2),cnt_1(2),off_1(2)  !Key(8)

      call get_tag_parameters(tiff,1,GTIFF_GeoDoubleParamsTag,typ,cnt,tiff%gDir%OffsetFloat,found) !offset of geoDir
      call get_tag_parameters(tiff,1,GTIFF_GeoAsciiParamsTag ,typ,cnt,tiff%gDir%OffsetAscii,found) !offset of geoDir

      print '("   GeoTIFF Keys Directory (geoDir): ",I6)'
      call get_tag_parameters(tiff,1,GTIFF_geoKeyDirectoryTag,typ,cnt,gDirOffset,found) !offset of geoDir

      print '("    Offset Float Keys=",I3,/,"    Offset ASCII Keys=",I3)',tiff%gDir%offsetFloat, tiff%gDir%offsetAscii 

      read(unit=tiff%iUnit, rec=gDirOffset+1)  v_1(1)       ! version        - read but not used
      read(unit=tiff%iUnit, rec=gDirOffset+2)  v_1(2)       ! version        - read but not used
      read(unit=tiff%iUnit, rec=gDirOffset+3)  r_1(1)       ! revision       - read but not used
      read(unit=tiff%iUnit, rec=gDirOffset+4)  r_1(2)       ! revision       - read but not used
      read(unit=tiff%iUnit, rec=gDirOffset+5) mr_1(1)       ! minor revision - read but not used
      read(unit=tiff%iUnit, rec=gDirOffset+6) mr_1(2)       ! minor revision - read but not used
      read(unit=tiff%iUnit, rec=gDirOffset+7) nk_1(1)       ! number of GeoKeys (1)
      read(unit=tiff%iUnit, rec=gDirOffset+8) nk_1(2)       ! number of GeoKeys (2)

      if (tiff%swapByte) then
          tiff%gDir%version       = transfer([ v_1(2:1:-1),0_1,0_1], int(1) )
          tiff%gDir%revision      = transfer([ r_1(2:1:-1),0_1,0_1], int(1) )
          tiff%gDir%minor_revision= transfer([mr_1(2:1:-1),0_1,0_1], int(1) )
          tiff%gDir%n_keys        = transfer([nk_1(2:1:-1),0_1,0_1], int(1) )
      else
          tiff%gDir%version       = transfer([  v_1, 0_1,0_1], int(1) )
          tiff%gDir%revision      = transfer([  r_1, 0_1,0_1], int(1) )
          tiff%gDir%minor_revision= transfer([ mr_1, 0_1,0_1], int(1) )
          tiff%gDir%n_keys        = transfer([ nk_1, 0_1,0_1], int(1) )
      end if
      allocate(tiff%gDir%key(tiff%gDir%n_Keys))
      print '("    # of Keys in gDir: ",I6)', tiff%gDir%n_Keys
      do k=1,tiff%gDir%n_Keys
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+1)  id_1(1) !id
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+2)  id_1(2) !id
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+3) typ_1(1) !tiffTagLocation (kind)
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+4) typ_1(2) !tiffTagLocation (kind)
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+5) cnt_1(1) !count
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+6) cnt_1(2) !count
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+7) off_1(1) !offset/value
         read(unit=tiff%iUnit, rec=gDirOffset+8+(k-1)*8+8) off_1(2) !offset/value

         if (tiff%swapByte) then
            tiff%gDir%key(k)%Id    = transfer([ id_1(2:1:-1),0_1,0_1], int(1) ) 
            tiff%gDir%key(k)%Typ   = transfer([typ_1(2:1:-1),0_1,0_1], int(1) )
            tiff%gDir%key(k)%Cnt   = transfer([cnt_1(2:1:-1),0_1,0_1], int(1) )
            tiff%gDir%key(k)%Offset= transfer([off_1(2:1:-1),0_1,0_1], int(1) )
         else
            tiff%gDir%key(k)%Id    = transfer([ id_1        ,0_1,0_1], int(1) ) 
            tiff%gDir%key(k)%Typ   = transfer([typ_1        ,0_1,0_1], int(1) )
            tiff%gDir%key(k)%Cnt   = transfer([cnt_1        ,0_1,0_1], int(1) )
            tiff%gDir%key(k)%Offset= transfer([off_1        ,0_1,0_1], int(1) )
         end if

         if ( tiff%gDir%key(k)%Id     < 0 )  tiff%gDir%key(k)%Id    = tiff%gDir%key(k)%Id     +  intAdj4
         if ( tiff%gDir%key(k)%Typ    < 0 )  tiff%gDir%key(k)%Typ   = tiff%gDir%key(k)%Typ    +  intAdj4
         if ( tiff%gDir%key(k)%Cnt    < 0 )  tiff%gDir%key(k)%Cnt   = tiff%gDir%key(k)%Cnt    +  intAdj4
         if ( tiff%gDir%key(k)%Offset < 0 )  tiff%gDir%key(k)%Offset= tiff%gDir%key(k)%Offset +  intAdj4 
         
         call print_key(k,tiff%gDir%key(k))
      enddo
end subroutine

!=== TIFF_GET_TAG_VALUE  ====================
subroutine get_tag_value_int(tiff,i,tagId,val)
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: i, tagId
   integer        , intent(inout)  :: val
   integer                         :: tmp_arr(1)
   call get_tag_values_int(tiff,i,tagId,tmp_arr)
   val=tmp_arr(1)
end subroutine

subroutine get_tag_value_float(tiff,i,tagId,val)
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: i, tagId
   real           , intent(inout)  :: val
   real                            :: tmp_arr(1)
   call get_tag_values_float(tiff,i,tagId,tmp_arr)
   val=tmp_arr(1)
end subroutine

subroutine get_tag_value_double(tiff,i,tagId,val)
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: i, tagId
   double precision, intent(inout) :: val
   double precision                :: tmp_arr(1)
   call get_tag_values_double(tiff,i,tagId,tmp_arr)
   val=tmp_arr(1)
end subroutine

subroutine get_tag_values_int(tiff,i,tagId,values)  !for INTEGERs
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: i, tagId
   integer        , intent(inout)  :: values(:)
   integer :: typ,cnt,off,siz
   logical :: found=.false.
   integer(kind=1), allocatable    :: values_1(:)
   integer :: c
   integer(kind=2) :: tmpInt2
   integer(kind=4) :: tmpInt4

   call get_tag_parameters(tiff,i,tagId,typ,cnt,off,found)
   if (found) then
   siz=typeSize(typ)
      if ( cnt * siz  <= 4 ) then  !if value size (cnt*size) <= 4-bytes, then offset is the value
         values=off
      else
         allocate(values_1(siz*cnt))
         call get_field_as_byte_array(tiff,off,values_1) !,siz,cnt

         if ( typ == 3 ) then
            do c=1,cnt
               if (tiff%swapByte) then
                   values(c)=transfer(values_1(c*siz:1+(c-1)*siz:-1), tmpInt2)
               else
                   values(c)=transfer(values_1(1+(c-1)*siz:c*siz   ), tmpInt2)
               end if
               if ( values(c) < 0) values(c)=values(c)+intAdj2   !short (2-bytes) unsigned
            enddo
            deallocate(values_1)
         end if
         if ( typ == 4 )  then
            do c=1,cnt
               if (tiff%swapByte) then
                   values(c)=transfer(values_1(c*siz:1+(c-1)*siz:-1), tmpInt4)
               else
                   values(c)=transfer(values_1(1+(c-1)*siz:c*siz   ), tmpInt4)
               end if
               if ( values(c) < 0) values(c)=values(c)+intAdj4   !short (2-bytes) unsigned
            enddo
            deallocate(values_1)
         end if

      endif
   endif
end subroutine

subroutine get_tag_values_float(tiff,i,tagId,values)  !for REAL (float)
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: i, tagId
   real           , intent(inout)  :: values(:)
   integer :: typ,cnt,off,siz
   logical :: found=.false.
   integer(kind=1), allocatable    :: values_1(:)
   integer :: c
   call get_tag_parameters(tiff,i,tagId,typ,cnt,off,found)
   if (found) then
      siz=typeSize(typ)
      if ( cnt * siz  <= 4 ) then  !if value size (cnt*size) <= 4-bytes, then offset is the value
         values=real(off)
      else
         allocate(values_1(siz*cnt))
         call get_field_as_byte_array(tiff,off,values_1) !,siz,cnt
         do c=1,cnt
            if (tiff%swapByte) then
                values(c)=transfer(values_1(c*siz:1+(c-1)*siz:-1), float(1))
            else
                values(c)=transfer(values_1(1+(c-1)*siz:c*siz   ), float(1))
            end if
         enddo 
         deallocate(values_1)
      endif
   endif
end subroutine

subroutine get_tag_values_double(tiff,i,tagId,values)  !for DOUBLE
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: i, tagId
   double precision, intent(inout) :: values(:)
   integer :: typ,cnt,off,siz
   logical :: found=.false.
   integer(kind=1), allocatable    :: values_1(:)
   integer :: c
   call get_tag_parameters(tiff,i,tagId,typ,cnt,off,found)
   if (found) then
      siz=typeSize(typ)
      if ( cnt * siz  <= 4 ) then  !if value size (cnt*size) <= 4-bytes, then offset is the value
         values=dble(off)
      else
         allocate(values_1(siz*cnt))
         call get_field_as_byte_array(tiff,off,values_1) !,siz,cnt
         do c=1,cnt
            if (tiff%swapByte) then
                values(c)=transfer(values_1(c*siz:1+(c-1)*siz:-1), dble(1))
            else
                values(c)=transfer(values_1(1+(c-1)*siz:c*siz   ), dble(1))
            end if
         enddo 
         deallocate(values_1)
      endif
   endif
end subroutine

subroutine get_tag_values_char(tiff,i,tagId,values)  !for CHARACTER
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: i, tagId
   character(*)   , intent(inout)  :: values
   integer :: typ,cnt,off,siz
   logical :: found=.false.
   integer(kind=1), allocatable    :: values_1(:)
   integer :: c
   call get_tag_parameters(tiff,i,tagId,typ,cnt,off,found)
   if (found) then
   siz=typeSize(typ)
      if ( cnt * siz  <= 4 ) then  !if value size (cnt*size) <= 4-bytes, then offset is the value
         values=char(off)
      else
         allocate(values_1(siz*cnt))
         call get_field_as_byte_array(tiff,off,values_1) !,siz,cnt
         do c=1,cnt
            values(c:c)=transfer(values_1(c), values(1:1))
         enddo 
         deallocate(values_1)
      endif
   endif
end subroutine
!=== END TIFF_GET_TAG_VALUE =============

!=== GTIFF_GET_KEY_VALUE     ============
subroutine get_key_value_short(tiff, keyId, values)
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: keyId
   integer        , intent(inout)  :: values
   integer                         :: typ,cnt,off
   logical                         :: found
   call get_key_parameters(tiff,keyId,typ,cnt,off,found)
   values=off
end subroutine 

subroutine get_key_value_double(tiff,keyId,val)
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: keyId
   real           , intent(inout)  :: val
   real                            :: tmp_arr(1)
   tmp_arr(1)= val
   call get_key_values_double(tiff,keyId,tmp_arr)
   val=tmp_arr(1)
end subroutine

subroutine get_key_values_double(tiff, keyId, values)
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: keyId
   real           , intent(inout)  :: values(:)
   integer                         :: typ,cnt,off,siz
   logical                         :: found
   integer(kind=1), allocatable    :: values_1(:)
   integer :: c
   call get_key_parameters(tiff,keyId,typ,cnt,off,found)
   if (found) then
      siz=8; allocate(values_1(siz*cnt)) !double (float)
      call get_field_as_byte_array(tiff, tiff%gDir%offsetFloat+siz*off, values_1)
      do c=1,cnt
         if (tiff%swapByte) then
             values(c)=transfer(values_1(c*siz:1+(c-1)*siz:-1), float(1))
         else
             values(c)=transfer(values_1(1+(c-1)*siz:c*siz   ), float(1))
         end if
      enddo
      deallocate(values_1)
   endif
end subroutine 

subroutine get_key_values_ascii(tiff, keyId, values)
   implicit none
   type(TIFF_FILE), intent(in)     :: tiff
   integer        , intent(in)     :: keyId
   character(*)   , intent(inout)  :: values
   integer                         :: typ,cnt,off,siz
   logical                         :: found
   integer(kind=1), allocatable    :: values_1(:)
   integer :: c
   call get_key_parameters(tiff,keyId,typ,cnt,off,found)
   if (found) then
      siz=1; allocate(values_1(siz*cnt)) !ascii
      call get_field_as_byte_array(tiff, tiff%gDir%offsetAscii+siz*off, values_1)
      do c=1,cnt
         values(c:c)=transfer(values_1(c), char(1))
      enddo
      deallocate(values_1)
   endif
end subroutine 
!=== END GTIFF GET_KEY_VALUE ============
 
!MISC Functions =====================
logical function hasGKey(tiff,i,keyId)
   implicit none
   type(TIFF_FILE), intent(in) ::tiff
   integer        , intent(in) ::i, keyId
   integer :: t
   hasGKey=.false.
   if ( i <= tiff%n_imgs ) then
      do t=1,tiff%Gdir%n_keys
          if ( tiff%Gdir%key(t)%Id == keyId ) then
             hasGKey=.true.
             return
          endif
      enddo
   endif
end function


logical function hasTag(tiff,i,tagId)
   implicit none
   type(TIFF_FILE), intent(in) ::tiff
   integer        , intent(in) ::i, tagId
   integer :: t
   hasTag=.false.
   if ( i <= tiff%n_imgs ) then
      do t=1,tiff%IFD(i)%n_tags
          if ( tiff%IFD(i)%tag(t)%Id == tagId ) then
             hasTag=.true.
             return
          endif
      enddo
   endif
end function

subroutine get_tag_parameters(tiff,i,tagId,typ,cnt,off,found)
   implicit none
   type(TIFF_FILE),intent(in)    :: tiff
   integer        ,intent(in)    :: i, tagId
   integer        ,intent(inout) :: typ,cnt,off
   logical        ,intent(inout) :: found
   integer :: t  !loop indices

   do t=1,tiff%IFD(i)%n_tags
      if ( tiff%IFD(i)%tag(t)%Id == tagId ) then
         typ=tiff%IFD(i)%tag(t)%typ
         cnt=tiff%IFD(i)%tag(t)%cnt
         off=tiff%IFD(i)%tag(t)%offset
         found=.true.
         return
      endif
   enddo
   if (.not. found) print '("Error: TagId not found:",I5," (",A,")")',tagId,trim(tagName(tagId))
end subroutine

subroutine get_key_parameters(tiff,keyId,typ,cnt,off,found)
   implicit none
   type(TIFF_FILE),intent(in)    :: tiff
   integer        ,intent(in)    :: keyId
   integer        ,intent(inout) :: typ,cnt,off
   logical        ,intent(inout) :: found
   integer :: k    !loop indices
   do k=1,tiff%gDir%n_Keys
      if ( tiff%gDir%key(k)%Id == keyId ) then
         typ=tiff%gDir%key(k)%typ
         cnt=tiff%gDir%key(k)%cnt
         off=tiff%gDir%key(k)%offset
         found=.true.
         return
      endif
   enddo
   if (.not. found) print '("Error: KeyId not found:",I5," (",A,")")',keyId,trim(keyName(keyId))
end subroutine

subroutine get_field_as_byte_array(tiff,offset,values_1)!,siz,cnt
  implicit none
  type(TIFF_FILE),intent(in)     :: tiff
  integer        ,intent(in)     :: offset
  integer(kind=1),intent(inout)  :: values_1(:) !1-byte elements array
  integer :: i
  do i=1,size(values_1)
     read(unit=tiff%iUnit, rec=offset+1+i-1 ) values_1(i)
  enddo
end subroutine

character(len=22) function tagName(tagId)
  implicit none
  integer,intent(in) :: tagId
  select case (tagId)
     case(256 )   ;tagName='ImageWidth            '
     case(257 )   ;tagName='ImageLength           '
     case(258 )   ;tagName='BitsPerSample         '
     case(259 )   ;tagName='Compression           '
     case(262 )   ;tagName='PhotometricInt        '
     case(263 )   ;tagName='Threshholding         '
     case(264 )   ;tagName='CellWidth             '
     case(265 )   ;tagName='CellLength            '
     case(266 )   ;tagName='FillOrder             '
     case(270 )   ;tagName='ImageDescription      '
     case(271 )   ;tagName='Make                  '
     case(272 )   ;tagName='Model                 '
     case(273 )   ;tagName='StripOffsets          '
     case(274 )   ;tagName='Orientation           '
     case(277 )   ;tagName='SamplesPerPixel       '
     case(278 )   ;tagName='RowsPerStrip          '
     case(279 )   ;tagName='StripByteCounts       '
     case(280 )   ;tagName='MinSampleValue        '
     case(281 )   ;tagName='MaxSampleValue        '
     case(282 )   ;tagName='XResolution           '
     case(283 )   ;tagName='YResolution           '
     case(284 )   ;tagName='PlanarConfig          ' !
     case(306 )   ;tagName='DateTime              ' !
     case(322 )   ;tagName='TileWidth             '
     case(323 )   ;tagName='TileLength            '
     case(324 )   ;tagName='TileOffsets           '
     case(325 )   ;tagName='TileByteCounts        '
     case(339 )   ;tagName='SampleFormat          '
     case(3355)   ;tagName='ModelPixelScale       '
     case(3392)   ;tagName='ModelTiePoint         '
     case(34735)  ;tagName='GeoKeyDirectoryTag    ' !GeoTiff-tag
     case(34736)  ;tagName='GeoDoubleParamsTag    ' !GeoTiff-tag
     case(34737)  ;tagName='GeoAsciiParamsTag     ' !GeoTiff-tag
     case(33550)  ;tagName='ModelPixelScaleTag    ' !GeoTiff-tag
     case(33922)  ;tagName='ModelTiepointTag      ' !GeoTiff-tag
     case(34264)  ;tagName='ModelTransformationTag' !GeoTiff-tag
     case(42112)  ;tagName='GDAL_METADATA         ' 
     case(42113)  ;tagName='GDAL_NODATA           ' 
     case default ;tagName='Not Recognized TagId  '
  end select
end function

character(len=25) function keyName(keyId)
  implicit none
  integer,intent(in) :: keyId
  select case (keyId)
     !GeoTiff-tag
     case(34735); keyName='GeoKeyDirectoryTag       '
     case(34736); keyName='GeoDoubleParamsTag       ' 
     case(34737); keyName='GeoAsciiParamsTag        ' 
     case(33550); keyName='ModelPixelScaleTag       ' 
     case(33922); keyName='ModelTiepointTag         ' 
     case(34264); keyName='ModelTransformationTag   ' 
     !Config Keys:
     case (1024); keyName= 'GTModelType             '
     case (1025); keyName= 'GTRasterType            ' 
     case (1026); keyName= 'GTCitation              ' 
     !Geodetic CRS Params:
     case (2048); keyName= 'GeographicType          ' 
     case (2049); keyName= 'GeogCitation            '
     case (2050); keyName= 'GeogGeodeticDatum       '
     case (2051); keyName= 'GeogPrimeMeridian       '
     case (2052); keyName= 'GeogLinearUnits         '
     case (2053); keyName= 'GeogLinearUnitSize      '
     case (2054); keyName= 'GeogAngularUnits        '
     case (2055); keyName= 'GeogAngularUnitSize     '
     case (2056); keyName= 'GeogEllipsoid           '
     case (2057); keyName= 'GeogSemiMajorAxis       '
     case (2058); keyName= 'GeogSemiMinorAxis       '
     case (2059); keyName= 'GeogInvFlattening       '
     case (2060); keyName= 'GeogAzimuthUnits        '
     case (2061); keyName= 'GeogPrimeMeridianLong   '
     !Projected CRS Params: 
     case (3072); keyName= 'ProjectedCSType         ' !EPSG CODE!
     case (3073); keyName= 'PCSCitation             '
     case (3074); keyName= 'Projection              '
     case (3075); keyName= 'ProjCoordTrans          ' 
     case (3076); keyName= 'ProjLinearUnits         '
     case (3077); keyName= 'ProjLinearUnitSize      ' 
     case (3078); keyName= 'ProjStdParallel1        ' 
     case (3079); keyName= 'ProjStdParallel2        ' 
     case (3080); keyName= 'ProjNatOriginLong       ' 
     case (3081); keyName= 'ProjNatOriginLat        ' 
     case (3082); keyName= 'ProjFalseEasting        ' 
     case (3083); keyName= 'ProjFalseNorthing       ' 
     case (3084); keyName= 'ProjFalseOriginLong     ' 
     case (3085); keyName= 'ProjFalseOriginLat      ' 
     case (3086); keyName= 'ProjFalseOriginEasting  ' 
     case (3087); keyName= 'ProjFalseOriginNorthing '
     case (3088); keyName= 'ProjCenterLong          '
     case (3089); keyName= 'ProjCenterLat           '
     case (3090); keyName= 'ProjCenterEasting       '
     case (3091); keyName= 'ProjCenterNorthing      '
     case (3092); keyName= 'ProjScaleAtNatOrigin    '
     case (3093); keyName= 'ProjScaleAtCenter       '
     case (3094); keyName= 'ProjAzimuthAngle        '
     case (3095); keyName= 'ProjStraightVertPoleLong'
     case (4096); keyName= 'VerticalCSType          '  !Vertical CRS Params:
     case (4097); keyName= 'VerticalCitation        '
     case (4098); keyName= 'VerticalDatum           '
     case (4099); keyName= 'VerticalUnits           '
     case default;keyName= 'Not recognized geoKey   '
  end select
end function

character(6) function keyTypeName(keyType)
  implicit none
  integer,intent(in) :: keyType
  select case (keyType)
     case (0    ) ; keyTypeName= 'short '
     case (34736) ; keyTypeName= 'double'
     case (34737) ; keyTypeName= 'ascii '
     case default;  keyTypeName= 'short*'
  end select
end function

integer function keyTypeSize(keyType)
  implicit none
  integer,intent(in) :: keyType
  select case (keyType)
     case (0    ) ; keyTypeSize= 2 !short 
     case (34736) ; keyTypeSize= 8 !double
     case (34737) ; keyTypeSize= 1 !ascii 
     case default;  keyTypeSize=-99
  end select
end function

! Informative DEBUG support functions:
subroutine print_tag(t,tag)
 implicit none
 integer      , intent(in) :: t
 type(TIFF_TAG), intent(in) :: tag
 print '("    Tag ",i2,":",i9," (",A22,")",i10," (",A6,")", i12,i12)',t, tag%Id, tagName(tag%Id), tag%Typ, tagTypeName(tag%Typ), tag%Cnt, tag%Offset 
end subroutine

subroutine print_key(k,key)
 implicit none
 integer      , intent(in) :: k
 type(GEO_KEY), intent(in) :: key
 print '("    Key ",i2,":",i9," (",A22,")",i10," (",A6,")", i12,i12)',k, key%Id, keyName(key%Id), key%Typ, keyTypeName(key%Typ), key%Cnt, key%Offset 
end subroutine

subroutine debug_print_tiff_content(tiff)
   implicit none
   type(TIFF_FILE), intent(in) ::tiff
   integer :: i,t,k
   print*, "DEBUG: Check Tiff Content:"
   print*, "TIFF: ",tiff%path,  tiff%iUnit
   print*, "  Header:",tiff%byteOrder, tiff%magic_num, tiff%offset
   do i=1,tiff%n_imgs
   print*, "  IFS ",i, tiff%IFD(i)%n_Tags
   do t=1,tiff%IFD(i)%n_Tags
      call print_tag(t,tiff%IFD(i)%tag(t))
   enddo
   enddo
   print*, "  GDir", tiff%gDir%n_Keys
   do k=1,tiff%gDir%n_Keys
      call print_key(k,tiff%gDir%key(k))
   enddo
end subroutine

function cpuEnd()   ! Determines the endian-ness of the CPU
   implicit none
   character (len=2)  :: CpuEnd
   integer  (kind=2)   :: i2=1
   if (btest(i2,0)) then
      cpuend = 'II'             !(L) little-endian
   elseif (btest(i2,8)) then
      cpuend = 'MM'             !(B)    big-endian
   else
      cpuend = 'XX'             !Something else.. Error?
   end if
   return
end function  
! END MISC Functions =================

!=== TIFF_GET_IMAGE     =============
subroutine TIFF_GET_IMAGE(tiff,img_num,IMG)
   implicit none
   type(TIFF_FILE), intent(in)   :: tiff
   integer        , intent(in)   :: img_num
   real           , intent(inout):: IMG(:,:)  !extend to integer and double precission
   !global
   integer              :: imageWidth, imageLength, bytesPerSample, bitsPerSample!, samplesPerPixel,orientation
   integer, allocatable :: OffSets(:), byteCounts(:)
   integer              :: n_samples=1 ! (samplesPerStrip or samplesPerTile)
   !if strips:
   integer              :: rowsPerStrip,stripsPerImage!,samplesPerStrip
   !if tiles:
   integer              :: tileWidth,tileLength,tilesAcross,tilesDown,tilesPerImage!,samplesPerTile
   integer              :: e_i,e_j,e_ii,e_jj !test
   !
   integer :: i,j,k,b!,e!,recNum
   integer(kind=1), allocatable :: values_1(:)  !array of bytes of hole strip/tile
   !temporal vars:
   real   (kind=8) :: tmpFlt_8 ! double precission floating point (8-bytes)
   real   (kind=4) :: tmpFlt_4 ! single precission floating point (4-bytes)
   integer(kind=4) :: tmpInt_4 ! 
   integer(kind=2) :: tmpInt_2 !
   integer(kind=1) :: tmpInt_1 !

   !get Image parameters:
   call TIFF_GET_TAG_VALUE(tiff, img_num, TIFF_ImageWidth   , imageWidth   )
   call TIFF_GET_TAG_VALUE(tiff, img_num, TIFF_ImageLength  , imageLength  )
   call TIFF_GET_TAG_VALUE(tiff, img_num, TIFF_BitsPerSample, bitsPerSample)
   
   if ( tiff%samplesPerPixel /= 1 ) stop "Multi-band images not supported yet."
   if ( tiff%orientation     /= 1 ) stop "Only orientation=1 supported yet.   "

   bytesPerSample=bitsPerSample/8

   !Get strip/tile parameters:
   SELECT CASE(tiff%ImgType)
    CASE ("strip")
      call TIFF_GET_TAG_VALUE(tiff, img_num, TIFF_RowsPerStrip, rowsPerStrip )
      stripsPerImage=floor(real((imageLength+rowsPerStrip-1)/rowsPerStrip))

      allocate(Offsets   (stripsPerImage))
      allocate(byteCounts(stripsPerImage))
      call TIFF_GET_TAG_VALUE(tiff, img_num, TIFF_StripOffsets   ,Offsets   )
      call TIFF_GET_TAG_VALUE(tiff, img_num, TIFF_StripByteCounts,byteCounts)

      n_samples=rowsPerStrip*imageWidth !samplesPerStrip

    CASE ("tile")
      call TIFF_GET_TAG_VALUE(tiff, img_num, TIFF_TileWidth      ,tileWidth )
      call TIFF_GET_TAG_VALUE(tiff, img_num, TIFF_TileLength     ,tileLength)

      TilesAcross   = (ImageWidth  + TileWidth  - 1) / TileWidth  
      TilesDown     = (ImageLength + TileLength - 1) / TileLength 
      TilesPerImage = TilesAcross * TilesDown

      allocate(Offsets   (tilesPerImage))
      allocate(byteCounts(tilesPerImage))
      call TIFF_GET_TAG_VALUE(tiff, img_num, TIFF_TileOffsets    ,Offsets    )
      call TIFF_GET_TAG_VALUE(tiff, img_num, TIFF_TileByteCounts ,byteCounts )
      n_samples=tileWidth*tileLength  !samplesperTile

     CASE DEFAULT
      stop "TIFF type not a strip nor a tile!"
   END SELECT

   allocate(values_1(n_samples*bytesPerSample))
   do i=1,size(Offsets)
      !read hole strip or tile (because each strip/tile is compressed separately)
      do b=1,byteCounts(i)
         read(tiff%iUnit, rec=Offsets(i)+b) values_1(b)
      end do
      
      !uncompress secuence of bytes in strip/tile.
      call decode(values_1,tiff%compression,byteCounts(i)) !decode strip/tile if compression is applied

      !transfer data to IMG array:
      do j=1,size(values_1)/bytesPerSample
         k=(j-1)*bytesPerSample+1

         !indexing (strip vs tile)
         if ( trim(tiff%imgType) == 'strip' ) then
            e_i=mod((j-1),imageWidth) + 1                 !revisar!
            e_j=(i-1)*rowsPerStrip + (j-1)/imageWidth + 1 !revisar!

         else if ( trim(tiff%imgType) == 'tile'  ) then 
            e_ii= mod(j-1 ,tileWidth)+1                   !revisar!
            e_jj=    (j-1)/tileWidth +1                   !revisar!
            e_i = mod(i-1 ,tilesAcross)*tileWidth  + e_ii !revisar!
            e_j =    (i-1)/tilesAcross *tileLength + e_jj !revisar!
            !e_j = mod((i-1)/tilesAcross, tilesDown)*tileLength + e_jj !revisar!
         endif
         !do not copy paddings:
         if ( e_i > imageWidth .or. e_j > imageLength) then
            cycle
         endif
         
         !change indices values based on orientation    
         call change_index_orientation(tiff%orientation,e_i,e_j,imageWidth,imageLength)

         select case(bytesPerSample)
          case(1)
          !1-byte data
             if (tiff%swapByte) then
                 tmpInt_1=transfer( values_1(k+2:k:-1), tmpInt_1 )
             else                                                                    
                 tmpInt_1=transfer( values_1(k:k+2:1), tmpInt_1 )
             end if
             ! Check for unsigned integer SampleFormat; adjust for negative values if needed
             if ( tiff%SampleFormat == 1 .and. tmpInt_1 < 0 ) tmpint_1 = tmpint_1 + intAdj1
                                                                                             
             IMG(e_i,e_j) = real(tmpInt_1)

          case(2)
          !2-byte data (commonly integers)
             if (tiff%swapByte) then
                 tmpInt_2=transfer( values_1(k+2:k:-1), tmpInt_2 )
             else                                                                    
                 tmpInt_2=transfer( values_1(k:k+2:1), tmpInt_2 )
             end if
             ! Check for unsigned integer SampleFormat; adjust for negative values if needed
             if (tiff%SampleFormat == 1 .and. tmpInt_2 < 0 ) tmpint_2 = tmpint_2 + intAdj2

             IMG(e_i,e_j)=real(tmpInt_2)

          case(4)
          !4-byte data (it could be integer or float)
             if (tiff%SampleFormat == 3 ) then
             ! transfer to 4-byte floating point (real)
                if (tiff%swapByte) then
                    tmpFlt_4=transfer( values_1(k+4:k:-1), tmpFlt_4 )
                else                                                                    
                    tmpFlt_4=transfer( values_1(k:k+4:1) , tmpFlt_4 )
                end if
                IMG(e_i,e_j)=real(tmpFlt_4)

             else
             ! transfer to signed 4-byte integer
                if (tiff%swapByte) then
                    tmpInt_4=transfer( values_1(k+4:k:-1), tmpInt_4 )
                else                                                                    
                    tmpInt_4=transfer( values_1(k:k+4:1) , tmpInt_4 )
                end if

                ! Check for unsigned integer SampleFormat; adjust for negative values if needed
                if (tiff%SampleFormat == 1 .and. tmpInt_4 < 0 ) tmpint_4 = tmpint_4 + intAdj4

                IMG(e_i,e_j) =real(tmpInt_4)

             end if
          case(8)
          !8-byte data (normally floating point double precission)
             if (tiff%swapByte) then
                 tmpFlt_8=transfer( values_1(k+8:k:-1), tmpFlt_8 )
             else                                                                    
                 tmpFlt_8=transfer( values_1(k:k+8:1) , tmpFlt_8 )
             end if
             IMG(e_i,e_j)=real(tmpFlt_8)
         end select
      enddo

   end do!offsets

end subroutine TIFF_GET_IMAGE
!=== END TIFF_GET_IMAGE =============

!=== TIFF_GET_IMAGE_COORDINATES======
subroutine GTIFF_get_Image_Coordinates(tiff,x,y)
   implicit none
   type(tiff_FILE), intent(in)  :: tiff
   real, intent(inout)          :: x(:,:),y(:,:)
   real             :: M(4,4)=0  !transformation Matrix
   integer          :: i0,j0     
   real             :: x0,y0,dx,dy
   integer :: i,j,ii,jj

   if ( hasTag(tiff,1, GTIFF_ModelTransformationTag) ) then
      M=sngl(reshape(tiff%trans,[4,4]))
      do i=1,tiff%nx
        do j=1,tiff%ny
           ii=i;jj=j
           call change_index_orientation(tiff%orientation,ii,jj,tiff%nx,tiff%ny)
           x(ii,jj)=dot_product(M(:,1),[i,j,0,0])
           y(ii,jj)=dot_product(M(:,2),[i,j,0,0])
        enddo
      enddo
   else
      i0= int(tiff%tiePt(1)); j0= int(tiff%tiePt(2))
      x0=real(tiff%tiePt(4)); y0=real(tiff%tiePt(5))
      dx=real(tiff%scale(1)); dy=real(tiff%scale(2))
      do i=1,tiff%nx
        do j=1,tiff%ny
           ii=i;jj=j
           call change_index_orientation(tiff%orientation,ii,jj,tiff%nx,tiff%ny)
           x(ii,jj)=x0 + dx*((i-1) - i0)
           y(ii,jj)=y0 - dy*((j-1) - j0)
        enddo
      enddo
      !Debug:
      print '("Dimensions X:",I3," Y:",I3)',tiff%nx,tiff%ny
      print '("Origin      :",F12.3,",",F12.3)',x0,y0
      print '("Pixel size  :",F6.2,",",F6.2)',dx,dy
      print '("Extent      :",F16.3,",",F16.3,",",F16.3,",",F16.3)',minval(x),minval(y),maxval(x),maxval(y)
   endif
end subroutine
!=== END TIFF_GET_IMAGE_COORDINATES ==
!=============================================================== 

subroutine change_index_orientation(orientation,e_i,e_j,wid,len)
   implicit none
   integer, intent(inout) :: e_i,e_j
   integer, intent(in)    :: wid,len,orientation
   integer :: tmp
   !Global 1-D index: 
   select case (orientation)
     case (1)          ![0,0] top-left     !invert rows (j)
         e_j = (len-e_j) + 1              
     case (2)          ![0,0] top-right    !invert rows and cols
         e_i = (wid-e_i) + 1
         e_j = (len-e_j) + 1             
     case (3)          ![0,0] bottom-right !invert cols (i)
         e_i = (wid-e_i) + 1            
     case (4)          ![0,0] bottom-left  !OK
         continue                      
     case (5)          ![0,0] left-top     !transpose & invert rows
         tmp=e_i
         e_i=(len-e_j) + 1
         e_j=tmp
     case (6)          ![0,0] right-top    !transpose & invert rows and cols
         tmp=e_i
         e_i=(len-e_j) + 1
         e_j=(wid-tmp)
     case (7)          ![0,0] right-bottom !transpose & invert cols
         tmp=e_i
         e_i=e_j
         e_j=(wid-tmp) + 1
     case (8)          ![0,0] left-bottom  !transpose
         tmp=e_i
         e_i=e_j
         e_j=tmp
     case default
         stop "Orientation not a valid value"
   end select
end subroutine

!===============================================================
!COMPRESSION  (DECODE FUNCTIONS)
subroutine decode( values_1, method, lastindex )
   implicit none
   integer(1), allocatable, intent(inout) :: values_1(:)
   integer                , intent(in)    :: method, lastIndex
   integer(1), allocatable         :: tmp(:)
   integer(1)                      :: num
   integer :: i,j,rc!,k,n


   SELECT CASE( method ) 
    CASE ( 1 )                                       !uncompressed
      continue !do nothing

    CASE ( 32773 )                                   !PackBit (run-length method)
      allocate(tmp(size(values_1)))
      tmp=values_1                                   !save compressed original copy
      i=1  !index on tmp       (original copy)
      j=1  !index on values_1  ( result)
      do while ( i < lastIndex )
         num = tmp(i)                                !pick new element of tmp
         i=i+1 !next element
         if ( num >= -127 .and. num < 0 ) then       !next -n+1 values are the same as values_1(k+1)
            values_1(j:j-num+1)=tmp(i)
            i=i+1
            j=j-num+1
         else if ( num >= 0 .and. num <= 127 ) then  !the next n+1 values are copied literally
            values_1(j:j+num)=tmp(i:i+num+1)
            i=i+num+1
            j=j+num+1
         else 
             cycle
         end if
      end do

    CASE ( 2 )   !Modif. Hauffman (CCITT Group 3 1D facsimile compression scheme)
     stop "Modif. Hauffman compression scheme for bilevel images not supported!"
    CASE ( 3 )   !CCITT T.4
      stop "CCITT T.4 bi-level encoding compression scheme not supported!"
    CASE ( 4 )   !CCITT T.6
      stop "CCITT T.6 bi-level encoding compression scheme not supported!"
    CASE ( 5 )   !LZW method
      stop "LZW compression scheme not supported (yet)!"
    CASE ( 6 )   !JPEG obsolete
      stop "JPEG obsolete lossy compresion not supported!"
    CASE ( 7 )   !JPEG new  
      stop "JPEG lossy compression not supported!"
    CASE ( 8 )   !DEFLATE (LZ77 + Huffman)
      !stop "DEFLATE compression scheme not supported (yet)!"
      call zlib_DEFLATE(values_1, lastIndex)
      
    CASE DEFAULT
       print*, "Compression scheme=",method; stop 'Compression method not recognized'
   END SELECT
end subroutine


subroutine zlib_deflate(values_1, len_in)

   implicit none
   integer(1), intent(inout) :: values_1(:)
   integer, intent(in)       :: len_in

   !integer(1), allocatable   :: tmp_1(:)
   integer(kind=z_ulong)         :: len_xx, len_ou
   character(len=:), allocatable :: buff_xx,buff_ou
   integer :: i,rc

   len_xx = transfer([len_in],len_ou)
   len_ou = size(values_1, kind=z_ulong)
   allocate (character(len=len_xx):: buff_xx)
   allocate (character(len=len_ou):: buff_ou)
                                                      
   do i=1,len_xx     
     buff_xx(i:i) = transfer([values_1(i)], "a")
   enddo
                                                      
   print*,"zlib: uncompressing...",len_xx,len_ou
   rc = uncompress(buff_ou, len_ou, buff_xx, len_xx)
   if (rc /= Z_OK) stop 'Error: uncompress() failed'

   do i=1,len_ou     
     values_1(i) = transfer([buff_ou(i:i)], 1_1)
   end do

end subroutine

end module  
