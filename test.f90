program test_tiff

  use GeoTIFF
  use netcdf  !only for debugging purposes

  implicit none

  integer            :: ierr
  type(TIFF_file)    :: my_tiff

  integer              :: wid,len,sampleFMT
  !Test geotiff variables:
  integer :: crs

  !double precision,allocatable :: image(:,:)
  real,allocatable              :: image(:,:)
  !integer,allocatable          :: image(:,:)

  ! geotiff coordinates
  real, allocatable  :: x(:,:), y(:,:) 
  !Netcdf
  integer            :: ncid,x_dim_id,y_dim_id,var_id
  character(len=256) :: file_path
  !integer :: i,j,k
 
  ! Check if the command line argument is provided
  if (command_argument_count() /= 1) then
      print*, "Usage: ./program <file_path>";stop
  else
      call get_command_argument(1, file_path)
  endif

  !----------------------------------------
  !(1 PART) Read and extract data from TIFF:
   call TIFF_Open(124,trim(file_path) ,'r', my_tiff, ierr)      !TIFF little-endian
   if (ierr==0) then


      call TIFF_GET_TAG_VALUE(my_tiff, 1, TIFF_SampleFormat , SampleFMT)
      print*,"SampleFormat:",SampleFmt
      ![OK] TIFF_GET_TAG_VALUE(tiff, img_id, tag_id, value)
      call TIFF_GET_TAG_VALUE(my_tiff, 1, TIFF_ImageWidth , wid)
      call TIFF_GET_TAG_VALUE(my_tiff, 1, TIFF_ImageLength, len)

      ![~] TIFF_GET_IMGAGE(tiff, img_id, img)
      allocate(image(wid,len))
      call TIFF_GET_IMAGE(my_tiff, 1, image)

      !GeoTIFF procedures:
      ![OK] GTIFF_get_Key_Value(tiff, key_id, value)
      call GTIFF_GET_KEY_VALUE(my_tiff, GKEY_ProjectedCSType, crs) 
      print*, "CRS => EPSG:",crs
      
      ![OK] GTIFF_get_Image_Coordinates(tiff,i,x,y )
      allocate( x(wid,len))
      allocate( y(wid,len))
      call GTIFF_get_Image_Coordinates(my_tiff,x,y)

   call TIFF_Close(my_tiff)

   !DEBUG ======================
   print*,"Create NetCDF:"
   !Crear NetCDF
   call check(nf90_create('image.nc', NF90_CLOBBER, ncid ))
      !Defino dimensiones
      call check(nf90_def_dim(ncid, "x", wid, x_dim_id ))
      call check(nf90_def_dim(ncid, "y", len, y_dim_id ))
      !Creo variables:
      call check( nf90_def_var(ncid, 'image', NF90_FLOAT, [x_dim_id,y_dim_id], var_id ))
      call check( nf90_def_var(ncid, 'lon'  , NF90_FLOAT, [x_dim_id,y_dim_id], var_id ))
      call check( nf90_def_var(ncid, 'lat'  , NF90_FLOAT, [x_dim_id,y_dim_id], var_id ))
   call check(nf90_enddef(ncid))   !End NetCDF define mode

   !Abro NetCDF y guardo variables de salida
   call check(nf90_open('image.nc', nf90_write, ncid ))
      call check(nf90_inq_varid(ncid, 'image'    ,var_id));call check(nf90_put_var(ncid, var_id,reshape(image,[wid,len]) )) 
      call check(nf90_inq_varid(ncid, 'lon',var_id));call check(nf90_put_var(ncid, var_id,x )) 
      call check(nf90_inq_varid(ncid, 'lat' ,var_id));call check(nf90_put_var(ncid, var_id,y )) 
   call check(nf90_close(ncid))
   !DEBUG =======================

   else
      stop 'Failed to read TIFF file'
   endif

   !----------------------------------------
   !(2nd PART) Create/Write a TIFF:
   !la idea seria creat un TIFF_FILE, ir poniendo todo ahi y una vez completo escribirlo a un archivo.

   !call TIFF_Create_TIFF_FILE(tif)
   !call TIFF_Write_TIFF_FILE ('file_name.tif',tiff)


contains
subroutine check(status)            !netcdf error-check function
  integer, intent(in) :: status
  if (status /= nf90_noerr) then
    write(*,*) nf90_strerror(status); stop 'netcdf error'
  end if
end subroutine check

end program
