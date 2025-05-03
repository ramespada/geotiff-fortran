# geotiff-fortran

[![Language](https://img.shields.io/badge/-Fortran-734f96?logo=fortran&logoColor=white)](https://github.com/topics/fortran)
[![GitHub release](https://img.shields.io/github/release/ramespada/geotiff-fortran.svg)](https://github.com/ramespada/geotiff-fortran/releases/latest)
[![last-commit](https://img.shields.io/github/last-commit/ramespada/geotiff-fortran)](https://github.com/ramespada/geotiff-fortran/commits/main)

> Minimalistic Fortran GeoTIFF module, with sintax inspired on LIBTIFF C-Library.

## List of functions available

Open/close file:
- [x] `TIFF_Open  (iunit, fileName, 'r', tiff, ierror)` 
- [x] `TIFF_Close (tiff)`

Read commands:
- [x] `TIFF_Get_Tag_Value (tiff, tagId, value)`
- [x] `TIFF_Get_Image     (tiff, *nimg, image)`
- [x] `GTIFF_Get_Key_Value(tiff, keyId, value)`
- [x] `GTIFF_Get_Image_Coordinates(tiff, *nimg*, x, y)`

Write commands:
- [ ] `TIFF_Set_Tag_Value (tiff, tagId, value)`
- [ ] `TIFF_Set_Image     (tiff, *nimg, image)`
- [ ] `GTIFF_Set_Key_Value(tiff, keyId, value)`
- [ ] `TIFF_Write_TIFF(tiff)`

## Example of use

```fortran
program my_program

   use geoTIFF

   implicit none  
   type(TIFF_FILE) :: my_tiff
   integer              :: ierr,crs
   character(100)       :: descr
   real   , allocatable :: image(:,:),x(:,:),y(:,:)
   
   call TIFF_Open(124,"files/cea.tif",'r', my_tiff, ierr)
   if (ierr==0) then
   
      !get TIFF tag:
      call TIFF_GET_TAG_VALUE(my_tiff, 1, TIFF_ImageDescription, descr)
   
      !get TIFF image:
      allocate(image(my_tiff%nx, my_tiff%ny))
      call TIFF_GET_IMAGE(my_tiff, 1, image)

      !get GeoTIFF key:
      call GTIFF_GET_KEY_VALUE(my_tiff, GKEY_ProjectedCSType, crs)

      !get coordinates
      allocate(x(my_tiff%nx, my_tiff%ny))
      allocate(y(my_tiff%nx, my_tiff%ny))
      call GTIFF_GET_IMAGE_COORDINATES(my_tiff,1,x,y)

      call TIFF_Close(my_tiff)
   else
      stop 'Failed to read TIFF file'
   endif

end program
```

## To-do list:

General:
- [x] Read/Open    function implementation
- [ ] Write/Create function implementation
- [ ] IO-error management

TIFF 6.0 Baseline:
 + [x] Big-Enddian support (swap byte-order)
 + [x] Orientation: support for different orientation images (not all tested)
 + [x] Compression: Support for PackBits
 + [ ] Compression: Support for Modified Huffman (CCITT Group 3, 1-D) (only for bilevel data)
 + [x] Multi-image TIFF (more than 1 IFD)
 + [ ] Multi-band TIFFs
 + [ ] Planar configuration TIFFs
 + [ ] Extend `TIFF_Get_Image` for categorical (integer) layers

TIFF 6.0 Extensions:
 + [x] Tiled images
 + [ ] Compression: LZW
 + [x] Compression: DEFLATE (LZ77 + Huffman)

[OGC GEOTIFF 1.1](https://docs.ogc.org/is/19-008r4/19-008r4.html)
 + [x] GeoKey values access
 + [x] Find how to get coordinates info

Performance:
- [ ] Memory (RSS)
- [ ] CPU usage testing
- [ ] Parallel access? (is it possible?)

