submodule (h5mpi) hdf5_read

use hdf5, only : h5tget_native_type_f, h5tget_class_f, h5dget_type_f, h5tget_size_f, h5tclose_f, h5topen_f, &
h5pget_nfilters_f, h5pget_filter_f, h5pget_layout_f, h5pget_chunk_f, &
H5T_DIR_ASCEND_F, &
H5Z_FILTER_DEFLATE_F

implicit none (type, external)

contains


module procedure h5open_read

integer :: ierr
integer(HID_T) :: plist_id

filespace = H5S_ALL_F
memspace = H5S_ALL_F
plist_id = H5P_DEFAULT_F

call hdf_shape_check(self, dname, dims, dset_dims)

call h5dopen_f(self%file_id, dname, dset_id, ierr)
if(ierr /= 0) error stop 'ERROR:h5open_read: open ' // dname // ' from ' // self%filename

end procedure h5open_read


module procedure get_class

call get_dset_class(self, dname, get_class)

end procedure get_class


module procedure get_deflate
!! h5pget_filter_f doesn't work collectively, will crash on h5fclose_f
!! if(mpi_id==0) with mpi_bcast does not work, same crash.
!! better to use H5Pall_filters_avail_f when mpi=.true.

integer :: i, j, ierr
integer :: flags !< bit pattern
integer(HID_T) :: dcpl, dset_id
integer(SIZE_T) :: Naux
integer :: Aux(8)  !< arbitrary length
integer :: Nf, filter_id
character(32) :: filter_name

logical :: debug = .false.


get_deflate = .false.

Naux = size(Aux, kind=SIZE_T)

call h5dopen_f(self%file_id, dname, dset_id, ierr)
if (ierr/=0) error stop "ERROR:h5fortran:get_deflate:h5dopen: " // dname // " in " // self%filename

call h5dget_create_plist_f(dset_id, dcpl, ierr)
if (ierr/=0) error stop "ERROR:h5fortran:get_deflate:h5dget_create_plist: " // dname // " in " // self%filename

call h5dclose_f(dset_id, ierr)
if (ierr/=0) error stop "ERROR:h5fortran:get_deflate:h5dclose: " // dname // " in " // self%filename

call h5pget_nfilters_f(dcpl, Nf, ierr)
if (ierr/=0) error stop "ERROR:h5fortran:get_deflate:h5pget_nfilters: " // dname // " in " // self%filename

filters: do i = 1, Nf
  filter_name = ""

  call h5pget_filter_f(dcpl, i, &
  flags, &
  Naux, Aux, &
  len(filter_name, SIZE_T), filter_name, &
  filter_id, ierr)
  if(ierr/=0) error stop "ERROR:h5fortran:get_deflate:h5pget_filter: " // dname // " in " // self%filename
  if(filter_id < 0) write(stderr,'(a,i0)') "ERROR:h5fortran:get_deflate:h5pget_filter: index error " // dname, i

  if (debug) then
    j = index(filter_name, c_null_char)
    if(j>0) print *, "TRACE:get_filter: filter name: ", filter_name(:j-1)
  endif

  get_deflate = filter_id == H5Z_FILTER_DEFLATE_F
  if(get_deflate) exit filters

end do filters

call h5pclose_f(dcpl, ierr)

end procedure get_deflate


subroutine get_dset_class(self, dname, class, ds_id, size_bytes)
!! get the dataset class (integer, float, string, ...)
!! {H5T_INTEGER_F, H5T_FLOAT_F, H5T_STRING_F}
class(hdf5_file), intent(in) :: self
character(*), intent(in) :: dname
integer, intent(out) :: class
integer(hid_t), intent(in), optional :: ds_id
integer(size_t), intent(out), optional :: size_bytes

integer :: ierr
integer(hid_t) :: dtype_id, native_dtype_id, dset_id

if(present(ds_id)) then
  dset_id = ds_id
else
  call h5dopen_f(self%file_id, dname, dset_id, ierr)
  if(ierr/=0) error stop 'ERROR:h5fortran:get_class: ' // dname // ' from ' // self%filename
endif

call h5dget_type_f(dset_id, dtype_id, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_class: dtype_id ' // dname // ' from ' // self%filename

if(.not.present(ds_id)) then
  call h5dclose_f(dset_id, ierr)
  if(ierr/=0) error stop 'ERROR:h5fortran:get_class: close dataset ' // dname // ' from ' // self%filename
endif

call h5tget_native_type_f(dtype_id, H5T_DIR_ASCEND_F, native_dtype_id, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_class: native_dtype_id ' // dname // ' from ' // self%filename

call h5tclose_f(dtype_id, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_class: closing dtype ' // dname // ' from ' // self%filename


!> compose datatype inferred
call h5tget_class_f(native_dtype_id, class, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_class: class ' // dname // ' from ' // self%filename

if(present(size_bytes)) then
  call h5tget_size_f(native_dtype_id, size_bytes, ierr)
  if(ierr/=0) error stop 'ERROR:h5fortran:get_class: byte size ' // dname // ' from ' // self%filename
endif

call h5tclose_f(native_dtype_id, ierr)
if(ierr/=0) error stop 'ERROR:h5fortran:get_class: closing native dtype ' // dname // ' from ' // self%filename

end subroutine get_dset_class


module procedure get_native_dtype
!! get the dataset variable type:
!! {H5T_NATIVE_REAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_INTEGER, H5T_NATIVE_CHARACTER, H5T_STD_I64LE}

integer :: class
! integer :: order, machine_order
integer(size_t) :: size_bytes

call get_dset_class(self, dname, class, ds_id, size_bytes)

!> endianness and within type casting is handled by HDF5
! call h5tget_order_f(native_dtype_id, order, ierr)
! if(ierr/=0) error stop 'ERROR:h5fortran:reader: get endianness ' // dname // ' from ' // self%filename
! !> check dataset endianness matches machine (in future, could swap endianness if needed)
! call h5tget_order_f(H5T_NATIVE_INTEGER, machine_order, ierr)
! if(order /= machine_order) error stop 'ERROR:h5fortran:read: endianness /= machine native: ' &
! // dname // ' from ' // self%filename

if(class == H5T_INTEGER_F) then
  if(size_bytes == 4) then
    get_native_dtype = H5T_NATIVE_INTEGER
  elseif(size_bytes == 8) then
    get_native_dtype = H5T_STD_I64LE
  else
    error stop "ERROR:h5fortran:get_native_dtype: expected 32-bit or 64-bit integer:" // dname // ' from ' // self%filename
  endif
elseif(class == H5T_FLOAT_F) then
  if(size_bytes == 4) then
    get_native_dtype = H5T_NATIVE_REAL
  elseif(size_bytes == 8) then
    get_native_dtype = H5T_NATIVE_DOUBLE
  else
    error stop "ERROR:h5fortran:get_native_dtype: expected 32-bit or 64-bit real:" // dname // ' from ' // self%filename
  endif
elseif(class == H5T_STRING_F) then
  get_native_dtype = H5T_NATIVE_CHARACTER
else
  error stop "ERROR:h5fortran:get_native_dtype: non-handled datatype: " // dname // " from " // self%filename
endif

end procedure get_native_dtype


module procedure hdf_get_ndim
!! get rank or "ndims"
integer :: ier

if(.not. self%is_open()) error stop 'ERROR:h5fortran:read: file handle is not open'

drank = -1

if (self%exist(dname)) then
  call h5ltget_dataset_ndims_f(self%file_id, dname, drank, ier)
else
  write(stderr, '(a)') 'ERROR:h5fortran:get_ndim: ' // dname // ' does not exist in ' // self%filename
endif

end procedure hdf_get_ndim


module procedure hdf_get_shape

!! must get rank before info, as "dims" must be allocated first.
integer(SIZE_T) :: type_size
integer :: type_class, drank, ier

if(.not. self%exist(dname)) error stop 'ERROR:h5fortran:get_shape: ' // dname // ' does not exist in ' // self%filename

call h5ltget_dataset_ndims_f(self%file_id, dname, drank, ier)
if (ier /= 0) error stop "ERROR:h5fortran:get_shape: could not get rank of " // dname // " in " // self%filename

allocate(dims(drank))
call h5ltget_dataset_info_f(self%file_id, dname, dims=dims, &
  type_class=type_class, type_size=type_size, errcode=ier)
if (ier /= 0) error stop "ERROR:h5fortran:get_shape: could not get shape of " // dname // " in " // self%filename

end procedure hdf_get_shape


module procedure hdf_get_chunk

integer :: ierr, drank
integer(HID_T) :: dapl, dset_id
integer(HSIZE_T) :: cs(size(chunk_size))

cs = -1

if (.not.self%is_open()) error stop 'ERROR:h5fortran:read: file handle is not open'
if (.not.self%exist(dname)) error stop 'ERROR:h5fortran:get_chunk: ' // dname // ' does not exist in ' // self%filename

if(self%is_chunked(dname)) then
  call h5ltget_dataset_ndims_f(self%file_id, dname, drank, ierr)
  if (ierr /= 0) error stop 'ERROR:h5fortran:get_chunk: get rank ' // dname // ' ' // self%filename

  call h5dopen_f(self%file_id, dname, dset_id, ierr)
  if (ierr /= 0) error stop 'ERROR:h5fortran:get_chunk: open dataset ' // dname // ' ' // self%filename

  call h5dget_create_plist_f(dset_id, dapl, ierr)
  if (ierr /= 0) error stop 'ERROR:h5fortran:get_chunk: get property list ID ' // dname // ' ' // self%filename

  call h5dclose_f(dset_id, ierr)
  if (ierr /= 0) error stop 'ERROR:h5fortran:get_chunk: close dataset: ' // dname // ' ' // self%filename

  call h5pget_chunk_f(dapl, drank, cs, ierr)
  if (ierr /= drank) error stop 'ERROR:h5fortran:get_chunk:h5pget_chunk ' // dname // ' ' // self%filename
  !! yes ierr == drank is success for this call

  call h5pclose_f(dapl, ierr)
  if (ierr /= 0) error stop 'ERROR:h5fortran:get_chunk: close property list ' // dname // ' ' // self%filename
endif

select type (chunk_size)
type is (integer(HSIZE_T))
  chunk_size = cs
type is (integer(int32))
  chunk_size = int(cs)
class default
  error stop 'ERROR:h5fortran:get_chunk: unknown type for chunk_size'
end select

end procedure hdf_get_chunk


module procedure hdf_get_layout

integer(HID_T) :: dapl, dset_id
integer :: ierr

if(.not. self%is_open()) error stop 'ERROR:h5fortran:read: file handle is not open'

layout = -1

if (.not. self%exist(dname)) error stop 'ERROR:h5fortran:get_layout: ' // dname // ' does not exist in ' // self%filename

call h5dopen_f(self%file_id, dname, dset_id, ierr)
if (ierr /= 0) error stop 'ERROR:h5fortran:get_layout: open dataset ' // dname // ' ' // self%filename

call h5dget_create_plist_f(dset_id, dapl, ierr)
if (ierr /= 0) error stop 'ERROR:h5fortran:get_layout: get property list ID ' // dname // ' ' // self%filename

call h5dclose_f(dset_id, ierr)
if (ierr /= 0) error stop 'ERROR:h5fortran:get_layout: close dataset: ' // dname //' ' // self%filename

call h5pget_layout_f(dapl, layout, ierr)
if (ierr /= 0) error stop 'ERROR:h5fortran:get_layout read ' // dname //' ' // self%filename

call h5pclose_f(dapl, ierr)
if (ierr /= 0) error stop 'ERROR:h5fortran:get_chunk: close property list ' // dname // ' ' // self%filename

end procedure hdf_get_layout


module procedure hdf_check_exist

integer :: ierr

if(.not. self%is_open()) error stop 'ERROR:h5fortran:exist: file handle is not open: ' // self%filename

call h5ltpath_valid_f(self%file_id, dname, .true., hdf_check_exist, ierr)
!! h5lexists_f can false error with groups--just use h5ltpath_valid

if (ierr/=0) error stop 'ERROR:h5fortran:check_exist: could not determine status of ' // dname // ' in ' // self%filename

end procedure hdf_check_exist

end submodule hdf5_read