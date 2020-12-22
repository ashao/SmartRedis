!> Get the (opaque) tensor from a dataset
subroutine get_dataset_tensor(this, name, data, data_type, dims, n_dims)
  class(dataset_type),                      intent(in   ) :: this      !< Fortran SILC client
  character(len=*),                         intent(in   ) :: name       !< The unique name used to query the database
  type(c_ptr),                              intent(  out) :: data      !< A pointer to the opaque tensor object
  integer,                                  intent(  out) :: data_type !< The type of data in the tensor object
  integer, dimension(:), allocatable,       intent(  out) :: dims      !< The length along each dimension
  integer,                                  intent(  out) :: n_dims    !< Number of dimensions

  character(kind=c_char) :: c_name(len_trim(name)+1)
  integer :: i
  integer(kind=enum_kind) :: c_data_type, c_mem_layout
  integer(kind=enum_kind), pointer :: enum_ptr
  integer(kind=c_size_t) :: c_n_dims, name_length
  integer(kind=c_size_t), dimension(:), pointer :: dims_f_ptr
  type(c_ptr) :: data_type_c_ptr, dims_c_ptr

  ! Process the name and calculate its length
  c_name = trim(name)
  name_length = len_trim(name)
  call get_tensor_c(this%dataset, c_name, name_length, data, dims_c_ptr, c_n_dims, data_type_c_ptr, c_mem_layout)

  ! Cast the number of dimensions to a standard Fortran integer
  n_dims = c_n_dims

  ! Fill the dims array
  if (allocated(dims)) deallocate(dims)
  allocate(dims(c_n_dims))
  call c_f_pointer( dims_c_ptr, dims_f_ptr, [c_n_dims] )
  dims(:) = dims_f_ptr(:)
  deallocate(dims_f_ptr)

  ! Cast the data type into a Fortran integer
  call c_f_pointer( data_type_c_ptr, enum_ptr )
  data_type = enum_ptr

end subroutine get_dataset_tensor