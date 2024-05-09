! BSD 2-Clause License
!
! Copyright (c) 2021-2024, Hewlett Packard Enterprise
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! 1. Redistributions of source code must retain the above copyright notice, this
!    list of conditions and the following disclaimer.
!
! 2. Redistributions in binary form must reproduce the above copyright notice,
!    this list of conditions and the following disclaimer in the documentation
!    and/or other materials provided with the distribution.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
! FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  !** Beginning of code common to all put_tensor subroutines

  ! Local variables
  integer(kind=c_size_t)                      :: c_n_dims ! Number of dimensions
  type(c_ptr) :: data_ptr, c_dims_ptr
  character(kind=c_char, len=len_trim(name)) :: c_name !< Transformed fortran 'name' to a c-string
  integer(kind=c_size_t) :: name_length
  integer(kind=c_size_t), target :: c_dims(size(dims))
  integer(kind=enum_kind) :: data_type
  logical :: convert_to_c_
  integer :: i
  integer, dimension(PRODUCT(dims)) :: c_indices
  integer, dimension(SIZE(dims)) :: array_idx

  convert_to_c_ = .true.
  if(PRESENT(convert_to_c)) convert_to_c_ = convert_to_c

  ! Determine the shape of the array and the length of each dimension
  c_n_dims = size(dims)
  if (convert_to_c_) then
    do i=1,PRODUCT(dims)
      call unravel_index(i, dims, array_idx)
      c_indices(i) = c_index(array_idx, dims)
    enddo
    ! Note do not combine this loop with the above because the out of order access
    ! this will cause cache thrashing
    do i=1,PRODUCT(dims)
      c_data(c_indices(i)) = data(i)
    enddo
    data_ptr = c_loc(c_data)
  else
    data_ptr = c_loc(data)
  endif

  ! Process the name and calculate its length
  c_name = trim(name)
  name_length = len_trim(name)

  c_dims(:) = dims(:)
  c_dims_ptr = c_loc(c_dims)

  !** End of code common to all put_tensor subroutines
