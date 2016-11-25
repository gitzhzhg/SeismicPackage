subroutine time(buft)
  character(len=*),intent(inout) :: buft
  integer                        :: date_time(8)
  call date_and_time(VALUES=date_time)
  write(buft,'(I2.2,":",I2.2,":",I2.2)')date_time(5:7)
end subroutine time
