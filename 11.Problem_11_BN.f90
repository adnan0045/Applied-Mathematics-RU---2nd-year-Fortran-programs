program biggest_number
implicit none

  real::u,v,w
  integer::si

  do
    print*,"Enter the serial number:"
	read*,si
	if(si==0)exit

	print*,"Enter the value of u,v and w:"
	read*,u,v,w

	if(u>v.and.u>w)then
	  print*,u,"is the biggest number"

	else if(v>u.and.v>w)then
	  print*,v,"is the biggest number"

	else
	  print*,w,"is the biggest number"

	end if

  end do

end program biggest_number
