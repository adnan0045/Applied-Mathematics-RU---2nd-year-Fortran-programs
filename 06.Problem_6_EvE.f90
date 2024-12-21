program expression
implicit none

  real::a,b,c,d,r,u,v,x,rt,xt
  real,parameter::pi=3.1416
  integer::si

  do
  print*,"Enter the serial number(enter 0 to exit):"
  read*,si
  if(si==0)exit

    print*,"Enter the value of a,b,c,d,r,x :"
    read*,a,b,c,d,r,x

	rt=((r-d)*pi)/180.0	  !converted into radian
	xt=(x*pi)/180.0		  !converted into radian


	if((sin(rt)==0).or.(b==0))then
	  print*,"The value of u and v are undefined"

	else
	  u = abs(a+b**(-1))/(sin(rt))
	  print*,"The value of u is= ",u

	end if

	if(u==0)then
	  print*,"The value of v is undefined"

	else
	  v = (c*u**(-1)-u*cos(xt))/b
	  print*,"The value of v= ",v

	end if
  
  end do

end program expression