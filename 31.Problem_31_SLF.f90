program stl_func
implicit none

  integer::i,n
  real,allocatable,dimension(:)::x,y
  real::sx,sy,sxy,ssq,slope,int,m,c

  print*,"Enter the value of n:"
  read*,n

  allocate(x(n),y(n))
  
  print*,"Enter the n pairs of x and y values:"
  read*,(x(i),y(i),i=1,n)

  sx=0.0
  sy=0.0
  sxy=0.0
  ssq=0.0

  do i=1,n
    sx=sx+x(i)
	sy=sy+y(i)
	sxy=sxy+x(i)*y(i)
	ssq=ssq+x(i)*x(i)
  end do

slope=m(sx,sy,sxy,ssq,n)
int=c(sx,sy,n,slope)

print*,"slope=",slope
print*,"Intercept=",int
print*,"Equation of the straight line is:"
print*,"y=",slope,"x+",int

end program stl_func

real function m(sx,sy,sxy,ssq,n)
implicit none
integer,intent(in)::n
real,intent(in)::sx,sy,sxy,ssq
real::t

t=n
m=((t*sxy-sx*sy)/(t*ssq-(sx**2)))
end function

real function c(sx,sy,n,slope)
implicit none
integer,intent(in)::n
real,intent(in)::sx,sy,slope
real::t
t=n
c=(sy-slope*sx)/t
end function 
    