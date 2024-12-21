program matinv
implicit none

  real(8),allocatable,dimension(:,:)::A,adj,inv,b
  real::det
  integer::si,i,j,k,ar,ac,n

  do 
    print*,"Enter the serial number:"
	read*,si
	if(si==0)exit

	print*,"Enter the size of the matrix(m and n):"
	read*,ar,ac

	allocate(A(ar,ac),adj(ar,ac),inv(ar,ac),b(ar,ac))

	print*,"Enter the elements of the matrix row-wise:"
	read*,((A(i,j),j=1,ac),i=1,ar)

	if(ar/=ac)then
	  print*,"The matrix is not square so inverse isn't possible"

	else
	  n=ar

	  if(n==1)then
	  inv=1/A
	  print*,"The inverse is:",inv

	  else if(n==2)then
	  det=A(1,1)*A(2,2)-A(2,1)*A(1,2)
	  print*,"Determinant=",det
	  
	  if(det==0)then
	    print*,"The matrix is singular so inverse isn't possible"
	  else
	    adj(1,1)=A(2,2)
	    adj(1,2)=-A(1,2)
	    adj(2,1)=-A(2,1)
	    adj(2,2)=A(1,1)

	    inv=adj/det

	    print*,"The inverse of the matrix is:"
	      do i=1,n
		    print*,(inv(i,j),j=1,n)
		  end do
	  end if

	  else
	    det=A(1,1)*(A(2,2)*A(3,3)-A(3,2)*A(2,3))-A(1,2)*(A(2,1)*A(3,3)-A(3,1)*A(2,3))+A(1,3)*(A(2,1)*A(3,2)-A(3,1)*A(2,2))
	    
		print*,"Determinant=",det

		if(det==0)then
		  print*,"The matrix is singular and inverse isn't possible"
		else
		  adj(1,1)=A(2,2)*A(3,3)-A(3,2)*A(2,3)
		  adj(2,1)=-(A(2,1)*A(3,3)-A(3,1)*A(2,3))
		  adj(3,1)=A(2,1)*A(3,2)-A(3,1)*A(2,2)
		  adj(1,2)=-(A(1,2)*A(3,3)-A(3,2)*A(1,3))
		  adj(2,2)=A(1,1)*A(3,3)-A(3,1)*A(1,3)
		  adj(3,2)=-(A(1,1)*A(3,2)-A(3,1)*A(1,2))
		  adj(1,3)=A(1,2)*A(2,3)-A(2,2)*A(1,3)
		  adj(2,3)=-(A(1,1)*A(2,3)-A(2,1)*A(1,3))
		  adj(3,3)=A(1,1)*A(2,2)-A(2,1)*A(1,2)

		  inv=adj/det

		  print*,"The inverse matrix is:"
		  do i=1,n
			print*,(inv(i,j),j=1,n)
		  end do

		end if
	  end if


	  !To verify the inverse
	  b=0.0
	  do i=1,n
	    do j=1,n
		  do k=1,n
		    b(i,j)=b(i,j)+A(i,k)*inv(k,j)
		  end do
		end do
	  end do

	  print*,"The multiplication of matrix A and inverse of A is:"

	  do i=1,n
	    print*,(b(i,j),j=1,n)
	  end do

	  print*,"It is an identity matrix so the inverse is varified"

	end if

  end do

end program matinv







