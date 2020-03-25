      parameter(mc=5000000,mn=1000000,ms=1000000,m1=301,mb=999)
  
!**********************************************************             
! mb  nr. of blocks                                                     
! mc  nr. of cells (including boundary-cells of each block)             
! ms  nr. of boundary-cells  (including edges and vertices)             
! mn  nr. of neighbours of all boundary-cells on interfaces             
! m1  maximum nr. of cells in one direction +1                          
!**********************************************************             
                                                                        
      double precision x,y,z,velu,velv,velw,pres,phi,dis,               &
     &                 tnue,tup1,tup2,tau1,tau2,tau3,rnada,anb          
      common/grid/x(mc),y(mc),z(mc),ni(mb),nj(mb),nk(mb),mblocks 
      common/vari/velu(mc),velv(mc),velw(mc),pres(mc),                  &
     &            tnue(mc),tup1(mc),tup2(mc),phi(mc),                   &
     &            tau1(ms),tau2(ms),tau3(ms)                            
      common/face/nbb(mn),nbs(mn),nb1(mn),nb2(mn),nb3(mn),anb(mn),      &
     &            irb(ms),nb(ms)                                        
      dimension ist(0:m1),jst(0:m1),kst(0:m1,0:m1),lst(0:m1),           &
     &          no(mb),nrb(999),irbplot(3)                                         
      character nome1*20,nome2*20,nome3*20,nr*3,fmt*4,block*8 
                                                                        
      pi=acos(-1.d0) 
                     
!      User-Input (nur Bsp)
      write(*,*)'Name of Neptun-file ?' 
      read(*,'(a20)')nome1 
      write(*,*)'Name for Tecplot-file with 3D data ?' 
      read(*,'(a20)')nome2 
      write(*,*)'Name for Tecplot-file with hull data ?' 
      read(*,'(a20)')nome3 

!******* Anfang Lesen Koordinaten der Netzknoten aller Bl"ocke *********
      open(77,file='netz.dat',form='unformatted',status='unknown') 
      read(77)mblocks 
                                                                        
      do 200 m=1,mblocks 
        ! write(*,*)ist                                            !for every new block
        call key3(m,ist,jst) 
        read(77)nx,ny,nz 
        ni(l)=nx-1
        nj(l)=ny-1
        nk(l)=nz-1
        read(77)(((x(ist(i)+jst(j)+k),y(ist(i)+jst(j)+k),               &
     &            z(ist(i)+jst(j)+k),k=1,nz+1),j=1,ny+1),i=1,nx+1)      

        200 continue
        close(77h)
!Koordinaten f"ur i=0 und/oder j=0 und/oder k=0 werden nicht eingelesen 

!***************** Anfang Lesen von erg.dat ****************************
        open(unit=88,file=nome1,form='unformatted',status='unknown') 
        iges=0 
        read(88) 
        do 300 l=1,mblocks 
          ianf=iges+1 
          iend=iges+(ni(l)+2)*(nj(l)+2)*(nk(l)+2) 
          iges=iend 
          read(88)(velu(i),i=ianf,iend) 
          read(88)(velv(i),i=ianf,iend) 
          read(88)(velw(i),i=ianf,iend) 
          read(88)(pres(i),i=ianf,iend) 
          read(88)(tnue(i),i=ianf,iend) 
          read(88)(tup1(i),i=ianf,iend) 
          read(88)(tup2(i),i=ianf,iend) 
          read(88)( phi(i),i=ianf,iend) 
  300   continue 
                                                                        
        close(88) 
!******************* Ende Lesen von erg.dat ****************************


!***************** Bsp. fuer Schreiben ****************************
      open(55,file='netz4.dat',form='formatted',status='unknown') 
      write(55,996)x,y,z
  996 format(3(1pe13.6,2x))
      close(55) 
!******************************************************************


      END


      subroutine key3(l,ist,jst) 
      ! Input l   -> Blocknummer , 
      !       ist ->     
      !       jst -> 
      ! parameter(mc=5000000,m1=301,mb=999) 
      ! Parameter Zellzahl, max zahl zellen pro richtung, Blockzahl
      ! implicit double precision(a-h,o-z) 
      ! ! double precision fuer a-h und o-z ??? Warum?
      ! common/grid/x(mc),y(mc),z(mc),ni(mb),nj(mb),nk(mb),mblocks 
      ! ! 
      ! common/block/mms(mb) 
      ! dimension ist(0:m1),jst(0:m1) 
      ! 'ist'  ist ein Array der laenge m1 mit start bei 0
      ! 'jst'  """""

      ist(0)=mms(l) 
       
      do 111 i=1, ni(l)+1  ! von 1 bis Zahl Zellen in x Richtung
        ist(i)=ist(i-1) + (nj(l)+2)*(nk(l)+2) 

    111 continue 
      
      jst(0)=1 
      
      do 222 j=1,nj(l)+1   ! von 1 bis Zahl Zellen in y Richtung
        jst(j)=jst(j-1) + nk(l)+2 
  222 continue 
      
      END                                           
