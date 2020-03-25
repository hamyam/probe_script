      parameter(mc=100,mb=5,mn=10000)
  
!**********************************************************             
! mb  nr. of blocks                                                     
! mc  nr. of cells (including boundary-cells of each block)             
! ms  nr. of boundary-cells  (including edges and vertices)             
! mn  nr. of neighbours of all boundary-cells on interfaces             
! m1  maximum nr. of cells in one direction +1                          
!**********************************************************             
                                                                        
      double precision x,y,z        
      integer nx,ny,nz
      dimension x(mc,mc,mc),y(mc,mc,mc),z(mc,mc,mc)
      dimension nx(mb),ny(mb),nz(mb)

      double precision velu, velv, velw, pres, tnue, tup1, tup2, phi, bufferno
      dimension velu(mn), velv(mn), velw(mn), pres(mn), tnue(mn), tup1(mn), tup2(mn),phi(mn)


!******* Anfang Lesen Koordinaten der Netzknoten aller Bl"ocke *********
      write(*,*) "Lese Gitter"
      open(66,file='netz.dat',form='unformatted',status='unknown') 
      read(66)mblocks 
      write(*,*) "Anzahl Bloecke: ", mblocks
                                                                        
      do 200 m=1,mblocks 
                                                    !for every new block
        read(66)nz(m),ny(m),nx(m)
        ! Reihenfolge z, y, x 
        write(*,*)"Zellzahl in Richtungen: nx:",nx(m), "ny:",ny(m),"nz:",nz(m)
        write(*,*)"Gesamte Anzahl Zellen: " , nx(m)*ny(m)*nz(m)
        write(*,*)"Eckenanzahl in Richtungen: ", (nx(m)+1),(ny(m)+1),(nz(m)+1)
        write(*,*)"gesamtanzahl an Ecken: ", (nx(m)+1)*(ny(m)+1)*(nz(m)+1)

        read(66)(((x(k,j,i), y(k,j,i), z(k,j,i), &
          &     k=1, nz(m)+1) ,&
          &        j=1, ny(m)+1), &
          &            i=1, nx(m)+1 )
          ! Verschachtelte Read Anweisung. 
          ! aeussere  Iteration i ueber nx
          ! mittlere  Iteration j ueber ny
          ! innere    Iteration j ueber nz      
          ! Reihenfolge der Eintraege auch wieder 
        write(*,*)"x(1,1,i)",(x(1,1,i),i=1,nx(m))

        ! Debugg 
     !    write(*,996)(((x(k,j,i),y(k,j,i),               &
     ! &            z(k,j,i),k=1,nz(m)+1),j=1,ny(m)+1),i=1,nx(m)+1) 
                                                                        
  200 continue 
      write(*,*) "Netz wurde gelesen"
      close(66) 
      
!******** Ende Lesen Koordinaten der Netzknoten aller Bl"ocke **********

      write(*,*) ""
      write(*,*) "-----------------------"
      write(*,*) ""

!******* ANFANG Schreiben Netz File in ASCI ****************************
      
      write(*,*) "Schreibe Netz als ASCI File"

      open(55,file='netz5.dat',form='formatted',status='unknown') 

      do 100 m=1,mblocks
        do 101 i=1,nx(m)+1
          !   Auessere Iteration i   -> nx = 8  ????
          do 102 j=1,ny(m)+1
            ! Mittlere Iteration j   -> ny = 24 ????
            do 103 k=1,nz(m)+1
              ! Innere Iteration k   -> nz = 20 ????

              write(55,996)x(k,j,i),y(k,j,i),z(k,j,i)
              ! Schreibe Koordinaten x, y, z getrennt durch je zwei Leerzeilen

  103       continue
  102     continue
  101   continue
  100 continue
      
    996     format(3(1pe13.6,2x))
      close(55) 

!******* ENDE Schreiben Netz File in ASCI ****************************

      write(*,*) ""
      write(*,*) "-----------------------"
      write(*,*) ""

!******* Anfang Lesen erg File Bl"ocke *********
      write(*,*) "Lese erg File"
      open(77,file='erg_1.dat',form='unformatted',status='unknown') 
      read(77)mblocks 
      write(*,*) "mblocks in erg: ", mblocks
      ! Zaehler initialisieren
      iges=0  

      do 205 m=1,mblocks 
        ! erste 0 Dicke ueberspringen
        ianf=iges+1 
        
        ! gesamte Zellzahl berechnen
        ! iend=iges+(nx(m)+2)*(ny(m)+2)*(nz(m)+2)   ORIG
        iend=iges+(nx(m)+2)*(ny(m)+2)*(nz(m)+2) 
        iges=iend                                             !for every new block
        write(*,*) "Laenge des Ergebnisarrays:", iend

        read(77)(velu(i),i=ianf,iend)
        ! write(*,*)"vel U: ", velu

        read(77)(velv(i),i=ianf,iend)
        ! write(*,*)"vel V: ",velv

        read(77)(velw(i),i=ianf,iend) 
        read(77)(pres(i),i=ianf,iend) 
        read(77)(tnue(i),i=ianf,iend) 
        read(77)(tup1(i),i=ianf,iend) 
        read(77)(tup2(i),i=ianf,iend) 
        read(77)( phi(i),i=ianf,iend) 
                                                                        
  205 continue 
      close(77) 

!******* Ende Lesen erg File Bl"ocke *********

      write(*,*) ""
      write(*,*) "-----------------------"
      write(*,*) ""

!******* Anfang Schreiben erg File Bl"ocke *********

open(79, file='erg_out.dat', form='formatted',status='unknown', action="readwrite")
write(*,*) "Schreibe Erg als ASCI."
write(79,*) "# velu, velv, velw, pres"
do m=1, mblocks
! fuer jeden Block
  do n = 1, iges
    ! Entraege der Vektoren schreiben

    ! BERECHNUNG ZELLMITTELPUNKTE FUER WERTE EINFUEGEN 
    ! UMRECHNUNG VON INDEX AUF KOORINATEN ODER UMGEKEHRT 

  write(79,'(4(1pe13.6,x))') velu(n), velv(n), velw(n), pres(n)
  enddo
enddo
close(79)

itest=1
jtest=1 ! 
ktest=1 ! Index fuer x Richtung
write(*,*) "Knoten mit Index i = ",itest, " j=", jtest, " k= ", ktest
write(*,996) x(ktest,jtest,itest),y(ktest,jtest,itest),z(ktest,jtest,itest)

!******** Ende Lesen Koordinaten der Netzknoten aller Bl"ocke **********



END
