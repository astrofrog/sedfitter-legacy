module base_pgplot

  ! This module includes Fortran 2003 interfaces to all PGPLOT routines,
  ! which should prevent calling routines with the wrong number or types
  ! of arguments (for example real(dp) instead of real).

  ! Tested with g95, gfortran, and ifort on MacOS X

  ! Date   : 10 October 2007
  ! Author : Thomas Robitaille
  
  use base_types

  implicit none
  save

  interface

     subroutine pgarro (x1, y1, x2, y2)
       real :: x1, y1, x2, y2
     end subroutine pgarro

     subroutine pgask (flag)
       logical :: flag
     end subroutine pgask

     subroutine pgaxis (opt, x1, y1, x2, y2, v1, v2, step, nsub, dmajl,&
          & dmajr, fmin, disp, orient)
       character(len=*) :: opt
       real :: x1, y1, x2, y2, v1, v2, step, dmajl, dmajr, fmin, disp
       real :: orient
       integer :: nsub
     end subroutine pgaxis

     integer function pgband (mode, posn, xref, yref, x, y, ch)
       integer :: mode, posn
       real :: xref, yref, x, y
       character(len=*) :: ch
     end function pgband

     subroutine pgbbuf

     end subroutine pgbbuf

     integer function pgbeg (unit, file, nxsub, nysub)
       integer :: unit
       character(len=*) :: file
       integer :: nxsub, nysub
     end function pgbeg

     subroutine pgbin (nbin, x, data, center)
       integer :: nbin
       real :: x(*), data(*)
       logical :: center
     end subroutine pgbin

     subroutine pgbox (xopt, xtick, nxsub, yopt, ytick, nysub)
       character(len=*) :: xopt, yopt
       real :: xtick, ytick
       integer :: nxsub, nysub
     end subroutine pgbox

     subroutine pgcirc (xcent, ycent, radius)
       real :: xcent, ycent, radius
     end subroutine pgcirc

     subroutine pgclos
     end subroutine pgclos

     subroutine pgconb (a, idim, jdim, i1, i2, j1, j2, c, nc, tr, blank)
       integer :: idim, jdim, i1, i2, j1, j2, nc
       real :: a(idim,jdim), c(*), tr(6), blank
     end subroutine pgconb

     subroutine pgconf (a, idim, jdim, i1, i2, j1, j2, c1, c2, tr)
       integer :: idim, jdim, i1, i2, j1, j2
       real :: a(idim,jdim), c1, c2, tr(6)
     end subroutine pgconf

     subroutine pgconl (a, idim, jdim, i1, i2, j1, j2, c, tr, label,&
          & intval, minint)
       integer :: idim, jdim, i1, j1, i2, j2, intval, minint
       real :: a(idim,jdim), c, tr(6)
       character(len=*) :: label
     end subroutine pgconl

     subroutine pgcons (a, idim, jdim, i1, i2, j1, j2, c, nc, tr)
       integer :: idim, jdim, i1, i2, j1, j2, nc
       real :: a(idim,jdim), c(*), tr(6)
     end subroutine pgcons

     subroutine pgcont (a, idim, jdim, i1, i2, j1, j2, c, nc, tr)
       integer :: idim, jdim, i1, j1, i2, j2, nc
       real :: a(idim,jdim), c(*), tr(6)
     end subroutine pgcont

     subroutine pgconx (a, idim, jdim, i1, i2, j1, j2, c, nc, plot)
       integer :: idim, jdim, i1, j1, i2, j2, nc
       real :: a(idim,jdim), c(*)
       external plot
     end subroutine pgconx

     subroutine plot (visble,x,y,z)
       real :: x, y, z, xworld, yworld
       integer :: visble
     end subroutine plot

     subroutine pgctab(l, r, g, b, nc, contra, bright)
       integer :: nc
       real :: l(nc), r(nc), g(nc), b(nc), contra, bright
     end subroutine pgctab

     subroutine pgcurs (x, y, ch)
       real :: x, y
       character(len=*) :: ch
     end subroutine pgcurs

     subroutine pgdraw (x, y)
       real :: x, y
     end subroutine pgdraw

     subroutine pgebuf
     end subroutine pgebuf

     subroutine pgend
     end subroutine pgend

     subroutine pgenv (xmin, xmax, ymin, ymax, just, axis)
       real :: xmin, xmax, ymin, ymax
       integer :: just, axis
     end subroutine pgenv

     subroutine pgeras
     end subroutine pgeras

     subroutine pgerr1 (dir, x, y, e, t)
       integer :: dir
       real :: x, y, e
       real :: t
     end subroutine pgerr1

     subroutine pgerrb (dir, n, x, y, e, t)
       integer :: dir, n
       real :: x(*), y(*), e(*)
       real :: t
     end subroutine pgerrb

     subroutine pgerrx (n, x1, x2, y, t)
       integer :: n
       real :: x1(*), x2(*), y(*)
       real :: t
     end subroutine pgerrx

     subroutine pgerry (n, x, y1, y2, t)
       integer :: n
       real :: x(*), y1(*), y2(*)
       real :: t
     end subroutine pgerry

     subroutine pgetxt
     end subroutine pgetxt

     subroutine pgfunt (fx, fy, n, tmin, tmax, pgflag)
       real :: fx, fy
       integer :: n
       real :: tmin, tmax
       integer :: pgflag
     end subroutine pgfunt

     subroutine pgfunx (fy, n, xmin, xmax, pgflag)
       real,external :: fy
       integer :: n
       real :: xmin, xmax
       integer :: pgflag
     end subroutine pgfunx

     subroutine pgfuny (fx, n, ymin, ymax, pgflag)
       real :: fx
       integer :: n
       real :: ymin, ymax
       integer :: pgflag
     end subroutine pgfuny

     subroutine pggray (a, idim, jdim, i1, i2, j1, j2, FG, BG, TR)
       integer :: idim, jdim, i1, i2, j1, j2
       real :: a(idim,jdim), fg, bg, tr(6)
     end subroutine pggray

     subroutine pghi2d (data, nxv, nyv, ix1, ix2, iy1, iy2, x, ioff,&
          & bias, center, ylims)
       integer :: nxv, nyv, ix1, ix2, iy1, iy2
       real :: data(nxv,nyv)
       real :: x(ix2-ix1+1), ylims(ix2-ix1+1)
       integer :: ioff
       real :: bias
       logical :: center
     end subroutine pghi2d

     subroutine pghist(n, data, datmin, datmax, nbin, pgflag)
       integer :: n
       real :: data(*)
       real :: datmin, datmax
       integer :: nbin, pgflag
     end subroutine pghist

     subroutine pgiden
     end subroutine pgiden

     subroutine pgimag (a, idim, jdim, i1, i2, j1, j2, a1, a2, tr)
       integer :: idim, jdim, i1, i2, j1, j2
       real :: a(idim,jdim), a1, a2, tr(6)
     end subroutine pgimag

     subroutine pgimagrgb (ar,ag,ab,idim,jdim,i1,i2,j1,j2,&
          &ar1, ar2, ag1, ag2, ab1, ab2, tr)
       integer idim, jdim, i1, i2, j1, j2
       real    ar(idim,jdim), ag(idim,jdim), ab(idim,jdim), tr(6)
       real    ar1, ar2, ag1, ag2, ab1, ab2
     end subroutine pgimagrgb

     subroutine pglab (xlbl, ylbl, toplbl)
       character(len=*) :: xlbl, ylbl, toplbl
     end subroutine pglab

     subroutine pglcur (maxpt, npt, x, y)
       integer :: maxpt, npt
       real :: x(*), y(*)
     end subroutine pglcur

     subroutine pgldev
     end subroutine pgldev

     subroutine pglen (units, string, xl, yl)
       real :: xl, yl
       integer :: units
       character(len=*) :: string
     end subroutine pglen

     subroutine pgline (n, xpts, ypts)
       integer :: n
       real :: xpts(*), ypts(*)
     end subroutine pgline

     subroutine pgmove (x, y)
       real :: x, y
     end subroutine pgmove

     subroutine pgmtxt (side, disp, coord, fjust, text)
       character(len=*) :: side, text
       real :: disp, coord, fjust
     end subroutine pgmtxt

     subroutine pgncur (maxpt, npt, x, y, symbol)
       integer :: maxpt, npt
       real :: x(*), y(*)
       integer :: symbol
     end subroutine pgncur

     subroutine pgnumb (mm, pp, form, string, nc)
       integer :: mm, pp, form
       character(len=*) :: string
       integer :: nc
     end subroutine pgnumb

     subroutine pgolin (maxpt, npt, x, y, symbol)
       integer :: maxpt, npt
       real :: x(*), y(*)
       integer :: symbol
     end subroutine pgolin

     integer function pgopen (device)
       character(len=*) :: device
     end function pgopen

     subroutine pgpage
     end subroutine pgpage

     subroutine pgpanl(ix, iy)
       integer :: ix, iy
     end subroutine pgpanl

     subroutine pgpap (width, aspect)
       real :: width, aspect
     end subroutine pgpap

     subroutine pgpixl (ia, idim, jdim, i1, i2, j1, j2, x1, x2, y1, y2)
       integer :: idim, jdim, i1, i2, j1, j2
       integer :: ia(idim,jdim)
       real :: x1, x2, y1, y2
     end subroutine pgpixl

     subroutine pgpnts (n, x, y, symbol, ns)
       integer :: n, ns
       real :: x(*), y(*)
       integer :: symbol(*)
     end subroutine pgpnts

     subroutine pgpoly (n, xpts, ypts)
       integer :: n
       real :: xpts(*), ypts(*)
     end subroutine pgpoly

     subroutine pgpt (n, xpts, ypts, symbol)
       integer :: n
       real :: xpts(*), ypts(*)
       integer :: symbol
     end subroutine pgpt

     subroutine pgpt1 (xpt, ypt, symbol)
       real :: xpt, ypt
       integer :: symbol
     end subroutine pgpt1

     subroutine pgptxt (x, y, angle, fjust, text)
       real :: x, y, angle, fjust
       character(len=*) :: text
     end subroutine pgptxt

     subroutine pgqah (fs, angle, barb)
       integer :: fs
       real :: angle, barb
     end subroutine pgqah

     subroutine pgqcf (font)
       integer :: font
     end subroutine pgqcf

     subroutine pgqch (size)
       real :: size
     end subroutine pgqch

     subroutine pgqci (ci)
       integer :: ci
     end subroutine pgqci

     subroutine pgqcir(icilo, icihi)
       integer :: icilo, icihi
     end subroutine pgqcir

     subroutine pgqclp(state)
       integer :: state
     end subroutine pgqclp

     subroutine pgqcol (ci1, ci2)
       integer :: ci1, ci2
     end subroutine pgqcol

     subroutine pgqcr (ci, cr, cg, cb)
       integer :: ci
       real :: cr, cg, cb
     end subroutine pgqcr

     subroutine pgqcs(units, xch, ych)
       integer :: units
       real :: xch, ych
     end subroutine pgqcs

     subroutine pgqdt(n, type, tlen, descr, dlen, inter)
       integer :: n
       character(len=*) :: type, descr
       integer :: tlen, dlen, inter
     end subroutine pgqdt

     subroutine pgqfs (fs)
       integer :: fs
     end subroutine pgqfs

     subroutine pgqhs (angle, sepn, phase)
       real :: angle, sepn, phase
     end subroutine pgqhs

     subroutine pgqid (id)
       integer :: id
     end subroutine pgqid

     subroutine pgqinf (item, value, length)
       character(len=*) :: item, value
       integer :: length
     end subroutine pgqinf

     subroutine pgqitf (itf)
       integer :: itf
     end subroutine pgqitf

     subroutine pgqls (ls)
       integer :: ls
     end subroutine pgqls

     subroutine pgqlw (lw)
       integer :: lw
     end subroutine pgqlw

     subroutine pgqndt(n)
       integer :: n
     end subroutine pgqndt

     subroutine pgqpos (x, y)
       real :: x, y
     end subroutine pgqpos

     subroutine pgqtbg (tbci)
       integer :: tbci
     end subroutine pgqtbg

     subroutine pgqtxt (x, y, angle, fjust, text, xbox, ybox)
       real :: x, y, angle, fjust
       character(len=*) :: text
       real :: xbox(4), ybox(4)
     end subroutine pgqtxt

     subroutine pgqvp (units, x1, x2, y1, y2)
       integer :: units
       real :: x1, x2, y1, y2
     end subroutine pgqvp

     subroutine pgqvsz (units, x1, x2, y1, y2)
       integer :: units
       real :: x1, x2, y1, y2
     end subroutine pgqvsz

     subroutine pgqwin (x1, x2, y1, y2)
       real :: x1, x2, y1, y2
     end subroutine pgqwin

     subroutine pgrect (x1, x2, y1, y2)
       real :: x1, x2, y1, y2
     end subroutine pgrect

     real function pgrnd (x, nsub)
       real :: x
       integer :: nsub
     end function pgrnd

     subroutine pgrnge (x1, x2, xlo, xhi)
       real :: x1, x2, xlo, xhi
     end subroutine pgrnge

     subroutine pgsah (fs, angle, barb)
       integer :: fs
       real :: angle, barb
     end subroutine pgsah

     subroutine pgsave
     end subroutine pgsave

     subroutine pgscf (font)
       integer :: font
     end subroutine pgscf

     subroutine pgsch (size)
       real :: size
     end subroutine pgsch

     subroutine pgsci (ci)
       integer :: ci
     end subroutine pgsci

     subroutine pgscir(icilo, icihi)
       integer :: icilo, icihi
     end subroutine pgscir

     subroutine pgsclp(state)
       integer :: state
     end subroutine pgsclp

     subroutine pgscr (ci, cr, cg, cb)
       integer :: ci
       real :: cr, cg, cb
     end subroutine pgscr

     subroutine pgscrl (dx, dy)
       real :: dx, dy
     end subroutine pgscrl

     subroutine pgscrn(ci, name, ier)
       integer :: ci
       character(len=*) :: name
       integer :: ier
     end subroutine pgscrn

     subroutine pgsfs (fs)
       integer :: fs
     end subroutine pgsfs

     subroutine pgshls (ci, ch, cl, cs)
       integer :: ci
       real :: ch, cl, cs
     end subroutine pgshls

     subroutine pgshs (angle, sepn, phase)
       real :: angle, sepn, phase
     end subroutine pgshs

     subroutine pgsitf (itf)
       integer :: itf
     end subroutine pgsitf

     subroutine pgslct(id)
       integer :: id
     end subroutine pgslct

     subroutine pgsls (ls)
       integer :: ls
     end subroutine pgsls

     subroutine pgslw (lw)
       integer :: lw
     end subroutine pgslw

     subroutine pgstbg (tbci)
       integer :: tbci
     end subroutine pgstbg

     subroutine pgsubp (nxsub, nysub)
       integer :: nxsub, nysub
     end subroutine pgsubp

     subroutine pgsvp (xleft, xright, ybot, ytop)
       real :: xleft, xright, ybot, ytop
     end subroutine pgsvp

     subroutine pgswin (x1, x2, y1, y2)
       real :: x1, x2, y1, y2
     end subroutine pgswin

     subroutine pgtbox (xopt, xtick, nxsub, yopt, ytick, nysub)
       real :: xtick, ytick
       integer :: nxsub, nysub
       character :: xopt*(*), yopt*(*)
     end subroutine pgtbox

     subroutine pgtext (x, y, text)
       real :: x, y
       character(len=*) :: text
     end subroutine pgtext

     subroutine pgtick (x1, y1, x2, y2, v, tikl, tikr, disp, orient, str)
       real :: x1, y1, x2, y2, v, tikl, tikr, disp, orient
       character(len=*) :: str
     end subroutine pgtick

     subroutine pgupdt
     end subroutine pgupdt

     subroutine pgvect (a, b, idim, jdim, i1, i2, j1, j2, c, nc, tr, blank)
       integer :: idim, jdim, i1, i2, j1, j2, nc
       real :: a(idim,jdim), b(idim, jdim), tr(6), blank, c
     end subroutine pgvect

     subroutine pgvsiz (xleft, xright, ybot, ytop)
       real :: xleft, xright, ybot, ytop
     end subroutine pgvsiz

     subroutine pgvstd
     end subroutine pgvstd

     subroutine pgwedg(side, disp, width, fg, bg, label)
       character(len=*) :: side,label
       real :: disp, width, fg, bg
     end subroutine pgwedg

     subroutine pgwnad (x1, x2, y1, y2)
       real :: x1, x2, y1, y2
     end subroutine pgwnad

     subroutine pgadvance
     end subroutine pgadvance

     integer function pgbegin (unit, file, nxsub, nysub)
       integer :: unit
       character(len=*) :: file
       integer :: nxsub, nysub
     end function pgbegin

     integer function pgcurse (x, y, ch)
       real :: x, y
       character(len=1) :: ch
     end function pgcurse

     subroutine pglabel (xlbl, ylbl, toplbl)
       character(len=*) :: xlbl, ylbl, toplbl
     end subroutine pglabel

     subroutine pgmtext (side, disp, coord, fjust, text)
       character(len=*) :: side, text
       real :: disp, coord, fjust
     end subroutine pgmtext

     subroutine pgncurse (maxpt, npt, x, y, symbol)
       integer :: maxpt, npt
       real :: x(*), y(*)
       integer :: symbol
     end subroutine pgncurse

     subroutine pgpaper (width, aspect)
       real :: width, aspect
     end subroutine pgpaper

     subroutine pgpoint (n, xpts, ypts, symbol)
       integer :: n
       real :: xpts(*), ypts(*)
       integer :: symbol
     end subroutine pgpoint

     subroutine pgptext (x, y, angle, fjust, text)
       real :: x, y, angle, fjust
       character(len=*) :: text
     end subroutine pgptext

     subroutine pgvport (xleft, xright, ybot, ytop)
       real :: xleft, xright, ybot, ytop
     end subroutine pgvport

     subroutine pgvsize (xleft, xright, ybot, ytop)
       real :: xleft, xright, ybot, ytop
     end subroutine pgvsize

     subroutine pgvstand
     end subroutine pgvstand

     subroutine pgwindow (x1, x2, y1, y2)
       real :: x1, x2, y1, y2
     end subroutine pgwindow

  end interface
  
contains
  
  subroutine set_colormap(name)
    
    use base_messages

    implicit none

    character(len=*) :: name

    integer :: n

    real,allocatable,dimension(:) :: l,r,g,b

    select case(name)
    case('heat')
       n = 5
    case('rainbow')
       n = 6
    case('rainbow_extended')
       n = 8
    case('bluetored')
       n = 3
    case default
       call error("set_colormap","colormap unknown")
    end select

    allocate(l(n),r(n),g(n),b(n))

    select case(name)
    case('bluetored')
       l = (/0.00,0.50,1.00/)
       r = (/0.00,1.00,1.00/)
       g = (/0.00,1.00,0.00/)
       b = (/1.00,1.00,0.00/)
    case('heat')
       l = (/0.00,0.34,0.65,0.98,1.00/)
       r = (/0.00,1.00,1.00,1.00,1.00/)
       g = (/0.00,0.25,0.50,0.75,1.00/)
       b = (/0.00,0.00,0.00,1.00,1.00/)
    case('rainbow')
       l = (/0.00,0.20,0.40,0.60,0.80,1.00/)
       r = (/1.00,0.00,0.00,0.00,1.00,1.00/)
       g = (/0.00,0.00,1.00,1.00,1.00,0.00/)
       b = (/1.00,1.00,1.00,0.00,0.00,0.00/) 
    case('rainbow_extended')
       l = (/0.00,0.10,0.26,0.42,0.58,0.74,0.90,1.00/)
       r = (/0.00,1.00,0.00,0.00,0.00,1.00,1.00,1.00/)
       g = (/0.00,0.00,0.00,1.00,1.00,1.00,0.00,1.00/)
       b = (/0.00,1.00,1.00,1.00,0.00,0.00,0.00,1.00/) 
    end select
  
    call pgctab(l,r,g,b,n,1.0,0.5)

    deallocate(l,r,g,b)
 
  end subroutine set_colormap
  
  subroutine pgbox_i(xopt, xtick, nxsub, yopt, ytick, nysub)

      implicit none

     character(len=*),intent(in) :: xopt, yopt
     real,intent(in)             :: xtick, ytick
     integer,intent(in)          :: nxsub, nysub
     
     character(len=100) :: xopt_new,yopt_new
     real :: xmin,xmax,ymin,ymax
     integer :: first,last,i
     integer :: l,p
     
     logical :: xoptn,xoptl,yoptn,yoptl
     
     character(len=10) :: label
     
     XOPTN = INDEX(XOPT,'N').NE.0.or.INDEX(XOPT,'n').NE.0
     XOPTL = INDEX(XOPT,'L').NE.0.or.INDEX(XOPT,'l').NE.0
     YOPTN = INDEX(YOPT,'N').NE.0.or.INDEX(YOPT,'n').NE.0
     YOPTL = INDEX(YOPT,'L').NE.0.or.INDEX(YOPT,'l').NE.0
     
     call pgqwin(xmin,xmax,ymin,ymax)
     
     xopt_new = xopt
     yopt_new = yopt
     
     if(xoptn.and.xoptl.and.nint(xmax-xmin) > 8) then

       l = len(xopt_new) ; p = index(xopt_new,'n')
       xopt_new(p:l-1) = xopt_new(p+1:l)
       
       first= ceiling(xmin) ; last = floor(xmax)

       do i=first,last,2
         write(label,'("10\u",I0)') i
         if(i==-1) label='0.1'
         if(i==+0) label='1'
         if(i==+1) label='10'
         call plot_label('x',real(i),label)
       end do

     end if

     if(yoptn.and.yoptl.and.nint(ymax-ymin) > 8) then
       
       l = len(yopt_new) ; p = index(yopt_new,'n')
       yopt_new(p:l-1) = yopt_new(p+1:l)
       
       first= ceiling(ymin) ; last = floor(ymax)

       do i=first,last,2
         write(label,'("10\u",I0)') i
         if(i==-1) label='0.1'
         if(i==+0) label='1'
         if(i==+1) label='10'
         call plot_label('y',real(i),label) 
       end do

     end if
     
     call pgbox(xopt_new, xtick, nxsub, yopt_new, ytick, nysub)

  end subroutine pgbox_i
  
  subroutine plot_label(axis,value,label)
    implicit none
    real,intent(in)             :: value
    character(len=*),intent(in) :: axis,label
    real :: xmin,xmax,ymin,ymax,frac
    call pgqwin(xmin,xmax,ymin,ymax)
    select case(axis)
    case('x')
      frac = (value-xmin)/(xmax-xmin)
      call pgmtxt('B',1.2,frac,0.5,label)
    case('y')
      frac = (value-ymin)/(ymax-ymin)
      call pgmtxt('LV',0.7,frac,1.0,label)
    end select
  end subroutine plot_label

  !********************************************************************************
  ! Draw log labels on y axis
  !********************************************************************************

  subroutine log_labels_y(step,min,dx,dy)

    implicit none

    real :: xmin,xmax,ymin,ymax
    ! range of values

    integer :: curr_log

    real :: x0,y0

    integer :: min,step

    real :: dx,dy
    ! fractional displacement

    character(len=3) :: c_val

    real :: y_corr

    real :: vxmin,vxmax,vymin,vymax

    ! ### FIND WINDOW RANGE ### !

    call pgqwin(xmin,xmax,ymin,ymax)

    call pgqvp(1,vxmin,vxmax,vymin,vymax)

    x0=xmin-dx*(xmax-xmin)/(vxmax-vxmin)

    y_corr=dy*(ymax-ymin)/(vymax-vymin)

    ! ### CYCLE THROUGH LOG VALUES ### !

    curr_log=min

    do 

       ! --- FIND IF VALUE IS WITHIN RANGE --- !

       if(real(curr_log).gt.ymax) exit

       if(real(curr_log).ge.ymin) then

          y0=real(curr_log)+y_corr

          write(c_val,'(I3)') curr_log

          if(curr_log==-1) c_val='0.1'
          if(curr_log==+0) c_val='1'
          if(curr_log==+1) c_val='10'

          if(curr_log.ge.-1.and.curr_log.le.1) then
             call pgptxt(x0,y0,0.0,1.0,''//adjustl(c_val))
          else
             call pgptxt(x0,y0,0.0,1.0,'10\u'//adjustl(c_val))
          end if

       end if

       curr_log=curr_log+step

    end do

  end subroutine log_labels_y

  !********************************************************************************
  ! Draw log labels on x axis
  !********************************************************************************

  subroutine log_labels_x(step,min,dy)

    implicit none

    real :: xmin,xmax,ymin,ymax
    ! range of values

    integer :: curr_log

    real :: x0,y0

    integer :: min,step

    real :: dy
    ! fractional displacement

    character(len=3) :: c_val

    real :: vxmin,vxmax,vymin,vymax

    ! ### FIND WINDOW RANGE ### !

    call pgqwin(xmin,xmax,ymin,ymax)

    call pgqvp(1,vxmin,vxmax,vymin,vymax)

    y0=ymin-dy*(ymax-ymin)/(vymax-vymin)

    ! ### CYCLE THROUGH LOG VALUES ### !

    curr_log=min

    do 

       ! --- FIND IF VALUE IS WITHIN RANGE --- !

       if(real(curr_log).gt.xmax) exit

       if(real(curr_log).ge.xmin) then

          x0=real(curr_log)

          write(c_val,'(I3)') curr_log

          if(curr_log==-1) c_val='0.1'
          if(curr_log==+0) c_val='1'
          if(curr_log==+1) c_val='10'

          if(curr_log.ge.-1.and.curr_log.le.1) then
             call pgptxt(x0,y0,0.0,0.5,''//adjustl(c_val))
          else
             call pgptxt(x0,y0,0.0,0.5,'10\u'//adjustl(c_val))
          end if

       end if

       curr_log=curr_log+step

    end do

  end subroutine log_labels_x

  subroutine find_tr_matrix(sx,xmin,xmax,sy,ymin,ymax,tr)

    implicit none

    ! --- Input --- !

    integer,intent(in) :: sx,sy
    ! dimensions of the array to be plotted

    real,intent(in) :: xmin,xmax,ymin,ymax
    ! range of the array

    ! --- Output --- !

    real,dimension(6),intent(out) :: tr
    ! the TR matrix for PGPLOT

    ! --- Local variables --- !

    real :: xstep,ystep
    ! the size of each pixel

    xstep=(xmax-xmin)/real(sx)
    ystep=(ymax-ymin)/real(sy)

    tr(1)=xmin-xstep/2.
    tr(2)=xstep
    tr(3)=0.
    tr(4)=ymin-ystep/2.
    tr(5)=0.
    tr(6)=ystep

  end subroutine find_tr_matrix

  subroutine plot_hist(nbin,x,y,style,color)

    implicit none

    integer,intent(in) :: nbin
    ! number of bins

    real,dimension(nbin),intent(in) :: x,y
    ! the position and height of the bin

    real :: xmin,xmax
    ! min and max of each bin

    integer :: b
    ! loop variable
    
    integer :: st,co
    integer,intent(in) :: style,color

    real,dimension(2*nbin+2) :: xp,yp

    st = style
    co = color

    if(style==3) then
       call pgshs(+45.0,1.0,0.0)
       st=3
    end if

    if(style==5) then
       call pgshs(135.0,1.0,0.0)
       st=3
    end if

    call pgsfs(st)
    call pgsci(co)

    do b=1,nbin

       if(b.gt.1) then
          xmin=0.5*(x(b)+x(b-1))
       else
          xmin=x(b)-0.5*(x(b+1)-x(b))
       end if

       if(b.lt.nbin) then
          xmax=0.5*(x(b)+x(b+1))
       else
          xmax=x(b)+0.5*(x(b)-x(b-1))
       end if

       xp(2*b-1) = xmin
       xp(2*b)   = xmax
       yp(2*b-1) = y(b)
       yp(2*b)   = y(b)

!       call pgrect(xmin,xmax,-10.,y(b))

    end do

    xp(2*nbin+1) = xp(2*nbin)
    yp(2*nbin+1) = 0.
    
    xp(2*nbin+2) = xp(1)
    yp(2*nbin+2) = 0.

    call pgpoly(nbin*2+2,xp,yp)

    call pgsci(1)
    call pgsfs(1)

  end subroutine plot_hist

!!$  subroutine plot_hist(nbin,x,y,style,color)
!!$
!!$    implicit none
!!$
!!$    integer,intent(in) :: nbin
!!$    ! number of bins
!!$
!!$    real,dimension(nbin),intent(in) :: x,y
!!$    ! the position and height of the bin
!!$
!!$    real :: xmin,xmax
!!$    ! min and max of each bin
!!$
!!$    integer :: b
!!$    ! loop variable
!!$    
!!$    integer :: st,co
!!$    integer,intent(in) :: style,color
!!$
!!$    st = style
!!$    co = color
!!$
!!$    if(style==3) then
!!$       call pgshs(+45.0,1.0,0.0)
!!$       st=3
!!$    end if
!!$
!!$    if(style==5) then
!!$       call pgshs(135.0,1.0,0.0)
!!$       st=3
!!$    end if
!!$
!!$    call pgsfs(st)
!!$    call pgsci(co)
!!$
!!$    do b=1,nbin
!!$
!!$       if(b.gt.1) then
!!$          xmin=0.5*(x(b)+x(b-1))
!!$       else
!!$          xmin=x(b)-0.5*(x(b+1)-x(b))
!!$       end if
!!$
!!$       if(b.lt.nbin) then
!!$          xmax=0.5*(x(b)+x(b+1))
!!$       else
!!$          xmax=x(b)+0.5*(x(b)-x(b-1))
!!$       end if
!!$
!!$       call pgrect(xmin,xmax,-10.,y(b))
!!$
!!$
!!$    end do
!!$
!!$    call pgsci(1)
!!$    call pgsfs(1)
!!$
!!$  end subroutine plot_hist

end module base_pgplot
