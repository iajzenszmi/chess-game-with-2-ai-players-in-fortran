program chess_ai_vs_ai
  implicit none
  integer, parameter :: N=8
  integer :: board(N,N)
  integer :: side, ply, maxPlies
  integer :: fi,fj,ti,tj,promo
  call seed_rng()
  call init_board(board)
  side = 1             ! +1 = White, -1 = Black
  maxPlies = 300       ! move limit to avoid endless games

  call print_board(board)
  do ply = 1, maxPlies
     call choose_move(board, side, fi,fj,ti,tj,promo)
     if (fi == 0) then
        if (in_check(board, side)) then
           if (side==1) then
              print *, 'Checkmate on move ', ply, ': Black wins.'
           else
              print *, 'Checkmate on move ', ply, ': White wins.'
           end if
        else
           print *, 'Stalemate on move ', ply, '.'
        end if
        exit
     end if
     call make_move(board, fi,fj,ti,tj,promo)
     write(*,'(A,I0,A,A,A,A)') 'Move ', ply, ': ', side_name(-side), ' ', sq_name(fi,fj), ' -> '//sq_name(ti,tj)
     if (promo /= 0) print *, '  (promotion to Queen)'
     call print_board(board)
     side = -side
  end do
  if (ply > maxPlies) print *, 'Draw by move limit.'

contains

  !----------------------- Utilities & I/O -----------------------
  subroutine seed_rng()
    integer :: i, n, seed, un, t
    integer, allocatable :: seedv(:)
    call system_clock(count=t)
    call random_seed(size=n)
    allocate(seedv(n))
    seed = abs(1103515245*t + 12345)
    do i=1,n
       seed = xor(seed, ishft(seed, -13)); seed = seed*1664525 + 1013904223
       seedv(i) = ior(1, iand(seed, z'7FFFFFFF'))
    end do
    call random_seed(put=seedv)
    deallocate(seedv)
  end subroutine seed_rng

  pure function side_name(s) result(name)
    integer, intent(in) :: s
    character(len=5) :: name
    if (s==1) then
       name = 'White'
    else
       name = 'Black'
    end if
  end function side_name

  pure function sq_name(i,j) result(sq)
    integer, intent(in) :: i,j
    character(len=2) :: sq
    character(len=1) :: f, r
    f = achar(iachar('a') + j-1)
    r = achar(iachar('0') + i)
    sq = f//r
  end function sq_name

  subroutine print_board(b)
    integer, intent(in) :: b(N,N)
    integer :: r,c, p
    character(len=1) :: ch
    print *, '   a b c d e f g h'
    do r = N,1,-1
       write(*,'(I2,1X)', advance='no') r
       do c = 1,N
          p = b(r,c)
          ch = piece_char(p)
          write(*,'(A)', advance='no') ' '//ch
       end do
       print *, ''
    end do
    print *, '   a b c d e f g h'
    print *, ''
  end subroutine print_board

  pure function piece_char(p) result(ch)
    integer, intent(in) :: p
    character(len=1) :: ch
    select case (abs(p))
    case (0); ch='.'
    case (1); ch=merge('P','p',p>0)
    case (2); ch=merge('N','n',p>0)
    case (3); ch=merge('B','b',p>0)
    case (4); ch=merge('R','r',p>0)
    case (5); ch=merge('Q','q',p>0)
    case (6); ch=merge('K','k',p>0)
    case default; ch='?'
    end select
  end function piece_char

  pure logical function onboard(i,j)
    integer, intent(in) :: i,j
    onboard = (i>=1 .and. i<=N .and. j>=1 .and. j<=N)
  end function onboard

  !----------------------- Board & Moves -------------------------
  subroutine init_board(b)
    integer, intent(out) :: b(N,N)
    integer :: r,c
    b = 0
    ! White
    b(1,:) = (/ 4,2,3,5,6,3,2,4 /)
    b(2,:) = 1
    ! Black
    b(8,:) = -(/ 4,2,3,5,6,3,2,4 /)
    b(7,:) = -1
  end subroutine init_board

  subroutine make_move(b, fi,fj,ti,tj,promo)
    integer, intent(inout) :: b(N,N)
    integer, intent(in) :: fi,fj,ti,tj,promo
    integer :: p
    p = b(fi,fj)
    b(ti,tj) = p
    b(fi,fj) = 0
    ! Promotion to queen if requested/needed
    if (abs(p) == 1) then
       if (ti == 8 .and. p>0) b(ti,tj) = 5
       if (ti == 1 .and. p<0) b(ti,tj) = -5
    end if
    if (promo /= 0) then
       if (b(ti,tj) > 0) b(ti,tj) = 5
       if (b(ti,tj) < 0) b(ti,tj) = -5
    end if
  end subroutine make_move

  pure integer function piece_value(p) result(v)
    integer, intent(in) :: p
    select case (abs(p))
    case (1); v = 100
    case (2); v = 320
    case (3); v = 330
    case (4); v = 500
    case (5); v = 900
    case (6); v = 20000
    case default; v = 0
    end select
    if (p<0) v = -v
  end function piece_value

  pure integer function evaluate(b) result(score)
    integer, intent(in) :: b(N,N)
    integer :: r,c
    score = 0
    do r=1,N
       do c=1,N
          score = score + piece_value(b(r,c))
       end do
    end do
  end function evaluate

  ! Return TRUE if side 'att' attacks square (i,j)
  pure logical function attacked_by(b, att, i, j)
    integer, intent(in) :: b(N,N), att, i, j
    integer :: r,c, dr,dc, k
    integer, parameter :: KN(8,2) = reshape( (/ &
      2,1,  1,2,  -1,2,  -2,1,  -2,-1,  -1,-2,  1,-2,  2,-1 /), (/8,2/) )

    attacked_by = .false.

    ! Pawns
    if (att==1) then
       if (onboard(i-1,j-1) .and. b(i-1,j-1)==1) attacked_by=.true.
       if (onboard(i-1,j+1) .and. b(i-1,j+1)==1) attacked_by=.true.
    else
       if (onboard(i+1,j-1) .and. b(i+1,j-1)==-1) attacked_by=.true.
       if (onboard(i+1,j+1) .and. b(i+1,j+1)==-1) attacked_by=.true.
    end if
    if (attacked_by) return

    ! Knights
    do k=1,8
       r = i + KN(k,1); c = j + KN(k,2)
       if (onboard(r,c)) then
          if (b(r,c) ==  2*att) attacked_by=.true.
       end if
    end do
    if (attacked_by) return

    ! Bishops/Queens (diagonals)
    do dr=-1,1,2
       do dc=-1,1,2
          r=i+dr; c=j+dc
          do while (onboard(r,c))
             if (b(r,c) /= 0) then
                if (b(r,c) == 3*att .or. b(r,c) == 5*att) attacked_by=.true.
                exit
             end if
             r=r+dr; c=c+dc
          end do
          if (attacked_by) return
       end do
    end do

    ! Rooks/Queens (orthogonals)
    do dr=-1,1,2
       r=i+dr; c=j
       do while (onboard(r,c))
          if (b(r,c) /= 0) then
             if (b(r,c) == 4*att .or. b(r,c) == 5*att) attacked_by=.true.
             exit
          end if
          r=r+dr
       end do
       if (attacked_by) return
    end do
    do dc=-1,1,2
       r=i; c=j+dc
       do while (onboard(r,c))
          if (b(r,c) /= 0) then
             if (b(r,c) == 4*att .or. b(r,c) == 5*att) attacked_by=.true.
             exit
          end if
          c=c+dc
       end do
       if (attacked_by) return
    end do

    ! King
    do dr=-1,1
       do dc=-1,1
          if (dr==0 .and. dc==0) cycle
          r=i+dr; c=j+dc
          if (onboard(r,c)) then
             if (b(r,c) == 6*att) attacked_by=.true.
          end if
       end do
    end do
  end function attacked_by

  pure logical function in_check(b, s)
    integer, intent(in) :: b(N,N), s
    integer :: r,c
    in_check = .false.
    do r=1,N; do c=1,N
       if (b(r,c) == 6*s) then
          in_check = attacked_by(b, -s, r, c)
          return
       end if
    end do; end do
  end function in_check

  subroutine generate_legal_moves(b, s, moves, nm)
    integer, intent(in) :: b(N,N), s
    integer, intent(out) :: moves(:, :)
    integer, intent(out) :: nm
    integer :: i,j, k, r,c, dr,dc
    integer :: bb(N,N)
    integer, parameter :: KN(8,2) = reshape( (/ &
      2,1,  1,2,  -1,2,  -2,1,  -2,-1,  -1,-2,  1,-2,  2,-1 /), (/8,2/) )

    nm = 0
    do i=1,N
       do j=1,N
          if (b(i,j)*s <= 0) cycle
          select case (abs(b(i,j)))
          case (1)  ! Pawn
             r = i + s; c = j
             if (onboard(r,c) .and. b(r,c)==0) then
                call try_add(i,j,r,c,b,s,moves,nm)
                ! Double step
                if ((s==1 .and. i==2) .or. (s==-1 .and. i==7)) then
                   if (onboard(r+s,c) .and. b(r+s,c)==0) then
                      call try_add(i,j,r+s,c,b,s,moves,nm)
                   end if
                end if
             end if
             ! Captures
             if (onboard(i+s,j-1) .and. b(i+s,j-1)*s < 0) call try_add(i,j,i+s,j-1,b,s,moves,nm)
             if (onboard(i+s,j+1) .and. b(i+s,j+1)*s < 0) call try_add(i,j,i+s,j+1,b,s,moves,nm)

          case (2)  ! Knight
             do k=1,8
                r=i+KN(k,1); c=j+KN(k,2)
                if (.not. onboard(r,c)) cycle
                if (b(r,c)*s <= 0) call try_add(i,j,r,c,b,s,moves,nm)
             end do

          case (3)  ! Bishop
             do dr=-1,1,2
                do dc=-1,1,2
                   r=i+dr; c=j+dc
                   do while (onboard(r,c))
                      if (b(r,c)==0) then
                         call try_add(i,j,r,c,b,s,moves,nm)
                      else
                         if (b(r,c)*s < 0) call try_add(i,j,r,c,b,s,moves,nm)
                         exit
                      end if
                      r=r+dr; c=c+dc
                   end do
                end do
             end do

          case (4)  ! Rook
             do dr=-1,1,2
                r=i+dr; c=j
                do while (onboard(r,c))
                   if (b(r,c)==0) then
                      call try_add(i,j,r,c,b,s,moves,nm)
                   else
                      if (b(r,c)*s < 0) call try_add(i,j,r,c,b,s,moves,nm)
                      exit
                   end if
                   r=r+dr
                end do
             end do
             do dc=-1,1,2
                r=i; c=j+dc
                do while (onboard(r,c))
                   if (b(r,c)==0) then
                      call try_add(i,j,r,c,b,s,moves,nm)
                   else
                      if (b(r,c)*s < 0) call try_add(i,j,r,c,b,s,moves,nm)
                      exit
                   end if
                   c=c+dc
                end do
             end do

          case (5)  ! Queen
             ! rook-like
             do dr=-1,1,2
                r=i+dr; c=j
                do while (onboard(r,c))
                   if (b(r,c)==0) then
                      call try_add(i,j,r,c,b,s,moves,nm)
                   else
                      if (b(r,c)*s < 0) call try_add(i,j,r,c,b,s,moves,nm)
                      exit
                   end if
                   r=r+dr
                end do
             end do
             do dc=-1,1,2
                r=i; c=j+dc
                do while (onboard(r,c))
                   if (b(r,c)==0) then
                      call try_add(i,j,r,c,b,s,moves,nm)
                   else
                      if (b(r,c)*s < 0) call try_add(i,j,r,c,b,s,moves,nm)
                      exit
                   end if
                   c=c+dc
                end do
             end do
             ! bishop-like
             do dr=-1,1,2
                do dc=-1,1,2
                   r=i+dr; c=j+dc
                   do while (onboard(r,c))
                      if (b(r,c)==0) then
                         call try_add(i,j,r,c,b,s,moves,nm)
                      else
                         if (b(r,c)*s < 0) call try_add(i,j,r,c,b,s,moves,nm)
                         exit
                      end if
                      r=r+dr; c=c+dc
                   end do
                end do
             end do

          case (6)  ! King (no castling)
             do dr=-1,1
                do dc=-1,1
                   if (dr==0 .and. dc==0) cycle
                   r=i+dr; c=j+dc
                   if (onboard(r,c) .and. b(r,c)*s <= 0) then
                      call try_add(i,j,r,c,b,s,moves,nm)
                   end if
                end do
             end do
          end select
       end do
    end do
  end subroutine generate_legal_moves

  subroutine try_add(fi,fj,ti,tj,b,s,moves,nm)
    integer, intent(in) :: fi,fj,ti,tj, b(N,N), s
    integer, intent(inout) :: moves(:, :)
    integer, intent(inout) :: nm
    integer :: bb(N,N), promo
    bb = b
    promo = 0
    ! Handle promotion flagging (queen)
    if (abs(b(fi,fj))==1) then
       if (s==1 .and. ti==8) promo = 5
       if (s==-1 .and. ti==1) promo = 5
    end if
    call make_move(bb, fi,fj,ti,tj,promo)
    if (.not. in_check(bb, s)) then
       nm = nm + 1
       moves(nm,1)=fi; moves(nm,2)=fj
       moves(nm,3)=ti; moves(nm,4)=tj
       moves(nm,5)=promo
    end if
  end subroutine try_add

  !----------------------- Search & Move choice ------------------
  subroutine choose_move(b, s, fi,fj,ti,tj,promo)
    integer, intent(in) :: b(N,N), s
    integer, intent(out) :: fi,fj,ti,tj,promo
    integer, parameter :: MAXM=256
    integer :: moves(MAXM,5), nm, m
    integer :: bestScore, score, depth
    integer :: bb(N,N)
    real :: rpick
    integer, allocatable :: idx(:)
    integer :: i
    call generate_legal_moves(b, s, moves, nm)
    if (nm == 0) then
       fi=0; fj=0; ti=0; tj=0; promo=0
       return
    end if

    bestScore = -2147483647
    allocate(idx(nm))
    do m=1,nm
       bb = b
       call make_move(bb, moves(m,1),moves(m,2),moves(m,3),moves(m,4),moves(m,5))
       score = -search(bb, -s, 2, -2147483647/2, 2147483647/2)
       if (score > bestScore) then
          bestScore = score
       end if
    end do

    ! collect all best moves (within small epsilon)
    i=0
    do m=1,nm
       bb = b
       call make_move(bb, moves(m,1),moves(m,2),moves(m,3),moves(m,4),moves(m,5))
       score = -search(bb, -s, 2, -2147483647/2, 2147483647/2)
       if (score >= bestScore) then
          i=i+1; idx(i)=m
       end if
    end do

    call random_number(rpick)
    m = idx( max(1, int(rpick*i)+1) )
    fi=moves(m,1); fj=moves(m,2); ti=moves(m,3); tj=moves(m,4); promo=moves(m,5)
    deallocate(idx)
  end subroutine choose_move

  recursive integer function search(b, s, d, alpha, beta) result(val)
    integer, intent(in) :: b(N,N), s, d, alpha, beta
    integer :: a, be, sc
    integer, parameter :: MAXM=256
    integer :: moves(MAXM,5), nm, m
    integer :: bb(N,N)
    logical :: haveMove

    a = alpha; be = beta

    if (d == 0) then
       val = evaluate(b) * s
       return
    end if

    call generate_legal_moves(b, s, moves, nm)
    if (nm == 0) then
       if (in_check(b, s)) then
          val = -2000000 + (2 - d)   ! checkmated: bad for side to move
       else
          val = 0                    ! stalemate: draw
       end if
       return
    end if

    val = -2147483647
    do m=1,nm
       bb = b
       call make_move(bb, moves(m,1),moves(m,2),moves(m,3),moves(m,4),moves(m,5))
       sc = -search(bb, -s, d-1, -be, -a)
       if (sc > val) val = sc
       if (val > a) a = val
       if (a >= be) exit
    end do
  end function search

end program chess_ai_vs_ai
