program main
    use hk
    implicit none
    real(8), parameter :: dots = 1d3
    integer, parameter :: wdth = 5
    integer, parameter :: slowness = 1000 ! dont dare 100000
    integer :: j
    real(8) :: x

    open(unit = 1, file = "output.dat")
    do j = 1, dots
        x = 0 + 1.d0*j/dots
        write (1, *) x, prob(x, wdth, slowness)
    end do
    close(1)

    print *, "done!"



contains
    real(8) function prob(p, n, iter)
        implicit none
        real(8), intent(in) :: p
        integer, intent(in) :: iter, n
        integer, dimension(n, n) :: arr
        integer :: hits, j
        integer :: clst


        hits = 0
        do j = 1, iter
            arr=getArr(n, p)
            clst = hoshen_kopelman(arr)
            if (check(arr)) hits = hits + 1
        end do

        prob = real(hits)/iter
    end function

    pure logical function check(arg)
        implicit none
        integer, intent(in), dimension(:,:) :: arg
        integer :: i, j

        check = 0
        do i = 1, size(arg, 1)
            do j = 1, size(arg, 1)
                if ((arg(j, 1).ne.0).and.(arg(j, 1).eq.arg(i, size(arg, 2)))) then
                    check = 1
                    exit
                end if
            end do
        end do

    end function

    function getArr(n, p)
        implicit none
        integer, intent(in) :: n
        real(8), intent (in) :: p
        integer, dimension(n, n) :: getArr

        real(8) :: rnd
        integer :: i, j

        call random_seed()

        do i = 1, n
            do j = 1, n
                call random_number(rnd)
                if (rnd .lt. p) then
                    getArr(i, j) = 1
                else
                    getArr(i, j) = 0
                end if
            end do
        end do
    end function getArr

    subroutine printArr(arr)
        implicit none
        integer, dimension(:, :), intent(in) :: arr
        integer :: i

        do i = 1, size(arr, 1)
            write( * , "(100I3.1)" ) arr(i, :)
        end do
    end subroutine printArr

end program main
