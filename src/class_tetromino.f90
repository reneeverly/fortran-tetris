module class_tetromino
   implicit none
   type :: tetromino
      integer :: color
      integer, dimension(2) :: center
      integer :: block1(2)
      integer :: block2(2)
      integer :: block3(2)
      integer :: block4(2)
   contains
      procedure :: rotate => tetromino_rotate
      procedure :: unrotate => tetromino_unrotate
      procedure :: go_left => tetromino_go_left
      procedure :: go_right => tetromino_go_right
      procedure :: go_down => tetromino_go_down
      procedure :: go_up => tetromino_go_up
   end type
   interface tetromino
      procedure constructor
   end interface
contains
   function constructor(style)
      integer, intent(in) :: style
      type(tetromino) :: constructor
      constructor%center = [5,0]
      if (style == 0) then ! I
         ! |2|
         ! |1|
         ! |3|
         ! |4|
         constructor%block1 = [0, -1]
         constructor%block2 = [0, 0]
         constructor%block3 = [0, 1]
         constructor%block4 = [0, 2]

         constructor%color = 1 ! cyan
      else if (style == 1) then ! O
         !
         ! |1|3|
         ! |2|4|
         !
         constructor%block1 = [0, 0]
         constructor%block2 = [0, 1]
         constructor%block3 = [1, 0]
         constructor%block4 = [1, 1]

         constructor%color = 2 ! yellow
      else if (style == 2) then ! T
         !
         !   |3|
         ! |2|1|4|
         !
         constructor%block1 = [0, 1]
         constructor%block2 = [-1, 1]
         constructor%block3 = [0, 0]
         constructor%block4 = [1, 1]

         constructor%color = 3 ! magenta
      else if (style == 3) then ! J
         !
         !   |2|
         !   |1|
         ! |4|3|
         !
         constructor%block1 = [0, 0]
         constructor%block2 = [0, -1]
         constructor%block3 = [0, 1]
         constructor%block4 = [-1, 1]

         constructor%color = 4 ! blue
      else if (style == 4) then ! L
         !
         ! |2|
         ! |1|
         ! |3||4|
         !
         constructor%block1 = [0, 0]
         constructor%block2 = [0, -1]
         constructor%block3 = [0, 1]
         constructor%block4 = [1, 1]

         constructor%color = 5 ! orange
      else if (style == 5) then ! S
         !
         !   |1|4|
         ! |2|3|
         !
         constructor%block1 = [0, 0]
         constructor%block2 = [-1, 1]
         constructor%block3 = [0, 1]
         constructor%block4 = [1, 0]

         constructor%color = 6 ! lime
      else if (style == 6) then ! Z
         !
         ! |2|1|
         !   |3|4|
         !
         constructor%block1 = [0, 0]
         constructor%block2 = [-1, 0]
         constructor%block3 = [0, 1]
         constructor%block4 = [1, 1]

         constructor%color = 7 ! red
      end if
   end function
   subroutine tetromino_rotate(this)
      class(tetromino) :: this
      integer :: swapper

      swapper = this%block1(1)
      this%block1(1) = this%block1(2)
      this%block1(2) = -swapper

      swapper = this%block2(1)
      this%block2(1) = this%block2(2)
      this%block2(2) = -swapper

      swapper = this%block3(1)
      this%block3(1) = this%block3(2)
      this%block3(2) = -swapper

      swapper = this%block4(1)
      this%block4(1) = this%block4(2)
      this%block4(2) = -swapper

   end subroutine
   subroutine tetromino_unrotate(this)
      class(tetromino) :: this
      integer :: swapper

      swapper = this%block1(2)
      this%block1(2) = this%block1(1)
      this%block1(1) = -swapper

      swapper = this%block2(2)
      this%block2(2) = this%block2(1)
      this%block2(1) = -swapper

      swapper = this%block3(2)
      this%block3(2) = this%block3(1)
      this%block3(1) = -swapper

      swapper = this%block4(2)
      this%block4(2) = this%block4(1)
      this%block4(1) = -swapper
   end subroutine
   subroutine tetromino_go_left(this)
      class(tetromino) :: this
      this%center(1) = this%center(1) - 1
   end subroutine
   subroutine tetromino_go_right(this)
      class(tetromino) :: this
      this%center(1) = this%center(1) + 1
   end subroutine
   subroutine tetromino_go_down(this)
      class(tetromino) :: this
      this%center(2) = this%center(2) + 1
   end subroutine
   subroutine tetromino_go_up(this)
      class(tetromino) :: this
      this%center(2) = this%center(2) - 1
   end subroutine
end module
