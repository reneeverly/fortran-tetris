program main
   use, intrinsic :: iso_c_binding, only: c_null_char
   use raylib
   use class_tetromino
   implicit none

   integer, parameter :: screen_width = 879
   integer, parameter :: screen_height = 1002

   integer, parameter :: block_size = 50
   integer, parameter :: board_width = 10
   integer, parameter :: board_height = 20

   ! 1 indexed, so I guess 15 is blank now
   integer, dimension(board_width, board_height) :: board = 15

   type(color_type), dimension(15) :: color_list = [skyblue, yellow, magenta, darkblue, orange, lime, red, gray, gray, gray, gray, gray, gray, gray, black]

   class(tetromino), allocatable :: current_tetromino
   class(tetromino), allocatable :: next_tetromino

   integer :: x, y, x0, y0
   real :: random_real

   real :: old_time, new_time

   call init_window(screen_width, screen_height, 'Fortran Tetris' // c_null_char)
   call set_target_fps(60)

   call random_number(random_real)
   current_tetromino = tetromino(floor(random_real*7))

   call random_number(random_real)
   next_tetromino = tetromino(floor(random_real*7))

   old_time = get_time()

   do while (.not. window_should_close())
      ! do game functionality
      new_time = get_time()
      if (new_time > (old_time + 1)) then
         call current_tetromino%go_down()
         old_time = new_time
         if (check_for_intersection_or_bounds()) then
            ! solidify
            call current_tetromino%go_up()
            board(current_tetromino%center(1) + current_tetromino%block1(1), current_tetromino%center(2) + current_tetromino%block1(2)) = current_tetromino%color
            board(current_tetromino%center(1) + current_tetromino%block2(1), current_tetromino%center(2) + current_tetromino%block2(2)) = current_tetromino%color
            board(current_tetromino%center(1) + current_tetromino%block3(1), current_tetromino%center(2) + current_tetromino%block3(2)) = current_tetromino%color
            board(current_tetromino%center(1) + current_tetromino%block4(1), current_tetromino%center(2) + current_tetromino%block4(2)) = current_tetromino%color
            ! handle switching tetrominos
            deallocate(current_tetromino)
            call move_alloc(next_tetromino, current_tetromino)
            call random_number(random_real)
            next_tetromino = tetromino(floor(random_real*7))

            ! handle complete lines

            do y=1,board_height
               do x=1,board_width
                  if (board(x, y) == 15) exit
               end do
               if (x == board_width+1) then
                  ! we have a completed row
                  do x0=1,board_width
                     board(x0, y) = 15
                  end do
                  do y0=y,2,-1
                     print *, y0
                     do x0=1, board_width
                        board(x0, y0) = board(x0, y0-1)
                     end do
                  end do
               end if
            end do
            
         end if
      end if

      if (is_key_pressed(key_a) .or. is_key_pressed_repeat(key_a)) then
         call current_tetromino%go_left()
         if (check_for_intersection_or_bounds()) then
            call current_tetromino%go_right()
         end if
      end if

      if (is_key_pressed(key_d) .or. is_key_pressed_repeat(key_d)) then
         call current_tetromino%go_right()
         if (check_for_intersection_or_bounds()) then
            call current_tetromino%go_left()
         end if
      end if

      if (is_key_pressed(key_s) .or. is_key_pressed_repeat(key_s)) then
         call current_tetromino%go_down()
         if (check_for_intersection_or_bounds()) then
            call current_tetromino%go_up()
         end if
      end if

      if (is_key_pressed(key_q) .or. is_key_pressed_repeat(key_q)) then
         call current_tetromino%rotate()
         if (check_for_intersection_or_bounds()) then
            call current_tetromino%unrotate()
         end if
      end if

      if (is_key_pressed(key_e) .or. is_key_pressed_repeat(key_e)) then
         call current_tetromino%unrotate()
         if (check_for_intersection_or_bounds()) then
            call current_tetromino%rotate()
         end if
      end if

      ! draw everything
      call begin_drawing()
         call clear_background(lightgray)
         call draw_text('Hello, world!' // c_null_char, 190, 200, 20, darkgray)

         call draw_rectangle((board_width + 2)*block_size, (4 + 2)*block_size, block_size*4, block_size*4, black)


         ! draw board + blocks at bottom
         do y=1,board_height
            do x=1,board_width
               call draw_rectangle(x*block_size, (y-1)*block_size+1, block_size-1, block_size-1, color_list(board(x,y)))
            end do
         end do

         ! draw currently controlled tetromino
         call draw_rectangle((current_tetromino%center(1) + current_tetromino%block1(1))*block_size, (current_tetromino%center(2) + current_tetromino%block1(2) - 1)*block_size+1, block_size-1, block_size-1, color_list(current_tetromino%color))
         call draw_rectangle((current_tetromino%center(1) + current_tetromino%block2(1))*block_size, (current_tetromino%center(2) + current_tetromino%block2(2) - 1)*block_size+1, block_size-1, block_size-1, color_list(current_tetromino%color))
         call draw_rectangle((current_tetromino%center(1) + current_tetromino%block3(1))*block_size, (current_tetromino%center(2) + current_tetromino%block3(2) - 1)*block_size+1, block_size-1, block_size-1, color_list(current_tetromino%color))
         call draw_rectangle((current_tetromino%center(1) + current_tetromino%block4(1))*block_size, (current_tetromino%center(2) + current_tetromino%block4(2) - 1)*block_size+1, block_size-1, block_size-1, color_list(current_tetromino%color))

         ! draw next tetromino
         call draw_rectangle(((board_width + 3) + next_tetromino%block1(1))*block_size, ((4 + 3) + next_tetromino%block1(2))*block_size, block_size-1, block_size-1, color_list(next_tetromino%color))
         call draw_rectangle(((board_width + 3) + next_tetromino%block2(1))*block_size, ((4 + 3) + next_tetromino%block2(2))*block_size, block_size-1, block_size-1, color_list(next_tetromino%color))
         call draw_rectangle(((board_width + 3) + next_tetromino%block3(1))*block_size, ((4 + 3) + next_tetromino%block3(2))*block_size, block_size-1, block_size-1, color_list(next_tetromino%color))
         call draw_rectangle(((board_width + 3) + next_tetromino%block4(1))*block_size, ((4 + 3) + next_tetromino%block4(2))*block_size, block_size-1, block_size-1, color_list(next_tetromino%color))
               
      call end_drawing()
   end do

   call close_window()
contains

function check_for_intersection_or_bounds()
   logical :: check_for_intersection_or_bounds
   integer :: block1x, block1y, block2x, block2y, block3x, block3y, block4x, block4y

   ! calculate blocks out
   block1x = current_tetromino%center(1) + current_tetromino%block1(1)
   block2x = current_tetromino%center(1) + current_tetromino%block2(1)
   block3x = current_tetromino%center(1) + current_tetromino%block3(1)
   block4x = current_tetromino%center(1) + current_tetromino%block4(1)

   block1y = current_tetromino%center(2) + current_tetromino%block1(2)
   block2y = current_tetromino%center(2) + current_tetromino%block2(2)
   block3y = current_tetromino%center(2) + current_tetromino%block3(2)
   block4y = current_tetromino%center(2) + current_tetromino%block4(2)

   ! default case
   check_for_intersection_or_bounds = .false.

   ! check for side bounds
   if (block1x < 1 .or. block2x < 1 .or. block3x < 1 .or. block4x < 1 .or. block1x > board_width .or. block2x > board_width .or. block3x > board_width .or. block4x > board_width) then
      check_for_intersection_or_bounds = .true.
      return
   end if

   ! check for bottom bounds
   if (block1y > board_height .or. block2y > board_height .or. block3y > board_height .or. block4y > board_height) then
      check_for_intersection_or_bounds = .true.
      return
   end if

   ! avoid segfault
   if (block1y < 1) then
      block1y = 1
   end if
   if (block2y < 1) then
      block2y = 1
   end if
   if (block3y < 1) then
      block3y = 1
   end if
   if (block4y < 1) then
      block4y = 1
   end if

   ! check for intersection
   if (board(block1x, block1y) /= 15 .or. board(block2x, block2y) /= 15 .or. board(block3x, block3y) /= 15 .or. board(block4x, block4y) /= 15) then
      check_for_intersection_or_bounds = .true.
      return
   end if
   
end function
end program
