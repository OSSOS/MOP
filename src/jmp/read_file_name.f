C -*-compile-command: "make_lib"; -*-

      subroutine read_file_name (base_name, i1, i2, finished, len)

      implicit none

      integer*4
     $  i1, i2, len

      character
     $  base_name*(*)

      logical
     $  finished

      finished = .false.
      i1 = 1
 100  continue
      if ((base_name(i1:i1) .eq. char(0))
     $  .or. (base_name(i1:i1) .eq. char(9))
     $  .or. (base_name(i1:i1) .eq. ' ')) then
         i1 = i1 + 1
         if (i1 .eq. len) then
            finished = .true.
            return
         end if
         goto 100
      end if
 101  continue

      i2 = i1 + 1
 110  continue
      if ((base_name(i2:i2) .ne. char(0))
     $  .and. (base_name(i2:i2) .ne. char(9))
     $  .and. (base_name(i2:i2) .ne. ' ')) then
         i2 = i2 + 1
         if (i2 .eq. len) goto 111
         goto 110
      end if
 111  continue
      i2 = i2 - 1

      return
      end
