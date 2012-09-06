C -*-compile-command: "make_lib"; -*-
c
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine checks the MOP version of the file against that of the
c program.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      subroutine check_version (mopvers, filename, strl, code)

      implicit none


      real*4 MOP_version

      real*4
     $  mopvers

      integer*4
     $  code, strl

      character
     $  filename*100

      MOP_version = 1.20

      code = 0
      if (mopvers .ne. MOP_version) then
         write (6, *) 'MOP version of file '//filename(1:strl)//' (',
     $     mopvers, ') and program (', MOP_version, ') mismatch.'
         code = 1
         stop
      end if

      return
      end
