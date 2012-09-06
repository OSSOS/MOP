      open (unit=1, file='keep-f2c', status='unknown')

      write (1, '(a)') '\\rm delete-it'
      write (1, '(a)') '\\rm check-f2c'

      close (1)

      end
