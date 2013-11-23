xfn
===

COBOL procedures that mimic X"F4" and X"F5".

As tested in GNU COBOL 1.1 in Windows 7, with optimization level -O2,
the performance differential is not too bad. The XF4 and XF5 procedures
are still about 5 times slower than the X"F4" and X"F5" library
routines. However, that's much better than the first drafts of the XF4
and XF5 procedures, which were > 500 times slower than X"F4" and X"F5".
No testing has been done yet in Linux.
