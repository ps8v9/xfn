set GNUCOBOL=C:\OpenCobol
set COB_CONFIG_DIR=%GNUCOBOL%\config

cobc -I %GNUCOBOL% -L %GNUCOBOL% -x -O2 xfn.cbl
