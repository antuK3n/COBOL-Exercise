@echo off

if exist "main_data.db" (

    del "main_data.db"
    echo Successfully deleted main_data.db.
) else (
    echo File main_data.db does not exist.
)


echo The window will close in 5 seconds...
for /L %%i in (5,-1,1) do (
    echo %%i...
    timeout /t 1 >nul
)
exit
