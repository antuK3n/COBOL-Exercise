@echo off

if exist "app_data.db" (
    del "app_data.db"
    echo Successfully deleted app_data.db.
) else (
    echo File app_data.db does not exist.
)

echo The window will close in 5 seconds...
for /L %%i in (5,-1,1) do (
    echo %%i...
    timeout /t 1 >nul
)
exit
