@echo off
echo Starting complete cache cleanup for Easy NPC project...
echo.

echo Step 1: Running deepClean task...
call gradlew.bat -p core clean
call gradlew.bat -p config-ui clean
call gradlew.bat -p bundle clean
if %ERRORLEVEL% neq 0 (
    echo DeepClean failed, but continuing...
)

echo.
echo Step 2: Stopping Gradle daemon...
call gradlew.bat --stop

echo.
echo Step 3: Manually removing .gradle cache directories...
if exist ".gradle" (
    echo Removing .gradle directory...
    rmdir /s /q ".gradle"
)

if exist "core\.gradle" (
    echo Removing core\.gradle directory...
    rmdir /s /q "core\.gradle"
)

if exist "config-ui\.gradle" (
    echo Removing config-ui\.gradle directory...
    rmdir /s /q "config-ui\.gradle"
)

if exist "bundle\.gradle" (
    echo Removing bundle\.gradle directory...
    rmdir /s /q "bundle\.gradle"
)

echo.
echo Step 4: Removing additional cache directories...
for /d /r . %%d in (build) do (
    if exist "%%d" (
        echo Removing %%d...
        rmdir /s /q "%%d" 2>nul
    )
)

for /d /r . %%d in (out) do (
    if exist "%%d" (
        echo Removing %%d...
        rmdir /s /q "%%d" 2>nul
    )
)

echo.
echo ============================================
echo Complete cache cleanup finished!
echo All modules (core, config-ui, bundle) have been cleaned.
echo You can now run your builds with fresh caches.
echo ============================================
pause
