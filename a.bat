@echo off
echo Compilando o Jogo da Forca...

cd JogoDaForca

rem Compila o jogo usando o GHC
ghc -o ..\jogo.exe Main.hs -package directory  

rem Verifica se a compilação foi bem-sucedida
if %errorlevel% equ 0 (
    echo Compilacao concluida com sucesso!
    echo Executavel criado: jogo.exe
    
    rem Remove os arquivos intermediários
    echo Limpando arquivos intermediarios...
    del *.o
    del *.hi
    
    echo Limpeza concluida.
    
    cd ..
    
    echo.
    echo Iniciando o jogo...
    echo ==============================

    @REM timeout /t 1 >nul
    cls
    
    rem Executa o jogo
    jogo.exe
) else (
    echo Falha na compilacao! Verifique os erros acima.
    cd ..
)