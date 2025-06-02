@echo off
echo Compilando o Jogo da Forca...

REM Muda para o diretório dos arquivos fonte
cd JogoDaForca

REM Compila o jogo usando o GHC com o pacote directory para funções de sistema de arquivos
ghc -o ..\jogo.exe Main.hs -package directory  

REM Verifica se a compilação foi bem-sucedida
if %errorlevel% equ 0 (
    echo Compilacao concluida com sucesso!
    echo Executavel criado: jogo.exe
    
    REM Remove os arquivos intermediários (.o e .hi)
    echo Limpando arquivos intermediarios...
    del *.o
    del *.hi
    
    echo Limpeza concluida.
    
    REM Volta para o diretório principal
    cd ..
    
    echo.
    cls
    echo Iniciando o jogo...
    echo ==============================

    REM Pausa por 1 segundo
    timeout /t 1 >nul
    cls
    
    REM Executa o jogo
    jogo.exe
) else (
    echo Falha na compilacao! Verifique os erros acima.
    cd ..
)