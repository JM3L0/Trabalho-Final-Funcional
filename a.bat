@echo off
echo Compilando o Jogo da Forca...

cd divididos

rem Compila o jogo usando o GHC
ghc -o ..\jogo.exe Main.hs -package directory  

rem Verifica se a compilação foi bem-sucedida
if %errorlevel% equ 0 (
    echo Compilação concluída com sucesso!
    echo Executável criado: jogo.exe
    
    rem Remove os arquivos intermediários
    echo Limpando arquivos intermediários...
    del *.o
    del *.hi
    
    echo Limpeza concluída.
    
    cd ..
    
    echo.
    echo Iniciando o jogo...
    echo ==============================
    
    rem Executa o jogo
    jogo.exe
) else (
    echo Falha na compilação! Verifique os erros acima.
    cd ..
)