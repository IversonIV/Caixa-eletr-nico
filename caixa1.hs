module Main (main) where
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))
import System.Environment

type Estado = Double                                 

main :: IO()
main = do hSetBuffering stdout NoBuffering
          putStrLn "Simulacao de caixa automatico"
          args <- getArgs
          let file = (args !! 0)
          contents <- readFile file
          if contents == ""
            then menu 0.0
                else do let estadoInicial = read(contents)::Estado
                        menu estadoInicial
          return ()
                        
          
        
menu :: Estado -> IO Estado
menu estado = do putStrLn "============================="
                 putStrLn "    Banco Haskell S.A."
                 putStrLn "============================="
                 putStrLn "Opções:\n"
                 putStrLn "1. Saldo\n2. Depósito\n3. Saque\n4. Fim\n"
                 putStrLn "Escolha uma opção:"
                 opcao <- getLine

                 if opcao == "1" 
                    then do saldo estado
                            menu estado
                        else 
                            if opcao == "2" 
                            then do estado <- deposito estado
                                    menu estado
                                else 
                                    if opcao == "3" 
                                        then do estado <- saque estado
                                                menu estado
                                            else 
                                                if opcao == "4"
                                                    then do putStrLn "Obrigado por utilizar o nosso banco"
                                                            args <- getArgs
                                                            writeFile (args !! 0) (show estado)
                                                            return 0
                                                        else do putStrLn "Opção inválida!"
                                                                menu estado


saldo :: Estado -> IO Estado
saldo estado = do putStr "Saldo atual:" 
                  print estado
                  return estado


deposito :: Estado -> IO Estado
deposito estado = do putStr "Informe o valor do deposito:" 
                     valorDeposito <- readLn
                     let deposito = valorDeposito::Estado
                     if deposito < 0.0
                        then do putStrLn "Valor de deposito invalido."
                                return estado
                            else return (estado + deposito)

saque :: Estado -> IO Estado
saque estado = do putStr "Informe o valor do saque:" 
                  valorSaque <- readLn
                  let saque = valorSaque::Estado
                    
                  if saque < 0.0 
                    then do putStrLn "Valor de saque invalido."
                            return estado
                        else 
                            if estado - saque < 0.0
                                then do putStrLn "Saldo insuficiente para saque."
                                        return estado
                                        else return (estado - saque)


