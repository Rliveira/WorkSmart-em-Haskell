module Main where

-- Equipe --
-- Rony Elias de Oliveira
-- João Victor Morais B. da Silva

import Control.Exception (IOException, catch)
import Data.List (intercalate, nub, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Time 
import Text.Printf
import Text.Read (readMaybe)

main :: IO ()
main = do
  banco <- carregarDados
  _ <- menuPrincipal banco
  return ()

-- Tipos de Dados:
type IDTarefa = String
type IDFuncionario = String
type IDRegistro = String
type Nome = String
type Cargo = String
type Departamento = String
type Dia = Day -- Para datas no formato "AAAA-MM-DD"
type Hora = TimeOfDay -- Para horas no formato "HH:MM"
type Titulo = String
type Descricao = String
type Prazo = Int
type EstadoTarefa = String
type Nota = Float -- Uma nota (de 0 a 10) para avaliar a tarefa feita pelo funcionário

-- Entidades:

data Funcionario = Funcionario
  { getIdFuncionario :: IDFuncionario,
    getNomeFuncionario :: Nome,
    getCargoFuncionario :: Cargo,
    getDepartamentoFuncionario :: Departamento
  }
  deriving (Show, Read)

data Registro = Registro
  { getIdRegistro :: IDTarefa,
    getFuncionario :: Funcionario,
    getDataRegistro :: Dia,
    getHoraEntrada :: Hora,
    getHoraSaida :: Maybe Hora
  }
  deriving (Show, Read)

data Tarefa = Tarefa
  { getIdTarefa :: IDTarefa,
    getTitulo :: Titulo,
    getNota :: Nota, -- Uma nota de 0.0 a 10 avaliando o serviço prestado na tarefa
    getDescricaoTarefa :: Descricao,
    getDataAtribuicao :: Dia, -- Novo atributo para indicar o dia em que a tarefa foi atribuída
    getDataParaConclusao :: Dia, -- data para a conclusão da tarefa
    getDataDeEfetivacao :: Maybe Dia, -- data em que a tarefa foi efetivada/concluída pelo funcionário
    getEstadoTarefa :: EstadoTarefa, -- Estados possíveis: Em progresso, Concluído ou Não realizado
    getAtribuidor :: Funcionario, -- funcionário que atribuiu a tarefa
    getExecutor :: [Funcionario] -- funcionário(s) responsáveis por executar a tarefa
  }
  deriving (Show, Read)

type Banco = ([Funcionario], [Tarefa], [Registro])

inicializarBanco :: Banco -- Banco de todas as entidades (Funcionario, Tarefa, Registro)
inicializarBanco = ([], [], []) -- Inicializa o banco vazio

-- Função para salvar os dados no arquivo
salvarDados :: Banco -> IO ()
salvarDados banco = catch (writeFile "DadosWorksmartAnalyzer.txt" (show banco)) handler
  where
    handler :: IOException -> IO ()
    handler _ = putStrLn "Erro ao salvar os dados!"

-- Função para carregar os dados do arquivo
carregarDados :: IO Banco
carregarDados =
  catch
    ( do
        contents <- readFile "DadosWorksmartAnalyzer.txt"
        case readMaybe contents of
          Just banco -> do
            putStrLn "Dados carregados com sucesso."
            return banco
          Nothing -> do
            putStrLn "arquivo .txt não encontrado para a leitura de dados"
            return inicializarBanco
    )
    handler
  where
    handler :: IOException -> IO Banco
    handler _ = do
      putStrLn "Erro ao carregar os dados!"
      return ([], [], [])

-- Função para adicionar um funcionário
adicionarFuncionario :: Funcionario -> Banco -> [Funcionario]
adicionarFuncionario novoFuncionario (funcionarios, _, _) = 
  let funcionariosAtualizados = novoFuncionario : funcionarios
  in funcionariosAtualizados
  
-- Função para editar os dados de um funcionário
editarDadosFuncionario :: IDFuncionario -> Nome -> Cargo -> Departamento -> Banco -> [Funcionario]
editarDadosFuncionario myid novoNome novoCargo novoDepartamento (funcionarios, _, _) =
  map
    ( \f ->
        if getIdFuncionario f == myid
          then
            f
              { getNomeFuncionario = novoNome,
                getCargoFuncionario = novoCargo,
                getDepartamentoFuncionario = novoDepartamento
              }
          else f
    )
    funcionarios

-- Funções para gerar os IDs das entidades:

-- Extrai o valor numérico de um ID retornando apenas a parte inteira
extrairValorNumerico :: String -> Int
extrairValorNumerico = read . filter (`elem` ['0' .. '9'])

-- Função genérica para gerar um novo ID baseado no último elemento de uma lista
gerarNovoIDGenerico :: [String] -> String -> String
gerarNovoIDGenerico [] sufixo = "1" ++ sufixo
gerarNovoIDGenerico ids sufixo =
  let primeiroId = head ids
      valorNumerico = extrairValorNumerico primeiroId
      novoValor = valorNumerico + 1
   in show novoValor ++ sufixo

-- Funções para gerar IDs únicos para cada entidade
gerarNovoIDFuncionario :: [Funcionario] -> IDFuncionario
gerarNovoIDFuncionario funcionarios =
  let ids = map getIdFuncionario funcionarios
   in gerarNovoIDGenerico ids "f"

gerarNovoIDTarefa :: [Tarefa] -> IDTarefa
gerarNovoIDTarefa tarefas =
  let ids = map getIdTarefa tarefas
   in gerarNovoIDGenerico ids "t"

gerarNovoIDRegistro :: [Registro] -> IDRegistro
gerarNovoIDRegistro registros =
  let ids = map getIdRegistro registros
   in gerarNovoIDGenerico ids "r"

-- Função para verificar se um ID de funcionário existe na lista de funcionários
verificarIDFuncionario :: IDFuncionario -> [Funcionario] -> Bool
verificarIDFuncionario myid funcionarios = any (\f -> getIdFuncionario f == myid) funcionarios

-- Função para verificar se um ID de tarefa existe na lista de tarefas
verificarIDTarefa :: IDTarefa -> [Tarefa] -> Bool
verificarIDTarefa myid tarefas = any (\t -> getIdTarefa t == myid) tarefas

-- Função para verificar se um ID de registro existe na lista de registros
verificarIDRegistro :: IDRegistro -> [Registro] -> Bool
verificarIDRegistro myid registros = any (\r -> getIdRegistro r == myid) registros

{- Exemplo da lógica para gerar um novo ID único:
  Suponha que temos a lista de IDs da entidade tarefa ["3t", "2t", "1t"].

  1. Pegamos o primeiro ID da lista: 3t
  2. Extraímos o valor numérico desse ID e convertemos para inteiro: 3
  3. Incrementamos 1: 3 + 1 = 4
  4. Convertemos 4 para string e adicionamos o sufixo "t" para formar "4t"
  5. Novo ID gerado para uma nova entidade do tipo tarefa: "4t"
  6. Se a lista de IDs for vazia o ID gerado será "1t"
-}

-- Lista todos os funcionários cadastradas
listarFuncionarios :: Banco -> IO ()
listarFuncionarios (funcionarios, _, _) = do
  if null funcionarios
    then putStrLn "Não há funcionários cadastrados"
    else do
      putStrLn "\nLista de Funcionários:\n"
      putStrLn $ printf "%-5s | %-25s | %-15s | %-20s" "ID" "Nome" "Cargo" "Departamento"
      putStrLn $ replicate 72 '-'
      mapM_ exibirFuncionario funcionarios
      putStrLn ""

-- Função auxiliar para formatar a saída de um funcionário
exibirFuncionario :: Funcionario -> IO ()
exibirFuncionario f = do
  let idFuncionario = getIdFuncionario f
      nomeFuncionario = getNomeFuncionario f
      cargoFuncionario = getCargoFuncionario f
      departamentoFuncionario = getDepartamentoFuncionario f
  putStrLn $
    printf
      "%-5s | %-25s | %-15s | %-20s"
      idFuncionario
      nomeFuncionario
      cargoFuncionario
      departamentoFuncionario

-- Função Listar Todos os Registros --
listarTodosRegistros :: Banco -> IO ()
listarTodosRegistros (_, _, registros) = do
  if null registros
    then putStrLn "Não há registros cadastrados."
    else do
      putStrLn "\nLista de Todos os Registros:\n"
      mapM_
        ( \r ->
            putStrLn $
              "ID Registro: "
                ++ getIdRegistro r
                ++ "\nID Funcionário: "
                ++ getIdFuncionario (getFuncionario r)
                ++ "\nNome: "
                ++ getNomeFuncionario (getFuncionario r)
                ++ "\nData: "
                ++ formatarData (getDataRegistro r)
                ++ "\nHora de Entrada: "
                ++ formatarHora (getHoraEntrada r)
                ++ "\nHora de Saída: "
                ++ maybe "Não registrado" formatarHora (getHoraSaida r)
                ++ "\n"
        )
        registros
      putStrLn ""

-- Função para listar todas as tarefas
listarTodasTarefas :: Banco -> IO ()
listarTodasTarefas (_, tarefas, _) = do
  putStrLn "\nListar Todas as Tarefas\n"

  if null tarefas
    then putStrLn "Não há tarefas cadastradas."
    else imprimirTarefas tarefas

-- Função para obter a data atual no formato "AAAA-MM-DD" --
obterDataAtual :: IO Dia
obterDataAtual = do
  now <- getCurrentTime
  return (utctDay now)

-- Função para obter a hora atual
obterHoraAtual :: IO TimeOfDay
obterHoraAtual = do
  now <- getZonedTime  -- Obtém a hora atual com o fuso horário correto
  let localTime = zonedTimeToLocalTime now
      timeOfDay = localTimeOfDay localTime
      
      -- gambiarra pra subtrair 3 horas do horário obtido para se reajustar com o horário atual
      timeOfDayAdjustado = timeOfDay {todHour = (todHour timeOfDay - 3) `mod` 24}
  return timeOfDayAdjustado

-- Função para formatar uma data do tipo Day para o formato dd-mm-yyyy
formatarData :: Day -> String
formatarData dataDia =
  let formatoSaida = "%d-%m-%Y"
  in formatTime defaultTimeLocale formatoSaida dataDia

-- Função para formatar uma hora do tipo TimeOfDay para o formato hh:mm:ss
formatarHora :: TimeOfDay -> String
formatarHora hora = formatTime defaultTimeLocale "%H:%M:%S" hora

-- Função que verifica se os campos preenchidos pelo usuário estão vazios ou preenchidos apenas com espaços
camposValidos :: [String] -> Bool
camposValidos = all (not . null . trim)
  where
    trim = f . f
      where
        f = reverse . dropWhile (== ' ')

-- Função verifica se a String passada só contém valores numéricos
validarNota :: String -> Bool
validarNota str = case span (`elem` ['0' .. '9']) str of
  (xs, "") -> not (null xs)
  (xs, '.' : ys) -> not (null xs) && not (null ys) && all (`elem` ['0' .. '9']) ys
  _ -> False

-- Menu Principal
menuPrincipal :: Banco -> IO Banco
menuPrincipal banco = do
  putStrLn "\n------------------------------------------------------------------------"
  putStrLn "\n                          WorkSmart Analyzer                           "
  putStrLn "            Sistema de Análise de Produtividade de funcionários           \n"
  putStrLn "------------------------------------------------------------------------\n"
  putStrLn "1. Gerenciar Funcionários"
  putStrLn "2. Gerenciar Registros de Ponto"
  putStrLn "3. Gerenciar Tarefas"
  putStrLn "4. Consultar Produtividade"
  putStrLn "5. Sair\n"
  putStr "Escolha uma opção: "

  opcao <- getLine
  putStrLn "\n------------------------------------------------------------------------"

  case opcao of
    "1" -> subMenuFuncionarios banco
    "2" -> subMenuRegistros banco
    "3" -> subMenuTarefas banco
    "4" -> subMenuProdutividade banco
    "5" -> putStrLn "Saindo..." >> return banco
    _ -> do
      putStrLn "\nOpção inválida, tente novamente."
      menuPrincipal banco

subMenuFuncionarios :: Banco -> IO Banco
subMenuFuncionarios banco = do
  putStrLn "\n------------------------------------------------------------------------"
  putStrLn "\nGerenciar Funcionários\n"
  putStrLn "1. Registrar Funcionário"
  putStrLn "2. Editar Funcionário"
  putStrLn "3. Listar Funcionários"
  putStrLn "4. Voltar ao Menu Principal\n"
  putStr "Escolha uma opção: "
  opcao <- getLine
  putStrLn "\n------------------------------------------------------------------------"

  case opcao of
    "1" -> registrarFuncionario banco
    "2" -> editarFuncionario banco
    "3" -> listarFuncionarios banco >> subMenuFuncionarios banco
    "4" -> menuPrincipal banco
    _ -> do
      putStrLn "Opção inválida, tente novamente."
      subMenuFuncionarios banco

-- Função para verificar se já existe um funcionário com o mesmo nome, cargo e departamento
verificarFuncionarioDuplicado :: Nome -> Cargo -> Departamento -> [Funcionario] -> Bool
verificarFuncionarioDuplicado nome cargo departamento funcionarios =
  any
    ( \f ->
        getNomeFuncionario f == nome
          && getCargoFuncionario f == cargo
          && getDepartamentoFuncionario f == departamento
    )
    funcionarios

-- Função para registrar um novo funcionário
registrarFuncionario :: Banco -> IO Banco
registrarFuncionario banco@(funcionarios, tarefas, registros) = do
  putStrLn "\n------------------------------------------------------------------------"
  putStrLn "\nRegistrar Funcionário\n"
  putStr "Digite o Nome do Funcionário: "
  nome <- getLine
  putStr "Digite o Cargo do Funcionário: "
  cargo <- getLine
  putStr "Digite o Departamento do Funcionário: "
  departamento <- getLine

  -- Verifica se campos não estão vazios ou preenchidos com espaço
  if not (camposValidos [nome, cargo, departamento])
    then do
      putStrLn "\nErro: Todos os campos devem ser preenchidos corretamente!"
      subMenuFuncionarios banco
    else
      if verificarFuncionarioDuplicado nome cargo departamento funcionarios
        then do
          putStrLn "\nErro: Funcionário já cadastrado com o mesmo nome, cargo e departamento!"
          subMenuFuncionarios banco
        else do
          let myid = gerarNovoIDFuncionario funcionarios
          let novoFuncionario = Funcionario myid nome cargo departamento

          putStrLn $ "\nConfirme os dados abaixo para o novo funcionário:\n"
          putStrLn $ " - Nome: " ++ nome
          putStrLn $ " - Cargo: " ++ cargo
          putStrLn $ " - Departamento: " ++ departamento
          putStrLn "\nOs dados estão corretos?"
          putStrLn "1. Sim"
          putStrLn "2. Não\n"
          putStr "Escolha uma opção: "
          confirmacao <- getLine

          case confirmacao of
            "1" -> do
              let funcionariosAtualizado = adicionarFuncionario novoFuncionario banco
              let novoBanco = (funcionariosAtualizado, tarefas, registros)
              putStrLn "\nfuncionário cadastrado com sucesso!\n"
              salvarDados novoBanco
              subMenuFuncionarios novoBanco
            "2" -> do
              putStrLn "\nRegistro de funcionário cancelado."
              subMenuFuncionarios banco
            _ -> do
              putStrLn "\nOpção inválida, tente novamente."
              subMenuFuncionarios banco

-- Função para editar os dados de um funcionário
editarFuncionario :: Banco -> IO Banco
editarFuncionario banco@(funcionarios, tarefas, registros) = do

  -- Verifica se a lista de funcionários está vazia
  if null funcionarios
    then do
      putStrLn "Erro: Não há funcionários cadastrados!"
      subMenuFuncionarios banco
    else do
      listarFuncionarios banco
      putStr "Digite o ID do Funcionário a ser editado: "
      myid <- getLine

      -- Verifica se o ID existe na lista de funcionários
      if any (\f -> getIdFuncionario f == myid) funcionarios
        then do
          putStr "Digite o novo Nome do Funcionário: "
          novoNome <- getLine
          putStr "Digite o novo Cargo do Funcionário: "
          novoCargo <- getLine
          putStr "Digite o novo Departamento do Funcionário: "
          novoDepartamento <- getLine

          -- Verifica se campos não estão vazios ou preenchidos com espaço
          if not (camposValidos [novoNome, novoCargo, novoDepartamento])
            then do
              putStrLn "\nErro: Todos os campos devem ser preenchidos corretamente!"
              subMenuFuncionarios banco
            else
              if verificarFuncionarioDuplicado novoNome novoCargo novoDepartamento funcionarios
                then do
                  putStrLn "\nErro: Existe um outro funcionário com o mesmo nome, cargo e departamento já cadastrado!"
                  subMenuFuncionarios banco
                else do
                  putStrLn $ "\nConfirme os novos dados abaixo para o funcionário:\n"
                  putStrLn $ " - Novo Nome: " ++ novoNome
                  putStrLn $ " - Novo Cargo: " ++ novoCargo
                  putStrLn $ " - Novo Departamento: " ++ novoDepartamento
                  putStrLn "\nOs dados estão corretos?"
                  putStrLn "1. Sim"
                  putStrLn "2. Não"
                  putStr "\nEscolha uma opção: "
                  confirmacao <- getLine

                  case confirmacao of
                    "1" -> do
                      let funcionariosAtualizados = editarDadosFuncionario myid novoNome novoCargo novoDepartamento banco
                      let tarefasAtualizadas = atualizarTarefas myid novoNome novoCargo novoDepartamento tarefas
                      let registrosAtualizados = atualizarRegistros myid novoNome novoCargo novoDepartamento registros

                      let novoBanco = (funcionariosAtualizados, tarefasAtualizadas, registrosAtualizados)
                      putStrLn "Funcionário editado com sucesso!"
                      salvarDados novoBanco
                      subMenuFuncionarios novoBanco
                    "2" -> do
                      putStrLn "\nEdição de funcionário cancelada."
                      subMenuFuncionarios banco
                    _ -> do
                      putStrLn "\nOpção inválida, tente novamente."
                      subMenuFuncionarios banco
        else do
          putStrLn "\nErro: Funcionário não encontrado!"
          putStrLn "\nCertifique-se de digitar o ID corretamente"
          subMenuFuncionarios banco

-- Função para atualizar as tarefas com os novos dados do funcionário
atualizarTarefas :: IDFuncionario -> Nome -> Cargo -> Departamento -> [Tarefa] -> [Tarefa]
atualizarTarefas myid novoNome novoCargo novoDepartamento =
  map $ \tarefa ->
    tarefa
      { getAtribuidor =
          if getIdFuncionario (getAtribuidor tarefa) == myid
            then Funcionario myid novoNome novoCargo novoDepartamento
            else getAtribuidor tarefa,
        getExecutor =
          map
            ( \f ->
                if getIdFuncionario f == myid
                  then Funcionario myid novoNome novoCargo novoDepartamento
                  else f
            )
            (getExecutor tarefa)
      }

-- Função para atualizar os registros com os novos dados do funcionário
atualizarRegistros :: IDFuncionario -> Nome -> Cargo -> Departamento -> [Registro] -> [Registro]
atualizarRegistros myid novoNome novoCargo novoDepartamento =
  map $ \registro ->
    registro
      { getFuncionario =
          if getIdFuncionario (getFuncionario registro) == myid
            then Funcionario myid novoNome novoCargo novoDepartamento
            else getFuncionario registro
      }

subMenuRegistros :: Banco -> IO Banco
subMenuRegistros banco = do
  putStrLn "\n------------------------------------------------------------------------"
  putStrLn "\nGerenciar Registros de Ponto\n"
  putStrLn "1. Registrar Entrada de Funcionário"
  putStrLn "2. Registrar Saída de Funcionário"
  putStrLn "3. Listar Todos os Registros"
  putStrLn "4. Listar Funcionários que Registraram Entrada no Dia Atual"
  putStrLn "5. Listar Funcionários que Registraram Saída no Dia Atual"
  putStrLn "6. Voltar ao Menu Principal\n"
  putStr "Escolha uma opção: "
  opcao <- getLine
  case opcao of
    "1" -> do
      registrarEntrada banco
    "2" -> do
      registrarSaida banco
    "3" -> do
      listarTodosRegistros banco
      subMenuRegistros banco
    "4" -> do
      listarFuncionariosComEntrada banco
      subMenuRegistros banco
    "5" -> do
      listarFuncionariosComSaida banco
      subMenuRegistros banco
    "6" -> menuPrincipal banco
    _ -> do
      putStrLn "\nOpção inválida, tente novamente."
      subMenuRegistros banco

-- Função para verificar se um funcionário já registrou entrada no dia atual
jaRegistrouEntrada :: IDFuncionario -> Dia -> [Registro] -> Bool
jaRegistrouEntrada myid dataAtual = any (\r -> getIdFuncionario (getFuncionario r) == myid && getDataRegistro r == dataAtual && isNothing (getHoraSaida r))

-- Função para listar funcionários que ainda não registraram entrada no dia atual
listarFuncionariosSemRegistroEntrada :: Banco -> Dia -> [Funcionario]
listarFuncionariosSemRegistroEntrada (funcionarios, _, registros) dataAtual =
  filter (\f -> not (jaRegistrouEntrada (getIdFuncionario f) dataAtual registros)) funcionarios

listarFuncionariosSemEntrada :: Banco -> Dia -> IO Bool
listarFuncionariosSemEntrada (funcionarios, _, registros) dataAtual = do
  let registrosNoDia = filter (\r -> getDataRegistro r == dataAtual) registros
  let idsComEntrada = map (getIdFuncionario . getFuncionario) registrosNoDia
  let funcionariosSemEntrada = filter (\f -> not (getIdFuncionario f `elem` idsComEntrada)) funcionarios

  putStrLn "Funcionários sem registro de entrada no dia atual:"
  if null funcionariosSemEntrada
    then do
      putStrLn "Todos os funcionários têm registro de entrada hoje."
      return True
    else do
      mapM_ (\f -> putStrLn $ "ID: " ++ getIdFuncionario f ++ "- Nome: " ++ getNomeFuncionario f) funcionariosSemEntrada
      return False

-- Função que registra a entrada do funcionário na empresa
registrarEntrada :: Banco -> IO Banco
registrarEntrada banco@(funcionarios, tarefas, registros) = do
  dataAtual <- obterDataAtual
  putStrLn "\n------------------------------------------------------------------------"
  putStrLn "\nRegistrar Entrada de Funcionário\n"
  voltarSubMenuRegistros <- listarFuncionariosSemEntrada banco dataAtual

  -- se voltarSubMenuRegistros for True, o fluxo do código será interrompido e retonará ao submenu de registros
  if voltarSubMenuRegistros
    then subMenuRegistros banco
    else do
      putStr "Digite o ID do Funcionário: "
      idStr <- getLine

      -- Verifica se o campo ID não está vazio ou preenchido apenas com espaços
      if not (camposValidos [idStr])
        then do
          putStrLn "Erro: Todos os campos devem ser preenchidos corretamente!"
          subMenuRegistros banco
        else do
          let myid = idStr
          -- Verifica se o ID existe na lista de funcionários
          if verificarIDFuncionario myid funcionarios
            then do
              -- Verifica se o funcionário já registrou entrada no dia atual
              if not (jaRegistrouEntrada myid dataAtual registros)
                then do
                  horaAtual <- obterHoraAtual
                  let funcionario = head $ filter (\f -> getIdFuncionario f == myid) funcionarios
                  let novoRegistro = Registro (gerarNovoIDRegistro registros) funcionario dataAtual horaAtual Nothing
                  let novoBanco = (funcionarios, tarefas, novoRegistro : registros)
                  salvarDados novoBanco
                  putStrLn "\nEntrada registrada com sucesso!"
                  subMenuRegistros novoBanco
                else do
                  putStrLn "\nErro: Funcionário já registrou entrada no dia atual!"
                  subMenuRegistros banco
            else do
              putStrLn "\nErro: Funcionário não encontrado!"
              subMenuRegistros banco

-- Função para listar registros do dia atual sem saída e chamar o submenu de registros
listarRegistrosDoDiaAtualSemSaida :: Banco -> IO Bool
listarRegistrosDoDiaAtualSemSaida (_, _, registros) = do
  dataAtual <- obterDataAtual

  -- Filtrando os registros sem saída do dia atual
  let registrosSemSaida = filter (\r -> getDataRegistro r == dataAtual && isNothing (getHoraSaida r)) registros
      tuplasRegistros = map (\r -> (getIdRegistro r, getNomeFuncionario (getFuncionario r))) registrosSemSaida

  -- Verificando se há registros sem saída do dia atual
  if null tuplasRegistros
    then do
      putStrLn "Não há registros sem horário de saída para o dia atual."
      return True
    else do
      putStrLn "\nRegistros sem horário de saída para o dia atual:\n"
      mapM_ (\(idReg, nome) -> putStrLn $ "ID do registro: " ++ idReg ++ " - Nome: " ++ nome) tuplasRegistros
      putStrLn ""
      return False

-- Função para registrar a hora de saída de um funcionário
registrarSaida :: Banco -> IO Banco
registrarSaida banco@(funcionarios, tarefas, registros) = do
  putStrLn "\n------------------------------------------------------------------------"
  putStrLn "\nRegistrar Saída de Funcionário\n"
  voltarSubMenuRegistros <- listarRegistrosDoDiaAtualSemSaida banco

  -- se voltarSubMenuRegistros for True, o fluxo do código será interrompido e retonará ao submenu de registros
  if voltarSubMenuRegistros
    then subMenuRegistros banco
    else do
      putStr "Digite o ID do Registro: "
      idRegistroStr <- getLine

      -- Verifica se o campo ID do Registro não está vazio ou preenchido apenas com espaços
      if not (camposValidos [idRegistroStr])
        then do
          putStrLn "Erro: Todos os campos devem ser preenchidos corretamente!"
          subMenuRegistros banco
        else do
          let idRegistro = idRegistroStr

          -- Verifica se o ID do registro existe na lista de registros
          if verificarIDRegistro idRegistro registros
            then do
              let registroExistente = head $ filter (\r -> getIdRegistro r == idRegistro) registros

              -- Verifica se o funcionário já registrou entrada no dia atual
              let idFuncionario = getIdFuncionario (getFuncionario registroExistente)
              if not (jaRegistrouEntrada idFuncionario (getDataRegistro registroExistente) registros)
                then do
                  putStrLn "Erro: Funcionário não registrou entrada no dia atual!"
                  subMenuRegistros banco
                else do
                  horaSaidaAtual <- obterHoraAtual

                  -- Verifica se o horário de saída já foi registrado
                  if isJust (getHoraSaida registroExistente)
                    then do
                      putStrLn "Erro: Funcionário já possui horário de saída registrado no dia atual!"
                      subMenuRegistros banco
                    else do
                      let novoRegistro = registroExistente {getHoraSaida = Just horaSaidaAtual}
                          registrosAtualizados = novoRegistro : filter (\r -> getIdRegistro r /= idRegistro) registros

                      let novoBanco = (funcionarios, tarefas, registrosAtualizados)
                      salvarDados novoBanco
                      putStrLn "Saída registrada com sucesso!"
                      subMenuRegistros novoBanco
            else do
              putStrLn "Erro: Registro não encontrado!"
              subMenuRegistros banco

-- Função para listar todos os funcionários que registraram entrada no dia atual
listarFuncionariosComEntrada :: Banco -> IO ()
listarFuncionariosComEntrada (_, _, registros) = do
  dataAtual <- obterDataAtual

  -- Filtra os registros para encontrar aqueles com entrada registrada no dia atual
  let registrosComEntrada = filter (\r -> getDataRegistro r == dataAtual) registros

  -- Cria uma lista de pares (Nome do Funcionário, Hora de Entrada)
  let funcionariosComEntrada =
        [ (getNomeFuncionario (getFuncionario r), getHoraEntrada r)
          | r <- registrosComEntrada
        ]

  if null funcionariosComEntrada
    then putStrLn "Nenhum funcionário registrou entrada no dia atual."
    else do
      putStrLn "\nFuncionários que registraram entrada no dia atual:"
      mapM_ (\(nome, hora) -> putStrLn $ "Nome: " ++ nome ++ " - Hora de Entrada: " ++ formatarHora hora) funcionariosComEntrada

-- Função para listar todos os funcionários que registraram saída no dia atual
listarFuncionariosComSaida :: Banco -> IO ()
listarFuncionariosComSaida (_, _, registros) = do
  dataAtual <- obterDataAtual

  let registrosComSaida = filter (\r -> getDataRegistro r == dataAtual && isJust (getHoraSaida r)) registros

  -- Cria uma lista de pares (Nome do Funcionário, Hora de Saída)
  let funcionariosComSaida =
        [ (getNomeFuncionario (getFuncionario r), getHoraSaida r)
          | r <- registrosComSaida
        ]

  if null funcionariosComSaida
    then putStrLn "Nenhum funcionário registrou saída no dia atual."
    else do
      putStrLn "\nFuncionários que registraram saída no dia atual:"
      mapM_ (\(nome, hora) -> putStrLn $ " Nome: " ++ nome ++ " - Hora de Saída: " ++ maybe "Não registrado" formatarHora hora) funcionariosComSaida
