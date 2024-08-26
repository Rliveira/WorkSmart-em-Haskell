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

subMenuTarefas :: Banco -> IO Banco
subMenuTarefas banco = do
  putStrLn "\n------------------------------------------------------------------------"
  putStrLn "\nGerenciar Tarefas\n"
  putStrLn "1. Atribuir Tarefa a um Funcionário"
  putStrLn "2. Atualizar Estado de Tarefa"
  putStrLn "3. Listar Tarefas de um Funcionário"
  putStrLn "4. Listar Todas as Tarefas"
  putStrLn "5. Listar Tarefas Pendentes"
  putStrLn "6. Listar Tarefas Fora do Prazo"
  putStrLn "7. Voltar ao Menu Principal\n"
  putStr "Escolha uma opção: "
  opcao <- getLine

  case opcao of
    "1" -> atribuirTarefa banco
    "2" -> atualizarEstadoTarefa banco
    "3" -> listarTarefasDeUmFuncionario banco >> subMenuTarefas banco
    "4" -> listarTodasTarefas banco >> subMenuTarefas banco
    "5" -> listarTarefasPendentes banco >> subMenuTarefas banco
    "6" -> listarTarefasConcluidasForaDoPrazo banco >> subMenuTarefas banco
    "7" -> menuPrincipal banco
    _ -> do
      putStrLn "Opção inválida, tente novamente."
      subMenuTarefas banco

-- Função auxiliar para buscar um funcionário pelo ID
buscarFuncionarioPorID :: IDFuncionario -> [Funcionario] -> Maybe Funcionario
buscarFuncionarioPorID myid funcionarios =
  case filter (\f -> getIdFuncionario f == myid) funcionarios of
    (x : _) -> Just x
    [] -> Nothing

-- Função que converte string para dia
parseDia :: String -> Maybe Day
parseDia = parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- Função para remover espaços em branco do início e do fim de uma string
removerEspacos :: String -> String
removerEspacos = f . f
  where
    f = reverse . dropWhile (== ' ')

-- Função para atribuir uma tarefa a um ou mais funcionários
atribuirTarefa :: Banco -> IO Banco
atribuirTarefa banco@(funcionarios, tarefas, registros) = do
  putStrLn "\n------------------------------------------------------------------------"
  putStrLn "\nAtribuir Tarefa a um Funcionário\n"
  listarFuncionarios banco
  putStr "Digite o ID do Funcionário que irá atribuir a tarefa: "
  idAtribuidorStr <- getLine
  putStr "Digite o(s) ID(s) dos Funcionários que executarão a tarefa (separados por vírgula, pode ser um ou mais): "
  idsExecutoresStr <- getLine
  putStr "Digite o Título da Tarefa: "
  titulo <- getLine
  putStr "Digite a Descrição da Tarefa: "
  descricao <- getLine
  putStr "Digite a Data de Conclusão da Tarefa (AAAA-MM-DD): "
  dataConclusaoStr <- getLine
  dataAtual <- obterDataAtual

  -- Verifica se todos os campos foram preenchidos
  if not (camposValidos [idAtribuidorStr, titulo, descricao, dataConclusaoStr, idsExecutoresStr])
    then do
      putStrLn "Erro: Todos os campos devem ser preenchidos corretamente!"
      subMenuTarefas banco
    else case parseDia dataConclusaoStr of
      Nothing -> do
        putStrLn "Erro: A data de conclusão está no formato incorreto!"
        subMenuTarefas banco
      Just dataConclusao -> do
        -- Verifica se a data de conclusão é posterior à data atual
        if dataConclusao > dataAtual
          then do
            -- Verifica se o ID do atribuidor é válido
            case buscarFuncionarioPorID idAtribuidorStr funcionarios of
              Nothing -> do
                putStrLn "Erro: O ID do atribuidor não corresponde a nenhum funcionário registrado!"
                subMenuTarefas banco
              Just atribuidor -> do
                -- Verifica se todos os IDs dos executores são válidos
                let idsExecutores = map removerEspacos (splitOn "," idsExecutoresStr)
                let executoresValidos = mapMaybe (`buscarFuncionarioPorID` funcionarios) idsExecutores

                if length executoresValidos /= length idsExecutores
                  then do
                    putStrLn "Erro: Um ou mais IDs dos executores não correspondem a funcionários registrados!"
                    subMenuTarefas banco
                  else do
                    let novaTarefa = Tarefa (gerarNovoIDTarefa tarefas) titulo 0.0 descricao dataAtual dataConclusao Nothing "Pendente" atribuidor executoresValidos
                    let tarefasAtualizadas = novaTarefa : tarefas

                    putStrLn "\nConfirme os dados abaixo:\n"
                    putStrLn $ " - ID Tarefa: " ++ getIdTarefa novaTarefa
                    putStrLn $ " - Título: " ++ titulo
                    putStrLn $ " - Descrição: " ++ descricao
                    putStrLn $ " - Data de Conclusão: " ++ show dataConclusao
                    putStrLn $ " - ID Atribuidor: " ++ idAtribuidorStr
                    putStrLn $ " - IDs Executores: " ++ intercalate ", " idsExecutores
                    putStrLn "\nOs dados estão corretos?"
                    putStrLn "1. Sim"
                    putStrLn "2. Não"
                    putStr "\nEscolha uma opção: "
                    confirmacao <- getLine

                    case confirmacao of
                      "1" -> do
                        putStrLn "Tarefa atribuída com sucesso!"
                        let novoBanco = (funcionarios, tarefasAtualizadas, registros)
                        salvarDados novoBanco
                        subMenuTarefas novoBanco
                      "2" -> do
                        putStrLn "Atribuição de tarefa cancelada."
                        subMenuTarefas banco
                      _ -> do
                        putStrLn "Opção inválida!"
                        subMenuTarefas banco
          else do
            putStrLn "Erro: A data de conclusão deve ser posterior à data atual!"
            subMenuTarefas banco

-- Função para solicitar e validar a nota de avaliação
solicitarNota :: IO Float
solicitarNota = do
  putStr "Por favor, avalie com uma nota de 0.0 a 10.0 pela tarefa realizada: "
  notaStr <- getLine

  -- Verifica se o input não é vazio e contém somente números
  if not (camposValidos [notaStr]) || not (validarNota notaStr)
    then do
      putStrLn "\nErro: A avaliação deve conter apenas números e não pode estar vazia!"
      solicitarNota -- Entrando em recursão caso não seja válido
    else do
      let nota = read notaStr :: Float

      -- Verifica se a nota está dentro do intervalo permitido (0.0 a 10.0)
      if nota < 0.0 || nota > 10.0
        then do
          putStrLn "\nErro: A avaliação deve ser um número entre 0.0 e 10.0!"
          solicitarNota -- Entrando em recursão caso a nota não esteja dentro do intervalo
        else return nota

-- Função para atualizar o estado da tarefa --
atualizarEstadoTarefa :: Banco -> IO Banco
atualizarEstadoTarefa banco@(funcionarios, tarefas, registros) = do
  putStrLn "\n------------------------------------------------------------------------"
  putStrLn "\nAtualizar Estado da Tarefa\n"

  listarTarefasPendentes banco
  putStr "Digite o ID da Tarefa: "
  idTarefaStr <- getLine

  -- Verifica se o campo ID da Tarefa não está vazio ou preenchido apenas com espaços
  if not (camposValidos [idTarefaStr])
    then do
      putStrLn "Erro: Todos os campos devem ser preenchidos corretamente!"
      subMenuTarefas banco
    else do
      let tarefaExistente = filter (\t -> getIdTarefa t == idTarefaStr) tarefas

      -- verifica se o ID digitado corresponde ao ID de uma tarefa
      if null tarefaExistente
        then do
          putStrLn "Erro: Tarefa não encontrada!"
          subMenuTarefas banco
        else do
          let tarefa = head tarefaExistente
              dataConclusao = getDataParaConclusao tarefa

          dataAtual <- obterDataAtual

          -- Verifica se a data atual é posterior a data de conclusão
          if dataAtual > dataConclusao
            then do
              let tarefaAtualizada = tarefa {getEstadoTarefa = "Nao Realizada"} -- atualiza o estado da tarefa
                  tarefasAtualizadas = tarefaAtualizada : filter (\t -> getIdTarefa t /= idTarefaStr) tarefas

              putStrLn "Erro: A data de conclusão da tarefa já passou! Estado atualizado para Não Realizado."
              let novoBanco = (funcionarios, tarefasAtualizadas, registros)
              salvarDados novoBanco
              subMenuTarefas novoBanco
              
            else do
              nota <- solicitarNota -- chama a função que solicita uma nota de 0 a 10 avaliando a tarefa
              let tarefaAtualizada = tarefa {getEstadoTarefa = "Concluída", getNota = nota, getDataDeEfetivacao = Just dataAtual}
                  tarefasAtualizadas = tarefaAtualizada : filter (\t -> getIdTarefa t /= idTarefaStr) tarefas

              putStrLn $ "Alteração do estado da tarefa realizado com sucesso!"
              let novoBanco = (funcionarios, tarefasAtualizadas, registros)
              salvarDados novoBanco
              subMenuTarefas novoBanco

-- Função para listar tarefas atribuídas a um funcionário
listarTarefasDeUmFuncionario :: Banco -> IO ()
listarTarefasDeUmFuncionario banco@(funcionarios, tarefas, _) = do
  putStrLn "\n------------------------------------------------------------------------"
  
  listarFuncionarios banco
  putStrLn "\nListar Tarefas por Funcionário\n"
  putStr "Digite o ID do Funcionário: "
  idFuncionarioStr <- getLine

  -- Validação do ID do funcionário
  if not (camposValidos [idFuncionarioStr])
    then putStrLn "ID do funcionário não pode ser vazio ou apenas espaços."
    else do
      -- Verificação se o ID do funcionário existe
      if not (verificarIDFuncionario idFuncionarioStr funcionarios)
        then putStrLn "ID do funcionário não encontrado."
        else do
          let tarefasPorFuncionario = filter (\t -> getIdFuncionario (getAtribuidor t) == idFuncionarioStr || idFuncionarioStr `elem` map getIdFuncionario (getExecutor t)) tarefas

          if null tarefasPorFuncionario
            then putStrLn "Nenhuma tarefa encontrada para o ID fornecido."
            else do
              imprimirTarefas tarefasPorFuncionario


-- Função auxiliar para imprimir uma lista de tarefas
imprimirTarefas :: [Tarefa] -> IO ()
imprimirTarefas tarefas = do
  mapM_
    ( \t -> do
        putStrLn $ "ID Tarefa: " ++ getIdTarefa t
        putStrLn $ "Título: " ++ getTitulo t
        putStrLn $ "Descrição: " ++ getDescricaoTarefa t
        putStrLn $ "Data de Atribuição: " ++ formatarData (getDataAtribuicao t)
        putStrLn $ "Data para Conclusão: " ++ formatarData (getDataParaConclusao t)
        putStrLn $ "Data de Efetivação: " ++ maybe "N/A" formatarData (getDataDeEfetivacao t)
        putStrLn $ "Estado: " ++ getEstadoTarefa t
        putStrLn $ "Atribuidor: " ++ getNomeFuncionario (getAtribuidor t)
        putStrLn $ "Executores: " ++ intercalate ", " (map getNomeFuncionario (getExecutor t))
        putStrLn ""
    )
    tarefas

-- Função para listar todas as tarefas que não estão concluídas
listarTarefasPendentes :: Banco -> IO ()
listarTarefasPendentes (_, tarefas, _) = do
  putStrLn "\n------------------------------------------------------------------------"
  putStrLn "\nListar Tarefas Pendentes\n"
  let tarefasPendentes = filter (\t -> getEstadoTarefa t == "Pendente") tarefas

  if null tarefasPendentes
    then do
      putStrLn "Nenhuma tarefa pendente encontrada."
    else do
      putStrLn "Tarefas Pendentes:"
      imprimirTarefas tarefasPendentes

-- Função para listar as tarefas concluídas fora do prazo
listarTarefasConcluidasForaDoPrazo :: Banco -> IO ()
listarTarefasConcluidasForaDoPrazo (_, tarefas, _) = do
  let tarefasForaDoPrazo = filter (\t -> getEstadoTarefa t == "Nao Realizada") tarefas
  if null tarefasForaDoPrazo
    then putStrLn "\nNão há tarefas concluídas fora do prazo."
    else do
      putStrLn "\nTarefas Concluídas Fora do Prazo:\n"
      imprimirTarefas tarefasForaDoPrazo
      putStrLn ""

-- Submenu para consultar produtividade
subMenuProdutividade :: Banco -> IO Banco
subMenuProdutividade banco = do
  putStrLn "\n------------------------------------------------------------------------"
  putStrLn "\nConsultar Produtividade\n"
  putStrLn "1. Ranking por Produtividade"
  putStrLn "2. Ranking por Tarefas concluídas"
  putStrLn "3. Ranking por Frequência"
  putStrLn "4. Ranking por Notas"
  putStrLn "5. Voltar ao Menu Principal\n"

  putStr "Escolha uma opção: "

  opcao <- getLine
  case opcao of
    "1" -> do
      rankingPorProdutividade banco
      subMenuProdutividade banco
    "2" -> do
      rankingPorTarefasConcluidas banco
      subMenuProdutividade banco
    "3" -> do
      rankingPorRegistros banco
      subMenuProdutividade banco
    "4" -> do
      rankingPorNotas banco
      subMenuProdutividade banco
  
    "5" -> menuPrincipal banco
    _ -> do
      putStrLn "Opção inválida, tente novamente."
      subMenuProdutividade banco

-- Função para normalizar os critérios
normalizar :: (Real a) => a -> a -> Float
normalizar x maximo = fromRational (toRational x) / fromRational (toRational maximo)

-- calcula Horas Trabalhadas
calcularHorasTrabalhadas :: Funcionario -> [Registro] -> Float
calcularHorasTrabalhadas funcionario registros =
  let registrosFuncionario = filter (\r -> getIdFuncionario (getFuncionario r) == getIdFuncionario funcionario) registros
      registrosComSaida = filter (\r -> isJust (getHoraSaida r)) registrosFuncionario
      calcularHoras (Registro _ _ _ horaEntrada (Just horaSaida)) = timeOfDayToTime horaSaida - timeOfDayToTime horaEntrada
      calcularHoras _ = 0
   in fromRational $ toRational (sum $ map calcularHoras registrosComSaida) / 3600

-- Calcula a quantidade de tarefas realizadas por um funcionário
calcularQuantidadeTarefasRealizadas :: Funcionario -> [Tarefa] -> Int
calcularQuantidadeTarefasRealizadas funcionario tarefas = 
  let idFuncionario = getIdFuncionario funcionario
  
      -- Filtra todas as tarefas concluídas e obtém todos os IDs de todos executores
      idsExecutores = concatMap (map getIdFuncionario . getExecutor) $ filter (isJust . getDataDeEfetivacao) tarefas
      
      -- Conta as ocorrências do ID do funcionário
      quantidadeTarefas = length $ filter (== idFuncionario) idsExecutores
  in quantidadeTarefas

-- Calcula a média das notas de um funcionário
calcularMediaNotas :: Funcionario -> [Tarefa] -> (Float, Int)
calcularMediaNotas funcionario tarefas = do
  let idFuncionario = getIdFuncionario funcionario
  
  -- Filtra as tarefas que foram concluídas e que o funcionário está na lista de executores
  let notas = [getNota t | t <- tarefas, idFuncionario `elem` map getIdFuncionario (getExecutor t), getEstadoTarefa t == "Concluída"]
   in if null notas
        then (0.0, 0)
        else do 
          let qtdNotas = length notas
          let media = (foldr (+) 0 notas) / fromIntegral (length notas)
          (media, qtdNotas)
          
-- Função para listar a produtividade dos funcionários com pesos diferentes para cada critério
listarProdutividadeFuncionarios :: Banco -> IO ()
listarProdutividadeFuncionarios banco@(funcionarios, _, _) = do
  if null funcionarios
    then putStrLn "Não há funcionários cadastrados."
    else do
      putStrLn "\nProdutividade dos Funcionários:\n"
      mapM_ (\f -> do
        let produtividade = calcularProdutividade f banco
        putStrLn $ "ID: " ++ getIdFuncionario f
        putStrLn $ "Nome: " ++ getNomeFuncionario f
        putStrLn $ "Produtividade: " ++ printf "%3.f" produtividade
        putStrLn "") funcionarios

-- Pesos para cada critério de produtividade
pesoTarefasConcluidas :: Float
pesoTarefasConcluidas = 0.4

pesoNota :: Float
pesoNota = 0.3

pesoAjustePrazo :: Float
pesoAjustePrazo = 0.1

pesoHorasTrabalhadas :: Float
pesoHorasTrabalhadas = 0.2

-- calcularMediaPonderadaDaProdutividade --
calcularProdutividade :: Funcionario -> Banco -> Float
calcularProdutividade funcionario (_, tarefas, registros) =

-- Obetendo o cálculo dos 3critérios a serem analisados
  let horasTrabalhadas = calcularHorasTrabalhadas funcionario registros
      tarefasConcluidas = calcularQuantidadeTarefasRealizadas funcionario tarefas
      (mediaNotas, _) = calcularMediaNotas funcionario tarefas
      ajustePrazo = sum [calcularAjustePrazo (getDataParaConclusao tarefa) (getDataDeEfetivacao tarefa)
                  | tarefa <- tarefas,
                    any (\f -> getIdFuncionario f == getIdFuncionario funcionario) (getExecutor tarefa)]

      maxHorasTrabalhadas = 160  -- valor máximo esperado para horas trabalhadas (ex: 160 horas/mês)
      maxTarefasRealizadas = 50   -- valor máximo esperado para tarefas realizadas
      maxMediaNotas = 10.0        -- valor máximo para média de notas

      -- Normalizando os critérios
      horasNormalizadas = normalizar horasTrabalhadas (fromIntegral maxHorasTrabalhadas)
      tarefasNormalizadas = normalizar (fromIntegral tarefasConcluidas) (fromIntegral maxTarefasRealizadas)

      notasNormalizadas = normalizar mediaNotas maxMediaNotas
      ajusteNormalizado = ajustePrazo

      -- Calcular a média ponderada
      produtividade = (pesoHorasTrabalhadas * horasNormalizadas +
                      pesoTarefasConcluidas * tarefasNormalizadas +
                      pesoNota * notasNormalizadas + pesoAjustePrazo * ajusteNormalizado)
  in produtividade * 10

-- Função para calcular a produtividade diária de um funcionário
calcularProdutividadeDiaria :: Funcionario -> Banco -> Dia -> Float
calcularProdutividadeDiaria funcionario (funcionarios, tarefas, registros) dia =
  let tarefasDoDia = filter (\t -> getDataDeEfetivacao t == Just dia) tarefas
      bancoDoDia = (funcionarios, tarefasDoDia, registros)
   in calcularProdutividade funcionario bancoDoDia

-- Função para calcular a produtividade semanal de um funcionário
calcularProdutividadeSemanal :: Funcionario -> Banco -> Dia -> Float
calcularProdutividadeSemanal funcionario (funcionarios, tarefas, registros) dia =
  let inicioSemana = addDays (- (fromIntegral (mod (fromEnum dia) 7))) dia
      fimSemana = addDays 6 inicioSemana
      tarefasDaSemana = filter (\t -> maybe False (\d -> d >= inicioSemana && d <= fimSemana) (getDataDeEfetivacao t)) tarefas
      bancoDaSemana = (funcionarios, tarefasDaSemana, registros)
   in calcularProdutividade funcionario bancoDaSemana

-- Função para calcular a produtividade mensal de um funcionário
calcularProdutividadeMensal :: Funcionario -> Banco -> Integer -> Int -> Float
calcularProdutividadeMensal funcionario (funcionarios, tarefas, registros) ano mes =
  let tarefasDoMes = filter (\t -> maybe False (\d -> let (y, m, _) = toGregorian d in y == ano && m == mes) (getDataDeEfetivacao t)) tarefas
      bancoDoMes = (funcionarios, tarefasDoMes, registros)
   in calcularProdutividade funcionario bancoDoMes

-- Função para ajustar a nota de uma tarefa com base no prazo
ajustarNotaPeloPrazo :: Dia -> Maybe Dia -> Nota -> Nota
ajustarNotaPeloPrazo prazoConclusao (Just dataEfetivacao) nota =
  let diasDeDiferenca = diffDays dataEfetivacao prazoConclusao
      ajuste = if diasDeDiferenca < 0
                 then nota * (1 + (abs (fromIntegral diasDeDiferenca) / fromIntegral (diffDays prazoConclusao dataEfetivacao)))
                 else nota * (1 - (fromIntegral diasDeDiferenca / fromIntegral (diffDays dataEfetivacao prazoConclusao)))
   in max 0 (min 10 ajuste)
ajustarNotaPeloPrazo _ Nothing nota = nota

-- Função para calcular o ajuste pelo prazo em uma tarefa
calcularAjustePrazo :: Dia -> Maybe Dia -> Float
calcularAjustePrazo prazoConclusao (Just dataEfetivacao) =
  let diasDeDiferenca = diffDays dataEfetivacao prazoConclusao
   in if diasDeDiferenca < 0
        then abs (fromIntegral diasDeDiferenca) / fromIntegral (diffDays prazoConclusao dataEfetivacao)
        else - (fromIntegral diasDeDiferenca) / fromIntegral (diffDays dataEfetivacao prazoConclusao)
calcularAjustePrazo _ Nothing = 0.0

-- Função para consultar a média geral de produtividade da empresa
consultarMediaGeralEmpresa :: Banco -> IO ()
consultarMediaGeralEmpresa banco@(funcionarios, _, _) = do

  if null funcionarios
    then putStrLn "Não há funcionários cadastrados para calcular a média de produtividade da empresa."
    else do
      let mediasProdutividade = map (\f -> calcularProdutividade f banco) funcionarios
      let somaProdutividade = sum mediasProdutividade
      let mediaGeralEmpresa = somaProdutividade / fromIntegral (length mediasProdutividade)
      putStrLn $ "Média Geral de Produtividade da Empresa: " ++ printf "%.2f" mediaGeralEmpresa

-- Define a função `on` para evitar a importação de bibliotecas externas
on :: (b -> b -> Ordering) -> (a -> b) -> a -> a -> Ordering
on cmp f x y = cmp (f x) (f y)

-- Função para obter o ano atual
obterAnoAtual :: IO Integer
obterAnoAtual = do
  (year, _, _) <- toGregorian . utctDay <$> getCurrentTime
  return year

-- Função de ranking por produtividade
rankingPorProdutividade :: Banco -> IO ()
rankingPorProdutividade banco@(funcionarios, _, _) = do
  putStrLn "\n------------------------------------------------------------------------"
  putStrLn "\nRanking por Produtividade\n"
  let rankings = sortBy (flip compare `on` (`calcularProdutividade` banco)) funcionarios
  anoAtual <- obterAnoAtual
  diaAtual <- obterDataAtual
  mapM_
    ( \f -> do
        let produtividade = calcularProdutividade f banco
        let produtividadeDiaria = calcularProdutividadeDiaria f banco diaAtual
        let produtividadeSemanal = calcularProdutividadeSemanal f banco diaAtual
        let produtividadeMensal = calcularProdutividadeMensal f banco anoAtual (month diaAtual)
        
        putStrLn $ "ID Funcionário: " ++ getIdFuncionario f
        putStrLn $ "Nome: " ++ getNomeFuncionario f
        putStrLn $ "Produtividade geral: " ++ printf "%.2f" produtividade
        putStrLn $ "Produtividade diária: " ++ printf "%.2f" produtividadeDiaria
        putStrLn $ "Produtividade semanal: " ++ printf "%.2f" produtividadeSemanal
        putStrLn $ "Produtividade mensal: " ++ printf "%.2f" produtividadeMensal
        putStrLn ""
    )
    rankings
  consultarMediaGeralEmpresa banco
  where
    month :: Day -> Int
    month day = let (_, m, _) = toGregorian day in m

rankingPorTarefasConcluidas :: Banco -> IO ()
rankingPorTarefasConcluidas (funcionarios, tarefas, _) = do
  let listaTarefas = map (\f -> (f, calcularQuantidadeTarefasRealizadas f tarefas)) funcionarios
  let listaOrdenada = sortBy (\(_, qt1) (_, qt2) -> compare qt2 qt1) listaTarefas 
  putStrLn "-------------------------------------------------------"
  putStrLn "Ranking de Funcionários por Quantidade de Tarefas Realizadas:"
  
  mapM_
    (\(funcionario, qt) -> do
      putStrLn $ "ID Funcionário: " ++ getIdFuncionario funcionario
      putStrLn $ "Nome: " ++ getNomeFuncionario funcionario
      putStrLn $ "Tarefas Concluídas: " ++ show qt
      putStrLn ""
    )
    listaOrdenada
  
-- Ranking por Registros de Entrada e Saída
rankingPorRegistros :: Banco -> IO ()
rankingPorRegistros (_, _, registros) = do
  putStrLn "\n------------------------------------------------------------------------"
  putStrLn "\nRanking por Registros de Entrada e Saída\n"
  let idsFuncionarios = map (getIdFuncionario . getFuncionario) registros
  let funcionariosUnicos = nub idsFuncionarios
  let rankings = sortBy (flip compare `on` (\myid -> length $ filter (\r -> getIdFuncionario (getFuncionario r) == myid) registros)) funcionariosUnicos
  mapM_
    ( \myid -> do
        let registrosFuncionario = filter (\r -> getIdFuncionario (getFuncionario r) == myid) registros
        putStrLn $ "ID Funcionário: " ++ myid
        putStrLn $ "Nome: " ++ getNomeFuncionario (getFuncionario (head registrosFuncionario))
        putStrLn $ "Número de Registros: " ++ show (length registrosFuncionario)
        putStrLn ""
    )
    rankings

rankingPorNotas :: Banco -> IO ()
rankingPorNotas (funcionarios, tarefas, _) = do
  putStrLn "\n------------------------------------------------------------------------"
  putStrLn "Ranking de Funcionários por Média de Notas\n"

  -- Calcula a média de notas e a quantidade de notas para cada funcionário
  let listaNotas = map (\f -> (f, calcularMediaNotas f tarefas)) funcionarios

  -- Ordena a lista pela média de notas em ordem decrescente, e em caso de empate, pela quantidade de notas
  let listaOrdenada = sortBy (\(_, (media1, qtd1)) (_, (media2, qtd2)) -> 
                               compare (media2, qtd2) (media1, qtd1)) listaNotas

  mapM_ (\(funcionario, (media, qtdNotas)) -> do
           let nome = getNomeFuncionario funcionario
           let idFuncionario = getIdFuncionario funcionario
           printf "ID: %-3s | Nome: %-20s |  Média: %6.2f  |  Qtd de notas: %d\n" idFuncionario nome media qtdNotas
      
        ) listaOrdenada
  
  putStrLn "------------------------------------------------------------------------"
