# Projeto Quiz
**Deu algo errado e o trabalho ficou na branch "master-prototipo"

## 1. Identificação
- **Nome:** Renato Marquioro da Silva
- **Curso:** Sistemas de Informação

## 2. Tema/Objetivo

Minha proposta é desenvolver uma API REST para sistema de quiz com múltiplas categorias, sistema de pontuação automático e tabela de pontuações persistente.

O sistema permitirá gerar quizzes personalizados por categoria e dificuldade, calcular pontuações automaticamente, gerenciar jogadores com cadastro e histórico, e manter um ranking persistente. Os principais recursos incluem geração de quiz, submissão de respostas, visualização da tabela de pontuações e cadastro de jogadores.

A persistência provavelmente será implementada através de arquivos JSON para manter dados entre sessões. O projeto aplicará programação funcional com funções puras para lógica de quiz e pontuação, composição de funções, pattern matching para validação, e funções de alta ordem para processamento de dados.

### O que foi Implementado
- API REST básica do quiz
- Sistema de perguntas e respostas com cálculo de pontuação
- Rotas principais funcionando para geração de quiz
- Personalização de quizzes por dificuldade/categoria de forma completa
- Validação de respostas com pattern matching
- Três modos de jogo distintos (Clássico, Até Errar, Cronometrado)
- Integração com API externa para questões dinâmicas

### O que não foi Implementado e Por Quê

Durante o desenvolvimento, optei conscientemente por não implementar algumas funcionalidades inicialmente planejadas:

**Persistência de dados em JSON/Banco de Dados**: Decidi não fazer integração com banco de dados porque eu achei melhor não implementar do que fazer de forma incorreta. Como já tive que aprender muitos conceitos novos (haskell mais profundo, pattern matching, Scotty, funcionamento de Api, etc), adicionar persistência significaria aprender mais conceitos complexos. 

Preferi focar em fazer bem feito o que estava implementando ao invés de tentar abraçar tudo e comprometer a qualidade. 

**Sistema de usuários completo**: Estava diretamente ligado à persistência, então também foi deixado de lado.

**Ranking persistente**: Mesma situação - dependia de banco de dados para funcionar adequadamente.

Essa foi uma decisão estratégica: melhor ter um sistema funcional e bem estruturado nas funcionalidades principais do que tentar implementar tudo e acabar com um código mal feito ou cheio de bugs.

## 3. Processo de Desenvolvimento

### Parte 1 - Backend Haskell

Comecei fazendo todas as funções que achei necessárias pro quiz sem pensar na API. Só depois que achei que tinha tudo pronto é que fui implementar a REST API. Foi bem desorganizado no início.

#### Um adendo

O código do jogo que a professora passou para apresentação de trabalho ajudou muito na compreensão de como é a base de um jogo. Mesmo não tendo nem a ideia parecida, proporcionou um entendimento de algumas funcionalidades e de como usar os recursos diponíveis.

**No que ajudou:**
- Modelagem de Estado: Como definir e manipular o estado do jogo usando registros e tipos personalizados.
- Funções puras para lógica: Separar lógica de negócio de IO, facilitando testes e manutenção.
- Eventos e Manipulação de Input: Como tratar eventos (teclado, mouse) para atualizar o estado do jogo.
- Organização Modular: Separar funcionalidades para deixar o projeto modular
- Feedback Visual: Como transformar o estado do jogo em uma representação visual.
- Abordagem funcional: Uso de funções de ordem superior, composição e imutabilidade.

#### Dificuldades com Conceitos Avançados

**Data e Pattern Matching**: Tivemos isso por cima na aula, então precisei estudar bastante. Criar os tipos de dados foi complicado:

```haskell
data ModoJogo = AteErrar | Classico | Cronometrado
data EstadoJogo = EstadoJogo { questoes_respondidas :: Int, acertos :: Int, ... }
```

Fazer pattern matching para os diferentes modos de jogo foi onde mais travei:

```haskell
verificarEncerramentoGeral AteErrar _ resultado = return (encerrarAteErrar resultado)
verificarEncerramentoGeral Classico estado _ = return (encerrarClassico estado)
```

**Instâncias JSON**: Isso foi algo bem novo. Precisava converter tudo pra JSON pro frontend:

```haskell
instance ToJSON ModoJogo where
    toJSON AteErrar = "ateErrar"
    toJSON Classico = "classico"
```

Tive que estudar como funcionava o Aeson e como fazer parsing.

**Maybe para Erros**: Sempre ficava em dúvida quando usar Maybe. Principalmente quando a API podia falhar:

```haskell
case questoesNovas of
    (questao:_) -> return $ Just questao
    [] -> return Nothing
```

**IO e Monads**: Isso foi o mais difícil. Misturar operações IO com lógica pura. O modo cronometrado é um dos exemplos:

```haskell
encerrarCronometrado estado = do
    tempoAtual <- getCurrentTime
    let tempoDecorrido = diffUTCTime tempoAtual (tempo_iniciado estado)
    return (tempoDecorrido >= 60)
```

### Parte 2 - API REST com Scotty

#### O que Foi Implementado

Criei uma API completa com várias rotas:

- **GET /**: Serve a página principal
- **GET /api/categorias**: Lista categorias disponíveis  
- **POST /api/jogo/iniciar**: Inicia nova partida
- **POST /api/jogo/responder**: Processa resposta do jogador
- **GET /api/jogo/status**: Verifica estado atual
- **POST /api/jogo/encerrar**: Força encerramento
- **GET /api/tentar-questao**: Recovery quando API externa falha

```haskell
scotty 3000 $ do
    middleware simpleCors
    get "/" $ file "static/index.html"
    post "/api/jogo/iniciar" $ do
        config <- jsonData :: ActionM ConfiguracaoJogo
        estadoInicial <- liftIO $ iniciarJogo (modo config)
```

#### Maiores Problemas com GET e POST

**Conceitos totalmente novos**: Como eu nunca tinha lidado com API e interface web, os GETs e POSTs foram uma boa dificuldade. Era algo diferente que eu nunca tinha visto, então tive que ver bastante coisas e perguntar para a IA como fazia, pedir alguns exemplos.

**Entender ActionM**: Não sabia como funcionava a mônada do Scotty. Misturar IO com ActionM foi confuso:

```haskell
post "/api/jogo/responder" $ do
    resposta <- jsonData :: ActionM Resposta
    sessaoAtual <- liftIO $ readIORef gameSession  -- Precisava do liftIO
```

**Diferença de contexto**: Para o backend em Haskell, para a lógica do jogo em si, tive dificuldades, mas conseguia pensar bem porque era programação "normal". Mas a parte dos GETs e POSTs complicava porque era um paradigma completamente diferente - servidor respondendo requisições HTTP.

**Tratamento de JSON**: Converter dados entre frontend e backend foi algo complicado. Tive que ver exemplos de como ficavam as estruturas para se comunicar com a interface web, com a API em si. Tive algumas dificuldades em como fazer essa conversão. Erros de parsing aconteciam:

```haskell
config <- jsonData :: ActionM ConfiguracaoJogo  -- Falhava muito
```

Basicamente, nessa parte que envolve conexão na web, essas respostas e envios que se faz com a web, eu tive dificuldades a mais. Era diferente de programar lógica pura - tinha que pensar em formatos específicos.

#### Gerenciamento de Estado

O maior problema foi manter o estado do jogo. Usei IORef que não conhecia:

```haskell
type GameSession = IORef (Maybe (ModoJogo, String, Maybe String, EstadoJogo, Questao))
gameSession <- newIORef Nothing
```

Teve muito bug com concorrência e estado compartilhado. Não entendia quando usar `readIORef` vs `writeIORef`.

#### Estrutura de Resposta

Padronizar as respostas foi complicado. Cada rota retornava JSON diferente:

```haskell
-- Resposta de sucesso
json $ object [
    "resultado" .= resultado,
    "estado" .= novoEstado,
    "questao" .= questao,
    "jogo_encerrado" .= False
]

-- Resposta de erro
json $ object ["erro_api" .= True, "sucesso" .= False]
```

### Parte 3 - Integração com API Externa

#### O que Foi Implementado

Criei um módulo completo para buscar questões da Open Trivia Database:

```haskell
-- Função para buscar questões na API com dificuldade e categoria
buscarQuestoes :: Int -> String -> Maybe String -> IO [Questao]
buscarQuestoes quantidade dificuldade mbCategoria = do
    let baseUrl = "https://opentdb.com/api.php?amount=" ++ show quantidade ++ "&type=multiple&difficulty=" ++ dificuldade
```

Implementei também:
- Conversão de dados da API para tipos internos
- Embaralhamento de alternativas
- Busca de categorias disponíveis
- Tratamento de erros da API

#### Requisições HTTP

Usar Network.HTTP.Simple foi novo, pois, como disse, nunca havia mexido com:

```haskell
let baseUrl = "https://opentdb.com/api.php?amount=" ++ show quantidade
let url = case mbCategoria of
    Nothing -> baseUrl
    Just categoriaId -> baseUrl ++ "&category=" ++ categoriaId
```

#### Parsing JSON da API

A API retorna um formato, e eu precisava converter pro meu tipo:

```haskell
data ApiQuestion = ApiQuestion {
    category :: String,
    question :: String,
    correct_answer :: String,
    incorrect_answers :: [String]
} deriving (Show)
```

#### Embaralhar Alternativas

Precisei embaralhar as alternativas e rastrear qual era a correta:

```haskell
listaEmbaralhada <- shuffleM todasAlternativas
let indexRespostaCerta = fromMaybe 0 (elemIndex respostaCerta listaEmbaralhada)
```

### Parte 4 - Frontend e Integração

#### Desenvolvimento com IA

A integração do frontend com o backend foi feita quase inteiramente através de IA. Basicamente usei um prompt tipo: "Integre meu backend em Haskell com a interface web". Tive mínima influência sobre essa parte.

A IA gerou:
- Todo o código HTML/CSS/JavaScript
- As requisições AJAX para comunicar com a API
- O tratamento de respostas JSON
- A interface de usuário completa

#### Minha Participação Limitada

Minha contribuição no frontend foi praticamente zero. Só ajustei algumas coisas pequenas que a IA sugeriu, mas todo o trabalho pesado de integração foi feito automaticamente.

Eu usei como uma ferramenta de desenvolvimento para fazer a parte que eu não dominava, enquanto eu focava na programação funcional (a lógica em Haskell).

## 4. Orientações para Execução
Se prefirir, clone meu repositório:
```bash
git clone https://github.com/elc117/perso-2025b-Renatronis.git
```
Isso irá baixar uma cópia completa do seu projeto para o computador. Depois, entre na pasta clonada com:
```bash
cd perso-2025b-Renatronis
```
Se optar por isso, pule para a etapa 2 da execução

1. **Instalação de dependências:**
   ```bash
   cabal update
   cabal install
   ```
  Para rodar o programa será preciso dessas dependências:

  ```bash
  base (>=4.7 && <5)
  scotty
  text
  aeson
  containers
  http-conduit
  bytestring
  random
  random-shuffle
  time
  wai-cors
  http-types
  ```
  Para rodar os testes:
  ```bash
  - hspec
  ```
Como instalar: no **cabal**, basta rodar:
```bash
cabal update; cabal install --lib scotty text aeson containers http-conduit bytestring random random-shuffle time wai-cors http-types hspec
  ```

2. **Execução do servidor:**
   ```bash
   cabal run
   ```

3. **Acesso ao sistema:**
   - Abra o navegador em [http://localhost:3000](http://localhost:3000)

## 5. Resultado Final

Demonstração de execução:

Uploading videoQuizz.mp4…

### Funcionalidades Principais

**Modos de Jogo:**
- **Clássico**: 10 questões com contagem de acertos e erros
- **Até Errar**: Continue jogando até errar uma questão
- **Cronometrado**: Responda o máximo de questões em 1 minuto

**Personalização:**
- Escolha entre diferentes níveis de dificuldade
- Filtre por categoria específica ou jogue com questões mistas
- Sistema inteligente que evita repetição de questões durante a partida

**Sistema de Pontuação:**
- Pontuação automática baseada no número de acertos
- Cálculo de porcentagem de aproveitamento
- Feedback imediato após cada resposta

## 6. Implementações Futuras

### Persistência de Dados com Banco de Dados

A principal melhoria para o futuro seria implementar integração com banco de dados para manter persistência de dados dos usuários. Atualmente, tudo é perdido quando o servidor é reiniciado.

**Funcionalidades a implementar:**
- Sistema de cadastro e login de usuários
- Histórico de partidas por usuário
- Ranking global persistente
- Estatísticas detalhadas (média de acertos, categorias favoritas, etc.)

## 7. Referências e Créditos

### Documentação e Tutoriais
- **Documentação oficial do Haskell**: [haskell.org](https://www.haskell.org/)
- **Learn You a Haskell**: Para conceitos fundamentais de programação funcional
- **Real World Haskell**: Para padrões de desenvolvimento em projetos reais
- **Haskell Wiki**: Para resolução de problemas específicos com Monads e IO
- **Stack Overflow**: Para debugging de questões específicas do Aeson e parsing JSON
- **Aprendendo sobre tipos, newtipe, type, data e deriving.**: https://haskell.pesquisa.ufabc.edu.br/desenvolvimento-orientado-a-tipos/03.adts
- **Código do joguinho para apresentação em aula**: https://code.world/haskell#PSAP49qms7Ahg4z1gpLm2Jw
- **Pattern matching**: https://www.youtube.com/watch?v=pJayUV35zoU 
- **Maybe**: https://www.youtube.com/watch?v=n2PH7OV5vrs 


### APIs e Bibliotecas principais
- **Open Trivia Database**: API gratuita para questões de quiz
- **Scotty Framework**: Para implementação da API REST
- **Aeson**: Para parsing e geração de JSON

### Prompts de IA Utilizados
- **"Integre meu backend em Haskell com a interface web"**: Usado para gerar todo o frontend
- **"Como embaralhar alternativas em Haskell mantendo o índice correto?"**: Para implementar shuffleM
- **"Tratamento de erros JSON em Scotty"**: Para melhorar robustez da API
- **"Como usar IORef para estado compartilhado em Scotty?"**: Para gerenciamento de sessão
- **"Exemplo de uso de GET e POST. Crie um aletório e outro usando de base meu programa"** Para entender a sintaxe e como funciona

### Agradecimentos
- Professor pela orientação durante o desenvolvimento
- Aos colegas: Gabriel, Mateus, Otávio, Lucas, Michel. Por trocarmos ideias sobre nossos projetos e por me darem dicas de quais ferramentas usar para facilitar o desenvolvimento.
- IA pela geração do frontend completo e pela sua ajuda com conceitos e exemplos práticos, rápidos e de fácil entendimento.
