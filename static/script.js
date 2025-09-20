// =================== CONFIG ===================
const API = {
  iniciar: '/api/jogo/iniciar',
  responder: '/api/jogo/responder',
  status: '/api/jogo/status',
  encerrar: '/api/jogo/encerrar',
  tentarQuestao: '/api/tentar-questao',
  categorias: '/api/categorias'
};

// =================== ESTADO FRONTEND ===================
let estadoAtual = null;        // espelha EstadoJogo devolvido pelo backend
let modoAtual = 'classico';
let questaoAtual = null;       // objeto questao atual
let bloqueado = false;         // evita múltiplos cliques
let cronometroInterval = null; // para modo cronometrado
let tempoRestante = 60;        // exibido localmente apenas

const DELAY_PROXIMA_QUESTAO_CORRETA = 450;
const DELAY_PROXIMA_QUESTAO_INCORRETA = 900;

// =================== ELEMENTOS DOM ===================
const telas = {
  inicial: document.getElementById('tela-inicial'),
  jogo: document.getElementById('tela-jogo'),
  final: document.getElementById('tela-final')
};
const btnIniciar = document.getElementById('btn-iniciar');
const btnReiniciar = document.getElementById('btn-reiniciar');

const infoModo = document.getElementById('info-modo');
const infoProgresso = document.getElementById('info-progresso');
const cronometroBox = document.getElementById('cronometro');

const textoQuestao = document.getElementById('texto-questao');
const categoriaQuestao = document.getElementById('categoria-questao');
const alternativasGrid = document.getElementById('alternativas');
const feedbackBox = document.getElementById('feedback');
const resultadoFinalBox = document.getElementById('resultado-final');
const loadingArea = document.getElementById('loading-questao');
const loadingMsg = loadingArea ? loadingArea.querySelector('.loading-msg') : null;

const tplAlternativa = document.getElementById('tpl-alternativa');

// =================== HELPERS ===================
function trocarTela(nome) {
  Object.values(telas).forEach(sec => sec.classList.remove('ativa'));
  telas[nome].classList.add('ativa');
}

async function chamarAPI(url, opcoes = {}) {
  try {
    const resp = await fetch(url, { headers: { 'Content-Type': 'application/json' }, ...opcoes });
    if (!resp.ok) {
      console.warn('Falha HTTP', resp.status);
    }
    return await resp.json().catch(() => ({}));
  } catch (e) {
    console.error('Erro fetch', e);
    return { erro_rede: true };
  }
}

function limparFeedback() { feedbackBox.innerHTML = ''; }
function mostrarFeedback(msg, positivo = true) {
  feedbackBox.innerHTML = `<span class="${positivo ? 'positivo' : 'negativo'}">${msg}</span>`;
}

function obterNomeModo(modo) {
  const nomesModos = {
    'classico': 'Clássico',
    'ateErrar': 'Até Errar',
    'cronometrado': 'Contra o Tempo'
  };
  return nomesModos[modo] || modo;
}

function formatarCategoria(categoria) {
  const categoriasMap = {
    'General Knowledge': '🧠 Conhecimentos Gerais',
    'Entertainment: Books': '📚 Livros',
    'Entertainment: Film': '🎬 Cinema',
    'Entertainment: Music': '🎵 Música',
    'Entertainment: Musicals & Theatres': '🎭 Teatro',
    'Entertainment: Television': '📺 TV',
    'Entertainment: Video Games': '🎮 Games',
    'Entertainment: Board Games': '🎲 Jogos',
    'Science & Nature': '🔬 Ciências',
    'Science: Computers': '💻 Informática',
    'Science: Mathematics': '🔢 Matemática',
    'Mythology': '⚡ Mitologia',
    'Sports': '⚽ Esportes',
    'Geography': '🗺️ Geografia',
    'History': '📜 História',
    'Politics': '🏛️ Política',
    'Art': '🎨 Arte',
    'Celebrities': '⭐ Celebridades',
    'Animals': '🐾 Animais',
    'Vehicles': '🚗 Veículos'
  };
  
  return categoriasMap[categoria] || `📋 ${categoria}`;
}

function atualizarTopBar() {
  if (!estadoAtual) return;
  infoModo.textContent = `Modo: ${obterNomeModo(modoAtual)}`;
  infoProgresso.textContent = `Respondidas: ${estadoAtual.questoes_respondidas} | Acertos: ${estadoAtual.acertos} | Erros: ${estadoAtual.erros}`;
}

function iniciarCronometroSePreciso() {
  if (modoAtual !== 'cronometrado') {
    cronometroBox.classList.add('hidden');
    return;
  }
  cronometroBox.classList.remove('hidden');
  tempoRestante = 60;
  cronometroBox.textContent = `Tempo: ${tempoRestante}s`;
  clearInterval(cronometroInterval);
  cronometroInterval = setInterval(() => {
    tempoRestante--;
    cronometroBox.textContent = `Tempo: ${tempoRestante}s`;
    if (tempoRestante <= 0) {
      clearInterval(cronometroInterval);
      encerrarJogo();
    }
  }, 1000);
}

function renderizarQuestao(questao) {
  if (!questao) {
    textoQuestao.textContent = 'Sem questão disponível no momento (problema com API externa).';
    alternativasGrid.innerHTML = '';
    if (categoriaQuestao) categoriaQuestao.textContent = 'N/A';
    return;
  }
  questaoAtual = questao;
  textoQuestao.innerHTML = decodeHTML(questao.texto);
  
  // Atualizar categoria da questão
  if (categoriaQuestao && questao.categoria) {
    categoriaQuestao.textContent = formatarCategoria(decodeHTML(questao.categoria));
  }
  
  alternativasGrid.innerHTML = '';
  questao.alternativas.forEach((altTexto, idx) => {
    const clone = tplAlternativa.content.firstElementChild.cloneNode(true);
    clone.textContent = decodeHTML(altTexto);
    clone.dataset.index = idx;
    clone.addEventListener('click', () => responder(idx));
    alternativasGrid.appendChild(clone);
  });

  limparFeedback();
}

function marcarAlternativas(resultado) {
  const botoes = alternativasGrid.querySelectorAll('.alternativa-btn');
  botoes.forEach(btn => {
    btn.classList.add('bloqueada');
    const idx = Number(btn.dataset.index);
    if (idx === resultado.respostaCorreta) btn.classList.add('correta');
    if (idx === resultado.alternativaEscolhida && !resultado.acertou) btn.classList.add('incorreta');
  });
}

function decodeHTML(html) {
  const txt = document.createElement('textarea');
  txt.innerHTML = html;
  return txt.value;
}

// =================== FLUXO DO JOGO ===================
async function iniciarJogo() {
  bloqueado = true;
  // Obter dificuldade selecionada
  const dificuldadeSelecionada = document.querySelector('input[name="dificuldade"]:checked');
  const dificuldadeAtual = dificuldadeSelecionada ? dificuldadeSelecionada.value : 'medium';
  
  // Obter categoria selecionada
  const categoriaSelect = document.getElementById('categoria-select');
  const categoriaAtual = categoriaSelect && categoriaSelect.value ? categoriaSelect.value : null;
  
  const configuracao = {
    modo: modoAtual,
    dificuldade: dificuldadeAtual,
    categoria: categoriaAtual
  };
  const body = JSON.stringify(configuracao);
  const dados = await chamarAPI(API.iniciar, { method: 'POST', body });
  bloqueado = false;
  if (dados.jogo_ativo) {
    estadoAtual = dados.estado;
    modoAtual = dados.modo === 'ateErrar' ? 'ateErrar' : dados.modo === 'cronometrado' ? 'cronometrado' : 'classico';
    if (dados.questao) {
      renderizarQuestao(dados.questao);
    } else {
      mostrarFeedback('Carregando questões da API externa...', true);
      iniciarRetryProximaQuestao();
    }
    atualizarTopBar();
    iniciarCronometroSePreciso();
    trocarTela('jogo');
  } else {
    alert('Não foi possível iniciar o jogo.');
  }
}

async function responder(indice) {
  if (bloqueado || !questaoAtual) return;
  bloqueado = true;
  const payload = {
    questao_id: questaoAtual.id, 
    alternativa_escolhida: indice
  };
  console.debug('[Quiz] Enviando resposta', payload);
  const dados = await chamarAPI(API.responder, { method: 'POST', body: JSON.stringify(payload) });
  console.debug('[Quiz] Resposta do backend', dados);

  if (dados.resultado) {
    estadoAtual = dados.estado || estadoAtual; 
    marcarAlternativas(dados.resultado);
    if (dados.resultado.acertou) {
      mostrarFeedback('Correto! Bom trabalho!', true);
    } else {
      mostrarFeedback('Resposta incorreta!', false);
    }
    atualizarTopBar();

    if (dados.jogo_encerrado) {
      setTimeout(() => {
        mostrarTelaFinal(dados.resultado_final);
        bloqueado = false;
      }, 600);
      return;
    }

    const delay = dados.resultado.acertou ? DELAY_PROXIMA_QUESTAO_CORRETA : DELAY_PROXIMA_QUESTAO_INCORRETA;
    setTimeout(async () => {
      if (dados.questao) {
        renderizarQuestao(dados.questao);
        bloqueado = false;
      } else {
        console.debug('[Quiz] Backend não trouxe próxima questão, tentando recuperação...');
        iniciarRetryProximaQuestao();
      }
    }, delay);
  } else if (dados.erro) {
    mostrarFeedback('Erro: ' + dados.erro, false);
    bloqueado = false;
  } else {
  
    console.warn('[Quiz] Formato inesperado de resposta ao responder questão');
    bloqueado = false;
  }
}

// =================== RETRY DE PRÓXIMA QUESTÃO ===================
let retryAtivo = false;
let tentativas = 0;
const MAX_INTERVAL = 8000; 

async function tentarBuscarQuestao() {
  const rec = await chamarAPI(API.tentarQuestao);
  if (rec && rec.questao) {
    console.debug('[Quiz] Questão recuperada após', tentativas, 'tentativas');
    esconderLoading();
    renderizarQuestao(rec.questao);
    bloqueado = false;
    retryAtivo = false;
    tentativas = 0;
    return true;
  }
  return false;
}

function mostrarLoading(mensagem) {
  if (!loadingArea) return;
  loadingArea.classList.remove('hidden');
  if (loadingMsg && mensagem) loadingMsg.textContent = mensagem;
}
function esconderLoading() { if (loadingArea) loadingArea.classList.add('hidden'); }

function iniciarRetryProximaQuestao() {
  if (retryAtivo) return;
  retryAtivo = true;
  tentativas = 0;
  mostrarLoading('Tentando obter nova questão...');
  loopRetry();
}

async function loopRetry() {
  tentativas++;
  const sucesso = await tentarBuscarQuestao();
  if (sucesso) return;
  const intervalo = Math.min(1000 * Math.pow(1.6, tentativas), MAX_INTERVAL);
  if (loadingMsg) loadingMsg.textContent = `Tentando novamente (tentativa ${tentativas})...`; 
  setTimeout(loopRetry, intervalo);
}

btnReiniciar.addEventListener('click', () => reiniciar());

async function encerrarJogo() {
  clearInterval(cronometroInterval);
  const dados = await chamarAPI(API.encerrar, { method: 'POST' });
  if (dados.jogo_encerrado) mostrarTelaFinal(dados.resultado_final);
}

function mostrarTelaFinal(resultado) {
  trocarTela('final');
  if (!resultado) {
    resultadoFinalBox.innerHTML = '<div class="erro-resultado">❌ Não foi possível calcular resultado.</div>';
    return;
  }
  
  const modoNome = obterNomeModo(resultado.modo_jogado || modoAtual);
  const precisao = resultado.porcentagem_acerto || 0;
  
  const html = `
    <div class="stats-grid">
      <div class="stat-item modo-stat">
        <div class="stat-icon">🎮</div>
        <div class="stat-content">
          <div class="stat-label">Modo</div>
          <div class="stat-value">${modoNome}</div>
        </div>
      </div>
      
      <div class="stat-item questoes-stat">
        <div class="stat-icon">📚</div>
        <div class="stat-content">
          <div class="stat-label">Questões</div>
          <div class="stat-value">${resultado.total_questoes}</div>
        </div>
      </div>
      
      <div class="stat-item acertos-stat">
        <div class="stat-icon">✅</div>
        <div class="stat-content">
          <div class="stat-label">Acertos</div>
          <div class="stat-value">${resultado.total_acertos}</div>
        </div>
      </div>
      
      <div class="stat-item precisao-stat">
        <div class="stat-icon">🎯</div>
        <div class="stat-content">
          <div class="stat-label">Precisão</div>
          <div class="stat-value">${precisao}%</div>
        </div>
      </div>
      
      <div class="stat-item pontuacao-stat">
        <div class="stat-icon">⚡</div>
        <div class="stat-content">
          <div class="stat-label">Pontuação Final</div>
          <div class="stat-value destaque">${resultado.pontuacao}</div>
        </div>
      </div>
    </div>
  `;
  resultadoFinalBox.innerHTML = html;
}

function reiniciar() {
  estadoAtual = null;
  questaoAtual = null;
  clearInterval(cronometroInterval);
  document.querySelector('input[name="modo"][value="classico"]').checked = true;
  document.querySelector('input[name="dificuldade"][value="easy"]').checked = true;
  modoAtual = 'classico';
  trocarTela('inicial');
}

// =================== EVENTOS INICIAIS ===================
btnIniciar.addEventListener('click', () => {
  const selecionado = document.querySelector('input[name="modo"]:checked');
  modoAtual = selecionado ? selecionado.value : 'classico';
  iniciarJogo();
});

// Acessibilidade: permitir Enter para iniciar se focado
btnIniciar.addEventListener('keyup', e => { if (e.key === 'Enter') iniciarJogo(); });

// =================== CATEGORIAS ===================
async function carregarCategorias() {
  const categoriaSelect = document.getElementById('categoria-select');
  const categoriaLoader = document.getElementById('categoria-loader');
  
  if (!categoriaSelect) return;
  
  categoriaLoader.style.display = 'block';
  
  try {
    const dados = await chamarAPI(API.categorias);
    
    if (dados && dados.length > 0 && dados[0].trivia_categories) {
      const categorias = dados[0].trivia_categories;
      
      // Limpar opções existentes (exceto a primeira)
      categoriaSelect.innerHTML = '<option value="">🌟 Todas as Categorias</option>';
      
      // Adicionar categorias do servidor
      categorias.forEach(categoria => {
        const option = document.createElement('option');
        option.value = categoria.id;
        option.textContent = `📚 ${categoria.name}`;
        categoriaSelect.appendChild(option);
      });
    }
  } catch (error) {
    console.warn('Erro ao carregar categorias:', error);
    categoriaSelect.innerHTML = '<option value="">🌟 Todas as Categorias</option><option disabled>Erro ao carregar categorias</option>';
  }
  
  categoriaLoader.style.display = 'none';
}

// =================== INIT ===================
(async function init() {
  // Carregar categorias
  await carregarCategorias();
  
  // Verifica se já existe jogo ativo (refresh de página)
  const status = await chamarAPI(API.status);
  if (status.jogo_ativo) {
    modoAtual = status.modo;
    // tentar obter questão atual novamente
    const rec = await chamarAPI(API.tentarQuestao);
    estadoAtual = rec.estado || null;
    if (rec.questao) {
      renderizarQuestao(rec.questao);
      atualizarTopBar();
      iniciarCronometroSePreciso();
      trocarTela('jogo');
    }
  }
})();