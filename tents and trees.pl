/*ordenaPosicao(ListaPosicoes, ListaPosicoesOrdenadas) Ordena uma lista de posicoes
por ordem de leitura do tabuleiro*/
ordenaPosicao(ListaPosicoes, ListaPosicoesOrdenadas) :-
    msort(ListaPosicoes, ListaPosicoesOrdenadas).

/*vizinhanca((L, C), Vizinhanca) e verdade se Vizinhanca e uma lista ordenada de cima
para baixo e da esquerda para a direita, sem elementos repetidos, com as coordenadas das
posicoes imediatamente acima, imediatamente a esquerda, imediatamente a direita e
imediatamente abaixo da coordenada (L, C);*/
vizinhanca((L, C), Vizinhanca) :-
    Lcima is L-1,
    Cesquerda is C-1,
    Cdireita is C+1,
    Lbaixo is L+1,
    VizinhancaDesordenada = [(Lcima, C), (L, Cesquerda), (L, Cdireita), (Lbaixo, C)],
	ordenaPosicao(VizinhancaDesordenada, Vizinhanca).

/*diagonais((L,C),Diagonais) Posicoes nas diagonais de uma posicao*/
diagonais((L, C), Diagonais) :-
    Lcima is L-1,
    Cesquerda is C-1,
    Cdireita is C+1,
    Lbaixo is L+1,
	Diagonais = [(Lcima, Cesquerda), (Lcima, Cdireita), (Lbaixo, Cesquerda), (Lbaixo, Cdireita)].

/*vizinhancaAlargada((L, C), VizinhancaAlargada) e verdade se VizinhancaAlargada e
uma lista ordenada de cima para baixo e da esquerda para a direita, sem elementos repetidos,
com as coordenadas anteriores e ainda as diagonais da coordenada (L, C)*/
vizinhancaAlargada((L, C), VizinhancaAlargada) :-
    vizinhanca((L, C), Vizinhanca),
    diagonais((L, C), Diagonais),
    append(Vizinhanca, Diagonais, VizinhancaAlargadaDesordenada),
	ordenaPosicao(VizinhancaAlargadaDesordenada, VizinhancaAlargada).

/*todasCelulas(Tabuleiro, TodasCelulas) e verdade se TodasCelulas
e uma lista ordenada de cima para baixo e da esquerda para a direita,
sem elementos repetidos, com todas as coordenadas do tabuleiro Tabuleiro*/
todasCelulas(Tabuleiro, TodasCelulas) :-
    % Obtem o numero de linhas e colunas do tabuleiro
    length(Tabuleiro, NumLinhas),
	nth1(1, Tabuleiro, Colunas),
    length(Colunas, NumColunas),
    % Gera a lista de todas as celulas
    findall((L, C),
         (between(1, NumLinhas, L),
         between(1, NumColunas, C)),
         TodasCelulas).
todasCelulas(Tabuleiro, TodasCelulas) :- % Caso seja um tabuleiro vazio (provavelmente nao e possivel)
    length(Tabuleiro, NumLinhas),
	NumLinhas =:= 0,
    TodasCelulas = [].
	
/*contemObjeto(Tabuleiro, Objeto, (L, C)) Verifica se a celula (L,C) contem o objeto desejado*/
contemObjeto(Tabuleiro, Objeto, (L, C)) :-
    atom(Objeto),
    nth1(L, Tabuleiro, Linha),
    nth1(C, Linha, O),
    O == Objeto.
contemObjeto(Tabuleiro, Objeto, (L, C)) :- % Verifica se a celula contem um _
    \+atom(Objeto),
    nth1(L, Tabuleiro, Linha),
    nth1(C, Linha, O),
    \+atom(O).

/* todasCelulas(Tabuleiro, TodasCelulas, Objecto) e verdade se TodasCelulas e uma
lista ordenada de cima para baixo e da esquerda para a direita, sem elementos repetidos,
com todas as coordenadas do tabuleiro Tabuleiro em que existe um objecto do tipo Objecto
(neste contexto (tal como no anterior) objecto e uma tenda (t), relva (r), arvore (a) ou ainda
uma variavel (por exemplo X), para indicar os espacos nao preenchidos).*/
todasCelulas(Tabuleiro, TodasCelulas, Objeto) :-
    todasCelulas(Tabuleiro, TodasCelulasTabuleiro),
    include(contemObjeto(Tabuleiro, Objeto), TodasCelulasTabuleiro, TodasCelulas).

/*calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto) e
verdade se Tabuleiro for um tabuleiro, Objecto for o tipo de objecto que se procura, e
ContagemLinhas e ContagemColunas forem, respectivamente, listas com o numero desses
objectos por linha e por coluna.*/
calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objeto) :-
    length(Tabuleiro, Tamanho),
    todasCelulas(Tabuleiro, TodasCelulas, Objeto),
    findall(Linha, member((Linha, _), TodasCelulas), Linhas),
    findall(Coluna, member((_, Coluna), TodasCelulas), Colunas),
    contagemLinCol(Linhas, Tamanho, ContagemLinhas),
    contagemLinCol(Colunas, Tamanho, ContagemColunas).

/* contagemLinCol(Elemento, Tamanho, Contagem) Conta o numero de ocorrencias de
um objeto nas linhas ou nas colunas */
contagemLinCol(Elemento, Tamanho, Contagem) :-
    findall(Ocorrencias, 
        (between(1, Tamanho, Numero),
        contarOcorrencias(Elemento, Numero, Ocorrencias)),
        Contagem).

/*contarOcorrencias(Lista, Num, Ocorrencias) Conta o numero de ocorrencias de um numero numa lista*/
contarOcorrencias(Lista, Num, Ocorrencias) :-
    findall(NumLista, (member(NumLista, Lista), NumLista == Num), OcorrenciasNum),
    length(OcorrenciasNum, Ocorrencias).

/*celulaVazia(Tabuleiro, (L, C)) e verdade se Tabuleiro for um tabuleiro que nao tem
nada ou tem relva nas coordenadas (L, C). De notar que se as coordenadas nao fizerem parte
do tabuleiro, o predicado nao deve falhar.*/
celulaVazia(Tabuleiro, (L, C)) :-
    nth1(L, Tabuleiro, Linha),
    nth1(C, Linha, Celula),
    (var(Celula) ; Celula == r).
celulaVazia(Tabuleiro, (L, C)) :- % E celula vazia caso esteja fora dos limites do tabuleiro
    length(Tabuleiro, Tamanho),
    \+ (between(1, Tamanho, L), between(1, Tamanho, C)).

/*insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C)) e verdade se Tabuleiro e um
tabuleiro e (L, C) sao as coordenadas onde queremos inserir o objecto TendaOuRelva.*/
insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C)) :-
    nth1(L, Tabuleiro, Linha),
    nth1(C, Linha, Objeto),
    (\+ var(Objeto) ; Objeto = TendaOuRelva).

/* insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2))
e verdade se Tabuleiro e um tabuleiro, e (L, C1) e (L, C2) sao as coordenadas, na Linha L,
entre as quais (incluindo) se insere o objecto TendaOuRelva.*/
insereObjectoEntrePosicoes(Tabuleiro, Objeto, (L, C1), (L, C2)) :-
    findall((L, C), between(C1,C2, C), Posicoes),
    maplist(insereObjectoCelula(Tabuleiro, Objeto), Posicoes).

/*relva(Puzzle) e verdade se Puzzle e um puzzle que, apos a aplicacao do predicado, tem
relva em todas as linhas/colunas cujo numero de tendas ja atingiu o numero de tendas possivel
nessas linhas/colunas*/
relva(Puzzle) :-
    Puzzle = (Tabuleiro, TendasLinhas, TendasColunas),
    transpose(Tabuleiro, TabuleiroTransposto),
    maplist(relvaLCCompletas, TendasLinhas, Tabuleiro),
    maplist(relvaLCCompletas, TendasColunas, TabuleiroTransposto).

/* relvaLCCompletas(TendasLC, LC) Coloca relva nas linhas e colunas que estao com o numero certo de tendas*/
relvaLCCompletas(TendasLC, LC) :-
    contarTendas(LC, TendasLC), % Verifica que a Linha/Coluna esta completa
    preencherComRelva(LC).
relvaLCCompletas(_, _).

/*contarTendas(Tendas, Ocorrencias) Conta o numero de tendas numa linha ou coluna*/
contarTendas([], 0).
contarTendas([Cabeca|Resto], Ocorrencias) :-
    \+ var(Cabeca),
    Cabeca == t,
    contarTendas(Resto, OcorrenciasResto),
    Ocorrencias is OcorrenciasResto + 1.
contarTendas([_|Resto], Ocorrencias) :-
    contarTendas(Resto, Ocorrencias).

/*preencherComRelva(LinhaColuna) Preenche com relva as celulas nao preenchidas numa linha ou coluna*/
preencherComRelva([]).
preencherComRelva([Celula|Resto]) :-
    var(Celula),
    Celula = r,
    preencherComRelva(Resto).
preencherComRelva([_|Resto]) :-
    preencherComRelva(Resto).

/*inacessiveis(Tabuleiro) e verdade se Tabuleiro e um tabuleiro que, apos a aplicacao do
predicado, tem relva em todas as posicoes inacessiveis*/
inacessiveis(Tabuleiro) :-
    todasCelulas(Tabuleiro, TodasCelulas),
    maplist(preencherInacessiveis(Tabuleiro), TodasCelulas).

/* preencherInacessiveis(Tabuleiro, Posicao) Preenche com relva se nao houver arvore na vizinhanca*/
preencherInacessiveis(Tabuleiro, Posicao) :-
    \+ vizinhancaArvore(Tabuleiro, Posicao),
    insereObjectoCelula(Tabuleiro, r, Posicao).
preencherInacessiveis(_, _).

/*vizinhancaArvore(Tabuleiro, Posicao) E verdade se houver uma arvore na vizinhanca da posicao */
vizinhancaArvore(Tabuleiro, Posicao) :-
    vizinhanca(Posicao, Vizinhanca),
    member((L, C), Vizinhanca),
    contemObjeto(Tabuleiro, a, (L, C)),
    !.

/*aproveita(Puzzle) e verdade se Puzzle e um puzzle que, apos a aplicacao do predicado,
tem tendas em todas as linhas e colunas as quais faltavam colocar X tendas e que tinham
exactamente X posicoes livres. De notar que este predicado deve ser implementado resolvendo as
linhas, fazendo novas contagens, e resolvendo as colunas; nao e recursivo*/
aproveita(Puzzle) :-
    Puzzle = (Tabuleiro, TendasLinhas, TendasColunas),
    maplist(tendasLCAproveitaveis, TendasLinhas, Tabuleiro),
    transpose(Tabuleiro, TabuleiroTransposto),
    maplist(tendasLCAproveitaveis, TendasColunas, TabuleiroTransposto).

/* tendasLCAproveitaveis(TendasLC, LC) Coloca tendas nas linhas e colunas que ficarao com o numero certo de tendas*/
tendasLCAproveitaveis(TendasLC, LC) :-
    findall(Objeto, % Encontra o numero de tendas ou variaveis Linha/Coluna
        (member(Objeto, LC),
        (Objeto == t; var(Objeto))),
        OcorrenciasVarTenda),
    length(OcorrenciasVarTenda, TendasLC), % Verifica que a Linha/Coluna e aproveitavel
    preencherComTendas(LC).
tendasLCAproveitaveis(_, _).

/* preencherComTendas(LC) Preenche com tendas as celulas nao preenchidas numa linha ou coluna*/
preencherComTendas([]).
preencherComTendas([Celula|Resto]) :-
    var(Celula),
    Celula = t,
    preencherComTendas(Resto).
preencherComTendas([_|Resto]) :-
    preencherComTendas(Resto).

/* limpaVizinhancas(Puzzle) e verdade se Puzzle e um puzzle que, apos a aplicacao do
predicado, tem relva em todas as posicoes a volta de uma tenda */
limpaVizinhancas(Puzzle) :-
    Puzzle = (Tabuleiro, _, _),
    todasCelulas(Tabuleiro, TodasCelulas),
    maplist(relvaVizinhancasTendas(Tabuleiro), TodasCelulas).

/*relvaVizinhancasTendas(Tabuleiro, Posicao) Verifica que uma celula contem tenda e coloca relva na sua vizinhanca*/
relvaVizinhancasTendas(Tabuleiro, Posicao) :-
    contemObjeto(Tabuleiro, t, Posicao),
    vizinhancaAlargada(Posicao, Vizinhanca),
    findall(VizinhoVazio,
        (member(VizinhoVazio, Vizinhanca),
        posicaoVazia(Tabuleiro, VizinhoVazio)),
        VizinhancaVazia),
    maplist(relvaCelula(Tabuleiro), VizinhancaVazia).
relvaVizinhancasTendas(_, _).

/*relvaCelula(Tabuleiro, Posicao) Preenche com relva uma celula*/
relvaCelula(Tabuleiro, Posicao) :-
    insereObjectoCelula(Tabuleiro, r, Posicao).
relvaCelula(_,_).

/*unicaHipotese(Puzzle) e verdade se Puzzle e um puzzle que, apos a aplicacao do predicado,
todas as arvores que tinham apenas uma posicao livre na sua vizinhanca que lhes
permitia ficar ligadas a uma tenda, tem agora uma tenda nessa posicao.*/
unicaHipotese(Puzzle) :-
    Puzzle = (Tabuleiro, _, _),
    todasCelulas(Tabuleiro, TodasCelulas),
    maplist(tendasVizinhancasArvores(Tabuleiro), TodasCelulas).

/*tendasVizinhancasArvores(Tabuleiro, Posicao) Se a vizinhanca de uma arvore nao contem tendas
 e tem uma unica posicao livre, coloca uma tenda nessa posicao*/
tendasVizinhancasArvores(Tabuleiro, Posicao) :-
    contemObjeto(Tabuleiro, a, Posicao),
    vizinhanca(Posicao, Vizinhanca),
    \+ (member(VizinhancaPosicao, Vizinhanca), contemObjeto(Tabuleiro, t, VizinhancaPosicao)),
    include(posicaoVazia(Tabuleiro), Vizinhanca, PosicoesVazias),
    length(PosicoesVazias, 1),
    maplist(insereObjectoCelula(Tabuleiro, t), PosicoesVazias).
tendasVizinhancasArvores(_,_).

/* posicaoVazia(Tabuleiro, (L,C)) Verifica se (L,C) esta vazia*/
posicaoVazia(Tabuleiro, (L,C)) :-
    nth1(L, Tabuleiro, Linha),
    nth1(C, Linha, Celula),
    var(Celula).

/*valida(LArv, LTen) e verdade se LArv e LTen sao listas com todas as coordenadas em
que existem, respectivamente, arvores e tendas, e e avaliado para verdade se for possivel
estabelecer uma relacao em que existe uma e uma unica tenda para cada arvore nas suas
vizinhancas*/
valida(LArv, LTen) :-
    %Verifica que ha o mesmo numero de arvores e tendas
    length(LArv, NumeroAT),
    length(LTen, NumeroAT),
    % Verifica que nao ha nenhuma posicao que contenha uma arvore e uma tenda
    findall(Pos, (member(Pos, LArv), member(Pos, LTen)), Repetidos),
    length(Repetidos, 0),
    arvoreUmaTenda(LArv, LTen, LArv).

/* arvoreUmaTenda(LArv, LTen, LArv) Verifica se cada arvore tem exatamente uma tenda associada a ela*/
arvoreUmaTenda([], _, _).
arvoreUmaTenda([Arvore | RestoArvores], LTen, LArv) :- % Caso so haja uma entao essa tenda fica associada a arvore
    % Encontra todas as tendas na vizinhanca de uma arvore
    findall(TendaVizinhanca, 
        (vizinhanca(Arvore, Vizinhanca),
        member(TendaVizinhanca, Vizinhanca),
        member(TendaVizinhanca, LTen)),
        TendasVizinhasArvore),
    length(TendasVizinhasArvore, 1),
    arvoreUmaTenda(RestoArvores, LTen, LArv).
arvoreUmaTenda([Arvore | RestoArvores], LTen, LArv) :- % Caso haja mais que uma tenda na vizinhanca da arvore
    % Encontra todas as tendas na vizinhanca da arvore
    findall(TendaVizinhanca, 
        (vizinhanca(Arvore, VizinhancaArvore),
        member(TendaVizinhanca, VizinhancaArvore),
        member(TendaVizinhanca, LTen)),
        TendasVizinhasArvore),
    \+ length(TendasVizinhasArvore, 0),
    maplist(vizinhanca, TendasVizinhasArvore, VizinhancasTendas), % Encontra a vizinhanca das tendas vizinhas da arvore
    maplist(intersection(LArv), VizinhancasTendas, ArvoresVizTendas), % Encontra as arvores vizinhas dessas tendas
    include(menos2Elementos, ArvoresVizTendas, UnicaArvoreVizTenda), % Descobre quais dessas tendas so tem uma arvore na vizinhanca
    length(UnicaArvoreVizTenda, NumTendasComUmaArvoreVizinha),
    NumTendasComUmaArvoreVizinha =< 1, % Se for mais que uma, falha
    arvoreUmaTenda(RestoArvores, LTen, LArv).

/*menos2Elementos(Lista) Verifica se uma lista contem 0 ou 1 elementos*/
menos2Elementos(Lista) :-
    length(Lista, Tam),
    Tam < 2.

/*resolve(Puzzle) e verdade se Puzzle e um puzzle que, apos a aplicacao do predicado, fica
resolvido.*/
resolve(Puzzle) :-
    Puzzle = (Tabuleiro, _, _),
    copy_term(Tabuleiro, TabuleiroInicial),
    todasCelulas(TabuleiroInicial, CelulasVaziasInicio, _), 
    length(CelulasVaziasInicio, VaziasInicio), % Descobre o numero de espacos vazios no inicio
    inacessiveis(Tabuleiro),
    relva(Puzzle),
    unicaHipotese(Puzzle),
    aproveita(Puzzle),
    limpaVizinhancas(Puzzle),
    todasCelulas(Tabuleiro, CelulasVaziasFim, _),
    length(CelulasVaziasFim, VaziasFim), % Descobre o numero de espacos vazios no fim
    \+ VaziasInicio =:= VaziasFim, % Se forem diferentes tenta validar
    validaPuzzle(Puzzle).
resolve(Puzzle) :- % Se nao forem diferentes mete uma tenda numa posicao e tenta resolver esse puzzle
    Puzzle = (Tabuleiro, _, _),
    todasCelulas(Tabuleiro, CelulasVazias, _),
    nth1(1, CelulasVazias, CelulaVazia),
    insereObjectoCelula(Tabuleiro, t, CelulaVazia),
    validaPuzzle(Puzzle).
resolve(Puzzle) :- % Caso nao funcione com uma tenda nessa posicao, coloca uma relva nessa posicao
    Puzzle = (Tabuleiro, _, _),
    todasCelulas(Tabuleiro, CelulasVazias, _),
    nth1(1, CelulasVazias, CelulaVazia),
    insereObjectoCelula(Tabuleiro, r, CelulaVazia),
    resolve(Puzzle).

/*validaPuzzle(Puzzle) Verifica que um puzzle esta resolvido e caso nao esteja tenta resolver o puzzle*/
validaPuzzle(Puzzle) :- % Caso o puzzle seja valido
    Puzzle = (Tabuleiro, _, _),
    todasCelulas(Tabuleiro, LArv, a),
    todasCelulas(Tabuleiro, LTen, t),
    valida(LArv, LTen),
    relva(Puzzle).
validaPuzzle(Puzzle) :- % Caso o puzzle seja invalido
    Puzzle = (Tabuleiro, _, _),
    todasCelulas(Tabuleiro, CelulasVazias, _),
    \+ length(CelulasVazias, 0),
    resolve(Puzzle).
