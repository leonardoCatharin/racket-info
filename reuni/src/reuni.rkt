#lang racket

;; Este programa encontra horários disponíveis que sejam comuns entre vários
;; horários especificados e que tenham um tamanho mínimo especificado.
;;
;; ** Conceitos **
;;  Horário
;;    Um momento no tempo, definido em termos da hora e minutos
;;  Intervalo (abreviado inter)
;;    Um intervalo no tempo, tem um horário de início e um horário de fim
;;  Disponibilidade do dia (abreviado dispo)
;;    Uma lista de intervalos que estão disponíveis em um determinado dia
;;  Disponibilidade semanal (abreviado dispo-semana)
;;    Uma lista com as disponibilidades de cada dia
;;  Lista de associações
;;    Uma lista de pares. Um par é uma lista com dois elementos. O primeiro
;;    elemento do par é chamado de chave e o segundo elemento é chamado de
;;    valor. Uma lista de associações é uma maneira simples de implementar uma
;;    tabela associativa (dicionário).  Ex: o dicionário
;;    1 -> 4, 20 -> 12, 6 -> 70, pode ser representado pela lista associativa
;;    (list (list 1 4) (list 20 12) (list 6 70)).
;;    A função assoc é utilizada para consultar uma lista associativa.
;;
;; ** Formatação de entrada e saída **
;; Toda operação de entrada e saída deve ser feita respeitando essas
;; formatações. A sua implementação não precisa validar as entradas. Para os
;; testes automatizados as entradas sempre serão válidas.
;;
;;  Horário (HH:MM) (sempre 5 dígitos)
;;  Exemplos
;;     08:30 =  8 horas e 30 minutos
;;     12:07 = 12 horas e  7 minutos
;;
;;  Intervalo (HH:MM-HH:MM) (sempre 11 dígitos)
;;  Exemplos
;;     08:30-12:07 = o intervalo tem início às 8 horas e 30 minutos e tem
;;                   o fim às 12 horas e 7 minutos
;;
;;  Dias da semana
;;    Representados por strings de tamanho 3: dom seg ter qua qui sex sab
;;
;;  Disponibilidade semanal
;;    Uma sequência de linhas. Cada linha contém o dia e a lista de
;;    intervalos disponíveis naquele dia
;;  Exemplo
;;    ter 10:20-12:00 16:10-17:30
;;    sex 08:30-11:30
;;  Observe que nem todos os dias devem estar especificados. Os dias
;;  que não têm disponibilidades não devem ser especificados.


;; exporta as funções que podem ser utilizadas em outros arquivos
(provide horario
         intervalo
         intervalo-vazio
         intervalo-vazio?
         intervalo-intersecao
         encontrar-dispo-em-comum
         encontrar-dispo-semana-em-comum
         main)

(struct horario (h m) #:transparent)
;; Horário representa um momento no tempo, definido em termos da hora e minutos
;;    h : Número - horas
;;    m : Número - minutos

(struct intervalo (inicio fim) #:transparent)
;; Intervalo representa um intervalo no tempo, tem um horário de início e um
;; horário de fim
;;    inicio : Horário - horário de início
;;       fim : Horário - horário de fim

;; Constante que define um intervalo vazio
(define intervalo-vazio (void))

;; Intervalo -> bool
;; Retorna #t se inter representa o intervalo vazio, #f caso contrário
(define (intervalo-vazio? inter)
  (equal? inter intervalo-vazio))

;; Intervalo, Intervalo -> Intervalo
;; Calcula a interseção entre os intervalos a e b
(define (intervalo-intersecao a b)
  (error "Não implementado"))

;; list Intervalo, list Intervalo -> list Intervalo
;; Encontra a interseção dos intervalos de dispo-a e dispo-b.
(define (encontrar-dispo-em-comum dispo-a dispo-b)
  (error "Não implementado"))

;; Horário, list dispo-semana -> dispo-semana
;; Esta função encontra os intervalos disponíveis para cada dia da semana que
;; sejam maiores que tempo e que sejam comuns a todas as disponibilidades
;; da lista dispos.
;;
;; dispo-semana é uma lista de associações entre um dia (string) e a
;; disponibilidade naquele dia. Veja a definição de lista de associações no
;; início deste arquivo.
;;
;; Por exemplo, a disponibilidade semanal (dispo-semana):
;; ter 10:20-12:00 16:10-17:30
;; sex 08:30-11:30
;; é representada da seguinte maneira:
;; (list (list "ter" (list (intervalo (hora 10 20) (hora 12 00))
;;                         (intervalo (hora 16 10) (hora 17 30))))
;;       (list "sex" (list (intervalo (hora 08 30) (hora 11 30)))))
;;
;; Observe que esta função recebe como parâmetro uma lista de disponibilidades
;; semanais, o exemplo acima refere-se a apenas uma disponibilidade semanal.
;; Veja os testes de unidade para exemplos de entrada e saída desta função
(define (encontrar-dispo-semana-em-comum tempo dispos)
  (error "Não implementado"))

;; list string -> void
;; Esta é a função principal. Esta função é chamada a partir do arquivo
;; reuni-main.rkt
;;
;; args é a lista de parâmetros para o programa.
;;
;; O primeiro parâmetro é o tempo mínimo (string) que os intervalos em comum
;; devem ter. O tempo mínimo é especificado usando a formatação de horário.
;;
;; O restante dos parâmetros são nomes de arquivos. Cada arquivo de entrada
;; contêm uma disponibilidade semanal. Veja exemplos de arquivos no diretórios
;; testes.
;;
;; A saída desta função é a escrita na tela dos intervalos em comum que
;; foram encontrados. O formato da saída deve ser o mesmo da disponibilidade
;; semanal.
(define (main args)
  (error "Não implementado"))
;; Atribui para in o arquivo

;; [duração] '(arquivos) 


(define (insere lst x)
  (cond
    [(empty? lst)(list x)]
    [else (cons (first lst)(insere (rest lst) x))]
  ))

;; Transforma a linha em uma lista que irá conter o dia
(define (lista-com-dia linha) (string-split linha " "))

;; Transforma a lista com dia em uma lista sem o dia
(define (lista-sem-dia lista) (rest lista))
;; ---------------------------------------------------------------------------
;; Separa os intervalos em uma lista de strings no formato '("08:30" "10:30")
(define (intervalo-separado intervalo)(string-split intervalo "-"))

;; Separa a string horário em uma lista de strings no formato '("08" "30")
(define (separa-horario horario)(string-split horario ":"))
;; Separa o input do usuário em uma lista de strings no formato '(duração [arquivos])
(define (separa-input input) (string-split input " "))
;; ---------------------------------------------------------------------------
;; Transforma a lista '("08" "30") em '(horario "08" "30")
(define (string-para-horario string)
  (horario
   (string->number(first (separa-horario string)))
   (string->number(first (rest (separa-horario string))))
  )
)



;; Transforma ("08:30" "10:30") em '((horario "08" "30") (horario "10" "30"))
(define (lista-de-horarios list)
  (cond
    [(empty? list) list]
    [else (cons (string-para-horario (first list)) (lista-de-horarios (rest list)))]
   )
)

;; Transforma '("08:30-10:30" "14:03-16:00" "17:10-18:10") em '(("08:30" "10:30") ("14:03" "16:00") ("17:10" "18:10"))
(define (lista-de-pre-intervalos list)
  (cond
    [(empty? list) list]
    [else (cons (intervalo-separado (first list)) (lista-de-pre-intervalos (rest list)))]
  )
)
;; Transforma '(("08:30" "10:30") ("14:03" "16:00") ("17:10" "18:10"))
;; em '( '((horario "08" "30") (horario "10" "30")) '((horario "14" "03") (horario "16" "00")) '((horario "17" "10") (horario "18" "10")))
(define (lista-de-pre-intervalos-com-horario list) 
  (
   cond
    [(empty? list) list]
    [else (cons (lista-de-horarios (first list)) (lista-de-pre-intervalos-com-horario (rest list)))]
  )
)

;; Transforma '( '((horario "08" "30") (horario "10" "30")) '((horario "14" "03") (horario "16" "00")) '((horario "17" "10") (horario "18" "10")))
;; em '((intervalo (horario "08" "30") (horario "10" "30")) (intervalo (horario "14" "03") (horario "16" "00")) (intervalo (horario "17" "10") (horario "18" "10")))
(define (lista-de-intervalos list)
 (
  cond
   [(empty? list) list]
   [else (cons (intervalo (first (first list)) (first (rest (first list)))) (lista-de-intervalos (rest list)) )]
 )
)

;; Retorna a lista formatada com horários e seu respectivo dia.
(define (lista-de-intervalos-com-dia lista dia)(cons dia lista))

;; Retorna linha formatada.
(define (formata-linha linha)
  (
    lista-de-intervalos-com-dia(
                                lista-de-intervalos (
                                                     lista-de-pre-intervalos-com-horario (
                                                                                          lista-de-pre-intervalos (
                                                                                                                   lista-sem-dia (
                                                                                                                                  lista-com-dia linha))))) (first (lista-com-dia linha))
  )
)
;; Lê o arquivo inteiro e formata todas as entradas.
(define (lista-com-todos-os-dias-formatados descritor lista)
  (define linha (read-line descritor))
  (cond
    [(eof-object? linha) lista]
    [else (lista-com-todos-os-dias-formatados descritor (insere lista (formata-linha linha)))]
  )
)

;;recebe lista de arquivos.
(define (recebe-lista-de-arquivos lista)
  (cond
    [(empty? lista) empty]
    [else (cons (lista-com-todos-os-dias-formatados (open-input-file (first lista)) '())
          (recebe-lista-de-arquivos (rest lista)))]
  )
)

(define (arquivo-com-caminho arq) (string-append "../testes/" arq))


(define (arquivos-com-extensao list)
  (
   cond
    [(empty? list) list]
    [else (cons (arquivo-com-caminho (first list))  (arquivos-com-extensao (rest list)))]
  )
)

(define input (read-line))

;; Informações da Reunião ---------------------------------------------------------

(string-para-horario (first (separa-input input)))
(recebe-lista-de-arquivos (arquivos-com-extensao (rest (separa-input input))))

;; --------------------------------------------------------------------------------