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

;;é usada na função abaixo
;;constroi o horário inicial dado um intervalo x
(define (horario-inicial-construido x)
  (horario (horario-h(intervalo-inicio x)) (horario-m(intervalo-inicio x)))
)

;;Encontra o maior horário inicial entre os intervalos a e b
(define (max-intervalo-inicial a b)
  (cond
    [(> (horario-h(intervalo-inicio a)) (horario-h(intervalo-inicio b)))
       (horario-inicial-construido a)
    ]
    [(equal? (horario-h(intervalo-inicio a)) (horario-h(intervalo-inicio b)))
       (cond
         [(>= (horario-m(intervalo-inicio a)) (horario-m(intervalo-inicio b)))
          (horario-inicial-construido a)
         ]
         [else (horario-inicial-construido b)]
       )
    ]
    [else (horario-inicial-construido b)]
   )
)

;;é usada na função abaixo
;;constroi o horário final dado um intervalo x
(define (horario-final-construido x)
  (horario (horario-h(intervalo-fim x)) (horario-m(intervalo-fim x)))
)

;;Encontra o menor horário final entre os intervalos a e b
(define (min-intervalo-final a b)
  (cond
    [(< (horario-h(intervalo-fim a)) (horario-h(intervalo-fim b)))
       (horario-final-construido a)
    ]
    [(equal? (horario-h(intervalo-fim a)) (horario-h(intervalo-fim b)))
       (cond
         [(<= (horario-m(intervalo-fim a)) (horario-m(intervalo-fim b)))
          (horario-final-construido a)
         ]
         [else (horario-final-construido b)]
       )
    ]
    [else (horario-final-construido b)]
   )
)

;;Verifica se o horário final é maoir que o inicial
(define (horario-final-e-maior-inicial? inicial final)
  (cond
    [(> (horario-h final) (horario-h inicial)) #t]
    [(equal? (horario-h final) (horario-h inicial))
     (cond
       [(> (horario-m final) (horario-m inicial)) #t]
       [else #f]
     )
    ]
    [else #f]
  )
)

;; Intervalo, Intervalo -> Intervalo
;; Calcula a interseção entre os intervalos a e b
(define (intervalo-intersecao a b)
  (cond
    [(horario-final-e-maior-inicial? (max-intervalo-inicial a b) (min-intervalo-final a b))
     (intervalo (max-intervalo-inicial a b) (min-intervalo-final a b))
    ]
  )
)

;;verifica se no intervalo disposto pode ser feito a reunião (se o tempo do intervalo é maior que o tempo da reunião retorne o intervalo)
(define (intervalo-valido intervalo tempo)
  (cond
    [(positive? (- (- (horario-h(intervalo-fim intervalo)) (horario-h(intervalo-inicio intervalo))) (string->number(first(separa-horario tempo))) )) intervalo ]
    [(zero? (- (- (horario-h(intervalo-fim intervalo)) (horario-h(intervalo-inicio intervalo))) (string->number(first(separa-horario tempo))) ))
     (cond
       [(positive? (- (- (horario-m(intervalo-fim intervalo)) (horario-m(intervalo-inicio intervalo))) (string->number(first(rest(separa-horario tempo)))) )) intervalo ]
       [(zero? (- (- (horario-m(intervalo-fim intervalo)) (horario-m(intervalo-inicio intervalo))) (string->number(first(rest(separa-horario tempo)))) )) intervalo ]
       )
     ]
  )
)
;;'aplaina' as listas para a penas uma lista de intervalos 
(define (aplaina lst)
  (cond
    [(empty? lst) empty]
    [(list? (first lst))
     (append (aplaina (first lst))
             (aplaina (rest lst)))]
    [else
     (cons (first lst)
           (aplaina (rest lst)))]))
;; list Intervalo, list Intervalo -> list Intervalo
;; Encontra a interseção dos intervalos de dispo-a e dispo-b.
(define (encontrar-dispo-em-comum dispo-a dispo-b)
  (filter intervalo?
          (aplaina
           (for/list ([a dispo-a])
             (for/list ([b dispo-b])
               (intervalo-intersecao a b)
               )
             )
           )
          )
  )

(define dispo-b (list (intervalo (horario 08 32) (horario 09 45))
                      (intervalo (horario 10 20) (horario 11 15))
                      (intervalo (horario 13 30) (horario 18 00))))

(define dispo-c (list (intervalo (horario 09 00) (horario 14 21))
                      (intervalo (horario 15 29) (horario 16 12))))

(define dispo-d (list (intervalo (horario 09 00) (horario 09 45))
                      (intervalo (horario 10 20) (horario 11 15))
                      (intervalo (horario 13 30) (horario 14 21))
                      (intervalo (horario 15 29) (horario 16 12))))

;;(encontrar-dispo-em-comum dispo-b dispo-c)

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



(define dispo-semana-a
  (list (list "seg" (list (intervalo (horario 08 30) (horario 10 30))
                          (intervalo (horario 14 03) (horario 16 00))
                          (intervalo (horario 17 10) (horario 18 10))))
        (list "ter" (list (intervalo (horario 13 30) (horario 15 45))))
        (list "qua" (list (intervalo (horario 11 27) (horario 13 00))
                          (intervalo (horario 15 00) (horario 19 00))))
        (list "sex" (list (intervalo (horario 07 30) (horario 11 30))
                          (intervalo (horario 13 30) (horario 14 00))
                          (intervalo (horario 15 02) (horario 16 00))
                          (intervalo (horario 17 20) (horario 18 30))))))
(define dispo-semana-b
  (list (list "seg" (list (intervalo (horario 14 35) (horario 17 58))))
        (list "ter" (list (intervalo (horario 08 40) (horario 10 30))
                          (intervalo (horario 13 31) (horario 15 13))))
        (list "qui" (list (intervalo (horario 08 30) (horario 15 30))))
        (list "sex" (list (intervalo (horario 14 07) (horario 15 00))
                          (intervalo (horario 16 00) (horario 17 30))
                          (intervalo (horario 19 00) (horario 22 00))))))
(define dispo-semana-c
  (list (list "seg" (list (intervalo (horario 10 00) (horario 12 00))
                          (intervalo (horario 15 30) (horario 17 30))))
        (list "sex" (list (intervalo (horario 10 00) (horario 12 00))
                          (intervalo (horario 15 30) (horario 17 30))))))

(define (tem-o-dia? dia list)
  (cond
    [(empty? list) #f]
    [(equal? (first (first list)) dia) #t]
    [else (tem-o-dia? dia (rest list))]
    )
)
(define (retorna-lista-do-dia dia list)
  (cond
    [(empty? list) list]
    [(equal? (first (first list)) dia) (first list)]
    [else (retorna-lista-do-dia dia (rest list))]
   )
)

(define (remove-dia list)
  (map (λ (list-item)(rest list-item)) list)
)
(define (pessoas-com-o-dia dia lista)
  (filter (λ (pessoa)(tem-o-dia? dia pessoa))  lista)
)
(define (normalize list)
  (rest (rest list))
)

(define (pega-interseccoes lst acc)
  (cond
    [(empty? lst) acc]
    [else (pega-interseccoes (rest lst) (cons (first (first lst)) (list (encontrar-dispo-em-comum  (first (rest (first lst))) (first (rest acc))))))]
   )
  )

(define (encontrar-dispo-semana-em-comum tempo dispos)
  (let* 
      (
        [dias (map (λ (dia)(map (λ (lista-dispo-item)(retorna-lista-do-dia dia lista-dispo-item)) (pessoas-com-o-dia dia dispos)))'("seg" "ter" "qua" "qui" "sex"))]
        [dias-possiveis (filter (λ (dia)(equal? (length dispos) (length dia) )) dias )]
        [dias-com-dispos (map (λ (dia-dispo)(pega-interseccoes (rest dia-dispo) (first dia-dispo))) dias-possiveis)]
      )
      dias-com-dispos
   )
)

(encontrar-dispo-semana-em-comum "00:20" (list dispo-semana-a dispo-semana-b dispo-semana-c))

;;(foldr (λ (dispo result)(
 ;;                                 cond
 ;;                                 [(equal? result 0) dispo]
 ;;                                 [else (encontrar-dispo-em-comum (rest dispo) (rest result))]
 ;;                                 )) 0 dia-dispo)

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
  )
)

(define (lista-com-dia linha) (string-split linha " "))

(define (lista-sem-dia lista) (rest lista))

(define (intervalo-separado intervalo)(string-split intervalo "-"))

(define (separa-horario horario)(string-split horario ":"))

(define (separa-input input) (string-split input " "))

(define (string-para-horario string)
  (horario
   (string->number(first (separa-horario string)))
   (string->number(first (rest (separa-horario string))))
  )
)

(define (lista-de-horarios list)
  (cond
    [(empty? list) list]
    [else (cons (string-para-horario (first list)) (lista-de-horarios (rest list)))]
   )
)

(define (lista-de-pre-intervalos list)
  (cond
    [(empty? list) list]
    [else (cons (intervalo-separado (first list)) (lista-de-pre-intervalos (rest list)))]
  )
)

(define (lista-de-pre-intervalos-com-horario list) 
  (cond
    [(empty? list) list]
    [else (cons (lista-de-horarios (first list)) (lista-de-pre-intervalos-com-horario (rest list)))]
  )
)

;; Transforma '( '((horario "08" "30") (horario "10" "30")) '((horario "14" "03") (horario "16" "00")) '((horario "17" "10") (horario "18" "10")))
;; em '((intervalo (horario "08" "30") (horario "10" "30")) (intervalo (horario "14" "03") (horario "16" "00")) (intervalo (horario "17" "10") (horario "18" "10")))
(define (lista-de-intervalos list)
 (cond
   [(empty? list) list]
   [else (cons (intervalo (first (first list)) (first (rest (first list)))) (lista-de-intervalos (rest list)) )]
 )
)

;; Retorna a lista formatada com horários e seu respectivo dia.
(define (lista-de-intervalos-com-dia lista dia)(cons dia lista))

;; Retorna linha formatada.
(define (formata-linha linha)
  (
    lista-de-intervalos-com-dia( lista-de-intervalos ( lista-de-pre-intervalos-com-horario ( lista-de-pre-intervalos ( lista-sem-dia ( lista-com-dia linha))))) (first (lista-com-dia linha))
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

;;concatena o caminho do arquivo ao nome do arquivo passado no parâmetro arq

(define (arquivos-com-extensao list)
  (cond
    [(empty? list) list]
    [else (cons (string-append "../testes/" (first list))  (arquivos-com-extensao (rest list)))]
  )
)


(define (remove-dias-com-menos-pessoas numero lista) (filter (λ (dia)(equal? numero (length dia))) lista ))


(define lista-de-arquivos (recebe-lista-de-arquivos (arquivos-com-extensao (rest (separa-input (read-line))))))

;(first(rest (first lista-de-arquivos)))
(define segunda (map (λ (dia)(retorna-lista-do-dia "seg" dia)) (pessoas-com-o-dia "seg" lista-de-arquivos)))
;(define terca   (map (λ (dia)(retorna-lista-do-dia "ter" dia)) (lista-com-pessoas-do-dia "ter" lista-de-arquivos)))
;(define quarta  (map (λ (dia)(retorna-lista-do-dia "qua" dia)) (lista-com-pessoas-do-dia "qua" lista-de-arquivos)))
;(define quinta  (map (λ (dia)(retorna-lista-do-dia "qui" dia)) (lista-com-pessoas-do-dia "qui" lista-de-arquivos)))
;(define sexta   (map (λ (dia)(retorna-lista-do-dia "sex" dia)) (lista-com-pessoas-do-dia "sex" lista-de-arquivos)))

;(define todos-os-dias-possiveis (remove-dias-com-menos-pessoas (length lista-de-arquivos) (list segunda terca quarta quinta sexta)))

;todos-os-dias-possiveis