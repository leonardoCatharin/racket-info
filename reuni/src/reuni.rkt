#lang racket

(provide horario
         intervalo
         intervalo-intersecao
         intervalo-vazio		
         intervalo-vazio?
         encontrar-dispo-em-comum
         encontrar-dispo-semana-em-comum
         main)

;estruturas
(struct horario (h m) #:transparent)
(struct intervalo (inicio fim) #:transparent)
(define intervalo-vazio (void))
(define (intervalo-vazio? inter) (equal? inter intervalo-vazio))
;-------------------------------------------------------------

;tratamento dos arquivos

;-Funções split
;Separa o intervalo no caracter '-'
(define (intervalo-separado intervalo)(string-split intervalo "-"))

;o que faz?
(define (separa-input input) (string-split input " "))

;;Separa a string horário no caracter ':'
(define (separa-horario horario)(string-split horario ":"))
;---------------------------------

;-Funções de manipulação dos dados
;;Constroi o horário apartir da string passada
(define (string-para-horario string)
  (horario
   (string->number(first (separa-horario string)))
   (string->number(first (rest (separa-horario string))))))

;;concatena o caminho do arquivo ao nome do arquivo passado no parâmetro arq
(define (arquivos-com-extensao list)
  (cond
    [(empty? list) list]
    [else (cons (string-append "../testes/" (first list))  (arquivos-com-extensao (rest list)))]))

;;o que ela faz????????
(define (insere lst x)
  (cond
    [(empty? lst)(list x)]
    [else (cons (first lst)(insere (rest lst) x))]))

;; Retorna a lista formatada com horários e seu respectivo dia.
(define (lista-de-intervalos-com-dia lista dia)(cons dia lista))

;; Transforma '( '((horario "08" "30") (horario "10" "30")) '((horario "14" "03") (horario "16" "00")) '((horario "17" "10") (horario "18" "10")))
;; em '((intervalo (horario "08" "30") (horario "10" "30")) (intervalo (horario "14" "03") (horario "16" "00")) (intervalo (horario "17" "10") (horario "18" "10")))
(define (lista-de-intervalos list)
 (cond
   [(empty? list) list]
   [else (cons (intervalo (first (first list)) (first (rest (first list)))) (lista-de-intervalos (rest list)) )]))

;O q faz?
(define (lista-de-horarios list)
  (cond
    [(empty? list) list]
    [else (cons (string-para-horario (first list)) (lista-de-horarios (rest list)))]))

;O q faz?
(define (lista-de-pre-intervalos list)
  (cond
    [(empty? list) list]
    [else (cons (intervalo-separado (first list)) (lista-de-pre-intervalos (rest list)))]))

;O que faz???
(define (lista-de-pre-intervalos-com-horario list) 
  (cond
    [(empty? list) list]
    [else (cons (lista-de-horarios (first list)) (lista-de-pre-intervalos-com-horario (rest list)))]))

;o q faz?
(define (lista-com-dia linha) (string-split linha " "))

;o q faz?
(define (lista-sem-dia lista) (rest lista))

;; Retorna linha formatada.
;;qual a estrutura de retorno????????
(define (formata-linha linha)
  (lista-de-intervalos-com-dia( lista-de-intervalos ( lista-de-pre-intervalos-com-horario ( lista-de-pre-intervalos ( lista-sem-dia ( lista-com-dia linha))))) (first (lista-com-dia linha))))

;; Lê o arquivo inteiro e formata todas as entradas.
(define (lista-com-todos-os-dias-formatados descritor lista)
  (define linha (read-line descritor))
  (cond
    [(eof-object? linha) lista]
    [else (lista-com-todos-os-dias-formatados descritor (insere lista (formata-linha linha)))]))

;;recebe lista de arquivos.
(define (recebe-lista-de-arquivos lista)
  (cond
    [(empty? lista) empty]
    [else (cons (lista-com-todos-os-dias-formatados (open-input-file (first lista)) '())
          (recebe-lista-de-arquivos (rest lista)))]))
;-------------------------------------------------------------------------

;Funções de cálculo e filtragem

;;é usada na função abaixo
;;constroi o horário inicial dado um intervalo x
(define (horario-inicial-construido x)
  (let ([hora (horario-h(intervalo-inicio x))]
        [minuto (horario-m(intervalo-inicio x))]
    )
    (horario hora minuto)))

;;Encontra o maior horário inicial entre os intervalos a e b
(define (max-intervalo-inicial a b)
  (let ([hora-intervalo-inicio-a (horario-h(intervalo-inicio a))]
        [hora-intervalo-inicio-b (horario-h(intervalo-inicio b))]
        [minuto-intervalo-inicio-a (horario-m(intervalo-inicio a))]
        [minuto-intervalo-inicio-b (horario-m(intervalo-inicio b))]
       )
    (cond
      [(> hora-intervalo-inicio-a hora-intervalo-inicio-b) (horario-inicial-construido a)]
      [(equal? hora-intervalo-inicio-a hora-intervalo-inicio-b)
       (cond
         [(>= minuto-intervalo-inicio-a minuto-intervalo-inicio-b) (horario-inicial-construido a)]
         [else (horario-inicial-construido b)]
       )]
      [else (horario-inicial-construido b)])))

;;é usada na função abaixo
;;constroi o horário final dado um intervalo x
(define (horario-final-construido x)
  (horario (horario-h(intervalo-fim x)) (horario-m(intervalo-fim x))))

;;Encontra o menor horário final entre os intervalos a e b
(define (min-intervalo-final a b)
  (cond
    [(< (horario-h(intervalo-fim a)) (horario-h(intervalo-fim b)))
       (horario-final-construido a)]
    [(equal? (horario-h(intervalo-fim a)) (horario-h(intervalo-fim b)))
       (cond
         [(<= (horario-m(intervalo-fim a)) (horario-m(intervalo-fim b)))
          (horario-final-construido a)]
         [else (horario-final-construido b)]
       )]
    [else (horario-final-construido b)]))

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
    [else #f]))

;; Intervalo, Intervalo -> Intervalo
;; Calcula a interseção entre os intervalos a e b
(define (intervalo-intersecao a b)
  (cond
    [(horario-final-e-maior-inicial? (max-intervalo-inicial a b) (min-intervalo-final a b))
     (intervalo (max-intervalo-inicial a b) (min-intervalo-final a b))
    ]
  )
)

;;verifica se no intervalo disposto pode ser feito a reunião.
;;se o tempo do intervalo é maior que o tempo da reunião retorne o intervalo.
(define (intervalo-valido? intervalo tempo)
  (let ([hora-fim-intervalo (horario-h (intervalo-fim intervalo))]
        [minuto-fim-intervalo (horario-m(intervalo-fim intervalo))]
        [hora-inicio-intervalo (horario-h(intervalo-inicio intervalo))]
        [minuto-inicio-intervalo (horario-m(intervalo-inicio intervalo))]
        [hora-tempo (horario-h tempo)]
        [minuto-tempo (horario-m tempo)]
       )
   (cond
      [(positive? (- (- hora-fim-intervalo hora-inicio-intervalo) hora-tempo )) #t ]
      [(zero? (- (- hora-fim-intervalo hora-inicio-intervalo) hora-tempo ))
       (cond
        [(positive? (- (- minuto-fim-intervalo minuto-inicio-intervalo) minuto-tempo) ) #t ]
        [(zero? (- (- minuto-fim-intervalo minuto-inicio-intervalo) minuto-tempo) ) #t ]
        [else #f]
       )
      ]
      [else #f]
   )))

;;'aplaina' as listas para a penas uma lista de intervalos
;;função vista em sala
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
               (intervalo-intersecao a b))))))

;;Verifica se a lista tem o dia passado pelo 
(define (tem-o-dia? dia list)
  (cond
    [(empty? list) #f]
    [(equal? (first (first list)) dia) #t]
    [else (tem-o-dia? dia (rest list))]
  ))

(define (retorna-lista-do-dia dia list)
  (cond
    [(empty? list) list]
    [(equal? (first (first list)) dia) (first list)]
    [else (retorna-lista-do-dia dia (rest list))]
  ))

(define (remove-dia list) (map (λ (list-item)(rest list-item)) list))

(define (pessoas-com-o-dia dia lista) (filter (λ (pessoa)(tem-o-dia? dia pessoa))  lista))

(define (normalize list) (rest (rest list)))

(define (pega-interseccoes lst acc)
  (cond
    [(empty? lst) acc]
    [else (pega-interseccoes (rest lst) (cons (first (first lst)) (list (encontrar-dispo-em-comum  (first (rest (first lst))) (first (rest acc))) )))]
  ))

(define (encontrar-dispo-semana-em-comum tempo dispos)
  (let* 
      (
        [dias (map (λ (dia)(map (λ (lista-dispo-item)(retorna-lista-do-dia dia lista-dispo-item)) (pessoas-com-o-dia dia dispos)))'("dom" "seg" "ter" "qua" "qui" "sex" "sab"))]
        [dias-possiveis (filter (λ (dia)(equal? (length dispos) (length dia) )) dias )]
        [dias-com-dispos (map (λ (dia-dispo)(pega-interseccoes (rest dia-dispo) (first dia-dispo) )) dias-possiveis)]
      )
    (filter
     (λ (dia-resultado) (not (empty? (first (rest dia-resultado)))))
     (map 
      (λ (dia)
        (cons (first dia) (list (filter (λ (intervalo)(intervalo-valido? intervalo tempo))(first (rest dia)))))
        ) dias-com-dispos) 
     )))

;--------------------------------------------------------------------------


;; A saída desta função é a escrita na tela dos intervalos em comum que
;; foram encontrados. O formato da saída deve ser o mesmo da disponibilidade
;; semanal.

(define (identidade args) args)
(define (string-horario hora minuto) (string-append hora (string-append ":" minuto)))
(define (zero-formatado-ou-valor valor) (if (equal? 0 valor) "00" (if (<= valor 9) (string-append "0" (number->string valor)) (number->string valor))) )

(define (intervalo->string intervalo)
  (let
    ([hora-inicio (zero-formatado-ou-valor (horario-h (intervalo-inicio intervalo)))]
     [minuto-inicio (zero-formatado-ou-valor (horario-m (intervalo-inicio intervalo)))]
     [hora-fim (zero-formatado-ou-valor (horario-h (intervalo-fim intervalo)))]
     [minuto-fim (zero-formatado-ou-valor (horario-m (intervalo-fim intervalo)))]
     )
    (string-append (string-horario hora-inicio minuto-inicio) (string-append "-" (string-horario hora-fim minuto-fim)))
  )
)

(define (main args)
  (let*
      (
       [horario (string-para-horario (first args))]
       [dispos (recebe-lista-de-arquivos (rest args))]
       [dispos-formatados ( map (λ (dispo)( map (λ (dispo-dia) (cons (first dispo-dia) (list (rest dispo-dia))) ) dispo) )  dispos)]
       [dispos-separados (map (λ (val) ( string-append (first val) (foldl (λ (intervalo ac) (string-append (string-append ac " ") (intervalo->string intervalo) ) ) "" (first (rest val)) ) ) ) (encontrar-dispo-semana-em-comum horario dispos-formatados))]
       )
    
    (
     printf (foldr (λ (dispo-dia accumulator) (string-join (list dispo-dia accumulator) "~%")) "" dispos-separados)
     )
    )
)
;; (main (list "00:01" "../testes/a" "../testes/b"))