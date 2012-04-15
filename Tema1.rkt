;returneaza o lista cu elementele @e1 @e2 @e3
(define combine 
  (lambda(e1 e2 e3)
    (cons e1 (cons e2 (cons e3 '())))))
;returneaza o lista cu elemntele @e1 @e2
(define small_combine 
  (lambda(e1 e2)
    (cons e1 (cons e2 '()))))

;inlocuieste in expresia @t_body elementele @x_arg cu aplicatia @s_arg
(define repclr
  (lambda(x_arg t_body s_app)
    (if (list? t_body)
        (if (= (length t_body) 3)
            (if (equal? (cadr t_body) x_arg)
                t_body
                (combine (car t_body) (cadr t_body) (repclr x_arg (caddr t_body) s_app)))
            (if (= (length t_body) 2)
                (small_combine (repclr x_arg (car t_body) s_app) (repclr x_arg (cadr t_body) s_app))
                (repclr x_arg (car t_body) s_app)))
        (if (equal? t_body x_arg)
            s_app
            t_body))))

;returneza perechea generata a unei variabile din sita de perechi @l
(define search
  (lambda(x l)
    (if (eqv? x (car (car l)))
        (cdr (car l))
        (search x (cdr l)))))

;genereaza o lista de perechi cu numele variabilelor respectiv alta vriabila generata
(define gen_vars
  (lambda(l)
    (if (null? l)
        '()
        (cons (cons (car l) (gensym 'g)) (gen_vars (cdr l))))))

;acopera cazurile pentru alfa conversie
(define redem 
  (lambda(func l gen_v)
    (if (list? func)
        (if (= (length func) 3)
            (if (list? (member (cadr func) l))
                (combine (car func) (search (cadr func) gen_v) (redem (caddr func) l gen_v))
                (combine (car func) (cadr func) (redem (caddr func) l gen_v)))
            (if (= (length func) 2)
                (small_combine (redem (car func) l gen_v) (redem (cadr func) l gen_v))
                (redem (car func) l gen_v)))
        (if (list? (member func l))
            (search func gen_v)
            func))))
;mai face o verificare pentru aplicatie
(define redenumire 
  (lambda(func)
    (if (list? (cadr func))
        (let ((g (gen_vars (cadr func)))) (small_combine (redem (car func) (cadr func) g) (cadr func))) 
        (let ((g (gen_vars (list (cadr func))))) (small_combine (redem (car func) (list (cadr func)) g) (cadr func))))))

;verifica daca treuie alfa conversie
(define check?
  (lambda(lst)
    (if (list? lst)
        (if (null? lst)
            #t
            (if (or (list? (car lst)) (equal? (car lst) 'lambda0))
                #f
                (check? (cdr lst))))
        #t)))


;efectueaza beta reducerea
(define beta_red
  (lambda(func)
    (if (list?  func)   
        (if (= (length func) 3)
            (combine (car func) (cadr func) (beta_red (caddr func)))
            (if (= (length func) 2)
                (cond ((not (list? (car func)))
                       (if (list? (cdr func))
                           (small_combine (car func) (beta_red (cadr func))) 
                           func))
                      ((= (length (car func)) 3)
                       (repclr (cadar func) (caddar func) (cadr func)))
                      ((= (length (car func)) 2)
                       (small_combine (beta_red (car func)) (beta_red (cadr func)))))
                (beta_red (car func))))
        func)))


;functie recursiva pentru alfa echivalenta
(define reducer 
  (lambda(exp)
    (if (equal?  (beta_red exp) exp)
        exp
        (reducer (beta_red exp)))))
;functia principala
(define eval0
  (lambda(func)
    (if (and (= (length func) 2) (check? (cadr func)))
        (reducer (redenumire func))
        (reducer func))))

