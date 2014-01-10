(define nouns '(noun student professor cat class))

(define verbs '(verb studies lectures eats sleeps))

(define articles '(article the a))

(define prepositions '(prep for to in by with))

(define adjectives '(adjective gray tired nice))

(define (parse-sentence)
	(list 'sentence 
		(parse-noun-phrase) 
		(parse-verb-phrase)))

(define (parse-prepositional-phrase)
	(list 'prep-phrase 
		(parse-word prepositions) 
		(parse-noun-phrase)))

(define (parse-verb-phrase)
	(define (maybe-extend verb-phrase)
		(amb verb-phrase (maybe-extend 
							(list 'verb-phrase 
								verb-phrase
								(parse-prepositional-phrase)))))
	(maybe-extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
	(list 'simple-noun-phrase
		(parse-word articles)
		(parse-word nouns)))

(define (parse-noun-phrase)
	(define (maybe-extend noun-phrase)
		(amb noun-phrase (maybe-extend 
							(list 'noun-phrase
								noun-phrase
								(parse-prepositional-phrase)))))
	(maybe-extend (parse-simple-noun-phrase)))

(define (parse-word word-list)
	(require (not (null? *unparsed*)))
	(set! *unparsed* (cdr *unparsed*))
	(list (car word-list) (rchoose (cdr word-list))))

(define (require p)
	(if (not p) (amb)))

(define *unparsed* '())

(define (parse input)
	(set! *unparsed* input)
	(let ((sent (parse-sentence)))
		(require (null? *unparsed*))
		sent))

(define (nth n l)
	(if (eq? 0 n)
		(car l)
		(nth (- n 1) (cdr l))))

(define (rest a l)
	(if (eq? a (car l))
		(cdr l)
		(cons (car l) (rest a (cdr l)))))

(define (rchoose l)
	(let ((n (random (length l))))
		(nth n l)))

(parse '(the professor lectures to the student in the class with the cat))