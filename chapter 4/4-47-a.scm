(define nouns '(noun student professor cat class))

(define verbs '(verb studies lectures eats sleeps))

(define articles '(article the a))

(define prepositions '(prep for to in by with))

(define (parse-sentence)
	(list 'sentence 
		(parse-noun-phrase) 
		(parse-verb-phrase)))

(define (parse-prepositional-phrase)
	(list 'prep-phrase 
		(parse-word prepositions) 
		(parse-noun-phrase)))

(define (parse-verb-phrase)
	(amb 
		(parse-word verbs)
		(list 'verb-phrase
			(parse-verb-phrase)
			(parse-prepositional-phrase))))

(define (parse-simple-noun-phrase)
	(list 'simple-noun-phrase
		(parse-word articles)
		(parse-word nouns)))

(define (parse-noun-phrase)
	(amb 
		(parse-simple-noun-phrase)
		(list 'noun-phrase
			(parse-simple-noun-phrase)
			(parse-prepositional-phrase))))

(define (parse-word word-list)
	(require (not (null? *unparsed*)))
	(require (memq (car *unparsed*) (cdr word-list)))
	(let ((found-word (car *unparsed*)))
		(set! *unparsed* (cdr *unparsed*))
		(list (car word-list) found-word)))

(define (require p)
	(if (not p) (amb)))

(define *unparsed* '())

(define (parse input)
	(set! *unparsed* input)
	(let ((sent (parse-sentence)))
		(require (null? *unparsed*))
		sent))

(parse '(the professor lectures to the student in the class with the cat))